C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/exch_jam.F,v 1.3 2000/03/24 19:24:02 adcroft Exp $

#include "CPP_EEOPTIONS.h"

#ifndef JAM_WITH_TWO_PROCS_PER_NODE
C Single processor JAM stuff

#undef  USE_MPI_EXCH
#define USE_JAM_EXCH

      SUBROUTINE EXCH_XY_O1_R8_JAM( arr )

      IMPLICIT NONE
C     Width 1. Single tile. No X-axis decomp.
C     No. corner update. Exchange.

#define _OLx 1
#define _OLy 1

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EXCH_JAM.h"

#include "MPI_INFO.h"
#include "JAM_INFO.h"

C      == Compile time constants ==

C     == Routine arguments ==
      Real*8 arr(1-_OLx:sNx+_OLx,1-_OLy:sNy+_OLy)

#ifdef LETS_MAKE_JAM

C     == Local variables ==
      INTEGER I, J
      INTEGER northProc, southProc
      INTEGER farProc1, farProc2
      INTEGER toPid, fromPid
      INTEGER rc

#ifdef ALLOW_MPI
      INTEGER mpiStatus(MPI_STATUS_SIZE)
#endif

C     East-west halo update (without corners)
      DO J=1,sNy
       DO I=1,_OLx
        arr(1-I  ,J)     = arr(sNx-I+1,J)
        arr(sNx+I,J)     = arr(1+I-1  ,J)
       ENDDO
      ENDDO
   
C     Phase 1 pairing
C     | 0 |  ---> | 1 |
C     | 0 |  <--- | 1 |

C     | 2 |  ---> | 3 |
C     | 2 |  <--- | 3 |

C     | 4 |  ---> | 5 |
C     | 4 |  <--- | 5 |

C     etc ...             
C    

#ifdef USE_MPI_EXCH
C     North-south halo update (without corners)
C     Put my edges into a buffers
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       DO I=1,sNx
        exchBuf1(I) = arr(I,sNy)
        exchBuf2(I) = arr(I,1  )
       ENDDO
      ELSE
       DO I=1,sNx
        exchBuf1(I) = arr(I,1  )
        exchBuf2(I) = arr(I,sNy)
       ENDDO
      ENDIF

C     Exchange the buffers
      northProc = mpi_northId
      southProc = mpi_southId
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       farProc1 = northProc
       farProc2 = southProc
      ELSE
       farProc1 = southProc
       farProc2 = northProc
      ENDIF
C     Even-odd pairs
      IF ( farProc1 .NE. myProcId ) THEN
       CALL MPI_Sendrecv_replace(exchBuf1,sNx,MPI_REAL8,
     &                           farProc1,0,
     &                           farProc1,MPI_ANY_TAG,
     &                           MPI_COMM_WORLD,mpiStatus,
     &                           rc)
      ENDIF
C     Odd-even pairs
      IF ( farProc2 .NE. myProcId ) THEN
       CALL MPI_Sendrecv_replace(exchBuf2,sNx,MPI_REAL8,
     &                           farProc2,0,
     &                           farProc2,MPI_ANY_TAG,
     &                           MPI_COMM_WORLD,mpiStatus,
     &                           rc)
      ENDIF
#endif

#ifdef USE_JAM_EXCH
      northProc = jam_northId
      southProc = jam_southId
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
C      sendBuf1  = &arr(1,sNy  )
C      recvBuf1  = &arr(1,sNy+1)
C      sendBuf2  = &arr(1,1    )
C      recvBuf2  = &arr(1,0    )
       farProc1 = northProc
       farProc2 = southProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,arr(1,sNy),arr(1,sNy+1),
     &                    sNx*8,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,arr(1,1),arr(1,0),
     &                    sNx*8,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
      ELSE
C      sendBuf1  = &arr(1,1    )
C      recvBuf1  = &arr(1,0    )
C      sendBuf2  = &arr(1,sNy  )
C      recvBuf2  = &arr(1,sNy+1)
       farProc1 = southProc
       farProc2 = northProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,arr(1,1),arr(1,0),
     &                    sNx*8,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,arr(1,sNy),arr(1,sNy+1),
     &                    sNx*8,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
      ENDIF
C     IF ( farProc1 .NE. myProcId ) THEN
C      CALL JAM_EXCHANGE(farProc1,sendBuf1,recvBuf1,sNx*8,jam_exchKey)
C      jam_exchKey = jam_exchKey+1
C     ENDIF
C     IF ( farProc2 .NE. myProcId ) THEN
C      CALL JAM_EXCHANGE(farProc2,sendBuf2,recvBuf2,sNx*8,jam_exchKey)
C      jam_exchKey = jam_exchKey+1
C     ENDIF
#endif

#ifdef USE_MPI_EXCH
C     Fill overlap regions from the buffers
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       DO I=1,sNx
        arr(I,sNy+1) = exchBuf1(I)
        arr(I,0    ) = exchBuf2(I)
       ENDDO
      ELSE
       DO I=1,sNx
        arr(I,sNy+1) = exchBuf2(I)
        arr(I,0    ) = exchBuf1(I)
       ENDDO
      ENDIF
#endif

      IF ( numberOfProcs .EQ. 1 ) THEN
       DO I=1,sNx
        arr(I,sNy+1) = arr(I,1  )
        arr(I,0    ) = arr(I,sNy)
       ENDDO
      ENDIF

      RETURN
      END

      SUBROUTINE EXCH_XY_R8_JAM( arr )

      IMPLICIT NONE

C     Full width. Single tile. No X-axis decomp.
C     exchange.

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH_JAM.h"

#include "MPI_INFO.h"
#include "JAM_INFO.h"

C     == Routine arguments ==
      Real*8 arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     == Local variables ==
      INTEGER I, J
      INTEGER iLo, iHi, I0
      INTEGER northProc, southProc
      INTEGER farProc1, farProc2
      INTEGER toPid, fromPid
      INTEGER rc

#ifdef ALLOW_MPI
      INTEGER mpiStatus(MPI_STATUS_SIZE)
#endif

C     East-west halo update
      DO J=1-OLy,sNy+OLy
       DO I=1,OLx
        arr(1-I  ,J)     = arr(sNx-I+1,J)
        arr(sNx+I,J)     = arr(1+I-1  ,J)
       ENDDO
      ENDDO
   
C     Phase 1 pairing
C     | 0 |  ---> | 1 |
C     | 0 |  <--- | 1 |

C     | 2 |  ---> | 3 |
C     | 2 |  <--- | 3 |

C     | 4 |  ---> | 5 |
C     | 4 |  <--- | 5 |

C     etc ...             
C    

#ifdef USE_MPI_EXCH
C     North-south halo update (including corners)
C     Put my edges into a buffers
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       DO J=1,OLy
        iLo= 1-OLx
        iHi= sNx+OLx
        I0 = (J-1)*(iHi-iLo)+1
        DO I=iLo,iHi
         exchBuf1(I0+I-iLo) = arr(I,sNy-OLy+J)
         exchBuf2(I0+I-iLo) = arr(I,1+J-1    )
        ENDDO
       ENDDO
      ELSE
       DO J=1,OLy
        iLo= 1-OLx
        iHi= sNx+OLx
        I0 = (J-1)*(iHi-iLo)+1
        DO I=iLo,iHi
         exchBuf1(I0+I-iLo) = arr(I,1+J-1    )
         exchBuf2(I0+I-iLo) = arr(I,sNy-OLy+J)
        ENDDO
       ENDDO
      ENDIF

C     Exchange the buffers
      northProc = mpi_northId
      southProc = mpi_southId
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       farProc1 = northProc
       farProc2 = southProc
      ELSE
       farProc1 = southProc
       farProc2 = northProc
      ENDIF

C     Even-odd pairs
      IF ( farProc1 .NE. myProcId ) THEN
       CALL MPI_Sendrecv_replace(exchBuf1,OLy*(sNx+2*OLx),MPI_REAL8,
     &                           farProc1,0,
     &                           farProc1,MPI_ANY_TAG,
     &                           MPI_COMM_WORLD,mpiStatus,
     &                           rc)
      ENDIF
C     Odd-even pairs
      IF ( farProc2 .NE. myProcId ) THEN
       CALL MPI_Sendrecv_replace(exchBuf2,OLy*(sNx+2*OLx),MPI_REAL8,
     &                           farProc2,0,
     &                           farProc2,MPI_ANY_TAG,
     &                           MPI_COMM_WORLD,mpiStatus,
     &                           rc)
      ENDIF

C     Fill overlap regions from the buffers
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       DO J=1,OLy
        iLo= 1-OLx
        iHi= sNx+OLx
        I0 = (J-1)*(iHi-iLo)+1
        DO I=iLo,iHi
         arr(I,sNy+J    ) = exchBuf1(I0+I-iLo)
         arr(I,1-OLy+J-1) = exchBuf2(I0+I-iLo)
        ENDDO
       ENDDO
      ELSE
       DO J=1,OLy
        iLo= 1-OLx
        iHi= sNx+OLx
        I0 = (J-1)*(iHi-iLo)+1
        DO I=iLo,iHi
         arr(I,sNy+J     ) = exchBuf2(I0+I-iLo)
         arr(I,1-OLy+J-1 ) = exchBuf1(I0+I-iLo)
        ENDDO
       ENDDO
      ENDIF
#endif

#ifdef USE_JAM_EXCH
      northProc = jam_northId
      southProc = jam_southId
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
C      sendBuf1  = &arr(1-OLx,sNy-OLy+1)
C      recvBuf1  = &arr(1-OLx,sNy+1    )
C      sendBuf2  = &arr(1-OLx,1        )
C      recvBuf2  = &arr(1-OLx,1-OLy    )
       farProc1 = northProc
       farProc2 = southProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,
     &                    arr(1-OLx,sNy-OLy+1),
     &                    arr(1-OLx,sNy+1    ),
     &                    OLy*(sNx+2*OLx)*8,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,
     &                    arr(1-OLx,1        ),
     &                    arr(1-OLx,1-OLy    ),
     &                    OLy*(sNx+2*OLx)*8,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
      ELSE
C      sendBuf1  = &arr(1-OLx,1        )
C      recvBuf1  = &arr(1-OLx,1-OLy    )
C      sendBuf2  = &arr(1-OLx,sNy-OLy+1)
C      recvBuf2  = &arr(1-OLx,sNy+1    )
       farProc1 = southProc
       farProc2 = northProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,
     &                    arr(1-OLx,1        ),
     &                    arr(1-OLx,1-OLy    ),
     &                    OLy*(sNx+2*OLx)*8,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,
     &                    arr(1-OLx,sNy-OLy+1),
     &                    arr(1-OLx,sNy+1    ),
     &                    OLy*(sNx+2*OLx)*8,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
      ENDIF
#endif

      IF ( numberOfProcs .EQ. 1 ) THEN
       DO J=1,OLy
        iLo= 1-OLx
        iHi= sNx+OLx
        DO I=iLo,iHi
         arr(I,sNy+J    ) = arr(I,1+J-1    )
         arr(I,1-OLy+J-1) = arr(I,sNy-OLy+J)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
      SUBROUTINE EXCH_XYZ_R8_JAM( arr )
      IMPLICIT NONE

C     Full width, 3d. Single tile. No X-axis decomp.
C     exchange.

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH_JAM.h"

#include "MPI_INFO.h"
#include "JAM_INFO.h"

C     == Routine arguments ==
      Real*8 arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr)

C     == Local variables ==
      INTEGER I, J, K
      INTEGER iLo, iHi, I0
      INTEGER northProc, southProc
      INTEGER farProc1, farProc2
      INTEGER toPid, fromPid
      INTEGER rc

#ifdef ALLOW_MPI
      INTEGER mpiStatus(MPI_STATUS_SIZE)
#endif

C     East-west halo update
      DO K=1,Nr
       DO J=1-OLy,sNy+OLy
        DO I=1,OLx
         arr(1-I  ,J,K)     = arr(sNx-I+1,J,K)
         arr(sNx+I,J,K)     = arr(1+I-1  ,J,K)
        ENDDO
       ENDDO
      ENDDO
   
C     Phase 1 pairing
C     | 0 |  ---> | 1 |
C     | 0 |  <--- | 1 |

C     | 2 |  ---> | 3 |
C     | 2 |  <--- | 3 |

C     | 4 |  ---> | 5 |
C     | 4 |  <--- | 5 |

C     etc ...             
C    

#ifdef USE_MPI_EXCH
C     North-south halo update (including corners)
      DO K=1,Nr
C      Put my edges into a buffers
       IF ( MOD(myProcId,2) .EQ. 0 ) THEN
        DO J=1,OLy
         iLo= 1-OLx
         iHi= sNx+OLx
         I0 = (J-1)*(iHi-iLo)+1
         DO I=iLo,iHi
          exchBuf1(I0+I-iLo) = arr(I,sNy-OLy+J,K)
          exchBuf2(I0+I-iLo) = arr(I,1+J-1    ,K)
         ENDDO
        ENDDO
       ELSE
        DO J=1,OLy
         iLo= 1-OLx
         iHi= sNx+OLx
         I0 = (J-1)*(iHi-iLo)+1
         DO I=iLo,iHi
          exchBuf1(I0+I-iLo) = arr(I,1+J-1    ,K)
          exchBuf2(I0+I-iLo) = arr(I,sNy-OLy+J,K)
         ENDDO
        ENDDO
       ENDIF

C      Exchange the buffers
       northProc = mpi_northId
       southProc = mpi_southId
       IF ( MOD(myProcId,2) .EQ. 0 ) THEN
        farProc1 = northProc
        farProc2 = southProc
       ELSE
        farProc1 = southProc
        farProc2 = northProc
       ENDIF
C      Even-odd pairs
       IF ( farProc1 .NE. myProcId ) THEN
        CALL MPI_Sendrecv_replace(exchBuf1,OLy*(sNx+2*OLx),MPI_REAL8,
     &                            farProc1,0,
     &                            farProc1,MPI_ANY_TAG,
     &                            MPI_COMM_WORLD,mpiStatus,
     &                            rc)
       ENDIF
C      Odd-even pairs
       IF ( farProc2 .NE. myProcId ) THEN
        CALL MPI_Sendrecv_replace(exchBuf2,OLy*(sNx+2*OLx),MPI_REAL8,
     &                            farProc2,0,
     &                            farProc2,MPI_ANY_TAG,
     &                            MPI_COMM_WORLD,mpiStatus,
     &                            rc)
       ENDIF

C      Fill overlap regions from the buffers
       IF ( MOD(myProcId,2) .EQ. 0 ) THEN
        DO J=1,OLy
         iLo= 1-OLx
         iHi= sNx+OLx
         I0 = (J-1)*(iHi-iLo)+1
         DO I=iLo,iHi
          arr(I,sNy+J    ,K) = exchBuf1(I0+I-iLo)
          arr(I,1-OLy+J-1,K) = exchBuf2(I0+I-iLo)
         ENDDO
        ENDDO
       ELSE
        DO J=1,OLy
         iLo= 1-OLx
         iHi= sNx+OLx
         I0 = (J-1)*(iHi-iLo)+1
         DO I=iLo,iHi
          arr(I,sNy+J     ,K) = exchBuf2(I0+I-iLo)
          arr(I,1-OLy+J-1 ,K) = exchBuf1(I0+I-iLo)
         ENDDO
        ENDDO
       ENDIF
      ENDDO
#endif

#ifdef USE_JAM_EXCH
      northProc = jam_northId
      southProc = jam_southId
      DO K=1,Nr
       IF ( MOD(myProcId,2) .EQ. 0 ) THEN
C       sendBuf1  = &arr(1-OLx,sNy-OLy+1)
C       recvBuf1  = &arr(1-OLx,sNy+1    )
C       sendBuf2  = &arr(1-OLx,1        )
C       recvBuf2  = &arr(1-OLx,1-OLy    )
        farProc1 = northProc
        farProc2 = southProc
        IF ( farProc1 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc1,
     &                     arr(1-OLx,sNy-OLy+1,K),
     &                     arr(1-OLx,sNy+1    ,K),
     &                     OLy*(sNx+2*OLx)*8,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
        IF ( farProc2 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc2,
     &                     arr(1-OLx,1        ,K),
     &                     arr(1-OLx,1-OLy    ,K),
     &                     OLy*(sNx+2*OLx)*8,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
       ELSE
C       sendBuf1  = &arr(1-OLx,1        )
C       recvBuf1  = &arr(1-OLx,1-OLy    )
C       sendBuf2  = &arr(1-OLx,sNy-OLy+1)
C       recvBuf2  = &arr(1-OLx,sNy+1    )
        farProc1 = southProc
        farProc2 = northProc
        IF ( farProc1 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc1,
     &                     arr(1-OLx,1        ,K),
     &                     arr(1-OLx,1-OLy    ,K),
     &                     OLy*(sNx+2*OLx)*8,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
        IF ( farProc2 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc2,
     &                     arr(1-OLx,sNy-OLy+1,K),
     &                     arr(1-OLx,sNy+1    ,K),
     &                     OLy*(sNx+2*OLx)*8,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
       ENDIF
      ENDDO
#endif

      IF ( numberOfProcs .EQ. 1 ) THEN
       DO K=1,Nr
        DO J=1,OLy
         iLo= 1-OLx
         iHi= sNx+OLx
         DO I=iLo,iHi
          arr(I,sNy+J    ,K) = arr(I,1+J-1    ,K)
          arr(I,1-OLy+J-1,K) = arr(I,sNy-OLy+J,K)
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END

#undef  USE_MPI_EXCH
#define USE_JAM_EXCH

      SUBROUTINE EXCH_XY_O1_R4_JAM( arr )

      IMPLICIT NONE
C     Width 1. Single tile. No X-axis decomp.
C     No. corner update. Exchange.

#define ALLOW_MPI
#define _OLx 1
#define _OLy 1

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH_JAM.h"

#include "MPI_INFO.h"
#include "JAM_INFO.h"

C      == Compile time constants ==

C     == Routine arguments ==
      Real*4 arr(1-_OLx:sNx+_OLx,1-_OLy:sNy+_OLy)

C     == Local variables ==
      INTEGER I, J
      INTEGER northProc, southProc
      INTEGER farProc1, farProc2
      INTEGER toPid, fromPid
      INTEGER rc

#ifdef ALLOW_MPI
      INTEGER mpiStatus(MPI_STATUS_SIZE)
#endif

C     East-west halo update (without corners)
      DO J=1,sNy
       DO I=1,_OLx
        arr(1-I  ,J)     = arr(sNx-I+1,J)
        arr(sNx+I,J)     = arr(1+I-1  ,J)
       ENDDO
      ENDDO
   
C     Phase 1 pairing
C     | 0 |  ---> | 1 |
C     | 0 |  <--- | 1 |

C     | 2 |  ---> | 3 |
C     | 2 |  <--- | 3 |

C     | 4 |  ---> | 5 |
C     | 4 |  <--- | 5 |

C     etc ...             
C    

#ifdef USE_MPI_EXCH
C     North-south halo update (without corners)
C     Put my edges into a buffers
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       DO I=1,sNx
        exchBuf1(I) = arr(I,sNy)
        exchBuf2(I) = arr(I,1  )
       ENDDO
      ELSE
       DO I=1,sNx
        exchBuf1(I) = arr(I,1  )
        exchBuf2(I) = arr(I,sNy)
       ENDDO
      ENDIF

C     Exchange the buffers
      northProc = mpi_northId
      southProc = mpi_southId
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       farProc1 = northProc
       farProc2 = southProc
      ELSE
       farProc1 = southProc
       farProc2 = northProc
      ENDIF
C     Even-odd pairs
      IF ( farProc1 .NE. myProcId ) THEN
       CALL MPI_Sendrecv_replace(exchBuf1,sNx,MPI_REAL8,
     &                           farProc1,0,
     &                           farProc1,MPI_ANY_TAG,
     &                           MPI_COMM_WORLD,mpiStatus,
     &                           rc)
      ENDIF
C     Odd-even pairs
      IF ( farProc2 .NE. myProcId ) THEN
       CALL MPI_Sendrecv_replace(exchBuf2,sNx,MPI_REAL8,
     &                           farProc2,0,
     &                           farProc2,MPI_ANY_TAG,
     &                           MPI_COMM_WORLD,mpiStatus,
     &                           rc)
      ENDIF
#endif

#ifdef USE_JAM_EXCH
      northProc = jam_northId
      southProc = jam_southId
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
C      sendBuf1  = &arr(1,sNy  )
C      recvBuf1  = &arr(1,sNy+1)
C      sendBuf2  = &arr(1,1    )
C      recvBuf2  = &arr(1,0    )
       farProc1 = northProc
       farProc2 = southProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,arr(1,sNy),arr(1,sNy+1),
     &                    sNx*4,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,arr(1,1),arr(1,0),
     &                    sNx*4,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
      ELSE
C      sendBuf1  = &arr(1,1    )
C      recvBuf1  = &arr(1,0    )
C      sendBuf2  = &arr(1,sNy  )
C      recvBuf2  = &arr(1,sNy+1)
       farProc1 = southProc
       farProc2 = northProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,arr(1,1),arr(1,0),
     &                    sNx*4,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,arr(1,sNy),arr(1,sNy+1),
     &                    sNx*4,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
      ENDIF
C     IF ( farProc1 .NE. myProcId ) THEN
C      CALL JAM_EXCHANGE(farProc1,sendBuf1,recvBuf1,sNx*8,jam_exchKey)
C      jam_exchKey = jam_exchKey+1
C     ENDIF
C     IF ( farProc2 .NE. myProcId ) THEN
C      CALL JAM_EXCHANGE(farProc2,sendBuf2,recvBuf2,sNx*8,jam_exchKey)
C      jam_exchKey = jam_exchKey+1
C     ENDIF
#endif

#ifdef USE_MPI_EXCH
C     Fill overlap regions from the buffers
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       DO I=1,sNx
        arr(I,sNy+1) = exchBuf1(I)
        arr(I,0    ) = exchBuf2(I)
       ENDDO
      ELSE
       DO I=1,sNx
        arr(I,sNy+1) = exchBuf2(I)
        arr(I,0    ) = exchBuf1(I)
       ENDDO
      ENDIF
#endif

      IF ( numberOfProcs .EQ. 1 ) THEN
       DO I=1,sNx
        arr(I,sNy+1) = arr(I,1  )
        arr(I,0    ) = arr(I,sNy)
       ENDDO
      ENDIF

      RETURN
      END

      SUBROUTINE EXCH_XY_R4_JAM( arr )

      IMPLICIT NONE

C     Full width. Single tile. No X-axis decomp.
C     exchange.

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH_JAM.h"

#include "MPI_INFO.h"
#include "JAM_INFO.h"

C     == Routine arguments ==
      Real*4 arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     == Local variables ==
      INTEGER I, J
      INTEGER iLo, iHi, I0
      INTEGER northProc, southProc
      INTEGER farProc1, farProc2
      INTEGER toPid, fromPid
      INTEGER rc

#ifdef ALLOW_MPI
      INTEGER mpiStatus(MPI_STATUS_SIZE)
#endif

C     East-west halo update
      DO J=1-OLy,sNy+OLy
       DO I=1,OLx
        arr(1-I  ,J)     = arr(sNx-I+1,J)
        arr(sNx+I,J)     = arr(1+I-1  ,J)
       ENDDO
      ENDDO
   
C     Phase 1 pairing
C     | 0 |  ---> | 1 |
C     | 0 |  <--- | 1 |

C     | 2 |  ---> | 3 |
C     | 2 |  <--- | 3 |

C     | 4 |  ---> | 5 |
C     | 4 |  <--- | 5 |

C     etc ...             
C    

#ifdef USE_MPI_EXCH
C     North-south halo update (including corners)
C     Put my edges into a buffers
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       DO J=1,OLy
        iLo= 1-OLx
        iHi= sNx+OLx
        I0 = (J-1)*(iHi-iLo)+1
        DO I=iLo,iHi
         exchBuf1(I0+I-iLo) = arr(I,sNy-OLy+J)
         exchBuf2(I0+I-iLo) = arr(I,1+J-1    )
        ENDDO
       ENDDO
      ELSE
       DO J=1,OLy
        iLo= 1-OLx
        iHi= sNx+OLx
        I0 = (J-1)*(iHi-iLo)+1
        DO I=iLo,iHi
         exchBuf1(I0+I-iLo) = arr(I,1+J-1    )
         exchBuf2(I0+I-iLo) = arr(I,sNy-OLy+J)
        ENDDO
       ENDDO
      ENDIF

C     Exchange the buffers
      northProc = mpi_northId
      southProc = mpi_southId
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       farProc1 = northProc
       farProc2 = southProc
      ELSE
       farProc1 = southProc
       farProc2 = northProc
      ENDIF

C     Even-odd pairs
      IF ( farProc1 .NE. myProcId ) THEN
       CALL MPI_Sendrecv_replace(exchBuf1,OLy*(sNx+2*OLx),MPI_REAL8,
     &                           farProc1,0,
     &                           farProc1,MPI_ANY_TAG,
     &                           MPI_COMM_WORLD,mpiStatus,
     &                           rc)
      ENDIF
C     Odd-even pairs
      IF ( farProc2 .NE. myProcId ) THEN
       CALL MPI_Sendrecv_replace(exchBuf2,OLy*(sNx+2*OLx),MPI_REAL8,
     &                           farProc2,0,
     &                           farProc2,MPI_ANY_TAG,
     &                           MPI_COMM_WORLD,mpiStatus,
     &                           rc)
      ENDIF

C     Fill overlap regions from the buffers
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       DO J=1,OLy
        iLo= 1-OLx
        iHi= sNx+OLx
        I0 = (J-1)*(iHi-iLo)+1
        DO I=iLo,iHi
         arr(I,sNy+J    ) = exchBuf1(I0+I-iLo)
         arr(I,1-OLy+J-1) = exchBuf2(I0+I-iLo)
        ENDDO
       ENDDO
      ELSE
       DO J=1,OLy
        iLo= 1-OLx
        iHi= sNx+OLx
        I0 = (J-1)*(iHi-iLo)+1
        DO I=iLo,iHi
         arr(I,sNy+J     ) = exchBuf2(I0+I-iLo)
         arr(I,1-OLy+J-1 ) = exchBuf1(I0+I-iLo)
        ENDDO
       ENDDO
      ENDIF
#endif

#ifdef USE_JAM_EXCH
      northProc = jam_northId
      southProc = jam_southId
      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
C      sendBuf1  = &arr(1-OLx,sNy-OLy+1)
C      recvBuf1  = &arr(1-OLx,sNy+1    )
C      sendBuf2  = &arr(1-OLx,1        )
C      recvBuf2  = &arr(1-OLx,1-OLy    )
       farProc1 = northProc
       farProc2 = southProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,
     &                    arr(1-OLx,sNy-OLy+1),
     &                    arr(1-OLx,sNy+1    ),
     &                    OLy*(sNx+2*OLx)*4,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,
     &                    arr(1-OLx,1        ),
     &                    arr(1-OLx,1-OLy    ),
     &                    OLy*(sNx+2*OLx)*4,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
      ELSE
C      sendBuf1  = &arr(1-OLx,1        )
C      recvBuf1  = &arr(1-OLx,1-OLy    )
C      sendBuf2  = &arr(1-OLx,sNy-OLy+1)
C      recvBuf2  = &arr(1-OLx,sNy+1    )
       farProc1 = southProc
       farProc2 = northProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,
     &                    arr(1-OLx,1        ),
     &                    arr(1-OLx,1-OLy    ),
     &                    OLy*(sNx+2*OLx)*4,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,
     &                    arr(1-OLx,sNy-OLy+1),
     &                    arr(1-OLx,sNy+1    ),
     &                    OLy*(sNx+2*OLx)*4,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
      ENDIF
#endif

      IF ( numberOfProcs .EQ. 1 ) THEN
       DO J=1,OLy
        iLo= 1-OLx
        iHi= sNx+OLx
        DO I=iLo,iHi
         arr(I,sNy+J    ) = arr(I,1+J-1    )
         arr(I,1-OLy+J-1) = arr(I,sNy-OLy+J)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
      SUBROUTINE EXCH_XYZ_R4_JAM( arr )
      IMPLICIT NONE

C     Full width, 3d. Single tile. No X-axis decomp.
C     exchange.

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH_JAM.h"

#include "MPI_INFO.h"
#include "JAM_INFO.h"

C     == Routine arguments ==
      Real*4 arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr)

C     == Local variables ==
      INTEGER I, J, K
      INTEGER iLo, iHi, I0
      INTEGER northProc, southProc
      INTEGER farProc1, farProc2
      INTEGER toPid, fromPid
      INTEGER rc

#ifdef ALLOW_MPI
      INTEGER mpiStatus(MPI_STATUS_SIZE)
#endif

C     East-west halo update
      DO K=1,Nr
       DO J=1-OLy,sNy+OLy
        DO I=1,OLx
         arr(1-I  ,J,K)     = arr(sNx-I+1,J,K)
         arr(sNx+I,J,K)     = arr(1+I-1  ,J,K)
        ENDDO
       ENDDO
      ENDDO
   
C     Phase 1 pairing
C     | 0 |  ---> | 1 |
C     | 0 |  <--- | 1 |

C     | 2 |  ---> | 3 |
C     | 2 |  <--- | 3 |

C     | 4 |  ---> | 5 |
C     | 4 |  <--- | 5 |

C     etc ...             
C    

#ifdef USE_MPI_EXCH
C     North-south halo update (including corners)
      DO K=1,Nr
C      Put my edges into a buffers
       IF ( MOD(myProcId,2) .EQ. 0 ) THEN
        DO J=1,OLy
         iLo= 1-OLx
         iHi= sNx+OLx
         I0 = (J-1)*(iHi-iLo)+1
         DO I=iLo,iHi
          exchBuf1(I0+I-iLo) = arr(I,sNy-OLy+J,K)
          exchBuf2(I0+I-iLo) = arr(I,1+J-1    ,K)
         ENDDO
        ENDDO
       ELSE
        DO J=1,OLy
         iLo= 1-OLx
         iHi= sNx+OLx
         I0 = (J-1)*(iHi-iLo)+1
         DO I=iLo,iHi
          exchBuf1(I0+I-iLo) = arr(I,1+J-1    ,K)
          exchBuf2(I0+I-iLo) = arr(I,sNy-OLy+J,K)
         ENDDO
        ENDDO
       ENDIF

C      Exchange the buffers
       northProc = mpi_northId
       southProc = mpi_southId
       IF ( MOD(myProcId,2) .EQ. 0 ) THEN
        farProc1 = northProc
        farProc2 = southProc
       ELSE
        farProc1 = southProc
        farProc2 = northProc
       ENDIF
C      Even-odd pairs
       IF ( farProc1 .NE. myProcId ) THEN
        CALL MPI_Sendrecv_replace(exchBuf1,OLy*(sNx+2*OLx),MPI_REAL8,
     &                            farProc1,0,
     &                            farProc1,MPI_ANY_TAG,
     &                            MPI_COMM_WORLD,mpiStatus,
     &                            rc)
       ENDIF
C      Odd-even pairs
       IF ( farProc2 .NE. myProcId ) THEN
        CALL MPI_Sendrecv_replace(exchBuf2,OLy*(sNx+2*OLx),MPI_REAL8,
     &                            farProc2,0,
     &                            farProc2,MPI_ANY_TAG,
     &                            MPI_COMM_WORLD,mpiStatus,
     &                            rc)
       ENDIF

C      Fill overlap regions from the buffers
       IF ( MOD(myProcId,2) .EQ. 0 ) THEN
        DO J=1,OLy
         iLo= 1-OLx
         iHi= sNx+OLx
         I0 = (J-1)*(iHi-iLo)+1
         DO I=iLo,iHi
          arr(I,sNy+J    ,K) = exchBuf1(I0+I-iLo)
          arr(I,1-OLy+J-1,K) = exchBuf2(I0+I-iLo)
         ENDDO
        ENDDO
       ELSE
        DO J=1,OLy
         iLo= 1-OLx
         iHi= sNx+OLx
         I0 = (J-1)*(iHi-iLo)+1
         DO I=iLo,iHi
          arr(I,sNy+J     ,K) = exchBuf2(I0+I-iLo)
          arr(I,1-OLy+J-1 ,K) = exchBuf1(I0+I-iLo)
         ENDDO
        ENDDO
       ENDIF
      ENDDO
#endif

#ifdef USE_JAM_EXCH
      northProc = jam_northId
      southProc = jam_southId
      DO K=1,Nr
       IF ( MOD(myProcId,2) .EQ. 0 ) THEN
C       sendBuf1  = &arr(1-OLx,sNy-OLy+1)
C       recvBuf1  = &arr(1-OLx,sNy+1    )
C       sendBuf2  = &arr(1-OLx,1        )
C       recvBuf2  = &arr(1-OLx,1-OLy    )
        farProc1 = northProc
        farProc2 = southProc
        IF ( farProc1 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc1,
     &                     arr(1-OLx,sNy-OLy+1,K),
     &                     arr(1-OLx,sNy+1    ,K),
     &                     OLy*(sNx+2*OLx)*4,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
        IF ( farProc2 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc2,
     &                     arr(1-OLx,1        ,K),
     &                     arr(1-OLx,1-OLy    ,K),
     &                     OLy*(sNx+2*OLx)*4,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
       ELSE
C       sendBuf1  = &arr(1-OLx,1        )
C       recvBuf1  = &arr(1-OLx,1-OLy    )
C       sendBuf2  = &arr(1-OLx,sNy-OLy+1)
C       recvBuf2  = &arr(1-OLx,sNy+1    )
        farProc1 = southProc
        farProc2 = northProc
        IF ( farProc1 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc1,
     &                     arr(1-OLx,1        ,K),
     &                     arr(1-OLx,1-OLy    ,K),
     &                     OLy*(sNx+2*OLx)*4,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
        IF ( farProc2 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc2,
     &                     arr(1-OLx,sNy-OLy+1,K),
     &                     arr(1-OLx,sNy+1    ,K),
     &                     OLy*(sNx+2*OLx)*4,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
       ENDIF
      ENDDO
#endif

      IF ( numberOfProcs .EQ. 1 ) THEN
       DO K=1,Nr
        DO J=1,OLy
         iLo= 1-OLx
         iHi= sNx+OLx
         DO I=iLo,iHi
          arr(I,sNy+J    ,K) = arr(I,1+J-1    ,K)
          arr(I,1-OLy+J-1,K) = arr(I,sNy-OLy+J,K)
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#endif /* LETS_MAKE_JAM */

      RETURN
      END

#endif /* JAM_WITH_TWO_PROCS_PER_NODE */
#ifdef JAM_WITH_TWO_PROCS_PER_NODE
C Dual processor JAM stuff

#undef  USE_MPI_EXCH
#define USE_JAM_EXCH

      SUBROUTINE EXCH_XY_O1_R8_JAM( arr )

      IMPLICIT NONE
C     Width 1. Single tile. No X-axis decomp.
C     No. corner update. Exchange.

#define ALLOW_MPI
#define _OLx 1
#define _OLy 1

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EXCH_JAM.h"

#include "MPI_INFO.h"
#include "JAM_INFO.h"

C      == Compile time constants ==

C     == Routine arguments ==
      Real*8 arr(1-_OLx:sNx+_OLx,1-_OLy:sNy+_OLy)

C     == Local variables ==
      INTEGER I, J
      INTEGER northProc, southProc
      INTEGER farProc1, farProc2
      INTEGER toPid, fromPid
      INTEGER rc
      INTEGER myFourWayRank
      INTEGER exchangePhase

C     East-west halo update (without corners)
      DO J=1,sNy
       DO I=1,_OLx
        arr(1-I  ,J)     = arr(sNx-I+1,J)
        arr(sNx+I,J)     = arr(1+I-1  ,J)
       ENDDO
      ENDDO
   
C     Phase 1 pairing
C     | 0 |  ---> | 1 |
C     | 0 |  <--- | 1 |

C     | 2 |  ---> | 3 |
C     | 2 |  <--- | 3 |

C     | 4 |  ---> | 5 |
C     | 4 |  <--- | 5 |

C     etc ...             
C    

#ifdef USE_JAM_EXCH
      northProc = jam_northId
      southProc = jam_southId
      myFourWayRank = MOD(myProcId,4)

      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
       farProc1 = northProc
       farProc2 = southProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,arr(1,sNy),arr(1,sNy+1),
     &                    sNx*8,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
   10  CONTINUE
         CALL JAM_EXCHANGE_TEST( exchangePhase )
         IF ( myFourWayRank .EQ. 0 ) THEN
           IF ( exchangePhase .EQ. 0 ) GOTO 11
         ELSE
           IF ( exchangePhase .EQ. 1 ) GOTO 11
         ENDIF
         GOTO 10
   11  CONTINUE
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,arr(1,1),arr(1,0),sNx*8,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       CALL JAM_EXCHANGE_MARK
      ELSE
       farProc1 = southProc
       farProc2 = northProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,arr(1,1),arr(1,0),sNx*8,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
   20  CONTINUE
         CALL JAM_EXCHANGE_TEST( exchangePhase )
         IF ( myFourWayRank .EQ. 3 ) THEN
           IF ( exchangePhase .EQ. 0 ) GOTO 21
         ELSE
           IF ( exchangePhase .EQ. 1 ) GOTO 21
         ENDIF
         GOTO 20
   21  CONTINUE
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,arr(1,sNy),arr(1,sNy+1),
     &                    sNx*8,jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       CALL JAM_EXCHANGE_MARK
      ENDIF
#endif

      RETURN
      END

      SUBROUTINE EXCH_XY_R8_JAM( arr )

      IMPLICIT NONE

C     Full width. Single tile. No X-axis decomp.
C     exchange.

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH_JAM.h"

#include "MPI_INFO.h"
#include "JAM_INFO.h"

C     == Routine arguments ==
      Real*8 arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     == Local variables ==
      INTEGER I, J
      INTEGER iLo, iHi, I0
      INTEGER northProc, southProc
      INTEGER farProc1, farProc2
      INTEGER toPid, fromPid
      INTEGER rc
      INTEGER myFourWayRank, exchangePhase

#ifdef ALLOW_MPI
      INTEGER mpiStatus(MPI_STATUS_SIZE)
#endif

C     East-west halo update
      DO J=1-OLy,sNy+OLy
       DO I=1,OLx
        arr(1-I  ,J)     = arr(sNx-I+1,J)
        arr(sNx+I,J)     = arr(1+I-1  ,J)
       ENDDO
      ENDDO
   
C     Phase 1 pairing
C     | 0 |  ---> | 1 |
C     | 0 |  <--- | 1 |

C     | 2 |  ---> | 3 |
C     | 2 |  <--- | 3 |

C     | 4 |  ---> | 5 |
C     | 4 |  <--- | 5 |

C     etc ...             
C    

#ifdef USE_JAM_EXCH
      northProc = jam_northId
      southProc = jam_southId
      myFourWayRank = MOD(myProcId,4)

      IF ( MOD(myProcId,2) .EQ. 0 ) THEN
C      sendBuf1  = &arr(1-OLx,sNy-OLy+1)
C      recvBuf1  = &arr(1-OLx,sNy+1    )
C      sendBuf2  = &arr(1-OLx,1        )
C      recvBuf2  = &arr(1-OLx,1-OLy    )
       farProc1 = northProc
       farProc2 = southProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,
     &                    arr(1-OLx,sNy-OLy+1),
     &                    arr(1-OLx,sNy+1    ),
     &                    OLy*(sNx+2*OLx)*8,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
   10  CONTINUE
         CALL JAM_EXCHANGE_TEST( exchangePhase )
         IF ( myFourWayRank .EQ. 0 ) THEN
           IF ( exchangePhase .EQ. 0 ) GOTO 11
         ELSE
           IF ( exchangePhase .EQ. 1 ) GOTO 11
         ENDIF
         GOTO 10
   11  CONTINUE
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,
     &                    arr(1-OLx,1        ),
     &                    arr(1-OLx,1-OLy    ),
     &                    OLy*(sNx+2*OLx)*8,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       CALL JAM_EXCHANGE_MARK
      ELSE
C      sendBuf1  = &arr(1-OLx,1        )
C      recvBuf1  = &arr(1-OLx,1-OLy    )
C      sendBuf2  = &arr(1-OLx,sNy-OLy+1)
C      recvBuf2  = &arr(1-OLx,sNy+1    )
       farProc1 = southProc
       farProc2 = northProc
       IF ( farProc1 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc1,
     &                    arr(1-OLx,1        ),
     &                    arr(1-OLx,1-OLy    ),
     &                    OLy*(sNx+2*OLx)*8,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
   20  CONTINUE
         CALL JAM_EXCHANGE_TEST( exchangePhase )
         IF ( myFourWayRank .EQ. 3 ) THEN
           IF ( exchangePhase .EQ. 0 ) GOTO 21
         ELSE
           IF ( exchangePhase .EQ. 1 ) GOTO 21
         ENDIF
         GOTO 20
   21  CONTINUE
       IF ( farProc2 .NE. myProcId ) THEN
        CALL JAM_EXCHANGE(farProc2,
     &                    arr(1-OLx,sNy-OLy+1),
     &                    arr(1-OLx,sNy+1    ),
     &                    OLy*(sNx+2*OLx)*8,
     &                    jam_exchKey)
        jam_exchKey = jam_exchKey+1
       ENDIF
       CALL JAM_EXCHANGE_MARK
      ENDIF
#endif

      RETURN
      END
      SUBROUTINE EXCH_XYZ_R8_JAM( arr )
      IMPLICIT NONE

C     Full width, 3d. Single tile. No X-axis decomp.
C     exchange.

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH_JAM.h"

#include "MPI_INFO.h"
#include "JAM_INFO.h"

C     == Routine arguments ==
      INTEGER myThid
      Real*8 arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr)

C     == Local variables ==
      INTEGER I, J, K
      INTEGER iLo, iHi, I0
      INTEGER northProc, southProc
      INTEGER farProc1, farProc2
      INTEGER toPid, fromPid
      INTEGER rc
      INTEGER myFourWayRank, exchangePhase

#ifdef ALLOW_MPI
      INTEGER mpiStatus(MPI_STATUS_SIZE)
#endif

C     East-west halo update
      DO K=1,Nr
       DO J=1-OLy,sNy+OLy
        DO I=1,OLx
         arr(1-I  ,J,K)     = arr(sNx-I+1,J,K)
         arr(sNx+I,J,K)     = arr(1+I-1  ,J,K)
        ENDDO
       ENDDO
      ENDDO

CcnhDebugStarts
C     RETURN
CcnhDebugEnds
   
C     Phase 1 pairing
C     | 0 |  ---> | 1 |
C     | 0 |  <--- | 1 |

C     | 2 |  ---> | 3 |
C     | 2 |  <--- | 3 |

C     | 4 |  ---> | 5 |
C     | 4 |  <--- | 5 |

C     etc ...             
C    


#ifdef USE_JAM_EXCH
      northProc = jam_northId
      southProc = jam_southId
      myFourWayRank = MOD(myProcId,4)

      DO K=1,Nr
       IF ( MOD(myProcId,2) .EQ. 0 ) THEN
C       sendBuf1  = &arr(1-OLx,sNy-OLy+1)
C       recvBuf1  = &arr(1-OLx,sNy+1    )
C       sendBuf2  = &arr(1-OLx,1        )
C       recvBuf2  = &arr(1-OLx,1-OLy    )
        farProc1 = northProc
        farProc2 = southProc
        IF ( farProc1 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc1,
     &                     arr(1-OLx,sNy-OLy+1,K),
     &                     arr(1-OLx,sNy+1    ,K),
     &                     OLy*(sNx+2*OLx)*8,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
   10   CONTINUE
         CALL JAM_EXCHANGE_TEST( exchangePhase )
         IF ( myFourWayRank .EQ. 0 ) THEN
           IF ( exchangePhase .EQ. 0 ) GOTO 11
         ELSE
           IF ( exchangePhase .EQ. 1 ) GOTO 11
         ENDIF
         GOTO 10
   11   CONTINUE
        IF ( farProc2 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc2,
     &                     arr(1-OLx,1        ,K),
     &                     arr(1-OLx,1-OLy    ,K),
     &                     OLy*(sNx+2*OLx)*8,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
        CALL JAM_EXCHANGE_MARK
       ELSE
C       sendBuf1  = &arr(1-OLx,1        )
C       recvBuf1  = &arr(1-OLx,1-OLy    )
C       sendBuf2  = &arr(1-OLx,sNy-OLy+1)
C       recvBuf2  = &arr(1-OLx,sNy+1    )
        farProc1 = southProc
        farProc2 = northProc
        IF ( farProc1 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc1,
     &                     arr(1-OLx,1        ,K),
     &                     arr(1-OLx,1-OLy    ,K),
     &                     OLy*(sNx+2*OLx)*8,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
   20   CONTINUE
         CALL JAM_EXCHANGE_TEST( exchangePhase )
         IF ( myFourWayRank .EQ. 3 ) THEN
           IF ( exchangePhase .EQ. 0 ) GOTO 21
         ELSE
           IF ( exchangePhase .EQ. 1 ) GOTO 21
         ENDIF
         GOTO 20
   21   CONTINUE
        IF ( farProc2 .NE. myProcId ) THEN
         CALL JAM_EXCHANGE(farProc2,
     &                     arr(1-OLx,sNy-OLy+1,K),
     &                     arr(1-OLx,sNy+1    ,K),
     &                     OLy*(sNx+2*OLx)*8,
     &                     jam_exchKey)
         jam_exchKey = jam_exchKey+1
        ENDIF
        CALL JAM_EXCHANGE_MARK
       ENDIF
      ENDDO
#endif

      RETURN
      END

#endif /* JAM_WITH_TWO_PROCS_PER_NODE */
