C $Header: /u/gcmpack/MITgcm/eesupp/src/global_vec_sum.F,v 1.1 2005/09/28 01:58:25 edhill Exp $
C $Name:  $

C     Perform a global sum on an array of threaded vectors.
C     
C     Contents
C     o global_sum_r4
C     o global_sum_r8

#include "CPP_EEOPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_VEC_SUM_R4

C     !INTERFACE:
      SUBROUTINE GLOBAL_VEC_SUM_R4( 
     I     nval,
     U     sumPhi,
     I     myThid )

C     !DESCRIPTION:
C     Sum the vector over threads and then sum the result over all MPI
C     processes.  Within a process only one thread does the sum, each
C     thread is assumed to have already summed its local data.  The same
C     thread also does the inter-process sum for example with MPI and
C     then writes the result into a shared location. All threads wait
C     until the sum is avaiailable.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT PARAMETERS:
C     sumPhi - Result of sum.
C     myThid - My thread id.
      INTEGER nval, myThid
      Real*4 sumPhi(nval)
CEOP

C     !LOCAL VARIABLES:
C     I      - Loop counters
C     mpiRC  - MPI return code
      INTEGER I, j, it, ntot, nloop, ni
      Real*4  tmp1(MAX_VGS), tmp2(MAX_VGS)
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */


      nloop = nval/MAX_VGS + 1
      ntot = 0
      DO it = 1,nloop

        ni = nval - ntot
        IF ( ni .GT. MAX_VGS )  ni = MAX_VGS
        IF ( ni .LT. 1 ) GOTO 999

C       write local sum into array
        CALL BAR2( myThid )
        DO j = 1,ni
          phivGSRS(j,myThid) = sumPhi(ntot + j)
        ENDDO
C       Can not start until everyone is ready
        CALL BAR2( myThid )

C       Sum within the process first
        _BEGIN_MASTER( myThid )

        DO j = 1,ni
          tmp1(j) = 0.
        ENDDO
        DO I = 1,nThreads
          DO j = 1,ni
            tmp1(j) = tmp1(j) + phiGSRS(j,I)
          ENDDO
        ENDDO

#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
        IF ( usingMPI ) THEN
#endif
          CALL MPI_Allreduce(tmp1,tmp2,ni,MPI_REAL,MPI_SUM,
     &         MPI_COMM_MODEL,mpiRC)
#ifndef ALWAYS_USE_MPI
        ENDIF
#endif
#endif /*  ALLOW_USE_MPI */

C       Write solution to place where all threads can see it
        DO j = 1,ni
          phivGSRS(j,myThid) = tmp2(j)
        ENDDO

        _END_MASTER( myThid )
        
        CALL BAR2( myThid )        
C       set result for every process
        DO j = 1,ni
          sumPhi(ntot + j) = phivGSRS(j,1)
        ENDDO
        CALL BAR2( myThid )

        ntot = ntot + ni

      ENDDO

 999  CONTINUE
      RETURN
      END
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_VEC_SUM_R8

C     !INTERFACE:
      SUBROUTINE GLOBAL_VEC_SUM_R8( 
     I     nval,
     U     sumPhi,
     I     myThid )

C     !DESCRIPTION:
C     Sum the vector over threads and then sum the result over all MPI
C     processes.  Within a process only one thread does the sum, each
C     thread is assumed to have already summed its local data.  The same
C     thread also does the inter-process sum for example with MPI and
C     then writes the result into a shared location. All threads wait
C     until the sum is avaiailable.

C     !USES:
      IMPLICIT NONE 
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT PARAMETERS:
C     sumPhi - Result of sum.
C     myThid - My thread id.
      INTEGER nval, myThid
      Real*8 sumPhi(nval)
CEOP

C     !LOCAL VARIABLES:
C     I      - Loop counters
C     mpiRC  - MPI return code
      INTEGER I, j, it, ntot, nloop, ni
      Real*8  tmp1(MAX_VGS), tmp2(MAX_VGS)
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */


      nloop = nval/MAX_VGS + 1
      ntot = 0
      DO it = 1,nloop

        ni = nval - ntot
        IF ( ni .GT. MAX_VGS )  ni = MAX_VGS
        IF ( ni .LT. 1 ) GOTO 999

C       write local sum into array
        CALL BAR2( myThid )
        DO j = 1,ni
          phivGSRL(j,myThid) = sumPhi(ntot + j)
        ENDDO
C       Can not start until everyone is ready
        CALL BAR2( myThid )

C       Sum within the process first
        _BEGIN_MASTER( myThid )

        DO j = 1,ni
          tmp1(j) = 0.
        ENDDO
        DO I = 1,nThreads
          DO j = 1,ni
            tmp1(j) = tmp1(j) + phiGSRS(j,I)
          ENDDO
        ENDDO

#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
        IF ( usingMPI ) THEN
#endif
          CALL MPI_Allreduce(tmp1,tmp2,ni,MPI_REAL,MPI_SUM,
     &         MPI_COMM_MODEL,mpiRC)
#ifndef ALWAYS_USE_MPI
        ENDIF
#endif
#endif /*  ALLOW_USE_MPI */

C       Write solution to place where all threads can see it
        DO j = 1,ni
          phivGSRL(j,myThid) = tmp2(j)
        ENDDO

        _END_MASTER( myThid )
        
        CALL BAR2( myThid )        
C       set result for every process
        DO j = 1,ni
          sumPhi(ntot + j) = phivGSRL(j,1)
        ENDDO
        CALL BAR2( myThid )

        ntot = ntot + ni

      ENDDO

 999  CONTINUE
      RETURN
      END
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_VEC_SUM_INT

C     !INTERFACE:
      SUBROUTINE GLOBAL_VEC_SUM_INT( 
     I     nval,
     U     sumPhi,
     I     myThid )

C     !DESCRIPTION:
C     Sum the vector over threads and then sum the result over all MPI
C     processes.  Within a process only one thread does the sum, each
C     thread is assumed to have already summed its local data.  The same
C     thread also does the inter-process sum for example with MPI and
C     then writes the result into a shared location. All threads wait
C     until the sum is avaiailable.

C     !USES:
      IMPLICIT NONE 
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT PARAMETERS:
C     sumPhi - Result of sum.
C     myThid - My thread id.
      INTEGER nval, myThid
      INTEGER sumPhi(nval)
CEOP

C     !LOCAL VARIABLES:
C     I      - Loop counters
C     mpiRC  - MPI return code
      INTEGER I, j, it, ntot, nloop, ni
      INTEGER tmp1(MAX_VGS), tmp2(MAX_VGS)
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */


      nloop = nval/MAX_VGS + 1
      ntot = 0
      DO it = 1,nloop

        ni = nval - ntot
        IF ( ni .GT. MAX_VGS )  ni = MAX_VGS
        IF ( ni .LT. 1 ) GOTO 999

C       write local sum into array
        CALL BAR2( myThid )
        DO j = 1,ni
          phivGSI(j,myThid) = sumPhi(ntot + j)
        ENDDO
C       Can not start until everyone is ready
        CALL BAR2( myThid )

C       Sum within the process first
        _BEGIN_MASTER( myThid )

        DO j = 1,ni
          tmp1(j) = 0.
        ENDDO
        DO I = 1,nThreads
          DO j = 1,ni
            tmp1(j) = tmp1(j) + phiGSRS(j,I)
          ENDDO
        ENDDO

#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
        IF ( usingMPI ) THEN
#endif
          CALL MPI_Allreduce(tmp1,tmp2,ni,MPI_REAL,MPI_SUM,
     &         MPI_COMM_MODEL,mpiRC)
#ifndef ALWAYS_USE_MPI
        ENDIF
#endif
#endif /*  ALLOW_USE_MPI */

C       Write solution to place where all threads can see it
        DO j = 1,ni
          phivGSI(j,myThid) = tmp2(j)
        ENDDO

        _END_MASTER( myThid )
        
        CALL BAR2( myThid )        
C       set result for every process
        DO j = 1,ni
          sumPhi(ntot + j) = phivGSI(j,1)
        ENDDO
        CALL BAR2( myThid )

        ntot = ntot + ni

      ENDDO

 999  CONTINUE
      RETURN
      END
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
