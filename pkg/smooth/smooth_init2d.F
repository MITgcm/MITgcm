#include "SMOOTH_OPTIONS.h"

      SUBROUTINE SMOOTH_INIT2D( smoothOpNb, myThid )

C     *==========================================================*
C     | SUBROUTINE smooth_init2D
C     | o Routine that initializes one 2D smoothing/correlation operator
C     |   by computing/writing the corresponding diffusion operator
C     *==========================================================*

Cgf the choices of smooth2Dtype and smooth2Dsize need comments...

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SMOOTH.h"

      INTEGER smoothOpNb
      INTEGER myThid

      INTEGER i, j, bi, bj
      CHARACTER*( 80) fnamegeneric

      smooth2DtotTime = smooth2Dnbt(smoothOpNb)*smooth2DdelTime

      IF ( (smooth2Dtype(smoothOpNb).NE.0).AND.
     &     (smooth2Dsize(smoothOpNb).EQ.2) ) THEN
        WRITE(fnamegeneric(1:80),'(1A,I3.3)')
     &    'smooth2Dscales',smoothOpNb
        CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &                       1, smooth2D_Lx, 1, 1, myThid )
        CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &                       1, smooth2D_Ly, 2, 1, myThid )
        CALL EXCH_XY_RL( smooth2D_Lx, myThid )
        CALL EXCH_XY_RL( smooth2D_Ly, myThid )
      ELSE
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             smooth2D_Lx(i,j,bi,bj)=smooth2D_Lx0(smoothOpNb)
             smooth2D_Ly(i,j,bi,bj)=smooth2D_Ly0(smoothOpNb)
            ENDDO
           ENDDO
         ENDDO
        ENDDO
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           smooth2D_Kux(i,j,bi,bj)=smooth2D_Lx(i,j,bi,bj)*
     &            smooth2D_Lx(i,j,bi,bj)/smooth2DtotTime/2
           smooth2D_Kvy(i,j,bi,bj)=smooth2D_Ly(i,j,bi,bj)*
     &            smooth2D_Ly(i,j,bi,bj)/smooth2DtotTime/2
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XY_RL( smooth2D_Kux, myThid )
      CALL EXCH_XY_RL( smooth2D_Kvy, myThid )

C write diffusion operator to file
      WRITE(fnamegeneric(1:80),'(1A,I3.3)')
     &    'smooth2Doperator', smoothOpNb
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      1, smooth2D_Kux, 1, 1, myThid )
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      1, smooth2D_Kvy, 2, 1, myThid )

      RETURN
      END
