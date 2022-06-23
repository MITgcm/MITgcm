#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: GRAD_SIGMA
C     !INTERFACE:
      SUBROUTINE GRAD_SIGMA(
     I             bi, bj, iMin, iMax, jMin, jMax, k,
     I             rhoK, sigKm1, sigKp1,
     O             sigmaX, sigmaY, sigmaR,
     I             myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GRAD_SIGMA
C     | o Calculate isoneutral gradients
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj     :: tile indices
C     iMin,iMax  :: not used
C     jMin,jMax  :: not used
C     k          :: current level index
C     rhoK       :: density at level k
C     sigKm1     :: upper level density computed at current pressure
C     sigKp1     :: lower level density computed at current pressure
C     sigmaX,Y,R :: iso-neutral gradient of density in 3 directions X,Y,R
C     myThid     :: my Thread Id. number
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k
      _RL rhoK(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sigKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sigKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sigmaX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     rhoLoc     :: local copy of rhoK
      INTEGER i,j
      _RL rhoLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C-    safer to work on a local copy of rhoK (before a partial update)
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        rhoLoc(i,j) = rhoK(i,j)
       ENDDO
      ENDDO

C-    Internal exchange for calculations in X
      IF ( useCubedSphereExchange ) THEN
        CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                             rhoLoc, bi,bj, myThid )
      ENDIF
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx+1,sNx+OLx
        sigmaX(i,j,k)=_maskW(i,j,k,bi,bj)
     &        *_recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
     &        *(rhoLoc(i,j)-rhoLoc(i-1,j))
       ENDDO
      ENDDO

C-    Internal exchange for calculations in Y
      IF ( useCubedSphereExchange ) THEN
        CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                             rhoLoc, bi,bj, myThid )
      ENDIF
      DO j=1-OLy+1,sNy+OLy
       DO i=1-OLx,sNx+OLx
        sigmaY(i,j,k)=_maskS(i,j,k,bi,bj)
     &        *_recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
     &        *(rhoLoc(i,j)-rhoLoc(i,j-1))
       ENDDO
      ENDDO

      IF (k.EQ.1) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         sigmaR(i,j,k)= 0. _d 0
        ENDDO
       ENDDO
      ELSE
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         sigmaR(i,j,k)= maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
     &                *recip_drC(k)*rkSign
     &                *(sigKp1(i,j)-sigKm1(i,j))
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
