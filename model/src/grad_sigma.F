C $Header: /u/gcmpack/MITgcm/model/src/grad_sigma.F,v 1.2 2001/02/04 14:38:47 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE GRAD_SIGMA(
     I             bi, bj, iMin, iMax, jMin, jMax, K,
     I             rhoK, sigKm1, sigKp1,
     O             sigmaX, sigmaY, sigmaR,
     I             myThid )
C     /==========================================================\
C     | SUBROUTINE CALC_ISOSLOPES                                |
C     | o Calculate isoneutral gradients                         |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL rhoK(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sigKm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sigKp1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sigmaX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaY(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaR(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        sigmaX(i,j,k)=_maskW(i,j,k,bi,bj)
     &        *_recip_dxC(i,j,bi,bj)
     &        *(rhoK(i,j)-rhoK(i-1,j))
       ENDDO
      ENDDO

      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        sigmaY(i,j,k)=_maskS(i,j,k,bi,bj)
     &        *_recip_dyC(i,j,bi,bj)
     &        *(rhoK(i,j)-rhoK(i,j-1))
       ENDDO
      ENDDO

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        IF (K.NE.1 .AND. hfacC(i,j,k,bi,bj).NE.0.) THEN
         sigmaR(i,j,k)=recip_drC(k)*rkFac*(sigKm1(i,j)-sigKp1(i,j))
        ELSE
         sigmaR(i,j,k)=0.
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
