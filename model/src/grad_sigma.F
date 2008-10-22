C $Header: /u/gcmpack/MITgcm/model/src/grad_sigma.F,v 1.9 2008/10/22 00:26:20 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: GRAD_SIGMA
C     !INTERFACE:
      SUBROUTINE GRAD_SIGMA(
     I             bi, bj, iMin, iMax, jMin, jMax, K,
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
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     rhoK       :: density at level k
C     sigKm1     :: upper level density computed at current pressure
C     sigKp1     :: lower level density computed at current pressure
C     sigmaX,Y,R :: iso-neutral gradient of density in 3 directions X,Y,R
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL rhoK(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sigKm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sigKp1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sigmaX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaY(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaR(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     rhoLoc :: local copy of rhoK
      INTEGER i,j
      _RL rhoLoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
CEOP

C-    safer to work on a local copy of rhoK (before a partial update)
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        rhoLoc(i,j) = rhoK(i,j)
       ENDDO
      ENDDO

C-    Internal exchange for calculations in X
cph-exch2#ifndef ALLOW_AUTODIFF_TAMC
      IF ( useCubedSphereExchange ) THEN
        CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                             rhoLoc, bi,bj, myThid )
      ENDIF
cph-exch2#endif
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        sigmaX(i,j,k)=_maskW(i,j,k,bi,bj)
     &        *_recip_dxC(i,j,bi,bj)
     &        *(rhoLoc(i,j)-rhoLoc(i-1,j))
       ENDDO
      ENDDO

C-    Internal exchange for calculations in Y
cph-exch2#ifndef ALLOW_AUTODIFF_TAMC
      IF ( useCubedSphereExchange ) THEN
        CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                             rhoLoc, bi,bj, myThid )
      ENDIF
cph-exch2#endif
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        sigmaY(i,j,k)=_maskS(i,j,k,bi,bj)
     &        *_recip_dyC(i,j,bi,bj)
     &        *(rhoLoc(i,j)-rhoLoc(i,j-1))
       ENDDO
      ENDDO

      IF (K.EQ.1) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         sigmaR(i,j,k)=0.
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         sigmaR(i,j,k)= maskC(i,j,k,bi,bj)
     &                *recip_drC(k)*rkSign
     &                *(sigKp1(i,j)-sigKm1(i,j))
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
