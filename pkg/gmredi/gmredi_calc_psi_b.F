C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_calc_psi_b.F,v 1.3 2003/01/10 23:41:15 heimbach Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CStartOfInterface
      SUBROUTINE GMREDI_CALC_PSI_B(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_CALC_PSI_B                             |
C     | o Calculate stream-functions for GM bolus velocity       |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"
#include "GMREDI_DIAGS.h"

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     == Routine arguments ==
C
      _RL sigmaX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaY(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaR(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_GMREDI
#ifdef GM_BOLUS_ADVEC

C     == Local variables ==
      INTEGER i,j,k, km1
      _RL SlopeX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDrW(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDrS(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL taperX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL taperY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

C-    Initialization : <= done in S/R gmredi_init

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef GM_VISBECK_VARIABLE_K
CADJ STORE VisbeckK(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
# endif
#endif
      IF (GM_AdvForm) THEN
       DO k=2,Nr
       km1 = k-1

#ifdef ALLOW_AUTODIFF_TAMC
       kkey = (ikey-1)*Nr + k
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         SlopeX(i,j)       = 0. _d 0
         SlopeY(i,j)       = 0. _d 0
         dSigmaDrW(i,j)    = 0. _d 0
         dSigmaDrS(i,j)    = 0. _d 0
        ENDDO
       ENDDO
#endif

      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1

C      Gradient of Sigma below U and V points
        SlopeX(i,j)=0.5*( sigmaX(i,j,km1)+sigmaX(i,j,k) )
     &                 *maskW(i,j,k,bi,bj)
        dSigmaDrW(i,j)=0.5*( sigmaR(i-1,j,k)+sigmaR(i,j,k) )
     &                 *maskW(i,j,k,bi,bj)
        SlopeY(i,j)=0.5*( sigmaY(i,j,km1)+sigmaY(i,j,k) )
     &                 *maskS(i,j,k,bi,bj)
        dSigmaDrS(i,j)=0.5*( sigmaR(i,j-1,k)+sigmaR(i,j,k) )
     &                 *maskS(i,j,k,bi,bj)

       ENDDO
      ENDDO

C     Calculate slopes , taper and/or clip
      CALL GMREDI_SLOPE_PSI_B(
     I             dSigmaDrW, dSigmaDrS,
     I             rF(K),K,
     U             SlopeX, SlopeY,
     O             taperX, taperY,
     I             bi, bj, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE SlopeX(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE SlopeY(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE taperX(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE taperY(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1

C-  Compute the 2 stream-function Components ( GM bolus vel.) 
        GM_PsiX(i,j,k,bi,bj) = SlopeX(i,j)*taperX(i,j)
     &   *( GM_background_K
#ifdef GM_VISBECK_VARIABLE_K
     &    +0.5*(VisbeckK(i-1,j,bi,bj)+VisbeckK(i,j,bi,bj))
#endif
     &    )*maskW(i,j,k,bi,bj)
        GM_PsiY(i,j,k,bi,bj) = SlopeY(i,j)*taperY(i,j)
     &   *( GM_background_K
#ifdef GM_VISBECK_VARIABLE_K
     &    +0.5*(VisbeckK(i,j-1,bi,bj)+VisbeckK(i,j,bi,bj))
#endif
     &    )*maskS(i,j,k,bi,bj)

#ifdef ALLOW_TIMEAVE
C--     Time-average
        GM_PsiXtave(i,j,k,bi,bj)=GM_PsiXtave(i,j,k,bi,bj)
     &                          +GM_PsiX(i,j,k,bi,bj)*deltaTclock
        GM_PsiYtave(i,j,k,bi,bj)=GM_PsiYtave(i,j,k,bi,bj)
     &                          +GM_PsiY(i,j,k,bi,bj)*deltaTclock
#endif /* ALLOW_TIMEAVE */

       ENDDO
      ENDDO

C-----
       ENDDO
      ENDIF
#endif /* GM_BOLUS_ADVEC */
#endif /* ALLOW_GMREDI */

      RETURN
      END
