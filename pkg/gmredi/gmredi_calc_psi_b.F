#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: GMREDI_CALC_PSI_B
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_PSI_B(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             ldd97_LrhoW, ldd97_LrhoS,
     I             myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_CALC_PSI_B
C     | o Calculate stream-functions for GM bolus velocity
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"
#include "FFIELDS.h"
#ifdef ALLOW_CTRL
# include "CTRL_FIELDS.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      _RL sigmaX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL ldd97_LrhoW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ldd97_LrhoS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
#ifdef GM_BOLUS_ADVEC

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k, km1
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER igmkey, kkey
#endif
      _RL half_K
      _RL SlopeX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SlopeY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDrW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDrS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL taperX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL taperY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C-    Initialization : <= done in S/R gmredi_init

#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      igmkey = (act1 + 1) + act2*max1
     &                    + act3*max1*max2
     &                    + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef GM_VISBECK_VARIABLE_K
CADJ STORE VisbeckK(:,:,bi,bj) = comlev1_bibj, key=igmkey, byte=isbyte
# endif
#endif
      IF (GM_AdvForm) THEN
       DO k=2,Nr
        km1 = k-1

#ifdef ALLOW_AUTODIFF
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          SlopeX(i,j)       = 0. _d 0
          SlopeY(i,j)       = 0. _d 0
          dSigmaDrW(i,j)    = 0. _d 0
          dSigmaDrS(i,j)    = 0. _d 0
         ENDDO
        ENDDO
#endif

C      Gradient of Sigma below U and V points
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          SlopeX(i,j)=op5*( sigmaX(i,j,km1)+sigmaX(i,j,k) )
     &                   *maskW(i,j,km1,bi,bj)*maskW(i,j,k,bi,bj)
          dSigmaDrW(i,j)=op5*( sigmaR(i-1,j,k)+sigmaR(i,j,k) )
     &                   *maskW(i,j,km1,bi,bj)*maskW(i,j,k,bi,bj)
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          SlopeY(i,j)=op5*( sigmaY(i,j,km1)+sigmaY(i,j,k) )
     &                   *maskS(i,j,km1,bi,bj)*maskS(i,j,k,bi,bj)
          dSigmaDrS(i,j)=op5*( sigmaR(i,j-1,k)+sigmaR(i,j,k) )
     &                   *maskS(i,j,km1,bi,bj)*maskS(i,j,k,bi,bj)
         ENDDO
        ENDDO

C      Calculate slopes , taper and/or clip
        CALL GMREDI_SLOPE_PSI(
     O              taperX, taperY,
     U              SlopeX, SlopeY,
     U              dSigmaDrW, dSigmaDrS,
     I              ldd97_LrhoW, ldd97_LrhoS, rF(k), k,
     I              bi, bj, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
        kkey = (igmkey-1)*Nr + k
CADJ STORE SlopeX(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE SlopeY(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE taperX(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE taperY(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C-  Compute the 2 stream-function Components ( GM bolus vel.)
C   Note: since SlopeX,Y have been masked, no needs to mask again GM_PsiX,Y
        half_K = GM_background_K
     &         *(GM_bolFac1d(km1)+GM_bolFac1d(k))*op25
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          GM_PsiX(i,j,k,bi,bj) = SlopeX(i,j)*taperX(i,j)
#ifdef GM_READ_K3D_GM
     &     *( op25
     &        *( GM_inpK3dGM(i-1,j,km1,bi,bj)+GM_inpK3dGM(i,j,km1,bi,bj)
     &         + GM_inpK3dGM(i-1,j,k,bi,bj)+GM_inpK3dGM(i,j,k,bi,bj) )
#else
     &     *( half_K
     &          *(GM_bolFac2d(i-1,j,bi,bj)+GM_bolFac2d(i,j,bi,bj))
#endif
#ifdef GM_VISBECK_VARIABLE_K
     &      + op5*(VisbeckK(i-1,j,bi,bj)+VisbeckK(i,j,bi,bj))
#endif
#ifdef ALLOW_GM_LEITH_QG
     &      + op25*( GM_LeithQG_K(i-1,j,km1,bi,bj)
     &             + GM_LeithQG_K( i ,j,km1,bi,bj)
     &             + GM_LeithQG_K(i-1,j,k,bi,bj)
     &             + GM_LeithQG_K( i ,j,k,bi,bj) )
#endif
     &      )
c    &       *maskW(i,j,km1,bi,bj)*maskW(i,j,k,bi,bj)
#ifdef ALLOW_EDDYPSI
     &     +eddyPsiX(i,j,k,bi,bj)*maskW(i,j,k,bi,bj)
#endif
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          GM_PsiY(i,j,k,bi,bj) = SlopeY(i,j)*taperY(i,j)
#ifdef GM_READ_K3D_GM
     &     *( op25
     &        *( GM_inpK3dGM(i,j-1,km1,bi,bj)+GM_inpK3dGM(i,j,km1,bi,bj)
     &         + GM_inpK3dGM(i,j-1,k,bi,bj)+GM_inpK3dGM(i,j,k,bi,bj) )
#else
     &     *( half_K
     &          *(GM_bolFac2d(i,j-1,bi,bj)+GM_bolFac2d(i,j,bi,bj))
#endif
#ifdef GM_VISBECK_VARIABLE_K
     &      + op5*(VisbeckK(i,j-1,bi,bj)+VisbeckK(i,j,bi,bj))
#endif
#ifdef ALLOW_GM_LEITH_QG
     &      + op25*( GM_LeithQG_K(i,j-1,km1,bi,bj)
     &             + GM_LeithQG_K(i, j ,km1,bi,bj)
     &             + GM_LeithQG_K(i,j-1,k,bi,bj)
     &             + GM_LeithQG_K(i, j ,k,bi,bj) )
#endif
     &      )
c    &       *maskS(i,j,km1,bi,bj)*maskS(i,j,k,bi,bj)
#ifdef ALLOW_EDDYPSI
     &     +eddyPsiY(i,j,k,bi,bj)*maskS(i,j,k,bi,bj)
#endif
         ENDDO
        ENDDO

C----- end of loop on level k
       ENDDO

C     end if GM_AdvForm block
      ENDIF
#endif /* GM_BOLUS_ADVEC */
#endif /* ALLOW_GMREDI */

      RETURN
      END
