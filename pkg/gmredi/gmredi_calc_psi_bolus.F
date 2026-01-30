#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#if ( defined GM_READ_K3D_GM   || defined GM_VISBECK_VARIABLE_K \
      || defined ALLOW_GM_LEITH_QG || defined GM_BATES_K3D )
# define GM_USE_K3D_GM
#endif

CBOP
C     !ROUTINE: GMREDI_CALC_PSI_BOLUS
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_PSI_BOLUS(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR, locK3dGM,
     I             ldd97_LrhoW, ldd97_LrhoS,
     I             myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_CALC_PSI_BOLUS
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
# include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
      _RL sigmaX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef GM_USE_K3D_GM
      _RL locK3dGM(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#else
      _RL locK3dGM(1)
#endif
      _RL ldd97_LrhoW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ldd97_LrhoS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
#ifdef GM_BOLUS_ADVEC

C     !LOCAL VARIABLES:
      INTEGER i,j,k, km1
#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (depends on tiles)
C     kkey :: tape key (depends on levels and tiles)
      INTEGER tkey, kkey
#endif
      _RL halfSign
      _RL rDepth, half_K
      _RL SlopeX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SlopeY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDrW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDrS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL taperX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL taperY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C-    Initialization : <= done in S/R gmredi_init

#ifdef ALLOW_AUTODIFF_TAMC
      tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
#endif /* ALLOW_AUTODIFF_TAMC */

      IF (GM_AdvForm) THEN
C     change sign of vertical Sigma gradient to match stratification sign
       halfSign = halfRL*gravitySign

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
          SlopeX(i,j) = ( sigmaX(i,j,km1)+sigmaX(i,j,k) )*halfRL
     &                   *maskW(i,j,km1,bi,bj)*maskW(i,j,k,bi,bj)
          dSigmaDrW(i,j) = ( sigmaR(i-1,j,k)+sigmaR(i,j,k) )*halfSign
     &                   *maskW(i,j,km1,bi,bj)*maskW(i,j,k,bi,bj)
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          SlopeY(i,j) = ( sigmaY(i,j,km1)+sigmaY(i,j,k) )*halfRL
     &                   *maskS(i,j,km1,bi,bj)*maskS(i,j,k,bi,bj)
          dSigmaDrS(i,j) = ( sigmaR(i,j-1,k)+sigmaR(i,j,k) )*halfSign
     &                   *maskS(i,j,km1,bi,bj)*maskS(i,j,k,bi,bj)
         ENDDO
        ENDDO

C      set "rDepth" (= depth from the surface, in rUnit) for 'ldd97' tapering
        IF ( usingZcoords ) THEN
         rDepth = rF(1) - rF(k)
        ELSE
         rDepth = rF(k) - rF(Nr+1)
        ENDIF
C      Calculate slopes , taper and/or clip
        CALL GMREDI_SLOPE_PSI(
     O              taperX, taperY,
     U              SlopeX, SlopeY,
     U              dSigmaDrW, dSigmaDrS,
     I              ldd97_LrhoW, ldd97_LrhoS, rDepth, k,
     I              bi, bj, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
        kkey = k + (tkey-1)*Nr
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
#ifdef GM_USE_K3D_GM
     &     *( op25
     &        *( locK3dGM(i-1,j,km1)+locK3dGM(i,j,km1)
     &         + locK3dGM(i-1,j,k)+locK3dGM(i,j,k) )
#else
     &     *( half_K
     &          *(GM_bolFac2d(i-1,j,bi,bj)+GM_bolFac2d(i,j,bi,bj))
#endif
#ifdef GM_GEOM_VARIABLE_K
     &      + op5*(GEOM_K3d(i-1,j,k,bi,bj)+GEOM_K3d(i,j,k,bi,bj))
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
#ifdef GM_USE_K3D_GM
     &     *( op25
     &        *( locK3dGM(i,j-1,km1)+locK3dGM(i,j,km1)
     &         + locK3dGM(i,j-1,k)+locK3dGM(i,j,k) )
#else
     &     *( half_K
     &          *(GM_bolFac2d(i,j-1,bi,bj)+GM_bolFac2d(i,j,bi,bj))
#endif
#ifdef GM_GEOM_VARIABLE_K
     &      + op5*(GEOM_K3d(i,j-1,k,bi,bj)+GEOM_K3d(i,j,k,bi,bj))
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
