#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: GMREDI_SLOPE_LIMIT
C     !INTERFACE:
      SUBROUTINE GMREDI_SLOPE_LIMIT(
     O             SlopeX, SlopeY,
     O             SlopeSqr, taperFct,
     U             hTransLay, baseSlope, recipLambda,
     U             dSigmaDr,
     I             dSigmaDx, dSigmaDy,
     I             Lrho, hMixLay, depthZ, kLow,
     I             k, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_SLOPE_LIMIT
C     | o Calculate slopes for use in GM/Redi tensor
C     *==========================================================*
C     | On entry:
C     |            dSigmaDr     contains the d/dz Sigma
C     |            dSigmaDx/Dy  contains X/Y gradients of sigma
C     |            depthZ       contains the depth (< 0 !) [m]
C     |            Lrho
C     |            hMixLay      mixed layer depth (> 0)
C    U             hTransLay    transition layer depth (> 0)
C    U             baseSlope, recipLambda,
C     | On exit:
C     |            dSigmaDr     contains the effective dSig/dz
C     |            SlopeX/Y     contains X/Y slopes
C     |            SlopeSqr     contains Sx^2+Sy^2
C     |            taperFct     contains tapering funct. value ;
C     |                         = 1 when using no tapering
C    U             hTransLay    transition layer depth (> 0)
C    U             baseSlope, recipLambda
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "GMREDI.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     == Routine arguments ==
C     !INPUT PARAMETERS:
C     dSigmaDx :: zonal      gradient of density
C     dSigmaDy :: meridional gradient of density
C     Lrho     ::
C     hMixLay  :: mixed layer thickness (> 0) [m]
C     depthZ   :: model discretized depth (< 0) [m]
C     kLow     :: bottom level index 2-D array
C     k        :: level index
C     bi, bj   :: tile indices
C     myTime   :: time in simulation
C     myIter   :: iteration number in simulation
C     myThid   :: My Thread Id. number
C     !OUTPUT PARAMETERS:
C     SlopeX      :: isopycnal slope in X direction
C     SlopeY      :: isopycnal slope in Y direction
C     SlopeSqr    :: square of isopycnal slope
C     taperFct    :: tapering function
C     hTransLay   :: depth of the base of the transition layer (> 0) [m]
C     baseSlope   :: slope at the the base of the transition layer
C     recipLambda :: Slope vertical gradient at Trans. Layer Base (=recip.Lambda)
C     dSigmaDr    :: vertical gradient of density
      _RL SlopeX     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SlopeY     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SlopeSqr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL taperFct   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hTransLay  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL baseSlope  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recipLambda(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDx   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDy   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Lrho       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hMixLay    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS depthZ(*)
      INTEGER kLow   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
C     !LOCAL VARIABLES:
C     == Local variables ==
      _RL fpi
      PARAMETER( fpi = PI )
      INTEGER i, j
      _RL f1, Smod, f2, Rnondim
      _RL maxSlopeSqr
      _RL dSigmMod   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dRdSigmaLtd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpFld     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifndef GM_EXCLUDE_FM07_TAP
      _RL dTransLay, rLambMin, DoverLamb
      _RL taperFctLoc, taperFctHat
      _RL minTransLay
      _RL SlopeMod(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL locVar  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
#ifdef GMREDI_WITH_STABLE_ADJOINT
      _RL slopeSqTmp, slopeTmp, slopeMax
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ikey
#endif

C-   need to put this one in GM namelist :
      _RL GM_bigSlope
      GM_bigSlope = 1. _d +02

#ifdef ALLOW_AUTODIFF_TAMC
C     TAF thinks for some reason that depthZ is an active variable.
C     While this does not make the adjoint code wrong, the resulting
C     code inhibits vectorization in some cases so we tell TAF here
C     that depthZ is actually a passive grid variable that needs no adjoint
CADJ PASSIVE depthz
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ikey = (act1 + 1) + act2*max1
     &                  + act3*max1*max2
     &                  + act4*max1*max2*max3
C     Without this store directive, TAF generates an extra field anyway
C     so we do it here explicitly with a local tape and live without the
C     corresponding warning.
CADJ INIT loctape_gm = COMMON, 1
CADJ STORE dSigmaDr = loctape_gm
#endif /* ALLOW_AUTODIFF_TAMC */

      DO j=1-OLy+1,sNy+OLy-1
       DO i=1-OLx+1,sNx+OLx-1
        dSigmMod(i,j)    = 0. _d 0
        tmpFld(i,j)      = 0. _d 0
       ENDDO
      ENDDO

      IF (GM_taper_scheme.EQ.'orig' .OR.
     &    GM_taper_scheme.EQ.'clipping') THEN

#ifdef GM_EXCLUDE_CLIPPING

        STOP 'Need to compile without "#define GM_EXCLUDE_CLIPPING"'

#else  /* GM_EXCLUDE_CLIPPING */

C-      Original implementation in mitgcmuv
C       (this turns out to be the same as Cox slope clipping)

C-      Cox 1987 "Slope clipping"
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          tmpFld(i,j) = dSigmaDx(i,j)*dSigmaDx(i,j)
     &                + dSigmaDy(i,j)*dSigmaDy(i,j)
          IF ( tmpFld(i,j) .EQ. 0. ) THEN
           dSigmMod(i,j) = 0. _d 0
          ELSE
           dSigmMod(i,j) = SQRT( tmpFld(i,j) )
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE dSigmMod(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDr(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          IF (dSigmMod(i,j) .NE. 0.) THEN
           tmpFld(i,j) = -dSigmMod(i,j)*GM_rMaxSlope
           IF ( dSigmaDr(i,j) .GE. tmpFld(i,j) )
     &          dSigmaDr(i,j) = tmpFld(i,j)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE slopeX(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE slopeY(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDr(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          IF (dSigmMod(i,j) .EQ. 0.) THEN
           SlopeX(i,j) = 0. _d 0
           SlopeY(i,j) = 0. _d 0
          ELSE
           dRdSigmaLtd(i,j) = 1. _d 0/( dSigmaDr(i,j) )
           SlopeX(i,j) = -dSigmaDx(i,j)*dRdSigmaLtd(i,j)
           SlopeY(i,j) = -dSigmaDy(i,j)*dRdSigmaLtd(i,j)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE slopeX(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE slopeY(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          SlopeSqr(i,j) = SlopeX(i,j)*SlopeX(i,j)
     &                  + SlopeY(i,j)*SlopeY(i,j)
          taperFct(i,j) = 1. _d 0
         ENDDO
        ENDDO

#endif /* GM_EXCLUDE_CLIPPING */

      ELSEIF (GM_taper_scheme.EQ.'fm07' ) THEN
C--   Ferrari & Mc.Williams, 2007:

#ifdef GM_EXCLUDE_FM07_TAP

        STOP 'Need to compile without "#define GM_EXCLUDE_FM07_TAP"'

#else  /* GM_EXCLUDE_FM07_TAP */

C-    a) Calculate separately slope magnitude (SlopeMod)
C        and slope horiz. direction (Slope_X,Y : normalized)
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          IF ( k.GT.kLow(i,j) ) THEN
C-        Bottom or below:
            SlopeX  (i,j) = 0. _d 0
            SlopeY  (i,j) = 0. _d 0
            SlopeMod(i,j) = 0. _d 0
            taperFct(i,j) = 0. _d 0
          ELSE
C-        Above bottom:
           IF ( dSigmaDr(i,j).GE. -GM_Small_Number )
     &          dSigmaDr(i,j) = -GM_Small_Number
           tmpFld(i,j) = dSigmaDx(i,j)*dSigmaDx(i,j)
     &                 + dSigmaDy(i,j)*dSigmaDy(i,j)
           IF ( tmpFld(i,j).GT.0. ) THEN
            locVar(i,j) = SQRT( tmpFld(i,j) )
            SlopeX  (i,j) = dSigmaDx(i,j)/locVar(i,j)
            SlopeY  (i,j) = dSigmaDy(i,j)/locVar(i,j)
            SlopeMod(i,j) = -locVar(i,j)/dSigmaDr(i,j)
            taperFct(i,j) = 1. _d 0
           ELSE
            SlopeX  (i,j) = 0. _d 0
            SlopeY  (i,j) = 0. _d 0
            SlopeMod(i,j) = 0. _d 0
            taperFct(i,j) = 0. _d 0
           ENDIF
          ENDIF
         ENDDO
        ENDDO

C-    b) Set Transition Layer Depth:
        IF ( k.EQ.1 ) THEN
          minTransLay = GM_facTrL2dz*( depthZ(k) - depthZ(k+1) )
        ELSE
          minTransLay = GM_facTrL2dz*( depthZ(k-1) - depthZ(k) )
        ENDIF
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          IF ( hTransLay(i,j).LE.0. _d 0 ) THEN
C-     previously below the transition layer
            tmpFld(i,j) = Lrho(i,j)*SlopeMod(i,j)
C-     ensure that transition layer is at least as thick than current level and
C      not thicker than the larger of the 2 : maxTransLay and facTrL2ML*hMixLay
            dTransLay =
     &        MIN( MAX( tmpFld(i,j), minTransLay ),
     &             MAX( GM_facTrL2ML*hMixLay(i,j), GM_maxTransLay ) )
            IF ( k.GE.kLow(i,j) ) THEN
C-     bottom & below & 1rst level above the bottom :
              recipLambda(i,j) = 0. _d 0
              baseSlope(i,j)   = SlopeMod(i,j)
C-- note: do not catch the 1rst level/interface (k=kLow) above the bottom
C         since no baseSlope has yet been stored (= 0); but might fit
C         well into transition layer criteria (if locally not stratified)
            ELSEIF ( dTransLay+hMixLay(i,j)+depthZ(k) .GE. 0. ) THEN
C-     Found the transition Layer : set depth of base of Transition layer
              hTransLay(i,j) = -depthZ(k+1)
C      and compute inverse length scale "1/Lambda" from slope vert. grad
              IF ( baseSlope(i,j).GT.0. ) THEN
                recipLambda(i,j) = recipLambda(i,j)
     &                           / MIN( baseSlope(i,j), GM_maxSlope )
              ELSE
                recipLambda(i,j) = 0. _d 0
              ENDIF
C      slope^2 & Kwz should remain > 0 : prevent too large negative D/lambda
              IF ( hMixLay(i,j)+depthZ(k+1).LT.0. ) THEN
                rLambMin = 1. _d 0 /( hMixLay(i,j)+depthZ(k+1) )
                recipLambda(i,j) = MAX( recipLambda(i,j), rLambMin )
              ENDIF
            ELSE
C-     Still below Trans. layer: store slope & slope vert. grad.
              recipLambda(i,j) = ( MIN( SlopeMod(i,j), GM_maxSlope )
     &                           - MIN( baseSlope(i,j), GM_maxSlope )
     &                           ) / ( depthZ(k) - depthZ(k+1) )
              baseSlope(i,j)   = SlopeMod(i,j)
            ENDIF
          ENDIF
         ENDDO
        ENDDO

C-    c) Set Slope component according to vertical position
C      (in Mixed-Layer / in Transition Layer / below Transition Layer)
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          IF ( hTransLay(i,j).GT.0. _d 0 ) THEN
C-        already above base of transition layer:

            DoverLamb = (hTransLay(i,j)-hMixLay(i,j))*recipLambda(i,j)
            IF ( -depthZ(k).LE.hMixLay(i,j) ) THEN
C-        compute tapering function -G(z) in the mixed Layer:
              taperFctLoc =
     &          ( -depthZ(k)/(hTransLay(i,j)+hMixLay(i,j))
     &            *( 2. _d 0 + DoverLamb )
     &          )
C-        compute tapering function -G^(z) in the mixed Layer
              taperFctHat =
     &          ( -depthZ(k)/(hTransLay(i,j)+hMixLay(i,j))
     &            *  2. _d 0
     &            *( 1. _d 0 + DoverLamb )
     &          )
            ELSE
C-        compute tapering function -G(z) in the transition Layer:
              taperFctLoc =
     &          ( -depthZ(k)/(hTransLay(i,j)+hMixLay(i,j))
     &            *( 2. _d 0 + DoverLamb )
     &          )
     &        - ( (depthZ(k)+hMixLay(i,j))*(depthZ(k)+hMixLay(i,j))
     &            /( hTransLay(i,j)*hTransLay(i,j)
     &               - hMixLay(i,j)*hMixLay(i,j)  )
     &            *( 1. _d 0 + hTransLay(i,j)*recipLambda(i,j) )
     &          )
C-        compute tapering function -G^(z) in the transition Layer:
              taperFctHat =
     &          ( -depthZ(k)/(hTransLay(i,j)+hMixLay(i,j))
     &            *  2. _d 0
     &            *( 1. _d 0 + DoverLamb )
     &          )
     &        - ( (depthZ(k)+hMixLay(i,j))*(depthZ(k)+hMixLay(i,j))
     &            /( hTransLay(i,j)*hTransLay(i,j)
     &               - hMixLay(i,j)*hMixLay(i,j)  )
     &            *( 1. _d 0 + hTransLay(i,j)*recipLambda(i,j)*2. _d 0 )
     &          )
            ENDIF
C-        modify the slope (above bottom of transition layer):
c           Smod = baseSlope(i,j)
C-        safer to limit the slope (even if it might never exceed GM_maxSlope)
            Smod = MIN( baseSlope(i,j), GM_maxSlope )
            SlopeX(i,j) = SlopeX(i,j)*Smod*taperFctLoc
            SlopeY(i,j) = SlopeY(i,j)*Smod*taperFctLoc
c           SlopeSqr(i,j) = Smod*Smod*taperFctHat
c           SlopeSqr(i,j) = baseSlope(i,j)*Smod*taperFctHat
            SlopeSqr(i,j) = MIN( baseSlope(i,j), GM_bigSlope )
     &                     *Smod*taperFctHat

          ELSE
C--       Still below base of transition layer:
c           Smod = SlopeMod(i,j)
C-        safer to limit the slope:
            Smod = MIN( SlopeMod(i,j), GM_maxSlope )
            SlopeX(i,j) = SlopeX(i,j)*Smod
            SlopeY(i,j) = SlopeY(i,j)*Smod
c           SlopeSqr(i,j) = Smod*Smod
c           SlopeSqr(i,j) = SlopeMod(i,j)*Smod
            SlopeSqr(i,j) = MIN( SlopeMod(i,j), GM_bigSlope )
     &                     *Smod

C--       end if baseSlope > 0 / else => above/below base of Trans. Layer
          ENDIF

         ENDDO
        ENDDO

#endif  /* GM_EXCLUDE_FM07_TAP */

      ELSEIF (GM_taper_scheme.EQ.'ac02') THEN

#ifdef GM_EXCLUDE_AC02_TAP

        STOP 'Need to compile without "#define GM_EXCLUDE_AC02_TAP"'

#else  /* GM_EXCLUDE_AC02_TAP */

C-      New Scheme (A. & C. 2002): relax part of the small slope approximation
C         compute the true slope (no approximation)
C         but still neglect Kxy & Kyx (assumed to be zero)

        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          dRdSigmaLtd(i,j) = dSigmaDx(i,j)*dSigmaDx(i,j)
     &                     + dSigmaDy(i,j)*dSigmaDy(i,j)
     &                     + dSigmaDr(i,j)*dSigmaDr(i,j)
          taperFct(i,j) = 1. _d 0

          IF (dRdSigmaLtd(i,j).NE.0.) THEN
             dRdSigmaLtd(i,j) = 1. _d 0 / ( dRdSigmaLtd(i,j) )
             SlopeSqr(i,j) = ( dSigmaDx(i,j)*dSigmaDx(i,j)
     &                       + dSigmaDy(i,j)*dSigmaDy(i,j)
     &                       )*dRdSigmaLtd(i,j)
             SlopeX(i,j) = -dSigmaDx(i,j)
     &                     *dRdSigmaLtd(i,j)*dSigmaDr(i,j)
             SlopeY(i,j) = -dSigmaDy(i,j)
     &                     *dRdSigmaLtd(i,j)*dSigmaDr(i,j)
          ELSE
             SlopeSqr(i,j) = 0. _d 0
             SlopeX(i,j) = 0. _d 0
             SlopeY(i,j) = 0. _d 0
          ENDIF
#ifndef ALLOWW_AUTODIFF_TAMC
cph-- this part does not adjoint well
          IF ( SlopeSqr(i,j) .GT. maxSlopeSqr .AND.
     &         SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
           taperFct(i,j) = maxSlopeSqr/SlopeSqr(i,j)
          ELSEIF ( SlopeSqr(i,j) .GE. GM_slopeSqCutoff ) THEN
           taperFct(i,j) = 0. _d 0
          ENDIF
#endif
         ENDDO
        ENDDO

#endif /* GM_EXCLUDE_AC02_TAP */

      ELSE

#ifdef GM_EXCLUDE_TAPERING

        STOP 'Need to compile without "#define GM_EXCLUDE_TAPERING"'

#else  /* GM_EXCLUDE_TAPERING */

C----------------------------------------------------------------------

C- Compute the slope, no clipping, but avoid reverse slope in negatively
C                                  stratified (Sigma_Z > 0) region :

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE dSigmaDr(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
C     Without this store directive, TAF generates an extra field anyway
C     so we do it here explicitly with a local tape and live without the
C     corresponding warning.
CADJ STORE dSigmaDr = loctape_gm
#endif

        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          IF ( dSigmaDr(i,j) .NE. 0. ) THEN
           IF (dSigmaDr(i,j).GE.(-GM_Small_Number))
     &         dSigmaDr(i,j) = -GM_Small_Number
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE dSigmaDx(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDy(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDr(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          IF ( dSigmaDr(i,j) .EQ. 0. ) THEN
           IF ( dSigmaDx(i,j) .NE. 0. ) THEN
            SlopeX(i,j) = SIGN( GM_bigSlope, dSigmaDx(i,j) )
           ELSE
            SlopeX(i,j) = 0. _d 0
           ENDIF
           IF ( dSigmaDy(i,j) .NE. 0. ) THEN
            SlopeY(i,j) = SIGN( GM_bigSlope, dSigmaDy(i,j) )
           ELSE
            SlopeY(i,j) = 0. _d 0
           ENDIF
          ELSE
           dRdSigmaLtd(i,j) = 1. _d 0 / dSigmaDr(i,j)
           SlopeX(i,j) = -dSigmaDx(i,j)*dRdSigmaLtd(i,j)
           SlopeY(i,j) = -dSigmaDy(i,j)*dRdSigmaLtd(i,j)
c          SlopeX(i,j) = -dSigmaDx(i,j)/dSigmaDr(i,j)
c          SlopeY(i,j) = -dSigmaDy(i,j)/dSigmaDr(i,j)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE slopeX(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE slopeY(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          SlopeSqr(i,j) = SlopeX(i,j)*SlopeX(i,j)
     &                   +SlopeY(i,j)*SlopeY(i,j)
          taperFct(i,j) = 1. _d 0
          IF ( SlopeSqr(i,j) .GE. GM_slopeSqCutoff ) THEN
            slopeSqr(i,j) = GM_slopeSqCutoff
            taperFct(i,j) = 0. _d 0
          ENDIF
         ENDDO
        ENDDO

C- Compute the tapering function for the GM+Redi tensor :

       IF (GM_taper_scheme.EQ.'linear') THEN

C-      Simplest adiabatic tapering = Smax/Slope (linear)
        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1

          IF ( SlopeSqr(i,j) .EQ. 0. ) THEN
           taperFct(i,j) = 1. _d 0
          ELSEIF ( SlopeSqr(i,j) .GT. maxSlopeSqr .AND.
     &             SlopeSqr(i,j) .LT. GM_slopeSqCutoff )  THEN
           taperFct(i,j) = SQRT(maxSlopeSqr / SlopeSqr(i,j))
           slopeSqr(i,j) = MIN( slopeSqr(i,j),GM_bigSlope*GM_bigSlope )
          ENDIF

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'gkw91') THEN

C-      Gerdes, Koberle and Willebrand, Clim. Dyn. 1991
        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1

          IF ( SlopeSqr(i,j) .EQ. 0. ) THEN
           taperFct(i,j) = 1. _d 0
          ELSEIF ( SlopeSqr(i,j) .GT. maxSlopeSqr .AND.
     &             SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
           taperFct(i,j) = maxSlopeSqr/SlopeSqr(i,j)
          ENDIF

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'dm95') THEN

C-      Danabasoglu and McWilliams, J. Clim. 1995
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1

          IF ( SlopeSqr(i,j) .EQ. 0. ) THEN
           taperFct(i,j) = 1. _d 0
          ELSEIF ( SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
           Smod = SQRT(SlopeSqr(i,j))
           taperFct(i,j) = op5*( oneRL + TANH( (GM_Scrit-Smod)/GM_Sd ) )
          ENDIF
         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'ldd97') THEN

C-      Large, Danabasoglu and Doney, JPO 1997
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1

          IF (SlopeSqr(i,j) .EQ. 0.) THEN
           taperFct(i,j) = 1. _d 0
          ELSEIF ( SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
           Smod = SQRT(SlopeSqr(i,j))
           f1 = op5*( oneRL + TANH( (GM_Scrit-Smod)/GM_Sd ) )
           Rnondim = -depthZ(k)/(Lrho(i,j)*Smod)
           IF ( Rnondim.GE.1. _d 0 ) THEN
             f2 = 1. _d 0
           ELSE
             f2 = op5*( 1. _d 0 + SIN( fpi*(Rnondim-op5) ) )
           ENDIF
           taperFct(i,j) = f1*f2
          ENDIF

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'stableGmAdjTap') THEN

#ifdef GMREDI_WITH_STABLE_ADJOINT

C special choice for adjoint/optimization of parameters
C (~ strong clipping, reducing non linearity of kw=f(K))

        slopeMax= 2. _d -3

CADJ STORE SlopeX(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE SlopeY(:,:)     = comlev1_bibj, key=ikey, byte=isbyte

        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          slopeSqTmp = SlopeX(i,j)*SlopeX(i,j)
     &               + SlopeY(i,j)*SlopeY(i,j)

          IF ( slopeSqTmp .GT. slopeMax**2 ) then
           slopeTmp = SQRT(slopeSqTmp)
           SlopeX(i,j) = SlopeX(i,j)*slopeMax/slopeTmp
           SlopeY(i,j) = SlopeY(i,j)*slopeMax/slopeTmp
          ENDIF
         ENDDO
        ENDDO

C move the assignemnt of SlopeSqr to its own do-loop block from the do-loop
C block above to reduce TAF recomputations. The assignment of taperFct is
C also moved to keep the original order of operations.
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          SlopeSqr(i,j) = SlopeX(i,j)*SlopeX(i,j)
     &                  + SlopeY(i,j)*SlopeY(i,j)
          taperFct(i,j) = 1. _d 0
         ENDDO
        ENDDO

#else  /* GMREDI_WITH_STABLE_ADJOINT */

        STOP 'Need to compile wth "#define GMREDI_WITH_STABLE_ADJOINT"'

#endif /* GMREDI_WITH_STABLE_ADJOINT */

       ELSEIF (GM_taper_scheme.NE.' ') THEN
        STOP 'GMREDI_SLOPE_LIMIT: Bad GM_taper_scheme'
       ENDIF

#endif /* GM_EXCLUDE_TAPERING */

      ENDIF

#endif /* ALLOW_GMREDI */

      RETURN
      END
