C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_slope_limit.F,v 1.27 2008/05/30 21:10:25 gforget Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

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
#include "tamc_keys.h"
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
      _RL SlopeX     (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeY     (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeSqr   (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL taperFct   (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL hTransLay  (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL baseSlope  (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL recipLambda(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDr   (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDx   (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDy   (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL Lrho       (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL hMixLay    (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RS depthZ(*)
      INTEGER kLow   (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER k, bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI

C     !LOCAL VARIABLES:
C     == Local variables ==
      _RL Small_Taper
      PARAMETER(Small_Taper=1.D+03)

      _RL slopeTmpSpec,slopeMaxSpec
      _RL dSigmMod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeMod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dRdSigmaLtd(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL tmpfld(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL locVar(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL f1,Smod,f2,Rnondim
      _RL maxSlopeSqr
      _RL fpi
      PARAMETER( fpi = PI )
      INTEGER i,j
      _RL dTransLay, taperFctLoc, taperFctHat
      _RL  facTrL2ML
      _RL  maxTransLay
c     facTrL2ML = 1. _d 0
c     maxTransLay = 100. _d 0
      facTrL2ML = 5. _d 0
      maxTransLay = 500. _d 0

#ifdef ALLOW_AUTODIFF_TAMC
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
#endif /* ALLOW_AUTODIFF_TAMC */

      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        dSigmMod(i,j)    = 0. _d 0
        tmpfld(i,j)      = 0. _d 0
       ENDDO
      ENDDO


#ifndef GMREDI_WITH_STABLE_ADJOINT
c common case:

      IF (GM_taper_scheme.EQ.'orig' .OR.
     &    GM_taper_scheme.EQ.'clipping') THEN

#ifdef GM_EXCLUDE_CLIPPING

        STOP 'Need to compile without "#define GM_EXCLUDE_CLIPPING"'

#else  /* GM_EXCLUDE_CLIPPING */

C-      Original implementation in mitgcmuv
C       (this turns out to be the same as Cox slope clipping)

C-      Cox 1987 "Slope clipping"
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          tmpfld(i,j) = dSigmaDx(i,j)*dSigmaDx(i,j)
     &                + dSigmaDy(i,j)*dSigmaDy(i,j)
          IF ( tmpfld(i,j) .EQ. 0. ) THEN
           dSigmMod(i,j) = 0. _d 0
          ELSE
           dSigmMod(i,j) = SQRT( tmpfld(i,j) )
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE dSigmMod(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDr(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF (dSigmMod(i,j) .NE. 0.) THEN
           tmpfld(i,j) = -dSigmMod(i,j)*GM_rMaxSlope
           IF ( dSigmaDr(i,j) .GE. tmpfld(i,j) )
     &          dSigmaDr(i,j) = tmpfld(i,j)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE slopeX(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE slopeY(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDr(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF (dSigmMod(i,j) .EQ. 0.) THEN
           SlopeX(i,j) = 0. _d 0
           SlopeY(i,j) = 0. _d 0
          ELSE
           dRdSigmaLtd(i,j) = 1. _d 0/( dSigmaDr(i,j) )
           SlopeX(i,j)=-dSigmaDx(i,j)*dRdSigmaLtd(i,j)
           SlopeY(i,j)=-dSigmaDy(i,j)*dRdSigmaLtd(i,j)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE slopeX(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE slopeY(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          SlopeSqr(i,j)=SlopeX(i,j)*SlopeX(i,j)
     &                 +SlopeY(i,j)*SlopeY(i,j)
          taperFct(i,j)=1. _d 0
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
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
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
           tmpfld(i,j) = dSigmaDx(i,j)*dSigmaDx(i,j)
     &            + dSigmaDy(i,j)*dSigmaDy(i,j)
           IF ( tmpfld(i,j).GT.0. ) THEN
            locVar(i,j) = SQRT( tmpfld(i,j) )
c           dSigmMod(i,j) = locVar
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
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF ( hTransLay(i,j).LE.0. _d 0 ) THEN
C-     previously below the transition layer
            tmpfld(i,j) = Lrho(i,j)*SlopeMod(i,j)
C-     ensure that transition layer is not thicker than the larger
C      of the 2 : maxTransLay and facTrL2ML*hMixLay
            dTransLay =
     &        MIN( tmpfld(i,j),
     &             MAX( facTrL2ML*hMixLay(i,j), maxTransLay ) )
            IF ( k.GE.kLow(i,j) ) THEN
C-     bottom & below & 1rst level above the bottom :
              recipLambda(i,j) = 0. _d 0
              baseSlope(i,j)   = MIN( SlopeMod(i,j), GM_maxSlope )
C-- note: do not catch the 1rst level/interface (k=kLow) above the bottom
C         since no baseSlope has yet been stored (= 0); but might fit
C         well into transition layer criteria (if locally not stratified)
            ELSEIF ( dTransLay+hMixLay(i,j)+depthZ(k) .GE. 0. ) THEN 
C-     Found the transition Layer : set depth of base of Transition layer
              hTransLay(i,j) = -depthZ(k+1)
C      and compute inverse length scale "1/Lambda" from slope vert. grad
              IF ( baseSlope(i,j).GT.0. ) THEN
                recipLambda(i,j) = recipLambda(i,j) / baseSlope(i,j)
              ELSE
                recipLambda(i,j) = 0. _d 0
              ENDIF
            ELSE
C-     Still below Trans. layer: store slope & slope vert. grad.
              recipLambda(i,j) = ( SlopeMod(i,j) - baseSlope(i,j) )
     &                         / ( depthZ(k) - depthZ(k+1) )
              baseSlope(i,j)   = SlopeMod(i,j)
            ENDIF
          ENDIF
         ENDDO
        ENDDO

C-    c) Set Slope component according to vertical position
C      (in Mixed-Layer / in Transition Layer / below Transition Layer)
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF ( hTransLay(i,j).GT.0. _d 0 ) THEN
C-        already above base of transition layer:

            IF ( -depthZ(k).LE.hMixLay(i,j) ) THEN
C-        compute tapering function -G(z) in the mixed Layer:
              taperFctLoc =
     &          ( -depthZ(k)/(hTransLay(i,j)+hMixLay(i,j))
     &            *( 2. _d 0
     &               + (hTransLay(i,j)-hMixLay(i,j))*recipLambda(i,j) )
     &          )
C-        compute tapering function -G^(z) in the mixed Layer
              taperFctHat =
     &          ( -depthZ(k)/(hTransLay(i,j)+hMixLay(i,j))
     &            *  2. _d 0
     &            *( 1. _d 0
     &               + (hTransLay(i,j)-hMixLay(i,j))*recipLambda(i,j) )
     &          )
            ELSE
C-        compute tapering function -G(z) in the transition Layer:
              taperFctLoc =
     &          ( -depthZ(k)/(hTransLay(i,j)+hMixLay(i,j))
     &            *( 2. _d 0
     &               + (hTransLay(i,j)-hMixLay(i,j))*recipLambda(i,j) )
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
     &            *( 1. _d 0
     &               + (hTransLay(i,j)-hMixLay(i,j))*recipLambda(i,j) )
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
            SlopeSqr(i,j) = baseSlope(i,j)*Smod*taperFctHat

          ELSE
C--       Still below base of transition layer:
c           Smod = SlopeMod(i,j)
C-        safer to limit the slope:
            Smod = MIN( SlopeMod(i,j), GM_maxSlope )
            SlopeX(i,j) = SlopeX(i,j)*Smod
            SlopeY(i,j) = SlopeY(i,j)*Smod
c           SlopeSqr(i,j) = Smod*Smod
            SlopeSqr(i,j) = SlopeMod(i,j)*Smod

C--       end if baseSlope > 0 / else => above/below base of Trans. Layer
          ENDIF

         ENDDO
        ENDDO

#endif  /* GM_EXCLUDE_FM07_TAP */

      ELSE IF (GM_taper_scheme.EQ.'ac02') THEN

#ifdef GM_EXCLUDE_AC02_TAP

        STOP 'Need to compile without "#define GM_EXCLUDE_AC02_TAP"'

#else  /* GM_EXCLUDE_AC02_TAP */

C-      New Scheme (A. & C. 2002): relax part of the small slope approximation
C         compute the true slope (no approximation)
C         but still neglect Kxy & Kyx (assumed to be zero)

        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          dRdSigmaLtd(i,j)=
     &                        dSigmaDx(i,j)*dSigmaDx(i,j)
     &                      + dSigmaDy(i,j)*dSigmaDy(i,j)
     &                      + dSigmaDr(i,j)*dSigmaDr(i,j)
          taperFct(i,j) = 1. _d 0

          IF (dRdSigmaLtd(i,j).NE.0.) THEN
             dRdSigmaLtd(i,j)=1. _d 0
     &            / ( dRdSigmaLtd(i,j) )
             SlopeSqr(i,j)=(dSigmaDx(i,j)*dSigmaDx(i,j)
     &            +dSigmaDy(i,j)*dSigmaDy(i,j))*dRdSigmaLtd(i,j)
             SlopeX(i,j)=-dSigmaDx(i,j)
     &            *dRdSigmaLtd(i,j)*dSigmaDr(i,j)
             SlopeY(i,j)=-dSigmaDy(i,j)
     &            *dRdSigmaLtd(i,j)*dSigmaDr(i,j)
cph             T11(i,j)=(dSigmaDr(i,j)**2)*dRdSigmaLtd(i,j)
          ENDIF
#ifndef ALLOWW_AUTODIFF_TAMC
cph-- this part does not adjoint well
          IF ( SlopeSqr(i,j) .GT. maxSlopeSqr .AND.
     &         SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
           taperFct(i,j) = maxSlopeSqr/SlopeSqr(i,j)
          ELSE IF ( SlopeSqr(i,j) .GT. GM_slopeSqCutoff ) THEN
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
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
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

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF ( dSigmaDr(i,j) .EQ. 0. ) THEN
           IF ( dSigmaDx(i,j) .NE. 0. ) THEN
            SlopeX(i,j) = SIGN(Small_taper,dSigmaDx(i,j))
           ELSE
            SlopeX(i,j) = 0. _d 0
           ENDIF
           IF ( dSigmaDy(i,j) .NE. 0. ) THEN
            SlopeY(i,j) = SIGN(Small_taper,dSigmaDy(i,j))
           ELSE
            SlopeY(i,j) = 0. _d 0
           ENDIF
          ELSE
           dRdSigmaLtd(i,j) = 1. _d 0 / dSigmaDr(i,j)
           SlopeX(i,j)=-dSigmaDx(i,j)*dRdSigmaLtd(i,j)
           SlopeY(i,j)=-dSigmaDy(i,j)*dRdSigmaLtd(i,j)
c          SlopeX(i,j) = -dSigmaDx(i,j)/dSigmaDr(i,j)
c          SlopeY(i,j) = -dSigmaDy(i,j)/dSigmaDr(i,j)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE slopeX(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE slopeY(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          SlopeSqr(i,j) = SlopeX(i,j)*SlopeX(i,j)
     &                   +SlopeY(i,j)*SlopeY(i,j)
          taperFct(i,j) = 1. _d 0
          IF ( SlopeSqr(i,j) .GT. GM_slopeSqCutoff ) THEN
             slopeSqr(i,j) = GM_slopeSqCutoff
             taperFct(i,j) = 0. _d 0
          ENDIF
         ENDDO
        ENDDO

C- Compute the tapering function for the GM+Redi tensor :

       IF (GM_taper_scheme.EQ.'linear') THEN

C-      Simplest adiabatic tapering = Smax/Slope (linear)
        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF ( SlopeSqr(i,j) .EQ. 0. ) THEN
           taperFct(i,j) = 1. _d 0
          ELSE IF ( SlopeSqr(i,j) .GT. maxSlopeSqr .AND.
     &             SlopeSqr(i,j) .LT. GM_slopeSqCutoff )  THEN
           taperFct(i,j) = SQRT(maxSlopeSqr / SlopeSqr(i,j))
          ENDIF

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'gkw91') THEN

C-      Gerdes, Koberle and Willebrand, Clim. Dyn. 1991
        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF ( SlopeSqr(i,j) .EQ. 0. ) THEN
           taperFct(i,j) = 1. _d 0
          ELSE IF ( SlopeSqr(i,j) .GT. maxSlopeSqr .AND.
     &             SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
           taperFct(i,j) = maxSlopeSqr/SlopeSqr(i,j)
          ENDIF

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'dm95') THEN

C-      Danabasoglu and McWilliams, J. Clim. 1995
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF ( SlopeSqr(i,j) .EQ. 0. ) THEN
           taperFct(i,j) = 1. _d 0
          ELSE IF ( SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
           Smod=SQRT(SlopeSqr(i,j))
           taperFct(i,j)=op5*( 1. _d 0 + tanh( (GM_Scrit-Smod)/GM_Sd ))
          ENDIF
         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'ldd97') THEN

C-      Large, Danabasoglu and Doney, JPO 1997
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF (SlopeSqr(i,j) .EQ. 0.) THEN
           taperFct(i,j) = 1. _d 0
          ELSEIF ( SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
           Smod=SQRT(SlopeSqr(i,j))
           f1=op5*( 1. _d 0 + tanh( (GM_Scrit-Smod)/GM_Sd ))
           Rnondim= -depthZ(k)/(Lrho(i,j)*Smod)
           IF ( Rnondim.GE.1. _d 0 ) THEN
             f2 = 1. _d 0
           ELSE
             f2 = op5*( 1. _d 0 + SIN( fpi*(Rnondim-op5) ))
           ENDIF
           taperFct(i,j)=f1*f2
          ENDIF

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.NE.' ') THEN
        STOP 'GMREDI_SLOPE_LIMIT: Bad GM_taper_scheme'
       ENDIF

#endif /* GM_EXCLUDE_TAPERING */

      ENDIF


#else  /* GMREDI_WITH_STABLE_ADJOINT */
c special choice for adjoint/optimization of parameters
c (~ strong clipping, reducing non linearity of kw=f(K))


#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE dSigmaDr(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
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

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF ( dSigmaDr(i,j) .EQ. 0. ) THEN
           IF ( dSigmaDx(i,j) .NE. 0. ) THEN
            SlopeX(i,j) = SIGN(Small_taper,dSigmaDx(i,j))
           ELSE
            SlopeX(i,j) = 0. _d 0
           ENDIF
           IF ( dSigmaDy(i,j) .NE. 0. ) THEN
            SlopeY(i,j) = SIGN(Small_taper,dSigmaDy(i,j))
           ELSE
            SlopeY(i,j) = 0. _d 0
           ENDIF
          ELSE
           dRdSigmaLtd(i,j) = 1. _d 0 / dSigmaDr(i,j)
           SlopeX(i,j)=-dSigmaDx(i,j)*dRdSigmaLtd(i,j)
           SlopeY(i,j)=-dSigmaDy(i,j)*dRdSigmaLtd(i,j)
          ENDIF
         ENDDO
        ENDDO

        slopeMaxSpec=2. _d -3

CADJ STORE SlopeX(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE SlopeY(:,:)     = comlev1_bibj, key=ikey, byte=isbyte

        DO j=1-Oly,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
        slopeTmpSpec=sqrt( SlopeX(i,j)*SlopeX(i,j)
     &              +SlopeY(i,j)*SlopeY(i,j) )
       IF ( slopeTmpSpec .GT. slopeMaxSpec ) then
        SlopeX(i,j)=SlopeX(i,j)*slopeMaxSpec/slopeTmpSpec
        SlopeY(i,j)=SlopeY(i,j)*slopeMaxSpec/slopeTmpSpec
       ENDIF
          SlopeSqr(i,j) = SlopeX(i,j)*SlopeX(i,j)
     &                   +SlopeY(i,j)*SlopeY(i,j)
          taperFct(i,j) = 1. _d 0
         ENDDO
        ENDDO

#endif /* GMREDI_WITH_STABLE_ADJOINT */


#endif /* ALLOW_GMREDI */

      RETURN
      END
