C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_solve4temp.F,v 1.21 2012/01/30 03:38:20 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_SOLVE4TEMP
C     !INTERFACE:
      SUBROUTINE SEAICE_SOLVE4TEMP(
     I   UG, HICE_ACTUAL, HSNOW_ACTUAL,
#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET
     I   F_lh_max,
#endif
     U   TSURF,
     O   F_ia, IcePenetSW,
     O   FWsublim,
     I   bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SOLVE4TEMP
C     | o Calculate ice growth rate, surface fluxes and
C     |   temperature of ice surface.
C     |   see Hibler, MWR, 108, 1943-1973, 1980
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#ifdef SEAICE_VARIABLE_FREEZING_POINT
#include "DYNVARS.h"
#endif /* SEAICE_VARIABLE_FREEZING_POINT */
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT PARAMETERS:
C     UG           :: atmospheric wind speed (m/s)
C     HICE_ACTUAL  :: actual ice thickness
C     HSNOW_ACTUAL :: actual snow thickness
C     TSURF      :: surface temperature of ice/snow in Kelvin
C     bi,bj      :: tile indices
C     myTime     :: current time in simulation
C     myIter     :: iteration number in simulation
C     myThid     :: my Thread Id number
C     !OUTPUT PARAMETERS:
C     TSURF      :: updated surface temperature of ice/snow in Kelvin
C     F_ia       :: upward seaice/snow surface heat flux to atmosphere (W/m^2)
C     IcePenetSW :: short wave heat flux transmitted through ice (+=upward)
C     FWsublim   :: fresh water (mass) flux due to sublimation (+=up)(kg/m^2/s)
      _RL UG          (1:sNx,1:sNy)
      _RL HICE_ACTUAL (1:sNx,1:sNy)
      _RL HSNOW_ACTUAL(1:sNx,1:sNy)
#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET
      _RL F_lh_max    (1:sNx,1:sNy)
#endif
      _RL TSURF       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL F_ia        (1:sNx,1:sNy)
      _RL IcePenetSW  (1:sNx,1:sNy)
      _RL FWsublim    (1:sNx,1:sNy)
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#if defined(ALLOW_ATM_TEMP) && defined(ALLOW_DOWNWARD_RADIATION)
C     !LOCAL VARIABLES:
C     === Local variables ===
C     i, j  :: Loop counters
C     kSrf  :: vertical index of surface layer
      INTEGER i, j
#ifdef SEAICE_VARIABLE_FREEZING_POINT
      INTEGER kSrf
#endif /* SEAICE_VARIABLE_FREEZING_POINT */
      INTEGER ITER
C     TB :: ocean temperature in contact with ice (=seawater freezing point) (K)
      _RL TB         (1:sNx,1:sNy)
      _RL D1, D1I
      _RL D3(1:sNx,1:sNy)
      _RL TMELT, XKI, XKS, HCUT, XIO
      _RL SurfMeltTemp
C     effConduct :: effective conductivity of combined ice and snow
      _RL effConduct(1:sNx,1:sNy)
C     lhSublim :: latent heat of sublimation (SEAICE_lhEvap + SEAICE_lhFusion)
      _RL lhSublim
C     t1,t2,t3,t4 :: powers of temperature
      _RL  t1, t2, t3, t4

C     Constants to calculate Saturation Vapor Pressure
      _RL dqhice_dTice
#ifdef SEAICE_SOLVE4TEMP_LEGACY
      _RL C1, C2, C3, C4, C5, QS1
#else  /* SEAICE_SOLVE4TEMP_LEGACY */
      _RL dFiDTs1
      _RL lnTEN
      _RL aa1,aa2,bb1,bb2,Ppascals,cc0,cc1,cc2,cc3t
C     specific humidity at ice surface variables
      _RL mm_pi,mm_log10pi
#endif /* SEAICE_SOLVE4TEMP_LEGACY */

C     F_c      :: conductive heat flux through seaice+snow (+=downward)
C     F_lh     :: latent heat flux (sublimation) (+=upward)
#ifdef SEAICE_SOLVE4TEMP_LEGACY
C     A1 :: part of atmos surface flux (+=downward) independent of tsurf
C     A2 :: part of atmos surface flux (+=upward) which depends on tsurf
C     A3 :: derivative of (A2+F_c) versus tsurf
      _RL A2         (1:sNx,1:sNy)
      _RL A3         (1:sNx,1:sNy)
      _RL A1         (1:sNx,1:sNy)
#else /* SEAICE_SOLVE4TEMP_LEGACY */
      _RL F_lwu      (1:sNx,1:sNy)
      _RL F_sens     (1:sNx,1:sNy)
#endif /* SEAICE_SOLVE4TEMP_LEGACY */
      _RL F_lh       (1:sNx,1:sNy)
      _RL F_c        (1:sNx,1:sNy)
      _RL qhice      (1:sNx,1:sNy)
      _RL absorbedSW (1:sNx,1:sNy)
      _RL penetSWFrac

C     local copies of global variables
      _RL tsurfLoc   (1:sNx,1:sNy)
      _RL atempLoc   (1:sNx,1:sNy)
      _RL lwdownLoc  (1:sNx,1:sNy)
      _RL ALB        (1:sNx,1:sNy)
      _RL ALB_ICE    (1:sNx,1:sNy)
      _RL ALB_SNOW   (1:sNx,1:sNy)
C     iceOrNot :: this is HICE_ACTUAL.GT.0.
      LOGICAL iceOrNot(1:sNx,1:sNy)
#ifdef SEAICE_DEBUG
C     F_io_net :: upward conductive heat flux through seaice+snow
C     F_ia_net :: net heat flux divergence at the sea ice/snow surface:
C                 includes ice conductive fluxes and atmospheric fluxes (W/m^2)
      _RL F_io_net   (1:sNx,1:sNy)
      _RL F_ia_net   (1:sNx,1:sNy)
#endif /* SEAICE_DEBUG */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT comlev1_solve4temp = COMMON, sNx*sNy*NMAX_TICE
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef SEAICE_SOLVE4TEMP_LEGACY
C     MAYKUT CONSTANTS FOR SAT. VAP. PRESSURE TEMP. POLYNOMIAL
      C1=    2.7798202  _d -06
      C2=   -2.6913393  _d -03
      C3=    0.97920849 _d +00
      C4= -158.63779    _d +00
      C5= 9653.1925     _d +00

      QS1=0.622 _d +00/1013.0 _d +00
#else /* SEAICE_SOLVE4TEMP_LEGACY */
      lnTEN = LOG(10.0 _d 0)
      aa1 = 2663.5 _d 0
      aa2 = 12.537 _d 0
      bb1 = 0.622 _d 0
      bb2 = 1.0 _d 0 - bb1
      Ppascals = 100000. _d 0
C     cc0 = TEN ** aa2
      cc0 = EXP(aa2*lnTEN)
      cc1 = cc0*aa1*bb1*Ppascals*lnTEN
      cc2 = cc0*bb2
#endif /* SEAICE_SOLVE4TEMP_LEGACY */

#ifdef SEAICE_VARIABLE_FREEZING_POINT
      kSrf = 1
#endif /* SEAICE_VARIABLE_FREEZING_POINT */

C     SENSIBLE HEAT CONSTANT
      D1=SEAICE_dalton*SEAICE_cpAir*SEAICE_rhoAir

C     ICE LATENT HEAT CONSTANT
      lhSublim = SEAICE_lhEvap + SEAICE_lhFusion
      D1I=SEAICE_dalton*lhSublim*SEAICE_rhoAir

C     MELTING TEMPERATURE OF ICE
#ifdef SEAICE_SOLVE4TEMP_LEGACY
      TMELT        = 273.16  _d +00
      SurfMeltTemp = 273.159 _d +00
#else /* SEAICE_SOLVE4TEMP_LEGACY */
      TMELT        = celsius2K
      SurfMeltTemp = TMELT
#endif /* SEAICE_SOLVE4TEMP_LEGACY */

C     ICE CONDUCTIVITY
      XKI=SEAICE_iceConduct

C     SNOW CONDUCTIVITY
      XKS=SEAICE_snowConduct

C     CUTOFF SNOW THICKNESS
C     Snow-Thickness above HCUT: SW optically thick snow (=> snow-albedo).
C     Snow-Thickness below HCUT: linear transition to ice-albedo
#ifdef SEAICE_SOLVE4TEMP_LEGACY
      HCUT = SEAICE_snowThick
#else /* SEAICE_SOLVE4TEMP_LEGACY */
      HCUT = 0. _d 0
#endif /* SEAICE_SOLVE4TEMP_LEGACY */

C     PENETRATION SHORTWAVE RADIATION FACTOR
      XIO=SEAICE_shortwave

C     Initialize variables
      DO J=1,sNy
       DO I=1,sNx
C     HICE_ACTUAL is modified in this routine, but at the same time
C     used to decided where there is ice, therefore we save this information
C     here in a separate array
        iceOrNot  (I,J) = HICE_ACTUAL(I,J) .GT. 0. _d 0
        IcePenetSW(I,J) = 0. _d 0
        absorbedSW(I,J) = 0. _d 0
        qhice    (I,J) = 0. _d 0
        F_ia     (I,J) = 0. _d 0
c       F_io_net (I,J) = 0. _d 0
c       F_ia_net (I,J) = 0. _d 0
        F_lh     (I,J) = 0. _d 0

C     Reset the snow/ice surface to TMELT and bound the atmospheric temperature
#ifdef SEAICE_SOLVE4TEMP_LEGACY
        A1(I,J) = 0.0 _d 0
        A2(I,J) = 0.0 _d 0
        A3(I,J) = 0.0 _d 0
        tsurfLoc (I,J) = MIN(273.16 _d 0 + MAX_TICE, TSURF(I,J,bi,bj))
        atempLoc (I,J) = MAX(273.16 _d 0 + MIN_ATEMP,ATEMP(I,J,bi,bj))
        lwdownLoc(I,J) = MAX( MIN_LWDOWN, LWDOWN(I,J,bi,bj) )
#else /* SEAICE_SOLVE4TEMP_LEGACY */
        F_lwu    (I,J) = 0. _d 0
        F_sens   (I,J) = 0. _d 0
        tsurfLoc (I,J) = TSURF(I,J,bi,bj)
        atempLoc (I,J) = MAX( celsius2K+MIN_ATEMP, ATEMP(I,J,bi,bj) )
        lwdownLoc(I,J) = LWDOWN(I,J,bi,bj)
#endif /* SEAICE_SOLVE4TEMP_LEGACY */

C     FREEZING TEMPERATURE OF SEAWATER
#ifdef SEAICE_VARIABLE_FREEZING_POINT
C     Use a variable seawater freezing point
        TB(I,J) = -0.0575 _d 0*salt(I,J,kSrf,bi,bj) + 0.0901 _d 0
     &       + celsius2K
#else
C     Use a constant freezing temperature (SEAICE_VARIABLE_FREEZING_POINT undef)
#ifdef SEAICE_SOLVE4TEMP_LEGACY
        TB(I,J) = 271.2 _d 0
#else /* SEAICE_SOLVE4TEMP_LEGACY */
        TB(I,J) = celsius2K + SEAICE_freeze
#endif /* SEAICE_SOLVE4TEMP_LEGACY */
#endif /* SEAICE_VARIABLE_FREEZING_POINT */
        IF(HSNOW_ACTUAL(I,J).GT.0.0) THEN
C     Stefan-Boltzmann constant times emissivity
         D3(I,J)=SEAICE_snow_emiss*SEAICE_boltzmann
#ifdef EXF_LWDOWN_WITH_EMISSIVITY
C     This is now [(1-emiss)*lwdown - lwdown]
         lwdownloc(I,J) = SEAICE_snow_emiss*lwdownloc(I,J)
#else /* use the old hard wired inconsistent value  */
         lwdownloc(I,J) = 0.97 _d 0*lwdownloc(I,J)
#endif /* EXF_LWDOWN_WITH_EMISSIVITY */
        ELSE
C     Stefan-Boltzmann constant times emissivity
         D3(I,J)=SEAICE_ice_emiss*SEAICE_boltzmann
#ifdef EXF_LWDOWN_WITH_EMISSIVITY
C     This is now [(1-emiss)*lwdown - lwdown]
         lwdownloc(I,J) = SEAICE_ice_emiss*lwdownloc(I,J)
#else /* use the old hard wired inconsistent value  */
         lwdownloc(I,J) = 0.97 _d 0*lwdownloc(I,J)
#endif /* EXF_LWDOWN_WITH_EMISSIVITY */
        ENDIF
       ENDDO
      ENDDO

      DO J=1,sNy
       DO I=1,sNx

C     DECIDE ON ALBEDO
        IF ( iceOrNot(I,J) ) THEN

         IF ( YC(I,J,bi,bj) .LT. 0.0 _d 0 ) THEN
          IF (tsurfLoc(I,J) .GE. SurfMeltTemp) THEN
           ALB_ICE (I,J)   = SEAICE_wetIceAlb_south
           ALB_SNOW(I,J)   = SEAICE_wetSnowAlb_south
          ELSE                  ! no surface melting
           ALB_ICE (I,J)   = SEAICE_dryIceAlb_south
           ALB_SNOW(I,J)   = SEAICE_drySnowAlb_south
          ENDIF
         ELSE                   !/ Northern Hemisphere
          IF (tsurfLoc(I,J) .GE. SurfMeltTemp) THEN
           ALB_ICE (I,J)   = SEAICE_wetIceAlb
           ALB_SNOW(I,J)   = SEAICE_wetSnowAlb
          ELSE                  ! no surface melting
           ALB_ICE (I,J)   = SEAICE_dryIceAlb
           ALB_SNOW(I,J)   = SEAICE_drySnowAlb
          ENDIF
         ENDIF                  !/ Albedo for snow and ice

C     If actual snow thickness exceeds the cutoff thickness, use snow albedo
         IF (HSNOW_ACTUAL(I,J) .GT. HCUT) THEN
          ALB(I,J) = ALB_SNOW(I,J)
         ELSEIF ( HCUT.LE.ZERO ) THEN
          ALB(I,J) = ALB_ICE(I,J)
         ELSE
C     otherwise, use linear transition between ice and snow albedo
          ALB(I,J) = MIN( ALB_ICE(I,J) + HSNOW_ACTUAL(I,J)/HCUT
     &                                 *(ALB_SNOW(I,J) -ALB_ICE(I,J))
     &                  , ALB_SNOW(I,J) )
         ENDIF

C     Determine the fraction of shortwave radiative flux remaining
C     at ocean interface after scattering through the snow and ice.
C     If snow is present, no radiation penetrates through snow+ice
         IF (HSNOW_ACTUAL(I,J) .GT. 0.0 _d 0) THEN
          penetSWFrac = 0.0 _d 0
         ELSE
          penetSWFrac = XIO*EXP(-1.5 _d 0 * HICE_ACTUAL(I,J))
         ENDIF
C     The shortwave radiative flux leaving ocean beneath ice (+=up).
         IcePenetSW(I,J) = -(1.0 _d 0 - ALB(I,J))
     &                    *penetSWFrac * SWDOWN(I,J,bi,bj)
C     The shortwave radiative flux convergence in the seaice.
         absorbedSW(I,J) =  (1.0 _d 0 - ALB(I,J))
     &        *(1.0 _d 0 - penetSWFrac)* SWDOWN(I,J,bi,bj)

#ifdef SEAICE_SOLVE4TEMP_LEGACY
C     Now determine fixed (relative to tsurf) forcing term in heat budget
         A1(I,J) = absorbedSW(I,J) + lwdownLoc(I,J)
     &         +D1*UG(I,J)*atempLoc(I,J)+D1I*UG(I,J)*AQH(I,J,bi,bj)
#endif /* SEAICE_SOLVE4TEMP_LEGACY */

C     The effective conductivity of the two-layer
C     snow/ice system.
#ifdef SEAICE_SOLVE4TEMP_LEGACY
         effConduct(I,J)=
     &        XKS/(HSNOW_ACTUAL(I,J)/HICE_ACTUAL(I,J) +
     &        XKS/XKI)/HICE_ACTUAL(I,J)
#else /* SEAICE_SOLVE4TEMP_LEGACY */
C     Set a mininum sea ice thickness of 5 cm to bound
C     the magnitude of conductive heat fluxes.
Cif   * now taken care of by SEAICE_hice_reg in seaice_growth
c        hice_tmp = max(HICE_ACTUAL(I,J),5. _d -2)
         effConduct(I,J) = XKI * XKS /
     &        (XKS * HICE_ACTUAL(I,J) + XKI * HSNOW_ACTUAL(I,J))
#endif /* SEAICE_SOLVE4TEMP_LEGACY */

#ifdef SEAICE_DEBUG
         IF ( (I .EQ. SEAICE_debugPointI) .AND.
     &        (J .EQ. SEAICE_debugPointJ) ) THEN
          print '(A,i6)','-----------------------------------'
          print '(A,i6)','ibi merged initialization ', myIter
          print '(A,i6,4(1x,D24.15))',
     &         'ibi iter, TSL, TS     ',myIter,
     &         tsurfLoc(I,J), TSURF(I,J,bi,bj)
          print '(A,i6,4(1x,D24.15))',
     &         'ibi iter, TMELT       ',myIter,TMELT
          print '(A,i6,4(1x,D24.15))',
     &         'ibi iter, HIA, EFKCON ',myIter,
     &         HICE_ACTUAL(I,J), effConduct(I,J)
          print '(A,i6,4(1x,D24.15))',
     &         'ibi iter, HSNOW       ',myIter,
     &         HSNOW_ACTUAL(I,J), ALB(I,J)
          print '(A,i6)','-----------------------------------'
          print '(A,i6)','ibi energy balance iterat ', myIter
         ENDIF
#endif /* SEAICE_DEBUG */

        ENDIF                   !/* iceOrNot */
       ENDDO                    !/* i */
      ENDDO                     !/* j */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      DO ITER=1,IMAX_TICE
       DO J=1,sNy
        DO I=1,sNx
#ifdef ALLOW_AUTODIFF_TAMC
         iicekey = I + sNx*(J-1) + (ITER-1)*sNx*sNy
CADJ STORE tsurfloc(i,j) = comlev1_solve4temp,
CADJ &                     key = iicekey, byte = isbyte
#endif /*  ALLOW_AUTODIFF_TAMC */

         IF ( iceOrNot(I,J) ) THEN

          t1 = tsurfLoc(I,J)
          t2 = t1*t1
          t3 = t2*t1
          t4 = t2*t2

C     Calculate the specific humidity in the BL above the snow/ice
#ifdef SEAICE_SOLVE4TEMP_LEGACY
C     Use the Maykut polynomial
          qhice(I,J)=QS1*(C1*t4+C2*t3 +C3*t2+C4*t1+C5)
          dqhice_dTice = 0. _d 0

#else /* SEAICE_SOLVE4TEMP_LEGACY */
C     Use an approximation which is more accurate at low temperatures

C     log 10 of the sat vap pressure
          mm_log10pi = -aa1 / t1 + aa2
C     The saturation vapor pressure (SVP) in the surface
C     boundary layer (BL) above the snow/ice.
C         mm_pi = TEN **(mm_log10pi)
C     The following form does the same, but is faster
          mm_pi = EXP(mm_log10pi*lnTEN)
          qhice(I,J) = bb1*mm_pi / (Ppascals - (1.0 _d 0 - bb1) *
     &         mm_pi)
C     A constant for SVP derivative w.r.t TICE
C         cc3t = TEN **(aa1 / t1)
C     The following form does the same, but is faster
          cc3t = EXP(aa1 / t1 * lnTEN)
C     d(qh)/d(TICE)
          dqhice_dTice = cc1*cc3t/((cc2-cc3t*Ppascals)**2 *t2)
#endif /* SEAICE_SOLVE4TEMP_LEGACY */

C     Calculate the flux terms based on the updated tsurfLoc
          F_c(I,J)    = -effConduct(I,J)*(TB(I,J)-tsurfLoc(I,J))
          F_lh(I,J)   = D1I*UG(I,J)*(qhice(I,J)-AQH(I,J,bi,bj))
#ifdef SEAICE_SOLVE4TEMP_LEGACY
          A2(I,J) = D1*UG(I,J)*t1+D1I*UG(I,J)*qhice(I,J)+D3(I,J)*t4
          A3(I,J) = 4.0 _d 0*D3(I,J)*t3 + effConduct(I,J)+D1*UG(I,J)
     &         + D1I*UG(I,J)*dqhice_dTice
#else /* SEAICE_SOLVE4TEMP_LEGACY */
#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET
C     if the latent heat flux implied by tsurfLoc exceeds
C     F_lh_max, cap F_lh and decouple the flux magnitude from TICE
          IF (F_lh(I,J) .GT. F_lh_max(I,J)) THEN
             F_lh(I,J)  = F_lh_max(I,J)
             dqhice_dTice = ZERO
          ENDIF
#endif /* SEAICE_ADD_SUBLIMATION_TO_FWBUDGET */

C     d(F_ia)/d(TICE)
          dFiDTs1 = 4.0 _d 0*D3(I,J)*t3 + effConduct(I,J) + D1*UG(I,J)
     &         + D1I*UG(I,J)*dqhice_dTice

          F_lwu(I,J) = t4 * D3(I,J)
          F_sens(I,J)= D1 * UG(I,J) * (t1 - atempLoc(I,J))
          F_ia(I,J) = -lwdownLoc(I,J) -absorbedSW(I,J) + F_lwu(I,J)
     &              +  F_c(I,J) + F_sens(I,J) + F_lh(I,J)
#endif /* SEAICE_SOLVE4TEMP_LEGACY */

#ifdef SEAICE_DEBUG
          IF ( (I .EQ. SEAICE_debugPointI) .AND.
     &         (J .EQ. SEAICE_debugPointJ) ) THEN
           print '(A,i6,4(1x,D24.15))',
     &          'ice-iter qhICE,       ', ITER,qhIce(I,J)
#ifdef SEAICE_SOLVE4TEMP_LEGACY
           print '(A,i6,4(1x,D24.15))',
     &          'ice-iter A1 A2 B      ',ITER,A1(I,J),A2(I,J),-F_c(I,J)
           print '(A,i6,4(1x,D24.15))',
     &          'ice-iter A3 (-A1+A2)  ',ITER,A3(I,J),-A1(I,J)+A2(I,J)
#else /* SEAICE_SOLVE4TEMP_LEGACY */
           print '(A,i6,4(1x,D24.15))',
     &          'ice-iter dFiDTs1 F_ia ', ITER, dFiDTs1, F_ia(I,J)
#endif /* SEAICE_SOLVE4TEMP_LEGACY */
          ENDIF
#endif /* SEAICE_DEBUG */

C     Update tsurfLoc
#ifdef SEAICE_SOLVE4TEMP_LEGACY
C     update tsurf as solution of : A1 - A2 = Fc + A3 *delta.tsurf
          tsurfLoc(I,J)=tsurfLoc(I,J)
     &         +(A1(I,J)-A2(I,J)-F_c(I,J))/A3(I,J)
          tsurfLoc(I,J) = MAX( 273.16 _d 0+MIN_TICE, tsurfLoc(I,J) )

#else /* SEAICE_SOLVE4TEMP_LEGACY */
          tsurfLoc(I,J) = tsurfLoc(I,J) - F_ia(I,J) / dFiDTs1

C     If the search leads to tsurfLoc < 50 Kelvin, restart the search
C     at tsurfLoc = TMELT.  Note that one solution to the energy balance problem
C     is an extremely low temperature - a temperature far below realistic values.
          IF (tsurfLoc(I,J) .LT. 50.0 _d 0 ) THEN
           tsurfLoc(I,J) = TMELT
          ENDIF
#endif /* SEAICE_SOLVE4TEMP_LEGACY */
          tsurfLoc(I,J) = MIN( tsurfLoc(I,J), TMELT )

#ifdef SEAICE_DEBUG
          IF ( (I .EQ. SEAICE_debugPointI) .AND.
     &         (J .EQ. SEAICE_debugPointJ) ) THEN
           print '(A,i6,4(1x,D24.15))',
     &          'ice-iter tsurfLc,|dif|', ITER,
     &          tsurfLoc(I,J),
     &          LOG10(ABS(tsurfLoc(I,J) - t1))
          ENDIF
#endif /* SEAICE_DEBUG */

         ENDIF                  !/* iceOrNot */
        ENDDO                   !/* i */
       ENDDO                    !/* j */
      ENDDO                     !/* Iterations */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO J=1,sNy
       DO I=1,sNx
        IF ( iceOrNot(I,J) ) THEN

C     Save updated tsurf and finalize the flux terms
         TSURF(I,J,bi,bj) = tsurfLoc(I,J)

#ifdef SEAICE_SOLVE4TEMP_LEGACY
         F_ia(I,J)=-A1(I,J)+A2(I,J)
#else /* SEAICE_SOLVE4TEMP_LEGACY */
C     Recalculate the fluxes based on the (possibly) adjusted TSURF
         t1 = tsurfLoc(I,J)
         t2 = t1*t1
         t3 = t2*t1
         t4 = t2*t2

C     log 10 of the sat vap pressure
         mm_log10pi = -aa1 / t1 + aa2
C     saturation vapor pressure
C        mm_pi = TEN **(mm_log10pi)
C     The following form does the same, but is faster
         mm_pi = EXP(mm_log10pi*lnTEN)
C     over ice specific humidity
         qhice(I,J) = bb1*mm_pi/(Ppascals- (1.0 _d 0 - bb1) * mm_pi)
         F_lh(I,J) = D1I * UG(I,J)*(qhice(I,J)-AQH(I,J,bi,bj))
#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET
         IF (F_lh(I,J) .GT. F_lh_max(I,J)) THEN
             F_lh(I,J)  = F_lh_max(I,J)
         ENDIF
#endif /* SEAICE_ADD_SUBLIMATION_TO_FWBUDGET */

         F_c(I,J)  = -effConduct(I,J) * (TB(I,J) - t1)
         F_lwu(I,J)   = t4 * D3(I,J)
         F_sens(I,J)  = D1 * UG(I,J) * (t1 - atempLoc(I,J))

C     The flux between the ice/snow surface and the atmosphere.
C              (excludes upward conductive fluxes)
         F_ia(I,J) = -lwdownLoc(I,J) -absorbedSW(I,J) + F_lwu(I,J)
     &             +  F_sens(I,J) + F_lh(I,J)
#endif /* SEAICE_SOLVE4TEMP_LEGACY */

#ifdef SEAICE_MODIFY_GROWTH_ADJ
Cgf no additional dependency through solver, snow, etc.
         IF ( SEAICEadjMODE.GE.2 ) THEN
         CALL ZERO_ADJ_1D( 1, TSURF(I,J,bi,bj), myThid)
         t1 = TSURF(I,J,bi,bj)
         t2 = t1*t1
         t3 = t2*t1
         t4 = t2*t2
         qhice(I,J)=QS1*(C1*t4+C2*t3 +C3*t2+C4*t1+C5)

         A1(I,J)=0.3 _d 0 *SWDOWN(I,J,bi,bj)+lwdownLoc(I,J)
     &         +D1*UG(I,J)*atempLoc(I,J)+D1I*UG(I,J)*AQH(I,J,bi,bj)
         A2(I,J)= D1*UG(I,J)*t1+D1I*UG(I,J)*qhice(I,J)+D3(I,J)*t4

         F_ia(I,J)=-A1(I,J)+A2(I,J)
         IcePenetSW(I,J)= 0. _d 0
         ENDIF
#endif /* SEAICE_MODIFY_GROWTH_ADJ */

C     Fresh water flux (kg/m^2/s) from latent heat of sublimation.
C     F_lh is positive upward (sea ice looses heat) and FWsublim
C     is also positive upward (atmosphere gains freshwater)
         FWsublim(I,J) = F_lh(I,J)/lhSublim

#ifdef SEAICE_DEBUG
C     Caclulate the net ice-ocean and ice-atmosphere fluxes
         IF (F_c(I,J) .LT. 0.0 _d 0) THEN
          F_io_net(I,J) = -F_c(I,J)
          F_ia_net(I,J) = 0.0 _d 0
         ELSE
          F_io_net(I,J) = 0.0 _d 0
          F_ia_net(I,J) = F_ia(I,J)
         ENDIF                  !/* conductive fluxes up or down */

         IF ( (I .EQ. SEAICE_debugPointI) .AND.
     &        (J .EQ. SEAICE_debugPointJ) ) THEN
          print '(A)','----------------------------------------'
          print '(A,i6)','ibi complete ', myIter
          print '(A,4(1x,D24.15))',
     &         'ibi T(SURF, surfLoc,atmos) ',
     &         TSURF(I,J,bi,bj), tsurfLoc(I,J),atempLoc(I,J)
          print '(A,4(1x,D24.15))',
     &         'ibi LWL                    ', lwdownLoc(I,J)
          print '(A,4(1x,D24.15))',
     &         'ibi QSW(Total, Penetrating)',
     &         SWDOWN(I,J,bi,bj), IcePenetSW(I,J)
          print '(A,4(1x,D24.15))',
     &         'ibi qh(ATM ICE)            ',
     &         AQH(I,J,bi,bj),qhice(I,J)
#ifndef SEAICE_SOLVE4TEMP_LEGACY
         print '(A,4(1x,D24.15))',
     &         'ibi F(lwd,swi,lwu)         ',
     &         -lwdownLoc(I,J), -absorbedSW(I,J), F_lwu(I,J)
         print '(A,4(1x,D24.15))',
     &         'ibi F(c,lh,sens)           ',
     &         F_c(I,J), F_lh(I,J), F_sens(I,J)
#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET
         IF (F_lh_max(I,J) .GT. ZERO) THEN
             print '(A,4(1x,D24.15))',
     &         'ibi F_lh_max,  F_lh/lhmax) ',
     &         F_lh_max(I,J), F_lh(I,J)/ F_lh_max(I,J)
         ELSE
             print '(A,4(1x,D24.15))',
     &         'ibi F_lh_max = ZERO! '
         ENDIF
         print '(A,4(1x,D24.15))',
     &         'ibi FWsub, FWsubm*dT/rhoI  ',
     &          FWsublim(I,J),
     &          FWsublim(I,J)*SEAICE_deltaTtherm/SEAICE_rhoICE
#endif /* SEAICE_ADD_SUBLIMATION_TO_FWBUDGET */
#endif /* SEAICE_SOLVE4TEMP_LEGACY */
          print '(A,4(1x,D24.15))',
     &         'ibi F_ia, F_ia_net, F_c    ',
#ifdef SEAICE_SOLVE4TEMP_LEGACY
     &         -A1(I,J)+A2(I,J), -A1(I,J)+A2(I,J)+F_c(I,J), F_c(I,J)
#else /* SEAICE_SOLVE4TEMP_LEGACY */
     &         F_ia(I,J), F_ia_net(I,J), F_c(I,J)
#endif /* SEAICE_SOLVE4TEMP_LEGACY */
          print '(A)','----------------------------------------'
         ENDIF
#endif /* SEAICE_DEBUG */

        ENDIF                   !/* iceOrNot */
       ENDDO                    !/* i */
      ENDDO                     !/* j */

#endif /* ALLOW_ATM_TEMP && ALLOW_DOWNWARD_RADIATION */
      RETURN
      END
