#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_SOLVE4TEMP
C     !INTERFACE:
      SUBROUTINE SEAICE_SOLVE4TEMP(
     I   UG, HICE_ACTUAL, HSNOW_ACTUAL,
#ifdef SEAICE_CAP_SUBLIM
     I   F_lh_max,
#endif
     I   TSURFin,
     O   TSURFout,
#ifdef SEAICE_USE_GROWTH_ADX
     O   F_io_net, F_ia_net,
#endif /* SEAICE_USE_GROWTH_ADX */
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
#include "DYNVARS.h"
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
C     TSURFin    :: surface temperature of ice/snow in Kelvin
C     bi,bj      :: tile indices
C     myTime     :: current time in simulation
C     myIter     :: iteration number in simulation
C     myThid     :: my Thread Id number
C     !OUTPUT PARAMETERS:
C     TSURFout   :: updated surface temperature of ice/snow in Kelvin
C     F_io_net   :: upward conductive heat flux through seaice+snow
C     F_ia_net   :: net heat flux divergence at the sea ice/snow surface:
C                 includes ice conductive fluxes and atmospheric fluxes (W/m^2)
C     F_ia       :: upward seaice/snow surface heat flux to atmosphere (W/m^2)
C     IcePenetSW :: short wave heat flux transmitted through ice (+=upward)
C     FWsublim   :: fresh water (mass) flux due to sublimation (+=up)(kg/m^2/s)
C---- Notes:
C     1) should add IcePenetSW to F_ia to get the net surface heat flux
C        from the atmosphere (IcePenetSW not currently included in F_ia)
C     2) since zero ice/snow heat capacity is assumed, all the absorbed Short
C        -Wave is used to warm the ice/snow surface (heating profile ignored).
C----------
      _RL UG          (1:sNx,1:sNy)
      _RL HICE_ACTUAL (1:sNx,1:sNy)
      _RL HSNOW_ACTUAL(1:sNx,1:sNy)
#ifdef SEAICE_CAP_SUBLIM
      _RL F_lh_max    (1:sNx,1:sNy)
#endif
      _RL TSURFin     (1:sNx,1:sNy)
      _RL TSURFout    (1:sNx,1:sNy)
      _RL F_io_net    (1:sNx,1:sNy)
      _RL F_ia_net    (1:sNx,1:sNy)
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
C     kSurface  :: vertical index of surface layer
      INTEGER i, j
      INTEGER kSurface
      INTEGER ITER
      _RL D1, D1I
      _RL D3(1:sNx,1:sNy)
      _RL TMELT, XKI, XKS, HCUT, recip_HCUT, XIO
C     SurfMeltTemp :: Temp (K) above which wet-albedo values are used
      _RL SurfMeltTemp
C     effConduct :: effective conductivity of combined ice and snow
      _RL effConduct(1:sNx,1:sNy)
C     lhSublim :: latent heat of sublimation (SEAICE_lhEvap + SEAICE_lhFusion)
      _RL lhSublim
C     t1,t2,t3,t4 :: powers of temperature
      _RL  t1, t2, t3, t4

C-    Constants to calculate Saturation Vapor Pressure
C     Maykut Polynomial Coeff. for Sat. Vapor Press
      _RL C1, C2, C3, C4, C5, QS1
C     Extended temp-range expon. relation Coeff. for Sat. Vapor Press
      _RL lnTEN
      _RL aa1,aa2,bb1,bb2,Ppascals,cc0,cc1,cc2,cc3t
C     specific humidity at ice surface variables
      _RL mm_pi,mm_log10pi

C     tempFrz  :: ocean temperature in contact with ice
C                 (=seawater freezing point) (K)
C     F_c      :: conductive heat flux through seaice+snow (+=upward)
C     F_lwu    :: upward long-wave surface heat flux (+=upward)
C     F_sens   :: sensible surface heat flux         (+=upward)
C     F_lh     :: latent heat flux (sublimation) (+=upward)
C     qhice    :: saturation vapor pressure of snow/ice surface
C     dqh_dTs  :: derivative of qhice w.r.t snow/ice surf. temp
C     dFia_dTs :: derivative of surf heat flux (F_ia) w.r.t surf. temp
      _RL tempFrz    (1:sNx,1:sNy)
      _RL F_c        (1:sNx,1:sNy)
      _RL F_lwu      (1:sNx,1:sNy)
      _RL F_sens     (1:sNx,1:sNy)
      _RL F_lh       (1:sNx,1:sNy)
      _RL qhice      (1:sNx,1:sNy)
      _RL dqh_dTs    (1:sNx,1:sNy)
      _RL dFia_dTs   (1:sNx,1:sNy)
      _RL absorbedSW (1:sNx,1:sNy)
      _RL penetSWFrac
      _RL delTsurf

C     local copies of global variables
      _RL tsurfLoc   (1:sNx,1:sNy)
      _RL tsurfPrev  (1:sNx,1:sNy)
      _RL atempLoc   (1:sNx,1:sNy)
      _RL lwdownLoc  (1:sNx,1:sNy)
      _RL ALB        (1:sNx,1:sNy)
      _RL ALB_ICE    (1:sNx,1:sNy)
      _RL ALB_SNOW   (1:sNx,1:sNy)
C     iceOrNot :: this is HICE_ACTUAL.GT.0.
      LOGICAL iceOrNot(1:sNx,1:sNy)
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER iicekey
#endif
#ifdef SEAICE_DEBUG
#endif /* SEAICE_DEBUG */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
C     Since this is a "local" tape, is does not need to include number
C     of threads or tiles per mpi-process. It does not even lead to a
C     common block.
CADJ INIT comlev1_solve4temp = COMMON, NMAX_TICE
#endif /* ALLOW_AUTODIFF_TAMC */

C-    MAYKUT CONSTANTS FOR SAT. VAP. PRESSURE TEMP. POLYNOMIAL
      C1=    2.7798202  _d -06
      C2=   -2.6913393  _d -03
      C3=    0.97920849 _d +00
      C4= -158.63779    _d +00
      C5= 9653.1925     _d +00
      QS1=0.622 _d +00/1013.0 _d +00
C-    Extended temp-range expon. relation Coeff. for Sat. Vapor Press
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

      IF ( usingPCoords ) THEN
       kSurface        = Nr
      ELSE
       kSurface        = 1
      ENDIF

C     SENSIBLE HEAT CONSTANT
      D1=SEAICE_dalton*SEAICE_cpAir*SEAICE_rhoAir

C     ICE LATENT HEAT CONSTANT
      lhSublim = SEAICE_lhEvap + SEAICE_lhFusion
      D1I=SEAICE_dalton*lhSublim*SEAICE_rhoAir

C     MELTING TEMPERATURE OF ICE
      TMELT        = celsius2K

C     ICE CONDUCTIVITY
      XKI=SEAICE_iceConduct

C     SNOW CONDUCTIVITY
      XKS=SEAICE_snowConduct

C     CUTOFF SNOW THICKNESS
C     Snow-Thickness above HCUT: SW optically thick snow (=> snow-albedo).
C     Snow-Thickness below HCUT: linear transition to ice-albedo
      HCUT = SEAICE_snowThick
      recip_HCUT = 0. _d 0
      IF ( HCUT.GT.0. _d 0 ) recip_HCUT = 1. _d 0 / HCUT

C     PENETRATION SHORTWAVE RADIATION FACTOR
      XIO=SEAICE_shortwave

C     Temperature Threshold for wet-albedo:
      SurfMeltTemp = TMELT + SEAICE_wetAlbTemp
C     old SOLVE4TEMP_LEGACY setting, consistent with former celsius2K value:
c     TMELT        = 273.16  _d +00
c     SurfMeltTemp = 273.159 _d +00

C     Initialize variables
      DO J=1,sNy
       DO I=1,sNx
C     initialise output arrays:
        TSURFout (I,J) = TSURFin(I,J)
        F_ia     (I,J) = 0. _d 0
        F_ia_net (I,J) = 0. _d 0
        F_io_net (I,J) = 0. _d 0
        IcePenetSW(I,J)= 0. _d 0
        FWsublim (I,J) = 0. _d 0
C     HICE_ACTUAL is modified in this routine, but at the same time
C     used to decided where there is ice, therefore we save this information
C     here in a separate array
        iceOrNot  (I,J) = HICE_ACTUAL(I,J) .GT. 0. _d 0
        absorbedSW(I,J) = 0. _d 0
        qhice    (I,J) = 0. _d 0
        dqh_dTs  (I,J) = 0. _d 0
        F_lh     (I,J) = 0. _d 0
        F_lwu    (I,J) = 0. _d 0
        F_sens   (I,J) = 0. _d 0
C     Make a local copy of LW, surface & atmospheric temperatures
        tsurfLoc (I,J) = TSURFin(I,J)
c       tsurfLoc (I,J) = MIN( celsius2K+MAX_TICE, TSURFin(I,J) )
        lwdownLoc(I,J) = MAX( MIN_LWDOWN, LWDOWN(I,J,bi,bj) )
        atempLoc (I,J) = MAX( celsius2K+MIN_ATEMP, ATEMP(I,J,bi,bj) )

c     FREEZING TEMP. OF SEA WATER (K)
        tempFrz(I,J) = SEAICE_dTempFrz_dS *salt(I,J,kSurface,bi,bj)
     &     + SEAICE_tempFrz0 + celsius2K

C     Now determine fixed (relative to tsurf) forcing term in heat budget

        IF(HSNOW_ACTUAL(I,J).GT.0.0) THEN
C     Stefan-Boltzmann constant times emissivity
         D3(I,J)=SEAICE_snow_emiss*SEAICE_boltzmann
#ifdef EXF_LWDOWN_WITH_EMISSIVITY
C     This is now [(1-emiss)*lwdown - lwdown]
         lwdownLoc(I,J) = SEAICE_snow_emiss*lwdownLoc(I,J)
#else /* use the old hard wired inconsistent value  */
         lwdownLoc(I,J) = 0.97 _d 0*lwdownLoc(I,J)
#endif /* EXF_LWDOWN_WITH_EMISSIVITY */
        ELSE
C     Stefan-Boltzmann constant times emissivity
         D3(I,J)=SEAICE_ice_emiss*SEAICE_boltzmann
#ifdef EXF_LWDOWN_WITH_EMISSIVITY
C     This is now [(1-emiss)*lwdown - lwdown]
         lwdownLoc(I,J) = SEAICE_ice_emiss*lwdownLoc(I,J)
#else /* use the old hard wired inconsistent value  */
         lwdownLoc(I,J) = 0.97 _d 0*lwdownLoc(I,J)
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
          ALB(I,J) = MIN( ALB_ICE(I,J) + HSNOW_ACTUAL(I,J)*recip_HCUT
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

C     The effective conductivity of the two-layer snow/ice system.
C     Set a minimum sea ice thickness of 5 cm to bound
C     the magnitude of conductive heat fluxes.
Cif   * now taken care of by SEAICE_hice_reg in seaice_growth
c        hice_tmp = max(HICE_ACTUAL(I,J),5. _d -2)
         effConduct(I,J) = XKI * XKS /
     &        (XKS * HICE_ACTUAL(I,J) + XKI * HSNOW_ACTUAL(I,J))

#ifdef SEAICE_DEBUG
         IF ( (I .EQ. SEAICE_debugPointI) .AND.
     &        (J .EQ. SEAICE_debugPointJ) ) THEN
          print '(A,i6)','-----------------------------------'
          print '(A,i6)','ibi merged initialization ', myIter
          print '(A,i6,4(1x,D24.15))',
     &         'ibi iter, TSL, TS     ',myIter,
     &         tsurfLoc(I,J), TSURFin(I,J)
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
#ifdef ALLOW_AUTODIFF_TAMC
       iicekey = ITER
CADJ STORE tsurfLoc = comlev1_solve4temp, key = iicekey, byte = isbyte
#endif
       DO J=1,sNy
        DO I=1,sNx

C-    save tsurf from previous iter
         tsurfPrev(I,J) = tsurfLoc(I,J)
         IF ( iceOrNot(I,J) ) THEN

          t1 = tsurfLoc(I,J)
          t2 = t1*t1
          t3 = t2*t1
          t4 = t2*t2

C--   Calculate the specific humidity in the BL above the snow/ice
          IF ( useMaykutSatVapPoly ) THEN
C-    Use the Maykut polynomial
           qhice(I,J)=QS1*(C1*t4+C2*t3 +C3*t2+C4*t1+C5)
           dqh_dTs(I,J) = 0. _d 0
          ELSE
C-    Use exponential relation approx., more accurate at low temperatures
C     log 10 of the sat vap pressure
           mm_log10pi = -aa1 / t1 + aa2
C     The saturation vapor pressure (SVP) in the surface
C     boundary layer (BL) above the snow/ice.
c          mm_pi = TEN **(mm_log10pi)
C     The following form does the same, but is faster
           mm_pi = EXP(mm_log10pi*lnTEN)
           qhice(I,J) = bb1*mm_pi/( Ppascals -(1.0 _d 0 - bb1)*mm_pi )
C     A constant for SVP derivative w.r.t TICE
c          cc3t = TEN **(aa1 / t1)
C     The following form does the same, but is faster
           cc3t = EXP(aa1 / t1 * lnTEN)
C     d(qh)/d(TICE)
           dqh_dTs(I,J) = cc1*cc3t/((cc2-cc3t*Ppascals)**2 *t2)
          ENDIF

C     Calculate the flux terms based on the updated tsurfLoc
          F_c(I,J)    = effConduct(I,J)*(tempFrz(I,J)-t1)
          F_lh(I,J)   = D1I*UG(I,J)*(qhice(I,J)-AQH(I,J,bi,bj))
#ifdef SEAICE_CAP_SUBLIM
C     if the latent heat flux implied by tsurfLoc exceeds
C     F_lh_max, cap F_lh and decouple the flux magnitude from tIce (tsurfLoc)
          IF (F_lh(I,J) .GT. F_lh_max(I,J)) THEN
             F_lh(I,J)  = F_lh_max(I,J)
             dqh_dTs(I,J) = ZERO
          ENDIF
#endif /* SEAICE_CAP_SUBLIM */

          F_lwu(I,J) = t4 * D3(I,J)
          F_sens(I,J)= D1 * UG(I,J) * (t1 - atempLoc(I,J))
          F_ia(I,J) = -lwdownLoc(I,J) -absorbedSW(I,J) + F_lwu(I,J)
     &              +  F_sens(I,J) + F_lh(I,J)
C     d(F_ia)/d(Tsurf)
          dFia_dTs(I,J) = 4.0 _d 0*D3(I,J)*t3 + D1*UG(I,J)
     &                  + D1I*UG(I,J)*dqh_dTs(I,J)

#ifdef SEAICE_DEBUG
          IF ( (I .EQ. SEAICE_debugPointI) .AND.
     &         (J .EQ. SEAICE_debugPointJ) ) THEN
           print '(A,i6,4(1x,D24.15))',
     &          'ice-iter qhICE,       ', ITER,qhIce(I,J)
           print '(A,i6,4(1x,D24.15))',
     &          'ice-iter dFiDTs1 F_ia ', ITER,
     &          dFia_dTs(I,J)+effConduct(I,J), F_ia(I,J)-F_c(I,J)
          ENDIF
#endif /* SEAICE_DEBUG */

C-    Update tsurf as solution of : Fc = Fia + d/dT(Fia - Fc) *delta.tsurf
          tsurfLoc(I,J) = tsurfLoc(I,J)
     &    + ( F_c(I,J)-F_ia(I,J) ) / ( effConduct(I,J)+dFia_dTs(I,J) )

         ENDIF
        ENDDO
       ENDDO
       IF ( useMaykutSatVapPoly ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE tsurfLoc = comlev1_solve4temp, key = iicekey, byte = isbyte
#endif
        DO J=1,sNy
         DO I=1,sNx
          tsurfLoc(I,J) = MAX( celsius2K+MIN_TICE, tsurfLoc(I,J) )
C     If the search leads to tsurfLoc < 50 Kelvin, restart the search at
C     tsurfLoc = TMELT. Note that one solution to the energy balance problem is
C     an extremely low temperature - a temperature far below realistic values.
c         IF (tsurfLoc(I,J) .LT. 50.0 _d 0 ) tsurfLoc(I,J) = TMELT
C   Comments & code above not relevant anymore (from older version, when
C   trying Maykut-Polynomial & dqh_dTs > 0 ?): commented out
         ENDDO
        ENDDO
       ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE tsurfLoc = comlev1_solve4temp, key = iicekey, byte = isbyte
#endif
       DO J=1,sNy
        DO I=1,sNx
         tsurfLoc(I,J) = MIN( tsurfLoc(I,J), TMELT )

#ifdef SEAICE_DEBUG
         IF ( (I .EQ. SEAICE_debugPointI) .AND.
     &        (J .EQ. SEAICE_debugPointJ) ) THEN
          print '(A,i6,4(1x,D24.15))',
     &         'ice-iter tsurfLc,|dif|', ITER,
     &         tsurfLoc(I,J),
     &         LOG10(ABS(tsurfLoc(I,J) - tsurfPrev(I,J)))
         ENDIF
#endif /* SEAICE_DEBUG */

        ENDDO                   !/* i */
       ENDDO                    !/* j */
      ENDDO                     !/* Iterations */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef SEAICE_MODIFY_GROWTH_ADJ
Cgf no additional dependency through solver, snow, etc.
      IF ( SEAICEadjMODE.GE.2 ) THEN
       DO J=1,sNy
        DO I=1,sNx
         IF ( iceOrNot(I,J) ) THEN
          CALL ZERO_ADJ_1D( 1, TSURFin(I,J), myThid)
          absorbedSW(I,J) = 0.3 _d 0 *SWDOWN(I,J,bi,bj)
          IcePenetSW(I,J)= 0. _d 0
         ENDIF
        ENDIF
       ENDDO
      ENDDO
      IF ( postSolvTempIter.EQ.2 .OR. SEAICEadjMODE.GE.2 ) THEN
       DO J=1,sNy
        DO I=1,sNx
         IF ( iceOrNot(I,J) ) THEN
          t1 = TSURFin(I,J)
#else /* SEAICE_MODIFY_GROWTH_ADJ */
      IF ( postSolvTempIter.EQ.2 ) THEN
       DO J=1,sNy
        DO I=1,sNx
         IF ( iceOrNot(I,J) ) THEN
C     Recalculate the fluxes based on the (possibly) adjusted TSURF
          t1 = tsurfLoc(I,J)
#endif /* SEAICE_MODIFY_GROWTH_ADJ */
          t2 = t1*t1
          t3 = t2*t1
          t4 = t2*t2

          IF ( useMaykutSatVapPoly ) THEN
           qhice(I,J)=QS1*(C1*t4+C2*t3 +C3*t2+C4*t1+C5)
          ELSE
C     log 10 of the sat vap pressure
           mm_log10pi = -aa1 / t1 + aa2
C     saturation vapor pressure
c          mm_pi = TEN **(mm_log10pi)
C     The following form does the same, but is faster
           mm_pi = EXP(mm_log10pi*lnTEN)
C     over ice specific humidity
           qhice(I,J) = bb1*mm_pi/( Ppascals -(1.0 _d 0 - bb1)*mm_pi )
          ENDIF
          F_c(I,J)  = effConduct(I,J) * (tempFrz(I,J) - t1)
          F_lh(I,J) = D1I * UG(I,J)*(qhice(I,J)-AQH(I,J,bi,bj))
#ifdef SEAICE_CAP_SUBLIM
          IF (F_lh(I,J) .GT. F_lh_max(I,J)) THEN
             F_lh(I,J)  = F_lh_max(I,J)
          ENDIF
#endif /* SEAICE_CAP_SUBLIM */
          F_lwu(I,J)  = t4 * D3(I,J)
          F_sens(I,J) = D1 * UG(I,J) * (t1 - atempLoc(I,J))
C     The flux between the ice/snow surface and the atmosphere.
          F_ia(I,J) = -lwdownLoc(I,J) -absorbedSW(I,J) + F_lwu(I,J)
     &              +  F_sens(I,J) + F_lh(I,J)

C IGF_SIR-b
          IF (-F_c(I,J) .LT. ZERO) THEN
C note that F_c is flipped sign in this rewrite for some reason
            F_io_net(I,J) = F_c(I,J)
            F_ia_net(I,J) = ZERO
          ELSE
            F_io_net(I,J) = ZERO
            F_ia_net(I,J) = F_ia(I,J)
          ENDIF
C IGF_SIR-e

         ENDIF
        ENDDO
       ENDDO
      ELSEIF ( postSolvTempIter.EQ.1 ) THEN
       DO J=1,sNy
        DO I=1,sNx
         IF ( iceOrNot(I,J) ) THEN
C     Update fluxes (consistent with the linearized formulation)
          delTsurf  = tsurfLoc(I,J)-tsurfPrev(I,J)
          F_c(I,J)  = effConduct(I,J)*(tempFrz(I,J)-tsurfLoc(I,J))
          F_ia(I,J) = F_ia(I,J) + dFia_dTs(I,J)*delTsurf
          F_lh(I,J) = F_lh(I,J)
     &              + D1I*UG(I,J)*dqh_dTs(I,J)*delTsurf

c        ELSEIF ( postSolvTempIter.EQ.0 ) THEN
C     Take fluxes from last iteration

         ENDIF
        ENDDO
       ENDDO
      ENDIF
      DO J=1,sNy
       DO I=1,sNx
        IF ( iceOrNot(I,J) ) THEN

C     Save updated tsurf and finalize the flux terms
         TSURFout(I,J) = tsurfLoc(I,J)
C     Fresh water flux (kg/m^2/s) from latent heat of sublimation.
C     F_lh is positive upward (sea ice looses heat) and FWsublim
C     is also positive upward (atmosphere gains freshwater)
         FWsublim(I,J) = F_lh(I,J)/lhSublim

#ifdef SEAICE_DEBUG
         IF ( (I .EQ. SEAICE_debugPointI) .AND.
     &        (J .EQ. SEAICE_debugPointJ) ) THEN
          print '(A)','----------------------------------------'
          print '(A,i6)','ibi complete ', myIter
          print '(A,4(1x,D24.15))',
     &         'ibi T(SURF, surfLoc,atmos) ',
     &         TSURFout(I,J), tsurfLoc(I,J),atempLoc(I,J)
          print '(A,4(1x,D24.15))',
     &         'ibi LWL                    ', lwdownLoc(I,J)
          print '(A,4(1x,D24.15))',
     &         'ibi QSW(Total, Penetrating)',
     &         SWDOWN(I,J,bi,bj), IcePenetSW(I,J)
          print '(A,4(1x,D24.15))',
     &         'ibi qh(ATM ICE)            ',
     &         AQH(I,J,bi,bj),qhice(I,J)
         print '(A,4(1x,D24.15))',
     &         'ibi F(lwd,swi,lwu)         ',
     &         -lwdownLoc(I,J), -absorbedSW(I,J), F_lwu(I,J)
         print '(A,4(1x,D24.15))',
     &         'ibi F(c,lh,sens)           ',
     &         F_c(I,J), F_lh(I,J), F_sens(I,J)
#ifdef SEAICE_CAP_SUBLIM
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
#endif /* SEAICE_CAP_SUBLIM */
          print '(A,4(1x,D24.15))',
     &         'ibi F_ia, F_ia_net, F_c    ',
     &         F_ia(I,J), F_ia_net(I,J), F_c(I,J)
          print '(A)','----------------------------------------'
         ENDIF
#endif /* SEAICE_DEBUG */

        ENDIF                   !/* iceOrNot */
       ENDDO                    !/* i */
      ENDDO                     !/* j */

#endif /* ALLOW_ATM_TEMP && ALLOW_DOWNWARD_RADIATION */
      RETURN
      END
