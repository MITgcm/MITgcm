C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_solve4temp.F,v 1.1 2010/09/23 22:46:24 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#define USE_ORIGINAL_SBI

CBOP
C     !ROUTINE: SEAICE_SOLVE4TEMP
C     !INTERFACE:
      SUBROUTINE SEAICE_SOLVE4TEMP(
     I   UG, HICE_ACTUAL, HSNOW_ACTUAL,
     U   TSURF,
#ifdef SEAICE_ALLOW_TD_IF
     O   F_io_net, F_ia_net,
#endif
     O   F_ia, IcePenetSWFlux,
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
#include "FFIELDS.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"
#ifdef SEAICE_VARIABLE_FREEZING_POINT
#include "DYNVARS.h"
#endif /* SEAICE_VARIABLE_FREEZING_POINT */
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
# include "EXF_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS
C     === Routine arguments ===
C     INPUT:
C     UG      :: thermal wind of atmosphere
C     HICE_ACTUAL  :: actual ice thickness
C     HSNOW_ACTUAL :: actual snow thickness
C     TSURF   :: surface temperature of ice in Kelvin, updated
C     bi,bj   :: loop indices
C     OUTPUT:
C     F_io_net :: net upward conductive heat flux through ice at the base of the ice
C     F_ia_net :: net heat flux divergence at the sea ice/snow surface:
C                 includes ice conductive fluxes and atmospheric fluxes (W/m^2)
C     F_ia     :: upward sea ice/snow surface heat flux to atmosphere (W/m^2)
C     IcePenetSWFlux :: short wave heat flux under ice
      _RL UG             (1:sNx,1:sNy)
      _RL HICE_ACTUAL    (1:sNx,1:sNy)
      _RL HSNOW_ACTUAL   (1:sNx,1:sNy)
      _RL TSURF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL F_io_net       (1:sNx,1:sNy)
      _RL F_ia_net       (1:sNx,1:sNy)
      _RL F_ia           (1:sNx,1:sNy)
      _RL IcePenetSWFlux (1:sNx,1:sNy)
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
      _RL F_swi      (1:sNx,1:sNy)
      _RL F_lwd      (1:sNx,1:sNy)
      _RL F_lwu      (1:sNx,1:sNy)
      _RL F_lh       (1:sNx,1:sNy)
      _RL F_sens     (1:sNx,1:sNy)
      _RL F_c        (1:sNx,1:sNy)
      _RL qhice      (1:sNx,1:sNy)

      _RL AbsorbedSWFlux       (1:sNx,1:sNy)
      _RL IcePenetSWFluxFrac   (1:sNx,1:sNy)

C     local copies of global variables
      _RL tsurfLoc   (1:sNx,1:sNy)
      _RL atempLoc   (1:sNx,1:sNy)
      _RL lwdownLoc  (1:sNx,1:sNy)
      _RL ALB        (1:sNx,1:sNy)
      _RL ALB_ICE    (1:sNx,1:sNy)
      _RL ALB_SNOW   (1:sNx,1:sNy)

      INTEGER KOPEN
C     i,j - Loop counters
      INTEGER i, j
      INTEGER ITER

      _RL  TB, D1, D1I, D3
      _RL  TMELT, XKI, XKS, HCUT, XIO, dFiDTs1
      _RL  SurfMeltTemp

c     Constants to calculate Saturation Vapor Pressure
#ifdef USE_ORIGINAL_SBI
      _RL  TMELTP, C1, C2, C3, C4, C5, QS1
      _RL A2        (1:sNx,1:sNy)
      _RL A3        (1:sNx,1:sNy)
      _RL B         (1:sNx,1:sNy)
      _RL A1        (1:sNx,1:sNy)
#else
      _RL aa1,aa2,bb1,bb2,Ppascals,cc0,cc1,cc2,cc3t
C     specific humidity at ice surface variables
      _RL  mm_pi,mm_log10pi,dqhice_dTice
#endif

C     effective conductivity of combined ice and snow
      _RL  effConduct


C     powers of temperature
      _RL  t1, t2, t3, t4
      _RL TEN


      TEN = 10.0 _d 0
#ifdef USE_ORIGINAL_SBI
C MAYKUTS CONSTANTS FOR SAT. VAP. PRESSURE TEMP. POLYNOMIAL
      C1=2.7798202 _d -06
      C2=-2.6913393 _d -03
      C3=0.97920849 _d +00
      C4=-158.63779 _d +00
      C5=9653.1925 _d +00

      QS1=0.622 _d +00/1013.0 _d +00

#else
      aa1 = 2663.5 _d 0
      aa2 = 12.537 _d 0
      bb1 = 0.622 _d 0
      bb2 = ONE - bb1
      Ppascals = 100000. _d 0
      cc0 = TEN ** aa2
      cc1 = cc0*aa1*bb1*Ppascals*log(10. _d 0)
      cc2 = cc0*bb2
#endif

C     FREEZING TEMPERATURE OF SEAWATER
#ifndef SEAICE_VARIABLE_FREEZING_POINT
c     Use a constant seaswater freezing point
#ifdef USE_ORIGINAL_SBI
      TB=271.2 _d 0
#else
      TB=273.15 _d 0 + SEAICE_freeze
#endif
#else
c     Use a variable seawater freezing point
         TB = -0.0575 _d 0*salt(I,J,kSrf,bi,bj) + 0.0901 _d 0
     &        + 273.15 _d 0
#endif

C     SENSIBLE HEAT CONSTANT
      D1=SEAICE_sensHeat

C     ICE LATENT HEAT CONSTANT
      D1I=SEAICE_latentIce

C     STEFAN BOLTZMAN CONSTANT TIMES 0.97 EMISSIVITY
      D3=SEAICE_emissivity

C     MELTING TEMPERATURE OF ICE
#ifdef USE_ORIGINAL_SBI
      TMELT=273.16 _d +00
      TMELTP=273.159 _d +00
      SurfMeltTemp = TMELTP
#else
      TMELT = 273.15 _d 0
      SurfMeltTemp = TMELT
#endif

C     ICE CONDUCTIVITY
      XKI=SEAICE_iceConduct

C     SNOW CONDUCTIVITY
      XKS=SEAICE_snowConduct

C     CUTOFF SNOW THICKNESS
      HCUT=SEAICE_snowThick

C     PENETRATION SHORTWAVE RADIATION FACTOR
      XIO=SEAICE_shortwave


c     Initialize variables
      DO J=1,sNy
         DO I=1,sNx
            IcePenetSWFlux     (I,J) = 0. _d 0
            IcePenetSWFluxFrac (I,J) = 0. _d 0
            AbsorbedSWFlux     (I,J) = 0. _d 0

            qhice    (I,J) = 0. _d 0
            F_ia     (I,J) = 0. _d 0

            F_io_net (I,J) = 0. _d 0
            F_ia_net (I,J) = 0. _d 0

c           Reset the snow/ice surface to TMELT and bound the atmospheric temperature
#ifdef USE_ORIGINAL_SBI
            tsurfLoc (I,J) = MIN(273.16 _d 0+MAX_TICE,TSURF(I,J,bi,bj))
            atempLoc (I,J) = MAX(273.16 _d 0+MIN_ATEMP,ATEMP(I,J,bi,bj))
            A1(I,J) = ZERO
            A2(I,J) = ZERO
            A3(I,J) = ZERO
c            B(I,J) = ZERO
            lwdownLoc(I,J) = MAX(MIN_LWDOWN,LWDOWN(I,J,bi,bj))
#else
            F_swi    (I,J) = 0. _d 0
            F_lwd    (I,J) = 0. _d 0
            F_lwu    (I,J) = 0. _d 0
            F_lh     (I,J) = 0. _d 0
            F_sens   (I,J) = 0. _d 0

            tsurfLoc(I,J) =  TSURF(I,J,bi,bj)
            atempLoc (I,J) = MAX(TMELT + MIN_ATEMP,ATEMP(I,J,bi,bj))
            lwdownLoc(I,J) = LWDOWN(I,J,bi,bj)
#endif

         ENDDO
      ENDDO

      DO J=1,sNy
         DO I=1,sNx

C     DECIDE ON ALBEDO
            IF (HICE_ACTUAL(I,J) .GT. ZERO) THEN

               IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
                  IF (tsurfLoc(I,J) .GE. SurfMeltTemp) THEN
                     ALB_ICE (I,J)   = SEAICE_wetIceAlb_south
                     ALB_SNOW(I,J)   = SEAICE_wetSnowAlb_south
                  ELSE          ! no surface melting
                     ALB_ICE (I,J)   = SEAICE_dryIceAlb_south
                     ALB_SNOW(I,J)   = SEAICE_drySnowAlb_south
                  ENDIF
               ELSE             !/ Northern Hemisphere
                  IF (tsurfLoc(I,J) .GE. SurfMeltTemp) THEN
                     ALB_ICE (I,J)   = SEAICE_wetIceAlb
                     ALB_SNOW(I,J)   = SEAICE_wetSnowAlb
                  ELSE          ! no surface melting
                     ALB_ICE (I,J)   = SEAICE_dryIceAlb
                     ALB_SNOW(I,J)   = SEAICE_drySnowAlb
                 ENDIF
               ENDIF            !/ Albedo for snow and ice

#ifdef USE_ORIGINAL_SBI
c              If actual snow thickness exceeds the cutoff thickness, use the
c              snow albedo
               IF (HSNOW_ACTUAL(I,J) .GT. HCUT) THEN
                  ALB(I,J) = ALB_SNOW(I,J)

c              otherwise, use some combination of ice and snow albedo
c              (What is the source of this formulation ?)
               ELSE
                  ALB(I,J) = MIN(ALB_ICE(I,J) + HSNOW_ACTUAL(I,J)/HCUT*
     &                         (ALB_SNOW(I,J) -ALB_ICE(I,J)),
     &                            ALB_SNOW(I,J))
               ENDIF

#else
               IF (HSNOW_ACTUAL(I,J) .GT. ZERO) THEN
                  ALB(I,J) = ALB_SNOW(I,J)
               ELSE
                  ALB(I,J) = ALB_ICE(I,J)
               ENDIF
#endif


#ifdef USE_ORIGINAL_SBI
C       NOW DETERMINE FIXED FORCING TERM IN HEAT BUDGET

#ifdef ALLOW_DOWNWARD_RADIATION
        IF(HSNOW_ACTUAL(I,J).GT.0.0) THEN
C        NO SW PENETRATION WITH SNOW
         A1(I,J)=(ONE-ALB(I,J))*SWDOWN(I,J,bi,bj)
     &        +lwdownLoc(I,J)*0.97 _d 0
     &        +D1*UG(I,J)*atempLoc(I,J)+D1I*UG(I,J)*AQH(I,J,bi,bj)
        ELSE
C        SW PENETRATION UNDER ICE
         A1(I,J)=(ONE-ALB(I,J))*SWDOWN(I,J,bi,bj)
     &        *(ONE-XIO*EXP(-1.5 _d 0*HICE_ACTUAL(I,J)))
     &        +lwdownLoc(I,J)*0.97 _d 0
     &        +D1*UG(I,J)*atempLoc(I,J)+D1I*UG(I,J)*AQH(I,J,bi,bj)
        ENDIF
#endif

#else

c     The longwave radiative flux convergence
               F_lwd(I,J) = - 0.97 _d 0 * lwdownLoc(I,J)

c     Determine the fraction of shortwave radiative flux
c     remaining after scattering through the snow and ice at
c     the ocean interface.  If snow is present, no radiation
c     penetrates to the ocean.
               IF (HSNOW_ACTUAL(I,J) .GT. ZERO) THEN
                  IcePenetSWFluxFrac(I,J) = ZERO
               ELSE
                  IcePenetSWFluxFrac(I,J) =
     &               XIO*EXP(-1.5 _d 0 * HICE_ACTUAL(I,J))
               ENDIF

c     The shortwave radiative flux convergence in the
c     seaice.
               AbsorbedSWFlux(I,J)       = -(ONE - ALB(I,J))*
     &            (ONE - IcePenetSWFluxFrac(I,J))
     &            *SWDOWN(I,J,bi,bj)

c     The shortwave radiative flux convergence in the
c     ocean beneath ice.
               IcePenetSWFlux(I,J) = -(ONE - ALB(I,J))*
     &            IcePenetSWFluxFrac(I,J)
     &            *SWDOWN(I,J,bi,bj)

               F_swi(I,J) = AbsorbedSWFlux(I,J)

c     Set a mininum sea ice thickness of 5 cm to bound
c     the magnitude of conductive heat fluxes.
               HICE_ACTUAL(I,J) = max(HICE_ACTUAL(I,J),5. _d -2)

#endif

c     The effective conductivity of the two-layer
c     snow/ice system.
#ifdef USE_ORIGINAL_SBI
               effConduct=
     &             XKS/(HSNOW_ACTUAL(I,J)/HICE_ACTUAL(I,J) +
     &                  XKS/XKI)/HICE_ACTUAL(I,J)
#else
               effConduct = XKI * XKS /
     &            (XKS * HICE_ACTUAL(I,J) + XKI * HSNOW_ACTUAL(I,J))
#endif



#ifdef SEAICE_DEBUG
               IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &            (J .EQ. SEAICE_debugPointY) ) THEN

                  print '(A,i6)','-----------------------------------'
                  print '(A,i6)','ibi merged initialization ', myIter

                  print '(A,i6,4(1x,D24.15))',
     &               'ibi iter, TSL, TS     ',myIter,
     &               tsurfLoc(I,J), TSURF(I,J,bi,bj)

                  print '(A,i6,4(1x,D24.15))',
     &               'ibi iter, TMELT       ',myIter,TMELT

                  print '(A,i6,4(1x,D24.15))',
     &               'ibi iter, HIA, EFKCON ',myIter,
     &               HICE_ACTUAL(I,J), effConduct

                  print '(A,i6,4(1x,D24.15))',
     &               'ibi iter, HSNOW       ',myIter,
     &              HSNOW_ACTUAL(I,J), ALB(I,J)

                  print '(A,i6)','-----------------------------------'
                  print '(A,i6)','ibi energy balance iterat ', myIter

               ENDIF
#endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
               DO ITER=1,IMAX_TICE

                  t1 = tsurfLoc(I,J)
                  t2 = t1*t1
                  t3 = t2*t1
                  t4 = t2*t2

c                 Calculate the specific humidity in the BL above the snow/ice
#ifdef USE_ORIGINAL_SBI
c                 Use the Maykut polynomial
                  qhice(I,J)=QS1*(C1*t4+C2*t3 +C3*t2+C4*t1+C5)

#else
c                 Use an approximation which is more accurate at low temperatures

c                 log 10 of the sat vap pressure
                  mm_log10pi = -aa1 / t1 + aa2

c                 The saturation vapor pressure (SVP) in the surface
c                 boundary layer (BL) above the snow/ice.
                  mm_pi = TEN **(mm_log10pi)

                  qhice(I,J) = bb1*mm_pi / (Ppascals - (ONE - bb1) *
     &               mm_pi)
#endif

c                 Caclulate the flux terms based on the updated tsurfLoc
#ifdef USE_ORIGINAL_SBI
                  A2(I,J)=-D1*UG(I,J)*t1-D1I*UG(I,J)*qhice(I,J)-D3*t4
                  A3(I,J) = 4.0 _d 0 * D3 * t3 + effConduct + D1*UG(I,J)
                  F_c(I,J)=-effConduct*(TB-tsurfLoc(I,J))
#else
c                 A constant for SVP derivative w.r.t TICE
                  cc3t = TEN **(aa1 / t1)

c                 d(qh)/d(TICE)
                  dqhice_dTice = cc1*cc3t/((cc2-cc3t*Ppascals)**TWO *t2)

c                 d(F_ia)/d(TICE)
                  dFiDTs1 = 4.0 _d 0 * D3*t3 + effConduct + D1*UG(I,J)
     &               + D1I*UG(I,J)*dqhice_dTice

                  F_lh(I,J) = D1I*UG(I,J)*(qhice(I,J)-AQH(I,J,bi,bj))

                  F_c(I,J)  = -effConduct * (TB - t1)

                  F_lwu(I,J)= t4 * D3

                  F_sens(I,J)= D1 * UG(I,J) * (t1 - atempLoc(I,J))

                  F_ia(I,J)  = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &               F_c(I,J) + F_sens(I,J) + F_lh(I,J)

#endif

#ifdef SEAICE_DEBUG
                  IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &               (J .EQ. SEAICE_debugPointY) ) THEN
                     print '(A,i6,4(1x,D24.15))',
     &                  'ice-iter qhICE,       ', ITER,qhIce(I,J)

#ifdef USE_ORIGINAL_SBI
                     print '(A,i6,4(1x,D24.15))',
     &                  'ice-iter A1 A2 B      ', ITER,A1(I,J), A2(I,J),
     &                         -F_c(I,J)

                     print '(A,i6,4(1x,D24.15))',
     &                  'ice-iter A3 (-A1+A2)  ', ITER, A3(I,J),
     &                  -(A1(I,J) + A2(I,J))
#else

                     print '(A,i6,4(1x,D24.15))',
     &                  'ice-iter dFiDTs1 F_ia ', ITER, dFiDTs1,
     &                  F_ia(I,J)
#endif

                  ENDIF
#endif

c                 Update tsurfLoc
#ifdef USE_ORIGINAL_SBI
                  tsurfLoc(I,J)=tsurfLoc(I,J)
     &                    +(A1(I,J)+A2(I,J)-F_c(I,J))/A3(I,J)

                  tsurfLoc(I,J) =MAX(273.16 _d 0+MIN_TICE,tsurfLoc(I,J))
                  tsurfLoc(I,J) =MIN(tsurfLoc(I,J),TMELT)

#else
                  tsurfLoc(I,J) = tsurfLoc(I,J) - F_ia(I,J) / dFiDTs1

c                 If the search leads to tsurfLoc < 50 Kelvin,
c                 restart the search at tsurfLoc = TMELT.  Note that one
c                 solution to the energy balance problem is an
c                 extremely low temperature - a temperature far below
c                 realistic values.

                  IF (tsurfLoc(I,J) .LT. 50.0 _d 0 ) THEN
                     tsurfLoc(I,J) = TMELT
                  ENDIF
#endif

#ifdef SEAICE_DEBUG
                  IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &               (J .EQ. SEAICE_debugPointY) ) THEN

                     print '(A,i6,4(1x,D24.15))',
     &                  'ice-iter tsurfLc,|dif|', ITER,
     &                  tsurfLoc(I,J),
     &                  log10(abs(tsurfLoc(I,J) - t1))
                  ENDIF
#endif

               ENDDO            !/* Iterations */
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c              Finalize the flux terms
#ifdef USE_ORIGINAL_SBI
               F_ia(I,J)=-A1(I,J)-A2(I,J)
               TSURF(I,J,bi,bj)=MIN(tsurfLoc(I,J),TMELT)

               IF (HSNOW_ACTUAL(I,J) .GT. ZERO ) THEN
C              NO SW PENETRATION WITH SNOW
                   IcePenetSWFlux(I,J)=ZERO
               ELSE
C              SW PENETRATION UNDER ICE

#ifdef ALLOW_DOWNWARD_RADIATION
                   IcePenetSWFlux(I,J)=-(ONE-ALB(I,J))*SWDOWN(I,J,bi,bj)
     &              *XIO*EXP(-1.5 _d 0*HICE_ACTUAL(I,J))
#endif
               ENDIF

#else
               tsurfLoc(I,J) = MIN(tsurfLoc(I,J),TMELT)
               TSURF(I,J,bi,bj) = tsurfLoc(I,J)

c              Recalculate the fluxes based on the (possibly) adjusted TSURF
               t1 = tsurfLoc(I,J)
               t2 = t1*t1
               t3 = t2*t1
               t4 = t2*t2

c              log 10 of the sat vap pressure
               mm_log10pi = -aa1 / t1 + aa2

c              saturation vapor pressure
               mm_pi = TEN **(mm_log10pi)

c              over ice specific humidity
               qhice(I,J) = bb1*mm_pi/(Ppascals- (ONE - bb1) * mm_pi)

               F_lh(I,J) = D1I * UG(I,J)*(qhice(I,J)-AQH(I,J,bi,bj))
               F_c(I,J)  = -effConduct * (TB - t1)
               F_lwu(I,J)   = t4 * D3
               F_sens(I,J)  = D1 * UG(I,J) * (t1 - atempLoc(I,J))

c              The flux between the ice/snow surface and the atmosphere.
c              (excludes upward conductive fluxes)
               F_ia(I,J)    = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &            F_sens(I,J) + F_lh(I,J)
#endif

c              Caclulate the net ice-ocean and ice-atmosphere fluxes
               IF (F_c(I,J) .LT. ZERO) THEN
                  F_io_net(I,J) = -F_c(I,J)
                  F_ia_net(I,J) = ZERO
               ELSE
                  F_io_net(I,J) = ZERO
                  F_ia_net(I,J) = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &               F_sens(I,J) + F_lh(I,J)
               ENDIF  !/* conductive fluxes up or down */


#ifdef SEAICE_DEBUG
               IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &            (J .EQ. SEAICE_debugPointY) ) THEN

                  print '(A)','----------------------------------------'
                  print '(A,i6)','ibi complete ', myIter

                  print '(A,4(1x,D24.15))',
     &               'ibi T(SURF, surfLoc,atmos) ',
     &               TSURF(I,J,bi,bj), tsurfLoc(I,J),atempLoc(I,J)

                  print '(A,4(1x,D24.15))',
     &               'ibi LWL                    ', lwdownLoc(I,J)

                  print '(A,4(1x,D24.15))',
     &               'ibi QSW(Total, Penetrating)',
     &               SWDOWN(I,J,bi,bj), IcePenetSWFlux(I,J)

                  print '(A,4(1x,D24.15))',
     &               'ibi qh(ATM ICE)            ',
     &               AQH(I,J,bi,bj),qhice(I,J)

c                  print '(A,4(1x,D24.15))',
c     &               'ibi F(lwd,swi,lwu)         ',
c     &               F_lwd(I,J), F_swi(I,J), F_lwu(I,J)

c                  print '(A,4(1x,D24.15))',
c     &               'ibi F(c,lh,sens)           ',
c     &               F_c(I,J), F_lh(I,J), F_sens(I,J)

                   print '(A,4(1x,D24.15))',
     &               'ibi F_ia, F_ia_net, F_c    ',
#ifdef USE_ORIGINAL_SBI
     &                -(A1(I,J)+A2(I,J)),
     &                -(A1(I,J)+A2(I,J)-F_c(I,J)),
     &                F_c(I,J)
#else
     &                F_ia(I,J),
     &                F_ia_net(I,J),
     &                F_c(I,J)
#endif

                  print '(A)','----------------------------------------'

               ENDIF
#endif

            ENDIF               !/* HICE_ACTUAL > 0 */

         ENDDO                  !/* i */
      ENDDO                     !/* j */

      RETURN
      END
