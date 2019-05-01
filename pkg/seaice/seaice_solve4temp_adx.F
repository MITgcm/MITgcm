C     $Header: /home/ubuntu/mnt/e9_copy/MITgcm_contrib/ifenty/Fenty_Thermo_Code_Updates/code_updates/seaice_budget_ice_if.F,v 1.1 2010/09/03 13:05:44 ifenty Exp $
C     $Name:  $

#include "SEAICE_OPTIONS.h"

C     StartOfInterface
      SUBROUTINE SEAICE_BUDGET_ICE_IF(
     I   UG, HICE_ACTUAL, HSNOW_ACTUAL,
     U   TSURF,
     O   F_io_net,F_ia_net,F_ia, IcePenetSWFlux,
     I   bi, bj, myTime, myIter, myThid )

C     /================================================================\
C     | SUBROUTINE seaice_budget_ice_if                                |
C     | o Calculate ice growth rate, surface fluxes and temperature of |
C     |   ice surface.                                                 |
C     |   see Hibler, MWR, 108, 1943-1973, 1980                        |
C     |================================================================|
C     \================================================================/
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

C     === Routine arguments ===
C     INPUT:
C     UG      :: thermal wind of atmosphere
C     TSURF   :: surface temperature of ice in Kelvin, updated
C     HICE_ACTUAL    :: actual ice thickness
C     HSNOW_ACTUAL :: actual snow thickness
C     bi,bj   :: loop indices

C     OUTPUT:
C     netHeatFlux :: net heat flux under ice = growth rate
C     IcePenetSWFlux  :: short wave heat flux under ice

      _RL TSURF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL UG             (1:sNx,1:sNy)
      _RL HICE_ACTUAL    (1:sNx,1:sNy)
      _RL HSNOW_ACTUAL   (1:sNx,1:sNy)

      _RL F_ia           (1:sNx,1:sNy)
      _RL F_io_net       (1:sNx,1:sNy)
      _RL F_ia_net       (1:sNx,1:sNy)
      _RL IcePenetSWFlux (1:sNx,1:sNy)

      _RL F_swi      (1:sNx,1:sNy)
      _RL F_lwd      (1:sNx,1:sNy)
      _RL F_lwu      (1:sNx,1:sNy)
      _RL F_lh       (1:sNx,1:sNy)
      _RL F_sens     (1:sNx,1:sNy)
      _RL F_c        (1:sNx,1:sNy)
      _RL qhice_mm   (1:sNx,1:sNy)

      _RL AbsorbedSWFlux       (1:sNx,1:sNy)
      _RL IcePenetSWFluxFrac   (1:sNx,1:sNy)

C     local copies of global variables
      _RL tsurfLoc   (1:sNx,1:sNy)
      _RL atempLoc   (1:sNx,1:sNy)
      _RL lwdownLoc  (1:sNx,1:sNy)
      _RL ALB        (1:sNx,1:sNy)
      
      _RL tsurfLocOld

      INTEGER bi, bj, myIter, myThid
      INTEGER KOPEN
      _RL myTime
C     === Local variables ===
C     i,j - Loop counters
      INTEGER i, j
      INTEGER ITER

      _RL  QS1, C1, C2, C3, C4, C5, TB, D1, D1I, D3
      _RL  TMELT, TMELTP, XKI, XKS, HCUT, ASNOW, XIO

C     effective conductivity of combined ice and snow
      _RL  effConduct

C     specific humidity at ice surface variables
      _RL  mm_pi,mm_log10pi,dqhice_dTice
      
C     powers of temperature
      _RL  t1, t2, t3, t4
      _RL TEN
c     Ian Saturation Vapor Pressure 
      _RL aa1,aa2,bb1,bb2,Ppascals,cc0,cc1,cc2,cc3t,dFiDTs1

      TEN = 10.0 _d 0
      aa1 = 2663.5 _d 0
      aa2 = 12.537 _d 0
      bb1 = 0.622 _d 0
      bb2 = ONE - bb1
      Ppascals = 100000. _d 0
      cc0 = TEN ** aa2
      cc1 = cc0*aa1*bb1*Ppascals*log(10. _d 0)
      cc2 = cc0*bb2
      
C     FREEZING TEMPERATURE OF SEAWATER
      TB=273.15 _d 0 + SEAICE_freeze 

C     SENSIBLE HEAT CONSTANT
      D1=SEAICE_sensHeat

C     ICE LATENT HEAT CONSTANT
      D1I=SEAICE_latentIce

C     STEFAN BOLTZMAN CONSTANT TIMES 0.97 EMISSIVITY
      D3=SEAICE_emissivity

C     MELTING TEMPERATURE OF ICE
      TMELT = 273.15 _d 0 

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

            qhice_mm (I,J) = 0. _d 0
            F_ia     (I,J) = 0. _d 0 
            F_io_net (I,J) = 0. _d 0
            F_ia_net (I,J) = 0. _d 0

            F_swi    (I,J) = 0. _d 0
            F_lwd    (I,J) = 0. _d 0
            F_lwu    (I,J) = 0. _d 0
            F_lh     (I,J) = 0. _d 0
            F_sens   (I,J) = 0. _d 0

c     Reset the snow/ice surface to TMELT 
            tsurfLoc(I,J) = TMELT
            TSURF(I,J,bi,bj) = tsurfLoc(I,J)

c     Bound the atmospheric temperature
            atempLoc (I,J) = MAX(TMELT + MIN_ATEMP,ATEMP(I,J,bi,bj))
            lwdownLoc(I,J) = LWDOWN(I,J,bi,bj)
         ENDDO
      ENDDO

      
      DO J=1,sNy
         DO I=1,sNx

C     DECIDE ON ALBEDO
            IF (HICE_ACTUAL(I,J) .GT. ZERO) THEN

               IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
                  IF (tsurfLoc(I,J) .GE. TMELT) THEN
                     IF (HSNOW_ACTUAL(I,J) .EQ. ZERO) THEN
                        ALB(I,J)   = SEAICE_wetIceAlb_south
                     ELSE       ! some snow
                        ALB(I,J)   = SEAICE_wetSnowAlb_south
                     ENDIF
                  ELSE          ! no surface melting
                     IF (HSNOW_ACTUAL(I,J) .EQ. ZERO) THEN
                        ALB(I,J)   = SEAICE_dryIceAlb_south
                     ELSE       !  some snow
                        ALB(I,J)   = SEAICE_drySnowAlb_south
                     ENDIF 
                  ENDIF         !/ The snow/ice surface temperature

               ELSE             !/ Northern Hemisphere
                  IF (tsurfLoc(I,J) .GE. TMELT) THEN
                     IF (HSNOW_ACTUAL(I,J) .EQ. ZERO) THEN
                        ALB(I,J)   = SEAICE_wetIceAlb
                     ELSE       ! some snow
                        ALB(I,J)   = SEAICE_wetSnowAlb
                     ENDIF
                  ELSE          ! no surface melting
                     IF (HSNOW_ACTUAL(I,J) .EQ. ZERO) THEN
                        ALB(I,J)   = SEAICE_dryIceAlb
                     ELSE       !  some snow
                        ALB(I,J)   = SEAICE_drySnowAlb
                     ENDIF 
                  ENDIF
               ENDIF            !/ Albedo is determined

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

c     The effective conductivity of the two-layer 
c     snow/ice system.
               effConduct = XKI * XKS / 
     &            (XKS * HICE_ACTUAL(I,J) + XKI * HSNOW_ACTUAL(I,J))


#ifdef SEAICE_DEBUG
               IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &            (J .EQ. SEAICE_debugPointY) ) THEN

                  print '(A,i6)','-----------------------------------'
                  print '(A,i6)','ibi initialization ', myIter

                  print '(A,i6,4(1x,1PE33.26))',
     &               'ibi iter, TSL, TS ',myIter,
     &               tsurfLoc(I,J), TSURF(I,J,bi,bj)

                  print '(A,i6,4(1x,1PE33.26))',
     &               'ibi iter, TMELT ',myIter,TMELT

                  print '(A,i6,4(1x,1PE33.26))',
     &               'ibi iter, HIA, EFKCON ',myIter,
     &               HICE_ACTUAL(I,J), effConduct

                  print '(A,i6)','-----------------------------------'
                  print '(A,i6)','ibi energy balance iterat ', myIter
                  
               ENDIF
#endif

               
               DO ITER=1,IMAX_TICE
                  
                  t1 = tsurfLoc(I,J)
                  t2 = t1*t1
                  t3 = t2*t1
                  t4 = t2*t2

                  tsurfLocOld = t1

c     log 10 of the sat vap pressure
                  mm_log10pi = -aa1 / t1 + aa2

c     The saturation vapor pressure (SVP) in the surface 
c     boundary layer (BL) above the snow/ice.
                  mm_pi = TEN **(mm_log10pi)

c     The specific humidity in the BL above the snow/ice
                  qhice_mm(I,J) = bb1*mm_pi / (Ppascals - (ONE - bb1) *
     &               mm_pi)

c     A constant for SVP derivative w.r.t TICE
                  cc3t = TEN **(aa1 / t1)

c     d(qh)/d(TICE)
                  dqhice_dTice = cc1*cc3t/((cc2-cc3t*Ppascals)**TWO *t2)

c     d(F_ia)/d(TICE) 
                  dFiDTs1 = 4.0 _d 0 * D3*t3 + effConduct + D1*UG(I,J) +
     &               D1I*UG(I,J)*dqhice_dTice


                  F_lh(I,J) = D1I*UG(I,J)*(qhice_mm(I,J)-AQH(I,J,bi,bj))
                  F_c(I,J)  = -effConduct * (TB - t1)
                  F_lwu(I,J)= t4 * D3
                  F_sens(I,J)= D1 * UG(I,J) * (t1 - atempLoc(I,J))

                  F_ia(I,J)  = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &               F_c(I,J) + F_sens(I,J) + F_lh(I,J)

                  tsurfLoc(I,J) = tsurfLoc(I,J) - F_ia(I,J) / dFiDTs1


c     If the search leads to tsurfLoc < 50 Kelvin,
c     restart the search at tsurfLoc = TMELT.  Note that one
c     solution to the energy balance problem is an 
c     extremely low temperature - a temperature far below
c     what is observed.

                  IF (tsurfLoc(I,J) .LT. 50.0 _d 0 ) THEN 
                     tsurfLoc(I,J) = TMELT 
                  ENDIF 

#ifdef SEAICE_DEBUG
                  IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &               (J .EQ. SEAICE_debugPointY) ) THEN

                     print '(A,i6,4(1x,1PE33.26))',
     &                  'ice-iter tsurfLc,|dif|  ', ITER,
     &                  tsurfLoc(I,J),
     &                  log10(abs(tsurfLoc(I,J) - tsurfLocOld))
                  ENDIF
#endif  
                  
               ENDDO            !/* Iterations */


               tsurfLoc(I,J) = MIN(tsurfLoc(I,J),TMELT)
               TSURF(I,J,bi,bj) = tsurfLoc(I,J)

               t1 = tsurfLoc(I,J)
               t2 = t1*t1
               t3 = t2*t1
               t4 = t2*t2

c     log 10 of the sat vap pressure
               mm_log10pi = -aa1 / t1 + aa2
c     saturation vapor pressure
               mm_pi = TEN **(mm_log10pi)
c     over ice specific humidity
               qhice_mm(I,J) = bb1*mm_pi/(Ppascals- (ONE - bb1) * mm_pi)

               F_lh(I,J) = D1I * UG(I,J)*(qhice_mm(I,J)-AQH(I,J,bi,bj))
               F_c(I,J)  = -effConduct * (TB - t1)
               F_lwu(I,J)   = t4 * D3
               F_sens(I,J)  = D1 * UG(I,J) * (t1 - atempLoc(I,J))

c     exlude conductive flux, the actual flux with the atmosphere.
               F_ia(I,J)    = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &            F_sens(I,J) + F_lh(I,J)

               IF (F_c(I,J) .LT. ZERO) THEN
                  F_io_net(I,J) = -F_c(I,J)
                  F_ia_net(I,J) = ZERO
               ELSE
                  F_io_net(I,J) = ZERO
                  F_ia_net(I,J) = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &               F_sens(I,J) + F_lh(I,J)
               ENDIF            !/* conductive fluxes up or down */


#ifdef SEAICE_DEBUG
               IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &            (J .EQ. SEAICE_debugPointY) ) THEN

                  print '(A)','----------------------------------------'
                  print '(A,i6)','ibi complete ', myIter

                  print '(A,4(1x,1PE33.26))',
     &               'ibi T(SURF, surfLoc,atmos) ',
     &               TSURF(I,J,bi,bj), tsurfLoc(I,J),atempLoc(I,J) 

                  print '(A,4(1x,1PE33.26))',
     &               'ibi LWL               ', lwdownLoc(I,J)
                  
                  print '(A,4(1x,1PE33.26))',
     &               'ibi QSW(Tot, Abs, Pen)     ', 
     &               SWDOWN(I,J,bi,bj), AbsorbedSWFlux(I,J),
     &               IcePenetSWFlux(I,J)

                  print '(A,4(1x,1PE33.26))',
     &               'ibi IcePenSWFluxFrac, Alb  ' , 
     ^               IcePenetSWFluxFrac(I,J), ALB(I,J)
                  
                  print '(A,4(1x,1PE33.26))',
     &               'ibi qh(ATM ICE)            ', 
     &               AQH(I,J,bi,bj),qhice_mm(I,J)
                  
                  print '(A,4(1x,1PE33.26))',
     &               'ibi F(lwd,swi,lwu)         ', 
     &               F_lwd(I,J), F_swi(I,J), F_lwu(I,J)

                  print '(A,4(1x,1PE33.26))',
     &               'ibi F(c,lh,sens)           ', 
     &               F_c(I,J), F_lh(I,J), F_sens(I,J)

                  print '(A,4(1x,1PE33.26))',
     &               'ibi F(io_net,ia_net,ia)    ', 
     &               F_io_net(I,J), F_ia_net(I,J), F_ia(I,J)

                  print '(A)','----------------------------------------'

               ENDIF
#endif

            ENDIF               !/* HICE_ACTUAL > 0 */

         ENDDO                  !/* i */
      ENDDO                     !/* j */

      RETURN
      END
