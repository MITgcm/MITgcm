C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/seaice_budget_ice_if.F,v 1.3 2008/04/08 03:01:55 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_BUDGET_ICE_IF(
     I     UG, HICE_ACTUAL, HSNOW_ACTUAL,
     U     TSURF,
     O     F_io_net,F_ia_net,F_ia, IcePenetSWFlux,
     I     bi, bj )
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
C     HICE_ACTUAL    :: (actual) ice thickness with upper and lower limit
C     HSNOW_ACTUAL :: actual snow thickness
C     bi,bj   :: loop indices
C     OUTPUT:
C     netHeatFlux :: net heat flux under ice = growth rate
C     IcePenetSWFlux  :: short wave heat flux under ice
      _RL UG         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL TSURF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HICE_ACTUAL  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL HSNOW_ACTUAL (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL F_ia       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL F_io_net   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL F_ia_net   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL F_swi      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL F_lwd      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL F_lwu      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL F_lh       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL F_sens     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL F_c        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qhice_mm   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL IcePenetSWFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL AbsorbedSWFlux       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL IcePenetSWFluxFrac
     &           (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      INTEGER bi, bj
      INTEGER KOPEN

C     === Local variables ===
C     i,j - Loop counters
      INTEGER i, j
      INTEGER ITER
      _RL  QS1, C1, C2, C3, C4, C5, TB, D1, D1I, D3,IAN1
      _RL  TMELT, TMELTP, XKI, XKS, HCUT, ASNOW, XIO
C     effective conductivity of combined ice and snow
      _RL  effConduct
C     specific humidity at ice surface
      _RL  mm_pi,mm_log10pi,dqhice_dTice
     
C     powers of temperature
      _RL  t1, t2, t3, t4

C     local copies of global variables
      _RL tsurfLoc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL atempLoc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL lwdownLoc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ALB        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tsurfLocOld

c     Ian Saturation Vapor Pressure 
      _RL aa1,aa2,bb1,bb2,Ppascals,cc0,cc1,cc2,cc3t,dFiDTs1

      aa1 = 2663.5
      aa2 = 12.537
      bb1 = 0.622
      bb2 = 1.0 - bb1
      Ppascals = 1000.*100.
      cc0 = 10**aa2
      cc1 = cc0*aa1*bb1*Ppascals*log(10.0)
      cc2 = cc0*bb2
       
C FREEZING TEMPERATURE OF SEAWATER
      TB=273.15 _d + 00 - 1.96 _d + 00
C SENSIBLE HEAT CONSTANT
      D1=SEAICE_sensHeat
C ICE LATENT HEAT CONSTANT
      D1I=SEAICE_latentIce
C STEFAN BOLTZMAN CONSTANT TIMES 0.97 EMISSIVITY
      D3=SEAICE_emissivity
C MELTING TEMPERATURE OF ICE
      TMELT=273.15 _d +00

C ICE CONDUCTIVITY
      XKI=2.0340
C SNOW CONDUCTIVITY
      XKS=SEAICE_snowConduct
C CUTOFF SNOW THICKNESS
      HCUT=SEAICE_snowThick
C PENETRATION SHORTWAVE RADIATION FACTOR
      XIO=SEAICE_shortwave

      DO J=1,sNy
       DO I=1,sNx
        IcePenetSWFlux     (I,J) = 0. _d 0
        IcePenetSWFluxFrac (I,J) = 0. _d 0
        AbsorbedSWFlux (I,J) = 0. _d 0

        qhice_mm (I,J) = 0.0 _d 0
        F_ia     (I,J) = 0.0 _d 0 
        F_io_net (I,J) = 0.0 _d 0
        F_ia_net (I,J) = 0.0 _d 0

        F_swi    (I,J) = 0.0 _d 0
        F_lwd    (I,J) = 0.0 _d 0
        F_lwu    (I,J) = 0.0 _d 0
        F_lh     (I,J) = 0.0 _d 0
        F_sens   (I,J) = 0.0 _d 0

c set the surface temperature to zero if there is no ice there.       
c
c        IF (HICE_ACTUAL(I,J) .GT. 0.0) THEN        
c          tsurfLoc (I,J) = MIN(TMELT, TSURF(I,J,bi,bj))
c        ELSE
c          tsurfLoc(I,J) = TMELT
c        ENDIF

c reset the surface temperature to the freezing point each time around.
        tsurfLoc(I,J) = TMELT
        TSURF(I,J,bi,bj) = tsurfLoc(I,J)

        atempLoc (I,J) = MAX(TMELT + MIN_ATEMP,ATEMP(I,J,bi,bj))
        lwdownLoc(I,J) = LWDOWN(I,J,bi,bj)

       ENDDO
      ENDDO

C COME HERE AT START OF ITERATION

       DO J=1,sNy
        DO I=1,sNx

         IF (HICE_ACTUAL(I,J) .GT. 0.0) THEN

C         DECIDE ON ALBEDO
          IF (tsurfLoc(I,J) .GE. TMELT) THEN
           IF (HSNOW_ACTUAL(I,J) .EQ. 0.0) THEN
              ALB(I,J)   = SEAICE_wetIceAlb
           ELSE  ! some snow
              ALB(I,J)   = SEAICE_wetSnowAlb
           ENDIF
          ELSE   ! no surface melting
            IF (HSNOW_ACTUAL(I,J) .EQ. 0.0) THEN
              ALB(I,J)   = SEAICE_dryIceAlb
            ELSE !  some snow
              ALB(I,J)   = SEAICE_drySnowAlb
            ENDIF 
          ENDIF 

          F_lwd(I,J) = - 0.97 _d 0 * lwdownLoc(I,J)

          IF (HSNOW_ACTUAL(I,J) .GT. 0.0) THEN
           IcePenetSWFluxFrac(I,J) = ZERO
          ELSE
           IcePenetSWFluxFrac(I,J) = 
     &        XIO*EXP(-1.5 _d 0 * HICE_ACTUAL(I,J))
          ENDIF

           AbsorbedSWFlux(I,J)       = -(ONE - ALB(I,J))*
     &        (1.0 - IcePenetSWFluxFrac(I,J))
     &         *SWDOWN(I,J,bi,bj)

           IcePenetSWFlux(I,J) = -(ONE - ALB(I,J))*
     &        IcePenetSWFluxFrac(I,J)
     &        *SWDOWN(I,J,bi,bj)

          F_swi(I,J) = AbsorbedSWFlux(I,J)

c         set a min ice as 5 cm to limit arbitrarily large conduction.
          HICE_ACTUAL(I,J) = max(HICE_ACTUAL(I,J),5. _d -2)

          effConduct = XKI * XKS / 
     &        (XKS * HICE_ACTUAL(I,J) + XKI * HSNOW_ACTUAL(I,J))

          DO ITER=1,IMAX_TICE

           t1 = tsurfLoc(I,J)
           t2 = t1*t1
           t3 = t2*t1
           t4 = t2*t2

           tsurfLocOld = t1

c          log 10 of the sat vap pressure
           mm_log10pi = -aa1 / t1 + aa2
c          saturation vapor pressure
           mm_pi = 10**(mm_log10pi)
c          over ice specific humidity
           qhice_mm(I,J) = bb1*mm_pi / (Ppascals - (1.0 - bb1) * mm_pi)

c          constant for sat vap pressure derivative w.r.t tice        
           cc3t = 10**(aa1 / t1)
c          the actual derivative
           dqhice_dTice = cc1 * cc3t /( (cc2-cc3t*Ppascals)**2 * t2)

c          the full derivative 
           dFiDTs1 = 4.0 * D3*t3 + effConduct + D1*UG(I,J) +
     &        D1I*UG(I,J)*dqhice_dTice


           F_lh(I,J)    = D1I * UG(I,J) * (qhice_mm(I,J)-AQH(I,J,bi,bj))
           F_c(I,J)     = -effConduct * (TB - t1)
           F_lwu(I,J)   = t4 * D3
           F_sens(I,J)  = D1 * UG(I,J) * (t1 - atempLoc(I,J))

           F_ia(I,J)    = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &         F_c(I,J) + F_sens(I,J) + F_lh(I,J)

           tsurfLoc(I,J) = tsurfLoc(I,J) - F_ia(I,J) / dFiDTs1


c    If the search falls below 50 Kelvin then kick the search back up to  
c    TMELT.  Note that a solution to the equation is for a large negative
c    value of ice surface temperature since the longwave outgoing radiation
c    goes as the fourth power of temperature.

           IF (tsurfLoc(I,J) .LT. 50.0 ) THEN 
                tsurfLoc(I,J) = TMELT 
           ENDIF 

#ifdef SEAICE_DEBUG
          IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &          (J .EQ. SEAICE_debugPointY) ) THEN

            print *,'ice-iter tsurfLc,|dif|', I,J, ITER,tsurfLoc(I,J),
     &           log10(abs(tsurfLoc(I,J) - tsurfLocOld))
          ENDIF
#endif   
          ENDDO !/* Iterations */

          tsurfLoc(I,J) = MIN(tsurfLoc(I,J),TMELT)
          TSURF(I,J,bi,bj) = tsurfLoc(I,J)

          t1 = tsurfLoc(I,J)
          t2 = t1*t1
          t3 = t2*t1
          t4 = t2*t2

c         log 10 of the sat vap pressure
          mm_log10pi = -aa1 / t1 + aa2
c         saturation vapor pressure
          mm_pi = 10**(mm_log10pi)
c         over ice specific humidity
          qhice_mm(I,J) = bb1*mm_pi / (Ppascals - (1.0 - bb1) * mm_pi)

          F_lh(I,J)    = D1I * UG(I,J) * (qhice_mm(I,J)-AQH(I,J,bi,bj))
          F_c(I,J)     = -effConduct * (TB - t1)
          F_lwu(I,J)   = t4 * D3
          F_sens(I,J)  = D1 * UG(I,J) * (t1 - atempLoc(I,J))

c         exlude conductive flux, the actual flux with the atmosphere.
          F_ia(I,J)    = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &         F_sens(I,J) + F_lh(I,J)

          IF (F_c(I,J) .LT. 0.0) THEN
            F_io_net(I,J) = -F_c(I,J)
            F_ia_net(I,J) = 0.0
          ELSE
            F_io_net(I,J) = 0.0
            F_ia_net(I,J) = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &         F_sens(I,J) + F_lh(I,J)
          ENDIF !/* conductive fluxes up or down */


#ifdef SEAICE_DEBUG
          IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &         (J .EQ. SEAICE_debugPointY) ) THEN

          print '(A,2i4,3(1x,1P2E15.3))',
     &     'ibi i j T(SURF, surfLoc,atmos)',I,J, 
     &     TSURF(I,J,bi,bj), tsurfLoc(I,J),atempLoc(I,J) 

          print '(A,2i4,3(1x,1P2E15.3))',
     &     'ibi i j QSW(Tot, Abs, Pen)    ',I,J, 
     &     SWDOWN(I,J,bi,bj), AbsorbedSWFlux(I,J),
     &     IcePenetSWFlux(I,J)

          print '(A,2i4,3(1x,1P2E15.3))',
     &     'ibi i j IcePenSWFluxFrac, Alb ',I,J, 
     ^      IcePenetSWFluxFrac(I,J), ALB(I,J)
 
          print '(A,2i4,3(1x,1P2E15.3))',
     &     'ibi i j qh(ATM ICE)           ',I,J, 
     &      AQH(I,J,bi,bj),qhice_mm(I,J)
 
          print '(A,2i4,3(1x,1P2E15.3))',
     &     'ibi i j F(lwd,swi,lwu)        ',I,J, 
     &      F_lwd(I,J), F_swi(I,J), F_lwu(I,J)

          print '(A,2i4,3(1x,1P2E15.3))',
     &     'ibi i j F(c,lh,sens)          ',I,J, 
     &      F_c(I,J), F_lh(I,J), F_sens(I,J)

          print '(A,2i4,3(1x,1P2E15.3))',
     &     'ibi i j F(io_net,ia_net,ia)   ',I,J, 
     &      F_io_net(I,J), F_ia_net(I,J), F_ia(I,J)

         ENDIF
#endif

         ENDIF  !/* HICE_ACTUAL > 0 */

       ENDDO   !/* i */
      ENDDO    !/* j */

      RETURN
      END
