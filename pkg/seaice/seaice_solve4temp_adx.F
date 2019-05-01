C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_budget_ice.F,v 1.3 2006/12/19 18:57:09 dimitri Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_BUDGET_ICE(
     I     UG, HICE_ACTUAL, HSNOW_ACTUAL,
     U     TSURF,
     O     F_io_net,F_ia_net,F_ia, IcePenetratingShortwaveFlux,
     I     bi, bj )
C     /================================================================\
C     | SUBROUTINE seaice_budget_ice                                   |
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
#include "SEAICE_DIAGS.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_FFIELDS.h"
#ifdef SEAICE_VARIABLE_FREEZING_POINT
#include "DYNVARS.h"
#endif 
C/* SEAICE_VARIABLE_FREEZING_POINT */

C     === Routine arguments ===
C     INPUT:
C     UG      :: thermal wind of atmosphere
C     TSURF   :: surface temperature of ice in Kelvin, updated
C     HICE_ACTUAL    :: (actual) ice thickness with upper and lower limit
C     HSNOW_ACTUAL :: actual snow thickness
C     bi,bj   :: loop indices
C     OUTPUT:
C     netHeatFlux :: net heat flux under ice = growth rate
C     IcePenetratingShortwaveFlux  :: short wave heat flux under ice
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

      _RL IcePenetratingShortwaveFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL AbsorbedShortwaveFlux       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL IcePenetratingShortwaveFluxFraction
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


c     Ian Saturation Vapor Pressure 
      _RL aa1,aa2,bb1,bb2,Ppascals,cc0,cc1,cc2,cc3t,dFiDTs1
#ifdef SEAICE_DEBUG
      _RL tsurfLocInitial   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tsurfLocOld
      INTEGER CONV_IT
      _RL TSURF_CONV

      TSURF_CONV = 0.0 _d 0
      CONV_IT = 0
#endif 

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
      XKI=SEAICE_iceConduct
C SNOW CONDUCTIVITY
      XKS=SEAICE_snowConduct
C PENETRATION SHORTWAVE RADIATION FACTOR
      XIO=SEAICE_shortwave

c/* debug */

      DO J=1,sNy
       DO I=1,sNx

        IcePenetratingShortwaveFlux         (I,J) = 0. _d 0
        IcePenetratingShortwaveFluxFraction (I,J) = 0. _d 0
        AbsorbedShortwaveFlux               (I,J) = 0. _d 0

        qhice_mm (I,J) = 0.0 _d 0
        F_ia     (I,J) = 0.0 _d 0 
        F_io_net (I,J) = 0.0 _d 0
        F_ia_net (I,J) = 0.0 _d 0

        F_swi    (I,J) = 0.0 _d 0
        F_lwd    (I,J) = 0.0 _d 0
        F_lwu    (I,J) = 0.0 _d 0
        F_lh     (I,J) = 0.0 _d 0
        F_sens   (I,J) = 0.0 _d 0

        flux_LWI(I,J,bi,bj) = 0.0
        flux_SWI(I,J,bi,bj) = 0.0
        flux_LWU(I,J,bi,bj) = 0.0
        flux_CON(I,J,bi,bj) = 0.0
        flux_SEN(I,J,bi,bj) = 0.0
        flux_LAT(I,J,bi,bj) = 0.0
        flux_ION(I,J,bi,bj) = 0.0
        flux_IAN(I,J,bi,bj) = 0.0
 
        atempLoc (I,J) = MAX(TMELT + MIN_ATEMP,ATEMP(I,J,bi,bj))
        lwdownLoc(I,J) = LWDOWN(I,J,bi,bj)

c Set the ice surface temperature to the melting point if there is 
c no ice here, or to the minimum of the ice melting point or the
c last ice surface temperature.  You never know what could have
c happened to TMELT since it was last through this subroutine... 

        IF (HICE_ACTUAL(I,J) .GT. 0.0) THEN       
            tsurfLoc (I,J) = MIN(TMELT, TSURF(I,J,bi,bj))
            TSURF(I,J,bi,bj) = tsurfLoc(I,J)
        ELSE
            tsurfLoc(I,J) = TMELT
        ENDIF

#ifdef SEAICE_DEBUG
C       The snow/ice surface temperature when we enter the routine
        tsurfLocInitial(I,J) = tsurfLoc(I,J)
#endif
       ENDDO !/* I */
      ENDDO !/* J */


c     GO THROUGH EACH POINT.... 
       DO J=1,sNy
        DO I=1,sNx

c     DON'T BOTHER DOING THE REST OF THIS SUBROUTINE
c     IF THERE IS NO ICE HERE TO BEING WITH
c     THERE CAN BE NO ICE-ATM OR ICE-OCEAN FLUX IN THAT CASE
         IF (HICE_ACTUAL(I,J) .GT. 0.0) THEN

C         DECIDE ON ALBEDO BASED ON THE ICE SURFACE TEMPERATURE
C         OF THE PREVIOUS ITERATION
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
          ENDIF !/* Surf temp is below or at freezing */

#ifdef SEAICE_DEBUG
          IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &         (J .EQ. SEAICE_debugPointY) ) THEN

            print '(A,2i4,3(1x,1P2E15.3))',
     &      'ibi i j initial tsurfLoc ',I,J,tsurfLoc(I,J)

            print '(A,2i4,3(1x,1P2E15.3))',
     &      'ibi i j ALB/ATEMP/LWDOWN ',I,J,ALB(I,J),
     &      atempLoc(I,J),lwdownLoc(I,J)
    
            print *,''
          ENDIF !/* DEBUG POINT */
#endif

c THESE VARIABLES ARE INDEPENDENT OF THE ICE/SNOW SURFACE
c TEMPERATURE. CALCULATE THEM FIRST BEFORE THE ICE/SNOW
c SURFACE TEMPERATURE ITERATIVE LOOP
          F_lwd(I,J) = - 0.97 _d 0 * lwdownLoc(I,J)

          IF (HSNOW_ACTUAL(I,J) .GT. 0.0) THEN
           IcePenetratingShortwaveFluxFraction(I,J) = ZERO
          ELSE
           IcePenetratingShortwaveFluxFraction(I,J) = 
     &        XIO*EXP(-1.5 _d 0 * HICE_ACTUAL(I,J))
          ENDIF

          AbsorbedShortwaveFlux(I,J)       = -(ONE - ALB(I,J))*
     &        (1.0 - IcePenetratingShortwaveFluxFraction(I,J))
     &         *SWDOWN(I,J,bi,bj)

          IcePenetratingShortwaveFlux(I,J) = -(ONE - ALB(I,J))*
     &        IcePenetratingShortwaveFluxFraction(I,J)
     &        *SWDOWN(I,J,bi,bj)

          F_swi(I,J) = AbsorbedShortwaveFlux(I,J)

c         set a min ice as 5 cm to limit arbitrarily large conduction.
          HICE_ACTUAL(I,J) = max(HICE_ACTUAL(I,J),0.05)

          effConduct = XKI * XKS / 
     &        (XKS * HICE_ACTUAL(I,J) + XKI * HSNOW_ACTUAL(I,J))

#ifdef SEAICE_DEBUG
c         RESET THE ITERATION WHERE CONVERGENCE IS ACHIEVED
          CONV_IT = 0
#endif

c Onset of iterative search --------------------
          DO ITER=1,IMAX_TICE

C          ONLY BOTHER REFINING THE SOLUTION IF AN ENERGY BALANCE 
C          TO SPECIFICED PRECISION HAS NOT BEEN ACHIEVED.  ONE ITERATION
C          IS REQUIRED TO CALCULATE F_IA.
           IF ( (ITER .GT. 1 )                .AND. 
     &          (ABS(F_ia(I,J)) .LT. SEAICE_heatFluxPrecision))  THEN
c           We have achieved the precision required.  Be careful
c           not to set this value higher than machine precision.
c           1.0e-12 works well.  Convergence within 5 iterations is
c           typical.

#ifdef SEAICE_DEBUG
c           Record the iteration where precision was first achieved.
            IF (CONV_IT .EQ. 0) THEN
                CONV_IT = ITER
                print *,' convergence at it ',ITER-1 
            ENDIF !/* First time hitting precision*/
#endif         
           ELSE !/*  Required precision not yet achieved.  Iterate */

           t1 = tsurfLoc(I,J)
           t2 = t1*t1
           t3 = t2*t1
           t4 = t2*t2

#ifdef SEAICE_DEBUG
           tsurfLocOld = t1
#endif
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


c qhice_mm depends on t1 via mm_pi via mm_log10pi
           F_lh(I,J)    = D1I * UG(I,J) * (qhice_mm(I,J)-AQH(I,J,bi,bj))

c these depend on t1 directly
           F_c(I,J)     = -effConduct * (TB - t1)
           F_lwu(I,J)   = t4 * D3
           F_sens(I,J)  = D1 * UG(I,J) * (t1 - atempLoc(I,J))

#ifdef SEAICE_DEBUG                      
           IF ( (I .EQ. SEAICE_debugPointX)   .and. 
     &          (J .EQ. SEAICE_debugPointY) ) THEN

            print '(A,2i4,4(1x,1P2E15.3))',
     &      'ibi i j F_LH/F_C/F_LWU/F_SENS',I,J,
     &       F_lh(I,J),F_c(I,J),F_lwu(I,J),F_sens(I,J)

            print '(A,2i4,4(1x,1P2E15.3))',
     &      'ibi i j UG,QHICE,AQH, t1     ', I,J,
     &       UG(I,J),qhice_mm(I,J),AQH(I,J,bi,bj), t1
           ENDIF !/* debug point */
#endif 

C       This is the net flux into the surface of the ice, it includes
C       conductive heat fluxes.  This will be zero ultimately 
C       if the search works.

           F_ia(I,J)    = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &        F_c(I,J) + F_sens(I,J) + F_lh(I,J)

C  Update the ice surface temperature to further reduce the 
C  net heat flux at the ice surface.

           tsurfLoc(I,J) = tsurfLoc(I,J) - F_ia(I,J) / dFiDTs1


c    If the search falls below 50 Kelvin then reset the algorithm at
c    TMELT - 10 K.  Hope that a slightly different starting point 
c    will lead to a solution

c    Note that a solution to the equation is for a large negative
c    value of ice surface temperature since the longwave outgoing radiation
c    goes as the fourth power of temperature.

           IF (tsurfLoc(I,J) .LT. 50.0 ) THEN 
                tsurfLoc(I,J) = TMELT  - 10.0 
                print *,'ifice : had to wrap around i,j, iteration',
     &                   i,j, ITER
           ENDIF  !/* Tsurf too cold */

#ifdef SEAICE_DEBUG
c    Debug the difference in tsurf from iteration to  iteration
            IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &          (J .EQ. SEAICE_debugPointY) ) THEN
              TSURF_CONV= log10(abs(tsurfLoc(I,J) - tsurfLocOld))

              print *,'0076',ITER,TSURF_CONV,
     &          abs(tsurfLoc(I,J) - tsurfLocInitial(I,J))

            ENDIF !/* Debug point */ 
#endif     
 
           ENDIF !/* Not yet reached required precision */
          ENDDO !/* End of iterations */
c ---------------------------------------------------

c At this point the iterations are over and we have 
c an ice/snow surface temperature.

c First, we take the final ice/snow surface temperature and
c recalculate the fluxes.  The temperature that balanced heat 
c fluxes may be above the freezing point of ice/snow.  In 
c that case we limit the temperature to the freezing point.

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

C   recalculate the total heat convergence at the ice surface with the 
C   new perhaps bounded surface temperature.  If the surface ice temperature
C   that balances heat fluxes is above the freezing point of seawater then
C   there  will be conductive heat fluxes into the ice which will melt ice.
           F_ia(I,J)    = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &        F_c(I,J) + F_sens(I,J) + F_lh(I,J)

C   Determine whether the conductive fluxes indicate fluxes out of the ocean
C   or into the ice.
          IF (F_c(I,J) .LT. 0.0) THEN
C           Conductive fluxes through the ice are negative this implies
C           that we are taking the heat out of the ocean and there is no net
C           convergence of heat at the ice surface.  F_ia should be zero.

            F_io_net(I,J) = -F_c(I,J)
            F_ia_net(I,J) = 0.0 _d 0
          ELSE
C           Conductive fluxes through the ice are positive.  This implies
C           that heat has converged to the surface and is going to conduct
C           into the ice, melting it.  The rate of convergence is given by
C           F_ia_net.

            F_io_net(I,J) = 0.0
            F_ia_net(I,J) = F_lwd(I,J) + F_swi(I,J) + F_lwu(I,J) +
     &         F_sens(I,J) + F_lh(I,J)

          ENDIF !/* conductive fluxes up or down */


          flux_LWI(I,J,bi,bj) = F_lwd(I,J)
          flux_SWI(I,J,bi,bj) = F_swi(I,J)
          flux_LWU(I,J,bi,bj) = F_lwu(I,J)
          flux_CON(I,J,bi,bj) = F_c(I,J)
          flux_SEN(I,J,bi,bj) = F_sens(I,J)
          flux_LAT(I,J,bi,bj) = F_lh(I,J)
          flux_ION(I,J,bi,bj) = F_io_net(I,J)
          flux_IAN(I,J,bi,bj) = F_ia_net(I,J)

c Next, do some more detailed debugging for this particular point

#ifdef SEAICE_DEBUG
          IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &         (J .EQ. SEAICE_debugPointY) ) THEN

          print *,'------- IBI summary -------'
          print *,''
          print '(A,2i4,3(1x,1P2E15.6))',
     &     'ibi i j T(SURF, surfLoc,atmos)',I,J, 
     &     TSURF(I,J,bi,bj), tsurfLoc(I,J),atempLoc(I,J) 

          print '(A,2i4,3(1x,1P2E15.6))',
     &     'ibi i j QSW(Tot, Abs, Pen)    ',I,J, 
     &     SWDOWN(I,J,bi,bj), AbsorbedShortwaveFlux(I,J),
     &     IcePenetratingShortwaveFlux(I,J)

          print '(A,2i4,3(1x,1P2E15.6))',
     &     'ibi i j IcePenSWFluxFrac, Alb ',I,J, 
     &      IcePenetratingShortwaveFluxFraction(I,J), ALB(I,J)
 
          print '(A,2i4,3(1x,1P2E15.6))',
     &     'ibi i j qh(ATM ICE)           ',I,J, 
     &      AQH(I,J,bi,bj),qhice_mm(I,J)
 
          print '(A,2i4,3(1x,1P2E15.6))',
     &     'ibi i j F(lwd,swi,lwu)        ',I,J, 
     &      F_lwd(I,J), F_swi(I,J), F_lwu(I,J)

          print '(A,2i4,3(1x,1P2E15.6))',
     &     'ibi i j F(c,lh,sens)          ',I,J, 
     &      F_c(I,J), F_lh(I,J), F_sens(I,J)

          print '(A,2i4,3(1x,1P2E15.6))',
     &     'ibi i j F(io_net,ia_net,ia)   ',I,J, 
     &      F_io_net(I,J), F_ia_net(I,J), F_ia(I,J)

          ENDIF !/* Debug point */ 
#endif


c The whole point of this 
c algorithm is to find the ice/snow surface temperature that achieves an
c energy balance.  Here we do the oft-neglected task of checking that it 
c actually has worked.  Tolerance is arbitrarly set at 0.0001 W/m^2

c If the surface temperature is greater than the freezing point of salt
c water then we will not have an energy balance since the conductive flux
c will be transferring heat into the ice from the atmosphere.  It is only
c when the ice surface is cooler than the ocean surface that conductive 
c fluxes ultimately balance the net heat loss at the surface.

           IF (tsurfLoc(I,J) .LT. TB) THEN
             IF (ABS(F_ia(I,J)) .GT. SEAICE_heatFluxPrecision) THEN
                print *,'ifice : energy balance failure at i,j ',I,J
                print *,'tsurfLoc, F_ia_net',tsurfLoc(I,J),F_ia_net(I,J)
                print *,'tsurfLoc, F_ia',tsurfLoc(I,J),F_ia(I,J)
                stop
             ENDIF  !/* Heat flux imbalance too great */
           ENDIF !/* surface temperature is less than freezing point */  

        ENDIF  
       ENDDO   !/* i */
      ENDDO    !/* j */

      RETURN
      END
