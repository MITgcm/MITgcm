C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_atmosphere.F,v 1.1 2004/05/14 21:10:34 heimbach Exp $
C $Name:  $

#include "EBM_OPTIONS.h"

      SUBROUTINE EBM_ATMOSPHERE ( myTime, myiter, myThid )

C     |==========================================================|
C     | S/R CALCULATE FORCING FROM ENERGY AND MOISTURE           |
C     | BALANCE ATMOSPHERE                                       |
C     |==========================================================|
C      References:
C      * X. Wang, P. Stone and J. Marotzke, 1999:
C        Global thermohaline circulation. Part I:
C        Sensitivity to atmospheric moisture transport.
C        J. Climate 12(1), 71-82
C      * X. Wang, P. Stone and J. Marotzke, 1999:
C        Global thermohaline circulation. Part II:
C        Sensitivity with interactive transport.
C        J. Climate 12(1), 83-91
C      * M. Nakamura, P. Stone and J. Marotzke, 1994:
C        Destabilization of the thermohaline circulation
C        by atmospheric eddy transports.
C        J. Climate 7(12), 1870-1882

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_EBM
# include "EBM.h"
#endif

C     === Routine arguments ===
C     myThid - Instance number for this innvocation of CALC_FORCING
      INTEGER myThid
      INTEGER myIter
      _RL myTime
CEndOfInterface

#ifdef ALLOW_EBM

C     == Local variables ==
      _RL Dy
      _RL ReCountX(1-OLy:sNy+OLy)
      INTEGER bi, bj
      INTEGER i, j
      INTEGER no_so
      LOGICAL TOP_LAYER

C--   Top layer only
cph      TOP_LAYER = k .EQ. 1

cph      IF ( TOP_LAYER ) THEN
      
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO j=1-OLy,sNy+OLy
         S(j) = 0.0
         P2(j) = 0.0
         P4(j) = 0.0
         SW(j) = 0.0
         LW(j) = 0.0
         Hd(j) = 0.0
         Fw(j) = 0.0
         T(j) = 0.0
         ReCountX(j) = 0.0
        ENDDO

        print *, 'SH', TmlS-t_mlt, TtS-t_mlt
        print *, 'NH', TmlN-t_mlt, TtN-t_mlt

C--   account for ice (can absorb heat on an annual averaged basis)
C--   Greenland in Northern Hemisphere, Antarctica in Southern
        DO j = 1-OLy,sNy+OLy
         ReCountX(j) = CountX(j)
         IF (yC(1,j,bi,bj) .LE. -62.0) THEN
            ReCountX(j) = 90.
         ELSE IF (yC(1,j,bi,bj) .EQ. 74.0) THEN
            ReCountX(j) = CountX(j) + 9.0
         ELSE IF (yC(1,j,bi,bj) .EQ. 70.0) THEN
            ReCountX(j) = CountX(j) + 8.0
         ELSE IF (yC(1,j,bi,bj) .EQ. 66.0) THEN
            ReCountX(j) = CountX(j) + 5.0
         ELSE IF (yC(1,j,bi,bj) .EQ. 62.0) THEN
            ReCountX(j) = CountX(j) + 1.0
         ENDIF
        ENDDO
        

c=====================================================
c     Fit area-weighed  averaged SST north/south of 34
c     degree  to second  Legendre polynomial:
c=======================================================
        T_var(1) = SIN(lat(2)*deg2rad) - SIN(lat(1)*deg2rad)
        T_var(2) = SIN(lat(3)*deg2rad) - SIN(lat(2)*deg2rad)
        T_var(3) = SIN(lat(2)*deg2rad)**3. - SIN(lat(1)*deg2rad)**3.
        T_var(4) = SIN(lat(3)*deg2rad)**3. - SIN(lat(2)*deg2rad)**3.

c----------------------------------------
c     Southern hemisphere:
c----------------------------------------
        T2(1) =  2.*(TtS - TmlS)*T_var(1)*T_var(2)/
     <     (T_var(3)*T_var(2) - T_var(4)*T_var(1))
        T0(1) = TtS - 0.5*T2(1)*((T_var(3)/T_var(1)) - 1.)
c----------------------------------------
c	Northern hemisphere
c----------------------------------------
        T2(2) =  2.*(TtN - TmlN)*T_var(1)*T_var(2)/
     <     (T_var(3)*T_var(2) - T_var(4)*T_var(1))
        T0(2) = TtN - 0.5*T2(2)*((T_var(3)/T_var(1)) - 1.)
c-----------------------------------------
c     Temperature  at 35 N/S
c-----------------------------------------
        DO no_so = 1,2
         T35(no_so)= T0(no_so) + 
     <        T2(no_so)*0.5*
     <        ((3.*SIN(lat(2)*deg2rad)**2. - 1.))
        ENDDO
c-----------------------------------------
c     Temperature gradient at 35 N/S
c-----------------------------------------
        DO no_so = 1, 2
         DTDy35(no_so) = 3.*T2(no_so)*
     <        SIN(lat(2)*deg2rad)/rSphere
        ENDDO
c-----------------------------------------------------------
c     Magnitude of the heat and moisture transport at 35 N/S
c-----------------------------------------------------------


        DO no_so = 1, 2
         gamma = -T35(no_so)*beta*Hw*Nw*Nw/
     <        (gravity*f0*DTDy35(no_so))
         kappa = Hw/(1 + gamma)
         De = Hw/(0.48 + 1.48*gamma)
         C = 0.6*gravity*kappa*kappa*Nw/
     <        (Tw*f0*f0)
         Cs = rho_air*cp*C*
     <        (1/(1/Hw+1/De) - 1/(1/Hw+1/De+1/dz))
         Cf = htil*2.97e12*C/(T35(no_so)**3)*(
     <        1/(1/De + (5420*tau /(T35(no_so)**2)))
     <     - 1/(1/De+5420*tau/(T35(no_so)**2)+1/dz))
         Cl = Cf*lv
         Hd35(no_so) = 2.*PI*rSphere*COS(lat(2)*deg2rad)
     <        *(Cs + Cl*exp(-5420./T35(no_so)))
     <        *(abs(DTDy35(no_so))**trans_eff)
         Fw35(no_so) = 2.*PI*rSphere*COS(lat(2)*deg2rad)
     <        *(abs(DTDy35(no_so))**trans_eff)
     <        *Cf*exp(-5420./T35(no_so))
c        write(0,*) no_so, Hd35(no_so), Fw35(no_so)
        ENDDO
        Fw35(1) = 929944128.
        Fw35(2) = 678148032.
#ifdef EBM_VERSION_1BASIN
c      Fw35(2) = 0.7*Fw35(2)
#else
        Hd35(2) = 1.6*Hd35(2)
#endif
c======================================================
c     Calculation of latitudinal profiles
c======================================================
c  
        DO j=1-OLy,sNy+OLy
         DO i=1-Olx,sNx+Olx

          IF (yC(i,j,bi,bj) .LT. 0.) THEN
             no_so = 1
          ELSE
             no_so = 2
          ENDIF
C     sin(lat)
          S(j) = sin(yC(i,j,bi,bj)*deg2rad)
C     setup Legendre polynomials and  derivatives
          P2(j) = 0.5*(3.*S(j)**2 - 1.)
          P4(j) = 0.12*(35.*S(j)**4 - 30.*S(j)**2 + 3.)
c     net shortwave
          SW(j) = 0.25*Q0*(1 + Q2*P2(j))*
     <         (1 - A0 - A2*P2(j) - A4*P4(j) )
c     temperature
          T(j) = T0(no_so) + T2(no_so)*P2(j)
c     net longwave
          LW(j) = LW0 + LW1*(T(j)-t_mlt)
c     climate change run, the parameter to change is DLW
#ifdef EBM_CLIMATE_CHANGE
             LW(j) = LW(j) - 
     <            (myTime-startTime)*3.215e-8*DLW
c     <            - 6.0
c     <            *75.0*0.0474*
c     <            (-2.62*S(j)**8 + 0.73*S(j)**7 + 
c     <            4.82*S(j)**6 - 
c     <            1.12*S(j)**5 - 2.69*S(j)**4 + 0.47*S(j)**3 + 
c     <            0.51*S(j)**2 - 0.05*S(j)**1 + 0.17)
#endif
c     fluxes at ocean/atmosphere interface
c     Heat Flux = -Div(atmospheric heat transport) + SW - LW
#ifdef EBM_VERSION_1BASIN
         Qnet(i,j,bi,bj) = -1.0*( SW(j) - LW(j) - 
     <        Hd35(no_so)*(
     <        0.000728e4      - 0.00678e4*S(j) + 
     <        0.0955e4*S(j)**2 + 0.0769e4*S(j)**3 - 
     <        0.8508e4*S(j)**4 - 0.3581e4*S(j)**5 + 
     <        2.9240e4*S(j)**6 + 0.8311e4*S(j)**7 -   
     <        4.9548e4*S(j)**8 - 0.8808e4*S(j)**9 + 
     <        4.0644e4*S(j)**10 +0.3409e4*S(j)**11 - 
     <        1.2893e4*S(j)**12 )
     <        /(2*PI*rSphere*rSphere*25.0) )
c             Qnet(i,j,bi,bj) = -1.0*( SW(j) - LW(j) - 
c     <            0.5*Hd35(no_so)*(3.054e1 - 3.763e1*S(j) + 
c     <        1.892e2*S(j)**2 + 3.041e2*S(j)**3 - 
c     <        1.540e3*S(j)**4 - 9.586e2*S(j)**5 + 
c     <        2.939e3*S(j)**6 + 1.219e3*S(j)**7 -   
c     <        2.550e3*S(j)**8 - 5.396e2*S(j)**9 + 
c     <        8.119e2*S(j)**10)
c     <            /(2*PI*rSphere*rSphere*22.3) )
#else
          IF (ReCountX(j) .GT. 0.) THEN
             Qnet(i,j,bi,bj) = (-90./ReCountX(j))*
     <            ( SW(j) - LW(j) - 
     <            Hd35(no_so)*(3.054e1 - 3.763e1*S(j) + 
     <        1.892e2*S(j)**2 + 3.041e2*S(j)**3 - 
     <        1.540e3*S(j)**4 - 9.586e2*S(j)**5 + 
     <        2.939e3*S(j)**6 + 1.219e3*S(j)**7 -   
     <        2.550e3*S(j)**8 - 5.396e2*S(j)**9 + 
     <        8.119e2*S(j)**10)
     <            /(2*PI*rSphere*rSphere*22.3) )
          ELSE
             Qnet(i,j,bi,bj) = 0.
          ENDIF
#endif
c     Freshwater Flux = Div(atmospheric moisture transport)
c---  conversion of E-P from kg/(s m^2) -> m/s -> psu/s: 1e-3*35/delZ(1)
#ifdef EBM_VERSION_1BASIN
          EmPmR(i,j,bi,bj) = -1.e-3*Fw35(no_so)
     <    *(-0.8454e5*S(j)**14 + 0.5367e5*S(j)**13 
     <    +3.3173e5*S(j)**12 - 1.8965e5*S(j)**11 - 5.1701e5*S(j)**10
     <    +2.6240e5*S(j)**9 + 4.077e5*S(j)**8 - 1.791e5*S(j)**7
     <    -1.7231e5*S(j)**6 + 0.6229e5*S(j)**5 + 0.3824e5*S(j)**4
     <    -0.1017e5*S(j)**3 - 0.0387e5*S(j)**2 
     <    +0.00562e5*S(j)  + 0.0007743e5)
     <    /(2.0*12.0*PI*rSphere*rSphere)
c             EmPmR(i,j,bi,bj) = 1.e-3*Fw35(no_so)
c     <            *(50.0 + 228.0*S(j) -1.593e3*S(j)**2 
c     <            - 2.127e3*S(j)**3 + 7.3e3*S(j)**4 
c     <            + 5.799e3*S(j)**5 - 1.232e4*S(j)**6 
c     <            - 6.389e3*S(j)**7 + 9.123e3*S(j)**8 
c     <            + 2.495e3*S(j)**9 - 2.567e3*S(j)**10)
c     <            /(2*PI*rSphere*rSphere*15.0) 
#else
          IF (yC(i,j,bi,bj) .LT. -40.) THEN
c--   Southern Hemisphere
           EmPmR(i,j,bi,bj) = -1.e-3*(Fw35(no_so)*
     <            (-6.5 + 35.3 + 71.7*S(j)  
     <           - 1336.3*S(j)**2 - 425.8*S(j)**3 
     <           + 5434.8*S(j)**4 + 707.9*S(j)**5 
     <           - 6987.7*S(j)**6 - 360.4*S(j)**7 
     <           + 2855.0*S(j)**8)
     <            /(2*PI*rSphere*rSphere*18.0))
          ELSE
c--   Atlantic
           IF (xC(i,j,bi,bj) .GT. 284. 
     <      .OR. xC(i,j,bi,bj) .LT. 28.) THEN
              EmPmR(i,j,bi,bj) = -1.e-3*(Fw35(no_so)*
     <             (-6.5 -2.878 + 3.157e2*S(j) -
     <             2.388e3*S(j)**2 - 4.101e3*S(j)**3 + 
     <             1.963e4*S(j)**4 + 1.534e4*S(j)**5 - 
     <             6.556e4*S(j)**6 - 2.478e4*S(j)**7 + 
     <             1.083e5*S(j)**8 + 1.85e4*S(j)**9 - 
     <             8.703e4*S(j)**10 - 5.276e3*S(j)**11 + 
     <             2.703e4*S(j)**12)
     <             /(2*PI*rSphere*rSphere*12.0))
           ELSE
c--   Pacific
              EmPmR(i,j,bi,bj) = -1.e-3*(Fw35(no_so)
     <             *(-6.5 +51.89 + 4.916e2*S(j) - 
     <             1.041e3*S(j)**2 - 7.546e3*S(j)**3 + 
     <             2.335e3*S(j)**4 + 3.449e4*S(j)**5 + 
     <             6.702e3*S(j)**6 - 6.601e4*S(j)**7 - 
     <             2.594e4*S(j)**8 + 5.652e4*S(j)**9 + 
     <             2.738e4*S(j)**10 - 1.795e4*S(j)**11 - 
     <             9.486e3*S(j)**12)
     <             /(2*PI*rSphere*rSphere*12.0))
           ENDIF
          ENDIF
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C      CALL PLOT_FIELD_XYRS( Qnet, 'Qnet' , 1, myThid )
C      CALL PLOT_FIELD_XYRS( EmPmR, 'EmPmR' , 1, myThid )

cph  end of IF TOP_LAYER
cph      ENDIF

#endif /* ALLOW_EBM */

      END

    
      


