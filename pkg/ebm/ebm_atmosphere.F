#include "EBM_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: EBM_ATMOSPHERE

C !INTERFACE:
      SUBROUTINE EBM_ATMOSPHERE ( myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R CALCULATE FORCING FROM ENERGY AND MOISTURE
C     | BALANCE ATMOSPHERE
C     *==========================================================*
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

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "EBM.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT PARAMETERS:
C     === Routine arguments ===
C     myThid  :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_EBM
C     !LOCAL VARIABLES:
      INTEGER i, j, bi, bj
      INTEGER no_so
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER iebmkey
#endif /* ALLOW_AUTODIFF_TAMC */
      _RL ReCountX(1-OLy:sNy+OLy,nSy)

C--   Local arrays used for EBM computation (previously declared in EBM.h)
C-    sin(lat) and Legendre polynomials
cph We will make these three (i,j) arrays to
cph avoid AD recomputations
      _RL S(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSy)
      _RL P2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSy)
      _RL P4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSy)
C-    Shortwave and albedo parameters
      _RL SW(1-OLy:sNy+OLy,nSy)
C-    Longwave parameters
      _RL LW(1-OLy:sNy+OLy,nSy)
C-    Heat transport parameters
      _RL Hd(1-OLy:sNy+OLy,nSy), Hd35(2)
C-    Freshwater flux parameters
      _RL Fw(1-OLy:sNy+OLy,nSy), Fw35(2)
C-    Temperature parameterization
      _RL T(1-OLy:sNy+OLy,nSy)
      _RL T_var(4), T0(2), T2(2), T35(2), DTDy35(2)
C-    Parameters used to calculate the transport efficiency
      _RL Cl, Cf, Cs, C
      _RL gamma, kappa, De
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
        act1 = bi - myBxLo(myThid)
        max1 = myBxHi(myThid) - myBxLo(myThid) + 1
        act2 = bj - myByLo(myThid)
        max2 = myByHi(myThid) - myByLo(myThid) + 1
        act3 = myThid - 1
        max3 = nTx*nTy
        act4 = ikey_dynamics - 1
        iebmkey = (act1 + 1) + act2*max1
     &                       + act3*max1*max2
     &                       + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          S(i,j,bj) = 0.0
          P2(i,j,bj) = 0.0
          P4(i,j,bj) = 0.0
         ENDDO
         SW(j,bj) = 0.0
         LW(j,bj) = 0.0
         Hd(j,bj) = 0.0
         Fw(j,bj) = 0.0
         T(j,bj) = 0.0
         ReCountX(j,bj) = 0.0
        ENDDO

        print *, 'SH', TmlS-t_mlt, TtS-t_mlt
        print *, 'NH', TmlN-t_mlt, TtN-t_mlt

C--   account for ice (can absorb heat on an annual averaged basis)
C--   Greenland in Northern Hemisphere, Antarctica in Southern
        DO j = 1,sNy
         ReCountX(j,bj) = CountX(j,bj)
         IF (yC(1,j,bi,bj) .LE. -62.0) THEN
            ReCountX(j,bj) = 90.
         ELSE IF (yC(1,j,bi,bj) .EQ. 74.0) THEN
            ReCountX(j,bj) = CountX(j,bj) + 9.0
         ELSE IF (yC(1,j,bi,bj) .EQ. 70.0) THEN
            ReCountX(j,bj) = CountX(j,bj) + 8.0
         ELSE IF (yC(1,j,bi,bj) .EQ. 66.0) THEN
            ReCountX(j,bj) = CountX(j,bj) + 5.0
         ELSE IF (yC(1,j,bi,bj) .EQ. 62.0) THEN
            ReCountX(j,bj) = CountX(j,bj) + 1.0
         ENDIF
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ReCountX(:,bj) = comlev1_bibj, key=iebmkey, byte=isbyte
#endif

c=====================================================
c     Fit area-weighed  averaged SST north/south of 34
c     degree  to second  Legendre polynomial:
c=======================================================
        T_var(1) = SIN(latBnd(2)*deg2rad) - SIN(latBnd(1)*deg2rad)
        T_var(2) = SIN(latBnd(3)*deg2rad) - SIN(latBnd(2)*deg2rad)
        T_var(3) = SIN(latBnd(2)*deg2rad)**3 - SIN(latBnd(1)*deg2rad)**3
        T_var(4) = SIN(latBnd(3)*deg2rad)**3 - SIN(latBnd(2)*deg2rad)**3
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE T_var(:) = comlev1_bibj, key=iebmkey, byte=isbyte
#endif

c----------------------------------------
c     Southern hemisphere:
c----------------------------------------
        T2(1) =  2.*(TtS - TmlS)*T_var(1)*T_var(2)/
     &     (T_var(3)*T_var(2) - T_var(4)*T_var(1))
        T0(1) = TtS - 0.5*T2(1)*((T_var(3)/T_var(1)) - 1.)
c----------------------------------------
c     Northern hemisphere
c----------------------------------------
        T2(2) =  2.*(TtN - TmlN)*T_var(1)*T_var(2)/
     &     (T_var(3)*T_var(2) - T_var(4)*T_var(1))
        T0(2) = TtN - 0.5*T2(2)*((T_var(3)/T_var(1)) - 1.)
c-----------------------------------------
c     Temperature  at 35 N/S
c-----------------------------------------
        DO no_so = 1,2
         T35(no_so)= T0(no_so) +
     &        T2(no_so)*0.5*
     &        ( 3.*SIN(latBnd(2)*deg2rad)**2 - 1. )
        ENDDO
c-----------------------------------------
c     Temperature gradient at 35 N/S
c-----------------------------------------
        DO no_so = 1, 2
         DTDy35(no_so) = 3.*T2(no_so)*
     &        SIN(latBnd(2)*deg2rad)/rSphere
        ENDDO
c-----------------------------------------------------------
c     Magnitude of the heat and moisture transport at 35 N/S
c-----------------------------------------------------------

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE T35(:)    = comlev1_bibj, key=iebmkey, byte=isbyte
CADJ STORE DTDy35(:) = comlev1_bibj, key=iebmkey, byte=isbyte
#endif
        DO no_so = 1, 2
         IF ( DTDy35(no_so).NE.0. .AND. T35(no_so).NE.0. ) THEN
          gamma = -T35(no_so)*beta*Hw*Nw*Nw/
     &        (gravity*f0*DTDy35(no_so))
          kappa = Hw/(1. _d 0 + gamma)
          De = Hw/(0.48 _d 0 + 1.48 _d 0 *gamma)
          C = 0.6 _d 0 *gravity*kappa*kappa*Nw/
     &        (Tw*f0*f0)
          Cs = rho_air*cp*C*
     &        ( 1. _d 0 /(1. _d 0 /Hw + 1. _d 0 /De)
     &         -1. _d 0 /(1. _d 0 /Hw+1. _d 0 /De+1. _d 0 /dz) )
          Cf = htil*2.97 _d 12*C/(T35(no_so)**3)*(
     &        1. _d 0/(1. _d 0/De + (5420. _d 0*tau /(T35(no_so)**2)))
     &        -1. _d 0/(1. _d 0/De+5420. _d 0*tau/(T35(no_so)**2)
     &        +1. _d 0/dz))
          Cl = Cf*lv
          Hd35(no_so) = 2.*PI*rSphere*COS(latBnd(2)*deg2rad)
     &        *(Cs + Cl*exp(-5420./T35(no_so)))
     &        *(abs(DTDy35(no_so))**trans_eff)
          Fw35(no_so) = 2.*PI*rSphere*COS(latBnd(2)*deg2rad)
     &        *(abs(DTDy35(no_so))**trans_eff)
     &        *Cf*exp(-5420./T35(no_so))
         ELSE
          Hd35(no_so) = 0.
          Fw35(no_so) = 0.
         ENDIF
        ENDDO
c
        Fw35(1) = 929944128.
        Fw35(2) = 678148032.
c
#ifdef EBM_VERSION_1BASIN
c      Fw35(2) = 0.7*Fw35(2)
#else
        Hd35(2) = 1.6 _d 0*Hd35(2)
#endif
c======================================================
c     Calculation of latitudinal profiles
c======================================================
c
        DO j=1,sNy
         DO i=1,sNx
C     sin(lat)
          S(i,j,bj) = SIN(yC(i,j,bi,bj)*deg2rad)
C     setup Legendre polynomials and  derivatives
          P2(i,j,bj) = 0.5*(3.*S(i,j,bj)**2 - 1.)
          P4(i,j,bj) = 0.12 _d 0 *
     &                (35.*S(i,j,bj)**4 - 30.*S(i,j,bj)**2 + 3.)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE S(:,:,bj)    = comlev1_bibj, key=iebmkey, byte=isbyte
CADJ STORE P2(:,:,bj)   = comlev1_bibj, key=iebmkey, byte=isbyte
CADJ STORE P4(:,:,bj)   = comlev1_bibj, key=iebmkey, byte=isbyte
#endif
c
        DO j=1,sNy
         DO i=1,sNx

          IF (yC(i,j,bi,bj) .LT. 0.) THEN
             no_so = 1
          ELSE
             no_so = 2
          ENDIF
c     net shortwave
          SW(j,bj) = 0.25 _d 0 *Q0*(1. _d 0 + Q2*P2(i,j,bj))*
     &         (1. _d 0 - A0 - A2*P2(i,j,bj) - A4*P4(i,j,bj) )
c     temperature
          T(j,bj) = T0(no_so) + T2(no_so)*P2(i,j,bj)
c     net longwave
          LW(j,bj) = LW0 + LW1*(T(j,bj)-t_mlt)
c     climate change run, the parameter to change is DLW
#ifdef EBM_CLIMATE_CHANGE
             LW(j,bj) = LW(j,bj) -
     &            (myTime-startTime)*3.215 _d -8*DLW
c     <            - 6.0
c     <            *75.0*0.0474*
c     <            (-2.62*S(i,j,bj)**8 + 0.73*S(i,j,bj)**7 +
c     <            4.82*S(i,j,bj)**6 -
c     <            1.12*S(i,j,bj)**5 - 2.69*S(i,j,bj)**4 + 0.47*S(i,j,bj)**3 +
c     <            0.51*S(i,j,bj)**2 - 0.05*S(i,j,bj)**1 + 0.17)
#endif
c     fluxes at ocean/atmosphere interface
c     Heat Flux = -Div(atmospheric heat transport) + SW - LW
#ifdef EBM_VERSION_1BASIN
         Qnet(i,j,bi,bj) = -1.0 _d 0 *( SW(j,bj) - LW(j,bj) -
     &        Hd35(no_so)*(
     &        0.000728 _d 4      - 0.00678 _d 4*S(i,j,bj) +
     &        0.0955 _d 4*S(i,j,bj)**2 + 0.0769 _d 4*S(i,j,bj)**3 -
     &        0.8508 _d 4*S(i,j,bj)**4 - 0.3581 _d 4*S(i,j,bj)**5 +
     &        2.9240 _d 4*S(i,j,bj)**6 + 0.8311 _d 4*S(i,j,bj)**7 -
     &        4.9548 _d 4*S(i,j,bj)**8 - 0.8808 _d 4*S(i,j,bj)**9 +
     &        4.0644 _d 4*S(i,j,bj)**10 +0.3409 _d 4*S(i,j,bj)**11 -
     &        1.2893 _d 4*S(i,j,bj)**12 )
     &        /(2.*PI*rSphere*rSphere*25.) )
c             Qnet(i,j,bi,bj) = -1.0*( SW(j,bj) - LW(j,bj) -
c     <            0.5*Hd35(no_so)*(3.054e1 - 3.763e1*S(i,j,bj) +
c     <        1.892e2*S(i,j,bj)**2 + 3.041e2*S(i,j,bj)**3 -
c     <        1.540e3*S(i,j,bj)**4 - 9.586e2*S(i,j,bj)**5 +
c     <        2.939e3*S(i,j,bj)**6 + 1.219e3*S(i,j,bj)**7 -
c     <        2.550e3*S(i,j,bj)**8 - 5.396e2*S(i,j,bj)**9 +
c     <        8.119e2*S(i,j,bj)**10)
c     <            /(2*PI*rSphere*rSphere*22.3) )
#else
          IF (ReCountX(j,bj) .GT. 0.) THEN
             Qnet(i,j,bi,bj) = (-90. _d 0 /ReCountX(j,bj))*
     &            ( SW(j,bj) - LW(j,bj) -
     &            Hd35(no_so)*(3.054 _d 1 - 3.763 _d 1*S(i,j,bj) +
     &        1.892 _d 2*S(i,j,bj)**2 + 3.041 _d 2*S(i,j,bj)**3 -
     &        1.540 _d 3*S(i,j,bj)**4 - 9.586 _d 2*S(i,j,bj)**5 +
     &        2.939 _d 3*S(i,j,bj)**6 + 1.219 _d 3*S(i,j,bj)**7 -
     &        2.550 _d 3*S(i,j,bj)**8 - 5.396 _d 2*S(i,j,bj)**9 +
     &        8.119 _d 2*S(i,j,bj)**10)
     &            /(2.*PI*rSphere*rSphere*22.3 _d 0) )
          ELSE
             Qnet(i,j,bi,bj) = 0.
          ENDIF
#endif
c     Freshwater Flux = Div(atmospheric moisture transport)
c---  conversion of E-P from kg/(s m^2) -> m/s -> g/kg/s: 1e-3*35/delZ(1)
#ifdef EBM_VERSION_1BASIN
          EmPmR(i,j,bi,bj) = -1. _d -3*Fw35(no_so)
     &    *(-0.8454 _d 5*S(i,j,bj)**14 + 0.5367 _d 5*S(i,j,bj)**13
     &    +3.3173 _d 5*S(i,j,bj)**12 - 1.8965 _d 5*S(i,j,bj)**11
     &    -5.1701 _d 5*S(i,j,bj)**10
     &    +2.6240 _d 5*S(i,j,bj)**9 + 4.077 _d 5*S(i,j,bj)**8
     &    -1.791 _d 5*S(i,j,bj)**7
     &    -1.7231 _d 5*S(i,j,bj)**6 + 0.6229 _d 5*S(i,j,bj)**5
     &    +0.3824 _d 5*S(i,j,bj)**4
     &    -0.1017 _d 5*S(i,j,bj)**3 - 0.0387 _d 5*S(i,j,bj)**2
     &    +0.00562 _d 5*S(i,j,bj)  + 0.0007743 _d 5)
     &    /(2.0*12.0*PI*rSphere*rSphere)
c             EmPmR(i,j,bi,bj) = 1.e-3*Fw35(no_so)
c     <            *(50.0 + 228.0*S(i,j,bj) -1.593e3*S(i,j,bj)**2
c     <            - 2.127e3*S(i,j,bj)**3 + 7.3e3*S(i,j,bj)**4
c     <            + 5.799e3*S(i,j,bj)**5 - 1.232e4*S(i,j,bj)**6
c     <            - 6.389e3*S(i,j,bj)**7 + 9.123e3*S(i,j,bj)**8
c     <            + 2.495e3*S(i,j,bj)**9 - 2.567e3*S(i,j,bj)**10)
c     <            /(2*PI*rSphere*rSphere*15.0)
#else
          IF (yC(i,j,bi,bj) .LT. -40.) THEN
c--   Southern Hemisphere
           EmPmR(i,j,bi,bj) = -1. _d -3*(Fw35(no_so)*
     &            (-6.5 _d 0 + 35.3 _d 0 + 71.7 _d 0*S(i,j,bj)
     &           - 1336.3 _d 0*S(i,j,bj)**2 - 425.8 _d 0*S(i,j,bj)**3
     &           + 5434.8 _d 0*S(i,j,bj)**4 + 707.9 _d 0*S(i,j,bj)**5
     &           - 6987.7 _d 0*S(i,j,bj)**6 - 360.4 _d 0*S(i,j,bj)**7
     &           + 2855.0 _d 0*S(i,j,bj)**8)
     &            /(2.*PI*rSphere*rSphere*18.0))
          ELSE
c--   Atlantic
           IF (xC(i,j,bi,bj) .GT. 284.
     &      .OR. xC(i,j,bi,bj) .LT. 28.) THEN
              EmPmR(i,j,bi,bj) = -1. _d -3*(Fw35(no_so)*
     &             (-6.5 _d 0 -2.878 _d 0 + 3.157 _d 2*S(i,j,bj) -
     &             2.388 _d 3*S(i,j,bj)**2 - 4.101 _d 3*S(i,j,bj)**3 +
     &             1.963 _d 4*S(i,j,bj)**4 + 1.534 _d 4*S(i,j,bj)**5 -
     &             6.556 _d 4*S(i,j,bj)**6 - 2.478 _d 4*S(i,j,bj)**7 +
     &             1.083 _d 5*S(i,j,bj)**8 + 1.85 _d 4*S(i,j,bj)**9 -
     &             8.703 _d 4*S(i,j,bj)**10 - 5.276 _d 3*S(i,j,bj)**11 +
     &             2.703 _d 4*S(i,j,bj)**12)
     &             /(2.*PI*rSphere*rSphere*12.0))
           ELSE
c--   Pacific
              EmPmR(i,j,bi,bj) = -1. _d -3*(Fw35(no_so)
     &             *(-6.5 _d 0 +51.89 _d 0 + 4.916 _d 2*S(i,j,bj) -
     &             1.041 _d 3*S(i,j,bj)**2 - 7.546 _d 3*S(i,j,bj)**3 +
     &             2.335 _d 3*S(i,j,bj)**4 + 3.449 _d 4*S(i,j,bj)**5 +
     &             6.702 _d 3*S(i,j,bj)**6 - 6.601 _d 4*S(i,j,bj)**7 -
     &             2.594 _d 4*S(i,j,bj)**8 + 5.652 _d 4*S(i,j,bj)**9 +
     &             2.738 _d 4*S(i,j,bj)**10 - 1.795 _d 4*S(i,j,bj)**11 -
     &             9.486 _d 3*S(i,j,bj)**12)
     &             /(2.*PI*rSphere*rSphere*12.0))
           ENDIF
          ENDIF
#endif
          EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj)
     &                     - Run(i,j,bi,bj)*scale_runoff
          EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj)*rhoConstFresh
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RS(Qnet , myThid )
      _EXCH_XY_RS(EmPmR , myThid )

C      CALL PLOT_FIELD_XYRS( Qnet, 'Qnet' , 1, myThid )
C      CALL PLOT_FIELD_XYRS( EmPmR, 'EmPmR' , 1, myThid )

#endif /* ALLOW_EBM */

      RETURN
      END
