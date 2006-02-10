C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_solve4temp.F,v 1.6 2006/02/10 00:24:12 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_SOLVE4TEMP
C     !INTERFACE:
      SUBROUTINE THSICE_SOLVE4TEMP(
     I                     useBlkFlx, flxExcSw, Tf, hi, hs, 
     U                     flxSW, Tsf, qicen,
     O                     Tice, sHeating, flxCnB,
     O                     dTsf, flxAtm, evpAtm,
     I                     i,j,bi,bj, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_SOLVE4TEMP
C     *==========================================================*
C     | Solve (implicitly) for sea-ice and surface temperature
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C    useBlkFlx :: use surf. fluxes from bulk-forcing external S/R
C     flxExcSw :: surf. heat flux (+=down) except SW, function of surf. temp Ts:
C                0: Flx(Ts=0) ; 1: Flx(Ts=Ts^n) ; 2: d.Flx/dTs(Ts=Ts^n)
C     Tf       :: freezing temperature (oC) of local sea-water
C     hi       :: ice height
C     hs       :: snow height
C     flxSW    :: net Short-Wave flux (+=down) [W/m2]: input= at surface
C              ::               output= at the sea-ice base to the ocean 
C     Tsf      :: surface (ice or snow) temperature
C     qicen    :: ice enthalpy (J/kg)
C     Tice     :: internal ice temperatures
C     sHeating :: surf heating left to melt snow or ice (= Atmos-conduction)
C     flxCnB   :: heat flux conducted through the ice to bottom surface
C     dTsf     :: surf. temp adjusment: Ts^n+1 - Ts^n
C     flxAtm   :: net flux of energy from the atmosphere [W/m2] (+=down)
C                without snow precip. (energy=0 for liquid water at 0.oC)
C     evpAtm   :: evaporation to the atmosphere (kg/m2/s) (>0 if evaporate)
C   i,j,bi,bj  :: indices of current grid point
C     myThid   :: Thread no. that called this routine.
      LOGICAL useBlkFlx
      _RL flxExcSw(0:2)
      _RL Tf
      _RL hi
      _RL hs

      _RL flxSW
      _RL Tsf
      _RL qicen(nlyr)

      _RL Tice (nlyr)
      _RL sHeating
      _RL flxCnB
      _RL dTsf
      _RL flxAtm
      _RL evpAtm
      INTEGER i,j, bi,bj
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C ADAPTED FROM:
C LANL CICE.v2.0.2
C-----------------------------------------------------------------------
C.. thermodynamics (vertical physics) based on M. Winton 3-layer model
C.. See Bitz, C. M. and W. H. Lipscomb, 1999:  "An energy-conserving 
C..       thermodynamic sea ice model for climate study."  J. Geophys. 
C..       Res., 104, 15669 - 15677.
C..     Winton, M., 1999:  "A reformulated three-layer sea ice model."  
C..       Submitted to J. Atmos. Ocean. Technol.  
C.. authors Elizabeth C. Hunke and William Lipscomb
C..         Fluid Dynamics Group, Los Alamos National Laboratory
C-----------------------------------------------------------------------
Cc****subroutine thermo_winton(n,fice,fsnow,dqice,dTsfc)
C.. Compute temperature change using Winton model with 2 ice layers, of
C.. which only the top layer has a variable heat capacity.

C     == Local Variables ==
      INTEGER k, iterMax

      _RL  frsnow        ! fractional snow cover

      _RL  fswpen        ! SW penetrating beneath surface (W m-2)
      _RL  fswdn         ! SW absorbed at surface (W m-2)
      _RL  fswint        ! SW absorbed in ice (W m-2)
      _RL  fswocn        ! SW passed through ice to ocean (W m-2)

      _RL  flxExceptSw   ! net surface heat flux, except short-wave (W/m2)
C     evap           :: evaporation over snow/ice [kg/m2/s] (>0 if evaporate)
C     dEvdT          :: derivative of evap. with respect to Tsf [kg/m2/s/K]
      _RL  evap, dEvdT
      _RL  flx0          ! net surf heat flux, from Atmos. to sea-ice (W m-2)
      _RL  fct           ! heat conducted to top surface

      _RL  df0dT         ! deriv of flx0 wrt Tsf (W m-2 deg-1)

      _RL  k12, k32      ! thermal conductivity terms
      _RL  a10, b10      ! coefficients in quadratic eqn for T1
      _RL  a1, b1, c1    ! coefficients in quadratic eqn for T1
c     _RL  Tsf_start     ! old value of Tsf

      _RL  dt            ! timestep

      INTEGER iceornot
      LOGICAL dBug

 1010 FORMAT(A,I3,3F8.3)
 1020 FORMAT(A,1P4E11.3)

      dt  = thSIce_deltaT
      dBug = .FALSE.
c     dBug = ( bi.EQ.3 .AND. i.EQ.15 .AND. j.EQ.11 )
c     dBug = ( bi.EQ.6 .AND. i.EQ.18 .AND. j.EQ.10 )
      IF (dBug) WRITE(6,'(A,2I4,2I2)') 'ThSI_SOLVE4T: i,j=',i,j,bi,bj

      IF ( hi.LT.himin ) THEN
C If hi < himin, melt the ice.
         STOP 'THSICE_SOLVE4TEMP: should not enter if hi<himin'
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C fractional snow cover
      frsnow = 0. _d 0
      IF (hs .GT. 0. _d 0) frsnow = 1. _d 0

C Compute SW flux absorbed at surface and penetrating to layer 1.
      fswpen  = flxSW * (1. _d 0 - frsnow) * i0
      fswocn = fswpen * exp(-ksolar*hi)
      fswint = fswpen - fswocn

      fswdn = flxSW - fswpen

C Compute conductivity terms at layer interfaces.

      k12 = 4. _d 0*kice*ksnow / (ksnow*hi + 4. _d 0*kice*hs)
      k32 = 2. _d 0*kice  / hi

C compute ice temperatures
      a1 = cpice
      b1 = qicen(1) + (cpwater-cpice )*Tmlt1 - Lfresh
      c1 = Lfresh * Tmlt1
      Tice(1) = 0.5 _d 0 *(-b1 - SQRT(b1*b1-4. _d 0*a1*c1))/a1
      Tice(2) = (Lfresh-qicen(2)) / cpice

      IF (Tice(1).GT.0. _d 0 .OR. Tice(2).GT.0. _d 0) THEN
          WRITE (6,*) 'BBerr Tice(1) > 0 = ',Tice(1)
          WRITE (6,*) 'BBerr Tice(2) > 0 = ',Tice(2)
      ENDIF
      IF (dBug) WRITE(6,1010) 'ThSI_SOLVE4T: k, Ts, Tice=',0,Tsf,Tice

C Compute coefficients used in quadratic formula.

      a10 = rhoi*cpice *hi/(2. _d 0*dt) +
     &      k32 * (4. _d 0*dt*k32 + rhoi*cpice *hi)
     &       / (6. _d 0*dt*k32 + rhoi*cpice *hi)
      b10 = -hi*
     &      (rhoi*cpice*Tice(1)+rhoi*Lfresh*Tmlt1/Tice(1))
     &       /(2. _d 0*dt)
     &      - k32 * (4. _d 0*dt*k32*Tf+rhoi*cpice *hi*Tice(2))
     &       / (6. _d 0*dt*k32 + rhoi*cpice *hi) - fswint
      c1 = rhoi*Lfresh*hi*Tmlt1 / (2. _d 0*dt)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C Compute new surface and internal temperatures; iterate until
C Tsfc converges.

      IF ( useBlkFlx ) THEN
        iterMax = nitMaxTsf
      ELSE
        iterMax = 1
      ENDIF
      dTsf = Terrmax

C ----- begin iteration  -----
      DO k = 1,iterMax
       IF ( ABS(dTsf).GE.Terrmax ) THEN

C Save temperatures at start of iteration.
c        Tsf_start = Tsf

        IF ( useBlkFlx ) THEN
C Compute top surface flux.
         IF (hs.GT.3. _d -1) THEN
              iceornot=2
         ELSE
              iceornot=1
         ENDIF
         CALL THSICE_GET_BULKF(
     I                        iceornot, Tsf,
     O                        flxExceptSw, df0dT, evap, dEvdT,
     I                        i,j,bi,bj,myThid )
         flx0 = fswdn + flxExceptSw
        ELSE
         flx0 = fswdn + flxExcSw(1)
         df0dT = flxExcSw(2)
        ENDIF
        IF ( dBug ) WRITE(6,1020) 'ThSI_SOLVE4T: flx0,df0dT,k12,D=',
     &                                     flx0,df0dT,k12,k12-df0dT

C Compute new top layer and surface temperatures.
C If Tsfc is computed to be > 0 C, fix Tsfc = 0 and recompute T1
C with different coefficients. 

         a1 = a10 - k12*df0dT / (k12-df0dT)
         b1 = b10 - k12*(flx0-df0dT*Tsf) / (k12-df0dT)
         Tice(1) = -(b1 + SQRT(b1*b1-4. _d 0*a1*c1))/(2. _d 0*a1)
         dTsf = (flx0 + k12*(Tice(1)-Tsf)) / (k12-df0dT)
         Tsf = Tsf + dTsf
         IF (Tsf .GT. 0. _d 0) THEN
            IF(dBug) WRITE(6,1010) 'ThSI_SOLVE4T: k,ts,t1,dTs=',
     &                                          k,Tsf,Tice(1),dTsf
            a1 = a10 + k12
            b1 = b10          ! note b1 = b10 - k12*Tf0
            Tice(1) = (-b1 - SQRT(b1*b1-4. _d 0*a1*c1))/(2. _d 0*a1)
            Tsf = 0. _d 0
           IF ( useBlkFlx ) THEN
            IF (hs.GT.3. _d -1) THEN
                 iceornot=2
            ELSE
                 iceornot=1
            ENDIF
            CALL THSICE_GET_BULKF(
     I                        iceornot, Tsf,
     O                        flxExceptSw, df0dT, evap, dEvdT,
     I                        i,j,bi,bj,myThid )
            flx0 = fswdn + flxExceptSw
            dTsf = 0. _d 0
           ELSE
            flx0 = fswdn + flxExcSw(0)
            dTsf = 1000.
            df0dT = 0.
           ENDIF
         ENDIF

C Check for convergence.  If no convergence, then repeat.
C
C Convergence test: Make sure Tsfc has converged, within prescribed error.  
C (Energy conservation is guaranteed within machine roundoff, even
C if Tsfc has not converged.)
C If no convergence, then repeat.

         IF ( dBug ) WRITE(6,1010) 'ThSI_SOLVE4T: k,ts,t1,dTs=',
     &                                            k,Tsf,Tice(1),dTsf
         IF ( useBlkFlx .AND. k.EQ.nitMaxTsf
     &                  .AND. ABS(dTsf).GE.Terrmax ) THEN
            WRITE (6,*) 'BB: thermw conv err ',i,j,bi,bj,dTsf
            WRITE (6,*) 'BB: thermw conv err, iceheight ', hi
            WRITE (6,*) 'BB: thermw conv err: Tsf, flx0', Tsf,flx0
            IF (Tsf.LT.-70. _d 0) STOP
         ENDIF

100   continue  ! surface temperature iteration
       ENDIF
      ENDDO
150   continue
C ------ end iteration ------------

C Compute new bottom layer temperature.

      Tice(2) = (2. _d 0*dt*k32*(Tice(1)+2. _d 0*Tf)
     &        + rhoi*cpice *hi*Tice(2))
     &         /(6. _d 0*dt*k32 + rhoi*cpice *hi)
      IF (dBug) WRITE(6,1010) 'ThSI_SOLVE4T: k, Ts, Tice=',k,Tsf,Tice


C Compute final flux values at surfaces.

      fct    = k12*(Tsf-Tice(1))
      flxCnB = 4. _d 0*kice *(Tice(2)-Tf)/hi
      flx0   = flx0 + df0dT*dTsf
      IF ( useBlkFlx ) THEN
        evpAtm = evap
C--   needs to update also Evap (Tsf changes) since Latent heat has been updated
        evpAtm = evap + dEvdT*dTsf
C-    energy flux to Atmos: use net short-wave flux at surf. and
C     use latent heat = Lvap (energy=0 for liq. water at 0.oC)
        flxAtm = flxSW + flxExceptSw + df0dT*dTsf
     &                 + evpAtm*Lfresh
      ELSE
        flxAtm = 0.
        evpAtm = 0.
      ENDIF
      sHeating = flx0 - fct

C-    SW flux at sea-ice base left to the ocean
      flxSW = fswocn

      IF (dBug) WRITE(6,1020) 'ThSI_SOLVE4T: flx0,fct,Dif,flxCnB=',
     &    flx0,fct,flx0-fct,flxCnB

C Compute new enthalpy for each layer.

      qicen(1) = -cpwater*Tmlt1 + cpice *(Tmlt1-Tice(1)) + 
     &              Lfresh*(1. _d 0-Tmlt1/Tice(1))
      qicen(2) = -cpice *Tice(2) + Lfresh

C Make sure internal ice temperatures do not exceed Tmlt.
C (This should not happen for reasonable values of i0.)

      IF (Tice(1) .GE. Tmlt1) then 
        WRITE (6,'(A,2I4,2I3,1P2E14.6)')
     &   'BBerr - Bug: IceT(1) > Tmlt',i,j,bi,bj,Tice(1),Tmlt1
      ENDIF
      IF (Tice(2) .GE. 0. _d 0) THEN
       WRITE (6,'(A,2I4,2I3,1P2E14.6)')
     &   'BBerr - Bug: IceT(2) > 0',i,j,bi,bj,Tice(2)
      ENDIF

#endif  /* ALLOW_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
