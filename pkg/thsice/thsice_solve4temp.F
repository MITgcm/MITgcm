C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_solve4temp.F,v 1.11 2006/06/06 22:10:52 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_SOLVE4TEMP
C     !INTERFACE:
      SUBROUTINE THSICE_SOLVE4TEMP(
     I                  bi, bj, siLo, siHi, sjLo, sjHi,
     I                  iMin,iMax, jMin,jMax, dBugFlag, 
     I                  useBulkForce, useEXF,
     I                  iceMask, hIce, hSnow, tFrz, flxExSW,
     U                  flxSW, tSrf, qIc1, qIc2,
     O                  tIc1, tIc2, dTsrf,
     O                  sHeat, flxCnB, flxAtm, evpAtm,
     I                  myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_SOLVE4TEMP
C     *==========================================================*
C     | Solve (implicitly) for sea-ice and surface temperature
C     *==========================================================*
C     \ev

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

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     siLo,siHi   :: size of input/output array: 1rst dim. lower,higher bounds
C     sjLo,sjHi   :: size of input/output array: 2nd  dim. lower,higher bounds
C     bi,bj       :: tile indices
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C     dBugFlag    :: allow to print debugging stuff (e.g. on 1 grid point).
C     useBulkForce:: use surf. fluxes from bulk-forcing external S/R
C     useEXF      :: use surf. fluxes from exf          external S/R
C---  Input:
C         iceMask :: sea-ice fractional mask [0-1]
C  hIce    (hi)   :: ice height [m]
C  hSnow   (hs)   :: snow height [m]
C  tFrz    (Tf)   :: sea-water freezing temperature [oC] (function of S)
C  flxExSW  (=)   :: surf. heat flux (+=down) except SW, function of surf. temp Ts:
C                    0: Flx(Ts=0) ; 1: Flx(Ts=Ts^n) ; 2: d.Flx/dTs(Ts=Ts^n)
C---  Modified (input&output):
C  flxSW  (netSW) :: net Short-Wave flux (+=down) [W/m2]: input= at surface
C           (=)   ::                 output= below sea-ice, into the ocean
C  tSrf    (Tsf)  :: surface (ice or snow) temperature
C  qIc1   (qicen) :: ice enthalpy (J/kg), 1rst level
C  qIc2   (qicen) :: ice enthalpy (J/kg), 2nd level
C---  Output
C  tIc1    (Tice) :: temperature of ice layer 1 [oC]
C  tIc2    (Tice) :: temperature of ice layer 2 [oC]
C  dTsrf   (dTsf) :: surf. temp adjusment: Ts^n+1 - Ts^n
C  sHeat(sHeating):: surf heating flux left to melt snow or ice (= Atmos-conduction)
C  flxCnB   (=)   :: heat flux conducted through the ice to bottom surface
C  flxAtm   (=)   :: net flux of energy from the atmosphere [W/m2] (+=down)
C                    without snow precip. (energy=0 for liquid water at 0.oC)
C  evpAtm   (=)   :: evaporation to the atmosphere [kg/m2/s] (>0 if evaporate)
C---  Input:
C     myTime      :: current Time of simulation [s]
C     myIter      :: current Iteration number in simulation
C     myThid      :: my Thread Id number
      INTEGER siLo, siHi, sjLo, sjHi
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      LOGICAL dBugFlag
      LOGICAL useBulkForce
      LOGICAL useEXF
      _RL iceMask(siLo:siHi,sjLo:sjHi)
      _RL hIce   (siLo:siHi,sjLo:sjHi)
      _RL hSnow  (siLo:siHi,sjLo:sjHi)
      _RL tFrz   (siLo:siHi,sjLo:sjHi)
      _RL flxExSW(iMin:iMax,jMin:jMax,0:2)
      _RL flxSW  (siLo:siHi,sjLo:sjHi)
      _RL tSrf   (siLo:siHi,sjLo:sjHi)
      _RL qIc1   (siLo:siHi,sjLo:sjHi)
      _RL qIc2   (siLo:siHi,sjLo:sjHi)
      _RL tIc1   (siLo:siHi,sjLo:sjHi)
      _RL tIc2   (siLo:siHi,sjLo:sjHi)
c     _RL dTsrf  (siLo:siHi,sjLo:sjHi)
      _RL dTsrf  (iMin:iMax,jMin:jMax)
      _RL sHeat  (siLo:siHi,sjLo:sjHi)
      _RL flxCnB (siLo:siHi,sjLo:sjHi)
      _RL flxAtm (siLo:siHi,sjLo:sjHi)
      _RL evpAtm (siLo:siHi,sjLo:sjHi)
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C---  local copy of input/output argument list variables (see description above)
c     _RL flxExcSw(0:2)
      _RL Tf
      _RL hi
      _RL hs
      _RL netSW
      _RL Tsf
      _RL qicen(nlyr)
      _RL Tice (nlyr)
c     _RL sHeating
c     _RL flxCnB
      _RL dTsf
c     _RL flxAtm
c     _RL evpAtm

C     == Local Variables ==
      INTEGER i,j
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
      LOGICAL useBlkFlx

C-    define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"

 1010 FORMAT(A,I3,3F11.6)
 1020 FORMAT(A,1P4E14.6)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      useBlkFlx = useEXF .OR. useBulkForce

      dt  = thSIce_deltaT
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF ( iceMask(i,j).GT.0. _d 0) THEN
          hi      = hIce(i,j)
          hs      = hSnow(i,j)
          Tf      = tFrz(i,j)
          netSW   = flxSW(i,j)
          Tsf     = tSrf(i,j)
          qicen(1)= qIc1(i,j)
          qicen(2)= qIc2(i,j)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef ALLOW_DBUG_THSICE
          IF ( dBug(i,j,bi,bj) ) WRITE(6,'(A,2I4,2I2)')
     &         'ThSI_SOLVE4T: i,j=',i,j,bi,bj
#endif
      IF ( hi.LT.himin ) THEN
C If hi < himin, melt the ice.
         STOP 'THSICE_SOLVE4TEMP: should not enter if hi<himin'
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C fractional snow cover
      frsnow = 0. _d 0
      IF (hs .GT. 0. _d 0) frsnow = 1. _d 0

C Compute SW flux absorbed at surface and penetrating to layer 1.
      fswpen  = netSW * (1. _d 0 - frsnow) * i0
      fswocn = fswpen * exp(-ksolar*hi)
      fswint = fswpen - fswocn

      fswdn = netSW - fswpen

C Compute conductivity terms at layer interfaces.

      k12 = 4. _d 0*kice*ksnow / (ksnow*hi + 4. _d 0*kice*hs)
      k32 = 2. _d 0*kice  / hi

C compute ice temperatures
      a1 = cpice
      b1 = qicen(1) + (cpwater-cpice )*Tmlt1 - Lfresh
      c1 = Lfresh * Tmlt1
      Tice(1) = 0.5 _d 0 *(-b1 - SQRT(b1*b1-4. _d 0*a1*c1))/a1
      Tice(2) = (Lfresh-qicen(2)) / cpice

      IF (Tice(1).GT.0. _d 0 ) THEN 
       WRITE (standardMessageUnit,*) 'BBerr: Tice(1) > 0'
       WRITE (standardMessageUnit,'(A,4I5,2F11.4)')
     &      'BBerr: i,j,bi,bj,Tice = ',i,j,bi,bj,Tice
      ENDIF
      IF ( Tice(2).GT.0. _d 0) THEN
       WRITE (standardMessageUnit,*) 'BBerr: Tice(2) > 0'
       WRITE (standardMessageUnit,'(A,4I5,2F11.4)')
     &      'BBerr: i,j,bi,bj,Tice = ',i,j,bi,bj,Tice
      ENDIF
#ifdef ALLOW_DBUG_THSICE
      IF ( dBug(i,j,bi,bj) ) WRITE(6,1010)
     &  'ThSI_SOLVE4T: k, Ts, Tice=',0,Tsf,Tice
#endif

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
         IF ( useBulkForce ) THEN
          CALL THSICE_GET_BULKF(
     I                         iceornot, Tsf,
     O                         flxExceptSw, df0dT, evap, dEvdT,
     I                         i,j,bi,bj,myThid )
         ELSEIF ( useEXF ) THEN
          CALL THSICE_GET_EXF  (
     I                         iceornot, Tsf,
     O                         flxExceptSw, df0dT, evap, dEvdT,
     I                         i,j,bi,bj,myThid )
         ENDIF
        ELSE
         flxExceptSw = flxExSW(i,j,1)
         df0dT = flxExSW(i,j,2)
        ENDIF
        flx0 = fswdn + flxExceptSw
#ifdef ALLOW_DBUG_THSICE
      IF ( dBug(i,j,bi,bj) ) WRITE(6,1020)
     &  'ThSI_SOLVE4T: flx0,df0dT,k12,D=', flx0,df0dT,k12,k12-df0dT
#endif

C Compute new top layer and surface temperatures.
C If Tsfc is computed to be > 0 C, fix Tsfc = 0 and recompute T1
C with different coefficients.

         a1 = a10 - k12*df0dT / (k12-df0dT)
         b1 = b10 - k12*(flx0-df0dT*Tsf) / (k12-df0dT)
         Tice(1) = -(b1 + SQRT(b1*b1-4. _d 0*a1*c1))/(2. _d 0*a1)
         dTsf = (flx0 + k12*(Tice(1)-Tsf)) / (k12-df0dT)
         Tsf = Tsf + dTsf
         IF (Tsf .GT. 0. _d 0) THEN
#ifdef ALLOW_DBUG_THSICE
            IF ( dBug(i,j,bi,bj) ) WRITE(6,1010)
     &        'ThSI_SOLVE4T: k,ts,t1,dTs=', k,Tsf,Tice(1),dTsf
#endif
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
            IF ( useBulkForce ) THEN
             CALL THSICE_GET_BULKF(
     I                            iceornot, Tsf,
     O                            flxExceptSw, df0dT, evap, dEvdT,
     I                            i,j,bi,bj,myThid )
            ELSEIF ( useEXF ) THEN
             CALL THSICE_GET_EXF  (
     I                            iceornot, Tsf,
     O                            flxExceptSw, df0dT, evap, dEvdT,
     I                            i,j,bi,bj,myThid )
            ENDIF
            dTsf = 0. _d 0
           ELSE
            flxExceptSw = flxExSW(i,j,0)
            dTsf = 1000.
            df0dT = 0.
           ENDIF
           flx0 = fswdn + flxExceptSw
         ENDIF

C Check for convergence.  If no convergence, then repeat.
C
C Convergence test: Make sure Tsfc has converged, within prescribed error.
C (Energy conservation is guaranteed within machine roundoff, even
C if Tsfc has not converged.)
C If no convergence, then repeat.

#ifdef ALLOW_DBUG_THSICE
         IF ( dBug(i,j,bi,bj) ) WRITE(6,1010)
     &     'ThSI_SOLVE4T: k,ts,t1,dTs=', k,Tsf,Tice(1),dTsf
#endif
         IF ( useBlkFlx .AND. k.EQ.nitMaxTsf
     &                  .AND. ABS(dTsf).GE.Terrmax ) THEN
            WRITE (6,'(A,4I4,I12,F15.9)')
     &                 ' BB: thermw conv err: i,j,it,hi=',i,j,bi,bj,
     &                                                    myIter,hi
            WRITE (6,*) 'BB: thermw conv err: Tsf, dTsf=', Tsf,dTsf
            WRITE (6,*) 'BB: thermw conv err: flx0,dfdT=',flx0,df0dT
            IF (Tsf.LT.-70. _d 0) STOP
         ENDIF

       ENDIF
      ENDDO
C ------ end iteration ------------

C Compute new bottom layer temperature.

      Tice(2) = (2. _d 0*dt*k32*(Tice(1)+2. _d 0*Tf)
     &        + rhoi*cpice *hi*Tice(2))
     &         /(6. _d 0*dt*k32 + rhoi*cpice *hi)
#ifdef ALLOW_DBUG_THSICE
      IF ( dBug(i,j,bi,bj) ) WRITE(6,1010)
     &  'ThSI_SOLVE4T: k, Ts, Tice=',k,Tsf,Tice
#endif

C Compute final flux values at surfaces.

      fct    = k12*(Tsf-Tice(1))
      flxCnB(i,j) = 4. _d 0*kice *(Tice(2)-Tf)/hi
      flx0   = flx0 + df0dT*dTsf
      IF ( useBlkFlx ) THEN
C--   needs to update also Evap (Tsf changes) since Latent heat has been updated
        evpAtm(i,j) = evap + dEvdT*dTsf
      ELSE
C- WARNING: Evap & +Evap*Lfresh are missing ! (but only affects Diagnostics)
        evpAtm(i,j) = 0.
      ENDIF
C-    energy flux to Atmos: use net short-wave flux at surf. and
C     use latent heat = Lvap (energy=0 for liq. water at 0.oC)
      flxAtm(i,j) = netSW + flxExceptSw
     &                    + df0dT*dTsf + evpAtm(i,j)*Lfresh
C-    excess of energy @ surface (used for surface melting):
      sHeat(i,j) = flx0 - fct

C-    SW flux at sea-ice base left to the ocean
      flxSW(i,j) = fswocn

#ifdef ALLOW_DBUG_THSICE
      IF ( dBug(i,j,bi,bj) ) WRITE(6,1020)
     &  'ThSI_SOLVE4T: flx0,fct,Dif,flxCnB=',
     &                 flx0,fct,flx0-fct,flxCnB(i,j)
#endif

C Compute new enthalpy for each layer.

      qicen(1) = -cpwater*Tmlt1 + cpice *(Tmlt1-Tice(1))
     &            + Lfresh*(1. _d 0-Tmlt1/Tice(1))
      qicen(2) = -cpice *Tice(2) + Lfresh

C Make sure internal ice temperatures do not exceed Tmlt.
C (This should not happen for reasonable values of i0.)

      IF (Tice(1) .GE. Tmlt1) THEN
        WRITE (6,'(A,2I4,2I3,1P2E14.6)')
     &   'BBerr - Bug: IceT(1) > Tmlt',i,j,bi,bj,Tice(1),Tmlt1
      ENDIF
      IF (Tice(2) .GE. 0. _d 0) THEN
       WRITE (6,'(A,2I4,2I3,1P2E14.6)')
     &   'BBerr - Bug: IceT(2) > 0',i,j,bi,bj,Tice(2)
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--    Update Sea-Ice state :
          tSrf(i,j)  = Tsf
          tIc1(i,j)  = Tice(1)
          tic2(i,j)  = Tice(2)
          qIc1(i,j)  = qicen(1)
          qIc2(i,j)  = qicen(2)
c         dTsrf(i,j) = dTsf
          IF ( .NOT.useBlkFlx ) dTsrf(i,j) = dTsf
c         sHeat(i,j) = sHeating
c         flxCnB(i,j)= flxCnB
c         flxAtm(i,j)= flxAtm
c         evpAtm(i,j)= evpAtm
#ifdef ALLOW_DBUG_THSICE
          IF ( dBug(i,j,bi,bj) ) THEN
           WRITE(6,1020) 'ThSI_SOLV_4T: Tsf, Tice(1,2), dTsurf=',
     &                              Tsf, Tice, dTsf
           WRITE(6,1020) 'ThSI_SOLV_4T: sHeat, flxCndBt, Qice =',
     &           sHeat(i,j), flxCnB(i,j), qicen
           WRITE(6,1020) 'ThSI_SOLV_4T: flxA, evpA, fxSW_bf,af=',
     &           flxAtm(i,j), evpAtm(i,j), netSW, flxSW(i,j)
          ENDIF
#endif
        ELSE
          IF ( .NOT.useBlkFlx ) dTsrf(i,j) = 0. _d 0
        ENDIF
       ENDDO
      ENDDO
#endif  /* ALLOW_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
