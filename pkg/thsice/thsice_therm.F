C $Header: /u/gcmpack/MITgcm/pkg/thsice/Attic/thsice_therm.F,v 1.2 2003/12/31 17:44:32 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

      SUBROUTINE THSICE_THERM(
     I                     fSWabs, oceHs, oceV2s, oceSs, oceTs,
     U                     compact, hi, hs, Tsf, Tice, qicen,
     O                     qleft, fresh, fsalt, Fbot,
     O                     flxAtm, evpAtm,
     I                     i,j,bi,bj, myThid)
C     /==========================================================\
C     | S/R  THSICE_THERM
C     |==========================================================|
C     | Calculate thermodynamic changes to ice column            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "THSICE_SIZE.h"
c #include "THSICE.h"
#include "THSICE_PARAMS.h"

C     === Routine arguments ===
C     fSWabs  :: net Short-Wave flux (+=down) at surface (W/m^2)
C     oceHs   :: thickness  of the ocean surface level [m]
C     oceV2s  :: square of ocean surface-level velocity [m^2/s^2]
C     oceSs   :: surface level oceanic salinity [g/kg]
C     oceTs   :: surface level oceanic temperature [oC]
C     compact :: fraction of grid area covered in ice
C     hi      :: ice height
C     hs      :: snow height
C     Tsf     :: surface (ice or snow) temperature
C     Tice    :: internal ice temperatures
C     qicen   :: ice enthalpy (J m-3)
C     qleft   :: net heat flux to ocean    (W/m^2)          (> 0 downward)
C     fresh   :: Total fresh water flux to ocean (kg/m^2/s) (> 0 downward)
C     fsalt   :: salt flux to ocean        ( g/m^2/s)       (> 0 downward)
C     Fbot    :: oceanic heat flux used to melt/form ice
C     flxAtm  :: net flux of energy from the atmosphere (W/m^2)(+ = down)
C                without snow precip. (energy=0 for liquid water at 0.oC)
C     evpAtm  :: evaporation to the atmosphere (m/s) (>0 if evaporate)
C   i,j,bi,bj :: indices of current grid point
C     myThid  :: Thread no. that called this routine.
      _RL  fSWabs, oceHs, oceV2s, oceSs, oceTs
      _RL compact
      _RL  hi
      _RL  hs
      _RL  Tsf
      _RL  Tice (nlyr)
      _RL  qicen(nlyr)

      _RL qleft
      _RL fresh
      _RL fsalt
      _RL  Fbot
      _RL flxAtm
      _RL evpAtm
      INTEGER i,j, bi,bj
      INTEGER myThid

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

#ifdef ALLOW_THSICE


C Local variables

      INTEGER  k

      _RL  rnlyr         ! maximum number of ice layers (real value)

      _RL  frsnow        ! fractional snow cover

      _RL  fswpen        ! SW penetrating beneath surface (W m-2)
      _RL  fswdn         ! SW absorbed at surface (W m-2)
      _RL  fswint        ! SW absorbed in ice (W m-2)
      _RL  fswocn        ! SW passed through ice to ocean (W m-2)

      _RL  flxExceptSw   ! net surface heat flux, except short-wave (W/m2)
      _RL  flx0          ! net surf heat flux, from Atmos. to sea-ice (W m-2)
C     evap           ::  evaporation over snow/ice [m/s] (>0 if evaporate)
      _RL  evap

      _RL  df0dT         ! deriv of flx0 wrt Tsf (W m-2 deg-1)

      _RL  fct           ! heat conducted to top surface
      _RL  fcb           ! heat conducted to bottom surface
      _RL  frzmlt        ! freezing/melting potential

      _RL  k12, k32      ! thermal conductivity terms
      _RL  a10, b10      ! coefficients in quadratic eqn for T1
      _RL  a1, b1, c1    ! coefficients in quadratic eqn for T1
      _RL  dTsf          ! change in Tsf
      _RL  Tsf_start     ! old value of Tsf
      _RL  Tf,Tfk        ! freezing temperature (C)
 
      _RL  etop          ! energy for top melting    (J m-2)
      _RL  ebot          ! energy for bottom melting (J m-2)
      _RL  etope         ! energy (from top)    for lateral melting (J m-2)
      _RL  ebote         ! energy (from bottom) for lateral melting (J m-2)
      _RL  extend        ! total energy for lateral melting (J m-2)
      _RL  hnew(nlyr)    ! new ice layer thickness (m)
      _RL  hlyr          ! individual ice layer thickness (m)
      _RL  dhi           ! change in ice thickness
      _RL  dhs           ! change in snow thickness
      _RL  rq            ! rho * q for a layer
      _RL  rqh           ! rho * q * h for a layer
      _RL  qbot          ! q for new ice at bottom surf (J m-3)
      _RL  dt            ! timestep
      _RL  esurp         ! surplus energy from melting (J m-2)
      _RL  mwater0       ! fresh water mass gained/lost (kg/m^2)
      _RL  msalt0        ! salt gained/lost  (kg/m^2)
      _RL  freshe        ! fresh water gain from extension melting
      _RL  salte         ! salt gained from extension melting

      _RL  ustar, cphm, cpchr

      _RL  chi, chs
      _RL  frace, rs, hq
      INTEGER iceornot
      LOGICAL dBug

      rnlyr = nlyr
      dt  = thSIce_deltaT
      dBug = .FALSE.
c     dBug = ( bi.EQ.3 .AND. i.EQ.13 .AND. j.EQ.13 )
c     dBug = ( bi.EQ.6 .AND. i.EQ.18 .AND. j.EQ.10 )
      IF ( dBug ) WRITE(6,'(A,2I4,2I2)') 'ThSI_THERM: i,j=',i,j,bi,bj

      esurp = 0. _d 0   ! initialize energies

C.. specific melting temperature for grid cell
      Tf = -mu_Tf*oceSs
      cphm = cpwater*rhosw*oceHs
      frzmlt=(Tf-oceTs)*cphm/dt

      if ( hi.lt.himin ) then
C If hi < himin, melt the ice.
         STOP 'THSICE_THERM: should not enter if hi<himin'
      endif
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      if (frzmlt.ge. 0. _d 0) then
C     !-----------------------------------------------------------------
C     ! freezing conditions
C     !-----------------------------------------------------------------
C if higher than hihig, use all frzmlt energy to grow extra ice
        if (hi.gt.hihig.and. compact.le.iceMaskmax) then
          Fbot=0. _d 0
        else
          Fbot=frzmlt
        endif
      else
C     !-----------------------------------------------------------------
C     ! melting conditions
C     !-----------------------------------------------------------------
         ustar = 5. _d -2        !for no currents
C frictional velocity between ice and water
         ustar = sqrt(0.00536 _d 0*oceV2s)
         ustar=max(5. _d -3,ustar)
         cpchr =cpwater*rhosw*transcoef 
         Fbot = cpchr*(Tf-oceTs)*ustar  ! < 0
         Fbot = max(Fbot,frzmlt)    ! frzmlt < Fbot < 0
         Fbot = min(Fbot,0. _d 0)
      endif

C  mass of fresh water and salt initially present in ice
      mwater0 = rhos*hs + rhoi*hi
      msalt0  = rhoi*hi*saltice 

C fractional snow cover
      frsnow = 0. _d 0
      if (hs .gt. 0. _d 0) frsnow = 1. _d 0

C Compute SW flux absorbed at surface and penetrating to layer 1.

      fswpen  = fSWabs * (1. _d 0 - frsnow) * i0
      fswocn = fswpen * exp(-ksolar*hi)
      fswint = fswpen - fswocn

      fswdn = fSWabs - fswpen

C Compute conductivity terms at layer interfaces.

      k12 = 4. _d 0*kice*ksnow / (ksnow*hi + 4. _d 0*kice*hs)
      k32 = 2. _d 0*kice  / hi

C compute ice temperatures
      a1 = cpice
      b1 = qicen(1) + (cpwater-cpice )*Tmlt1 - Lfresh
      c1 = Lfresh * Tmlt1
      Tice(1) = 0.5 _d 0 *(-b1 - sqrt(b1*b1-4. _d 0*a1*c1))/a1
      Tice(2) = (Lfresh-qicen(2)) / cpice

      if (Tice(1).gt.0. _d 0 .or. Tice(2).gt.0. _d 0) then
          write (6,*) 'BBerr Tice(1) > 0 = ',Tice(1)
          write (6,*) 'BBerr Tice(2) > 0 = ',Tice(2)
      endif
      IF ( dBug ) WRITE(6,1010) 'ThSI_THERM: k,Tice=',0,Tsf,Tice
 1010 FORMAT(A,I3,3F8.3)
 1020 FORMAT(A,1P4E11.3)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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

C Compute new surface and internal temperatures; iterate until
C Tsfc converges.

C ----- begin iteration  -----
      do 100 k = 1,nitMaxTsf

C Save temperatures at start of iteration.
         Tsf_start = Tsf

C Compute top surface flux.
         if (hs.gt.3. _d -1) then
              iceornot=2
         else
              iceornot=1
         endif
         CALL THSICE_GET_BULKF(
     I                        iceornot, Tsf,
     O                        flxExceptSw, df0dT, evap,
     I                        i,j,bi,bj,myThid )
         flx0 = fswdn + flxExceptSw

C Compute new top layer and surface temperatures.
C If Tsfc is computed to be > 0 C, fix Tsfc = 0 and recompute T1
C with different coefficients. 

         a1 = a10 - k12*df0dT / (k12-df0dT)
         b1 = b10 - k12*(flx0-df0dT*Tsf) / (k12-df0dT)
         Tice(1) = -(b1 + sqrt(b1*b1-4. _d 0*a1*c1))/(2. _d 0*a1)
         dTsf = (flx0 + k12*(Tice(1)-Tsf)) / (k12-df0dT)
         Tsf = Tsf + dTsf
         if (Tsf .gt. 0. _d 0) then
            IF(dBug) WRITE(6,1010) 'ThSI_THERM: k,tice=',
     &                                          k,Tsf,Tice(1),dTsf
            a1 = a10 + k12
            b1 = b10          ! note b1 = b10 - k12*Tf0
            Tice(1) = (-b1 - sqrt(b1*b1-4. _d 0*a1*c1))/(2. _d 0*a1)
            Tsf = 0. _d 0
            if (hs.gt.3. _d -1) then
                 iceornot=2
            else
                 iceornot=1
            endif
            CALL THSICE_GET_BULKF(
     I                        iceornot, Tsf,
     O                        flxExceptSw, df0dT, evap,
     I                        i,j,bi,bj,myThid )
            flx0 = fswdn + flxExceptSw
            dTsf = 0. _d 0
            Tsf  = 0. _d 0
         endif

C Check for convergence.  If no convergence, then repeat.
C
C Convergence test: Make sure Tsfc has converged, within prescribed error.  
C (Energy conservation is guaranteed within machine roundoff, even
C if Tsfc has not converged.)
C If no convergence, then repeat.

         IF ( dBug ) WRITE(6,1010) 'ThSI_THERM: k,tice=',
     &                                            k,Tsf,Tice(1),dTsf
         if (abs(dTsf).lt.Terrmax) then
            goto 150
         elseif (k.eq.nitMaxTsf) then
            write (6,*) 'BB: thermw conv err ',i,j,bi,bj,dTsf
            write (6,*) 'BB: thermw conv err, iceheight ', hi
            write (6,*) 'BB: thermw conv err: Tsf, flx0', Tsf,flx0
            if (Tsf.lt.-70. _d 0) stop  !QQQQ fails
         endif

100   continue  ! surface temperature iteration
150   continue
C ------ end iteration ------------

c        if (Tsf.lt.-70. _d 0) then
cQQ        print*,'QQQQQ Tsf =',Tsf
cQQ        stop  !QQQQ fails
c        endif

C Compute new bottom layer temperature.

      Tice(2) = (2. _d 0*dt*k32*(Tice(1)+2. _d 0*Tf)
     &        + rhoi*cpice *hi*Tice(2))
     &         /(6. _d 0*dt*k32 + rhoi*cpice *hi)
      IF ( dBug ) WRITE(6,1010) 'ThSI_THERM: k,Tice=',k,Tsf,Tice


C Compute final flux values at surfaces.

      fct = k12*(Tsf-Tice(1))
      fcb = 4. _d 0*kice *(Tice(2)-Tf)/hi
      flx0 = flx0 + df0dT*dTsf
      evpAtm = evap
C-    energy flux to Atmos: use net short-wave flux at surf. and
C     use latent heat = Lvap (energy=0 for liq. water at 0.oC)
      flxAtm = fSWabs + flxExceptSw + df0dT*dTsf
     &                + evap*rhofw*Lfresh
c     if (Tsf.lt.0. _d 0) flx0 = flx0 + df0dT*dTsf

      IF (dBug) WRITE(6,1020) 'ThSI_THERM: evpAtm,frzmlt,Fbot=',
     &     evpAtm,frzmlt,Fbot
      IF (dBug) WRITE(6,1020) 'ThSI_THERM: flx0,fct,Dif,fcb=',
     &    flx0,fct,flx0-fct,fcb

C Compute new enthalpy for each layer.

      qicen(1) = -cpwater*Tmlt1 + cpice *(Tmlt1-Tice(1)) + 
     &              Lfresh*(1. _d 0-Tmlt1/Tice(1))
      qicen(2) = -cpice *Tice(2) + Lfresh

C Make sure internal ice temperatures do not exceed Tmlt.
C (This should not happen for reasonable values of i0.)

      if (Tice(1) .ge. Tmlt1) then 
        write (6,'(A,2I4,2I3,1P2E14.6)')
     &   'BBerr - Bug: IceT(1) > Tmlt',i,j,bi,bj,Tice(1),Tmlt1
      endif
      if (Tice(2) .ge. 0. _d 0) then
       write (6,'(A,2I4,2I3,1P2E14.6)')
     &   'BBerr - Bug: IceT(2) > 0',i,j,bi,bj,Tice(2)
      endif

C......................................................................
C.. Compute growth and/or melting at the top and bottom surfaces.......
C......................................................................

C Compute energy available for melting/growth.

      if (hi.lt.himin0) then
C below a certain height, all energy goes to changing ice extent
       frace=1. _d 0
      else
       frace=frac_energy
      endif
      if (hi.gt.hihig) then
C above certain height only melt from top
       frace=0. _d 0
      else
       frace=frac_energy
      endif
C force this when no ice fractionation
      if (frac_energy.eq.0. _d 0) frace=0. _d 0

      if (Tsf .eq. 0. _d 0 .and. flx0.gt.fct) then
          etop = (1. _d 0-frace)*(flx0 - fct) * dt
          etope = frace*(flx0 - fct) * dt
      else
          etop =  0. _d 0
          etope = 0. _d 0
C jmc: found few cases where Tsf=0 & flx0 < fct : add this line to conserv energy:
          esurp = (flx0 - fct) * dt
      endif
C--   flux at the base of sea-ice: 
C     conduction H.flx= fcb (+ =down); oceanic turbulent H.flx= Fbot (+ =down).
C-    ==> energy available(+ => melt)= (fcb-Fbot)*dt
c     if (frzmlt.lt.0. _d 0) then
c         ebot = (1. _d 0-frace)*(fcb-Fbot) * dt
c         ebote = frace*(fcb-Fbot) * dt
c     else
c         ebot = (fcb-Fbot) * dt
c         ebote = 0. _d 0
c     endif
C- original formulation(above): Loose energy when fcb < Fbot < 0
      ebot = (fcb-Fbot) * dt
      if (ebot.gt.0. _d 0) then
         ebote = frace*ebot
         ebot  = ebot-ebote
      else
         ebote = 0. _d 0
      endif
      IF (dBug) WRITE(6,1020) 'ThSI_THERM: etop,etope,ebot,ebote=',
     &            etop,etope,ebot,ebote

C Initialize layer thicknesses.
C Make sure internal ice temperatures do not exceed Tmlt.
C If they do, then eliminate the layer.  (Dont think this will happen
C for reasonable values of i0.)

      hlyr = hi / rnlyr
      do k = 1, nlyr
         hnew(k) = hlyr
      enddo

C Top melt: snow, then ice.

      if (etop .gt. 0. _d 0) then
         if (hs. gt. 0. _d 0) then
            rq =  rhos * qsnow
            rqh = rq * hs
            if (etop .lt. rqh) then
               hs = hs - etop/rq
               etop = 0. _d 0
            else
               etop = etop - rqh 
               hs = 0. _d 0
            endif
         endif
                  
         do k = 1, nlyr
            if (etop .gt. 0. _d 0) then
               rq =  rhoi * qicen(k)
               rqh = rq * hnew(k)
               if (etop .lt. rqh) then
                  hnew(k) = hnew(k) - etop / rq
                  etop = 0. _d 0
               else
                  etop = etop - rqh
                  hnew(k) = 0. _d 0
               endif
            endif
         enddo
      else
        etop=0. _d 0
      endif
C If ice is gone and melting energy remains
c     if (etop .gt. 0. _d 0) then
c        write (6,*)  'QQ All ice melts from top  ', i,j
c        hi=0. _d 0
c        go to 200
c     endif


C Bottom melt/growth. 

      if (ebot .lt. 0. _d 0) then
C Compute enthalpy of new ice growing at bottom surface.
         qbot =  -cpice *Tf + Lfresh
         dhi = -ebot / (qbot * rhoi)
         ebot = 0. _d 0
         k = nlyr
         qicen(k) = (hnew(k)*qicen(k)+dhi*qbot) / (hnew(k)+dhi)
         hnew(k) = hnew(k) + dhi
      else               
         do k = nlyr, 1, -1
            if (ebot.gt.0. _d 0 .and. hnew(k).gt.0. _d 0) then
               rq =  rhoi * qicen(k)
               rqh = rq * hnew(k)
               if (ebot .lt. rqh) then
                  hnew(k) = hnew(k) - ebot / rq
                  ebot = 0. _d 0
               else
                  ebot = ebot - rqh
                  hnew(k) = 0. _d 0
               endif
            endif
         enddo

C If ice melts completely and snow is left, remove the snow with 
C energy from the mixed layer

         if (ebot.gt.0. _d 0 .and. hs.gt.0. _d 0) then
            rq =  rhos * qsnow
            rqh = rq * hs
            if (ebot .lt. rqh) then
               hs = hs - ebot / rq
               ebot = 0. _d 0
            else
               ebot = ebot - rqh
               hs = 0. _d 0
            endif
         endif
c        if (ebot .gt. 0. _d 0) then
c           IF (dBug) WRITE(6,*) 'All ice (& snow) melts from bottom'
c           hi=0. _d 0
c           go to 200
c        endif
      endif

C Compute new total ice thickness.
      hi = 0. _d 0
      do k = 1, nlyr
         hi = hi + hnew(k)
      enddo
      IF (dBug) WRITE(6,1020) 'ThSI_THERM: top,bot: etop,ebot,hs,hi=',
     &    etop,ebot,hs,hi    

C If hi < himin, melt the ice. 
      if ( hi.lt.himin .AND. (hi+hs).gt.0. _d 0 ) then
         esurp = esurp - rhos*qsnow*hs
         do k = 1, nlyr
            esurp = esurp - rhoi*qicen(k)*hnew(k)
         enddo
         hi = 0. _d 0
         hs = 0. _d 0
         Tsf=0. _d 0
         compact = 0. _d 0
         do k = 1, nlyr
           qicen(k) = 0. _d 0
         enddo
         IF (dBug) WRITE(6,1020) 'ThSI_THERM: -1 : esurp=',esurp
      endif

C--   do a mass-budget of sea-ice to compute "fresh" = the fresh-water flux
C     that is returned to the ocean ; needs to be done before snow/evap
      fresh = (mwater0 - (rhos*hs + rhoi*hi))/dt

      IF ( hi .le. 0. _d 0 ) GOTO 200

C Let it snow

        CALL THSICE_GET_SNOW( 
     I                     Tsf, hi,
     U                     hs,
     O                     frsnow,
     I                     i,j,bi,bj,myThid )

C If ice evap is used to sublimate surface snow/ice or
C if no ice pass on to ocean
      if (hs.gt.0. _d 0) then
        if (evap*rhofw/rhos *dt.gt.hs) then
           evap=evap-hs*rhos/rhofw/dt
           hs=0. _d 0
        else
           hs = hs - evap*rhofw/rhos *dt
           evap=0. _d 0
        endif
      endif
      if (hi.gt.0. _d 0.and.evap.gt.0. _d 0) then
         do k = 1, nlyr
            if (evap .gt. 0. _d 0) then
C-- original scheme, does not care about ice temp.
C-  this can produce small error (< 1.W/m2) in the Energy budget
c              if (evap*rhofw/rhoi *dt.gt.hnew(k)) then
c                evap=evap-hnew(k)*rhoi/rhofw/dt
c                hnew(k)=0. _d 0
c              else
c                hnew(k) = hnew(k) - evap*rhofw/rhoi *dt
c                evap=0. _d 0
c              endif
C-- modified scheme. taking into account Ice enthalpy
               dhi = evap*rhofw/rhoi*dt 
               if (dhi.ge.hnew(k)) then
                 evap=evap-hnew(k)*rhoi/rhofw/dt
                 esurp = esurp - hnew(k)*rhoi*(qicen(k)-Lfresh)
                 hnew(k)=0. _d 0
               else
                 hq = hnew(k)*qicen(k)-dhi*Lfresh
                 hnew(k) = hnew(k) - dhi
                 qicen(k)=hq/hnew(k)
                 evap=0. _d 0
               endif
C-------
            endif
         enddo
      endif
c     if (evap .gt. 0. _d 0) then
c           write (6,*)  'BB All ice sublimates', i,j
c           hi=0. _d 0
c           go to 200
c     endif

C Compute new total ice thickness.

      hi = 0. _d 0
      do k = 1, nlyr
         hi = hi + hnew(k)
      enddo

C If hi < himin, melt the ice. 
      if ( hi.gt.0. _d 0 .AND. hi.lt.himin ) then
         fresh = fresh + (rhos*hs + rhoi*hi)/dt
         esurp = esurp - rhos*qsnow*hs
         do k = 1, nlyr
            esurp = esurp - rhoi*qicen(k)*hnew(k)
         enddo
         hi = 0. _d 0
         hs = 0. _d 0
         Tsf=0. _d 0
         compact = 0. _d 0
         do k = 1, nlyr
           qicen(k) = 0. _d 0
         enddo
         IF (dBug) WRITE(6,1020) 'ThSI_THERM: -2 : esurp,fresh=',
     &                   esurp, fresh
      endif
      IF ( hi .le. 0. _d 0 ) GOTO 200

C If there is enough snow to lower the ice/snow interface below 
C freeboard, convert enough snow to ice to bring the interface back 
C to sea-level.  Adjust enthalpy of top ice layer accordingly.

      if ( hs .gt. hi*rhoiw/rhos ) then
cBB               write (6,*)  'Freeboard adjusts'
         dhi = (hs * rhos - hi * rhoiw) / rhosw
         dhs = dhi * rhoi / rhos
         rqh = rhoi*qicen(1)*hnew(1) + rhos*qsnow*dhs
         hnew(1) = hnew(1) + dhi
         qicen(1) = rqh / (rhoi*hnew(1))
         hi = hi + dhi
         hs = hs - dhs
      end if


C limit ice height
C- NOTE: this part does not conserve Energy ;
C        but surplus of fresh water and salt are taken into account.
      if (hi.gt.hiMax) then
cBB      print*,'BBerr, hi>hiMax',i,j,hi
         chi=hi-hiMax
         do k=1,nlyr
            hnew(k)=hnew(k)-chi/2. _d 0
         enddo
         fresh = fresh + chi*rhoi/dt
      endif
      if (hs.gt.hsMax) then
         print*,'BBerr, hs>hsMax',i,j,hs
         chs=hs-hsMax
         hs=hsMax
         fresh = fresh + chs*rhos/dt
      endif

C Compute new total ice thickness.

      hi = 0. _d 0
      do k = 1, nlyr
         hi = hi + hnew(k)
      enddo
 
      IF (dBug) WRITE(6,1020) 'ThSI_THERM: b-Winton: hnew,qice=',
     &    hnew,qicen

       hlyr = hi/rnlyr
       call new_layers_winton(hs,hi,hlyr,hnew,qicen)

      IF (dBug) WRITE(6,1020) 'ThSI_THERM: compact,hi,qtot,hs=',
     &                  compact,hi,(qicen(1)+qicen(2))*0.5, hs

200   continue

C-  Compute surplus energy left over from melting.

      if (hi.le.0. _d 0) compact=0. _d 0

C.. heat fluxes left over for ocean
       qleft = fswocn
       qleft = qleft + (Fbot+(esurp+etop+ebot)/dt)
       IF (dBug) WRITE(6,1020)'ThSI_THERM: fswOc,[esurp,etop+ebot]/dt='
     &                        ,fswocn,esurp/dt,etop/dt,ebot/dt

C-- Evaporation left to the ocean :
       fresh = fresh - evap*rhofw
C-  Correct Atmos. fluxes for this different latent heat:
C   evap was computed over freezing surf.(Tsf<0), latent heat = Lvap+Lfresh
C   but should be Lvap only for the fraction "evap" that is left to the ocean. 
       qleft = qleft + evap*rhofw*Lfresh

C fresh and salt fluxes
c     fresh = (mwater0 - (rhos*(hs) + rhoi*(hi)))/dt-evap*rhofw
c     fsalt = (msalt0 - rhoi*hi*saltice)/35. _d 0/dt  ! for same units as fresh
C note (jmc): fresh is computed from a sea-ice mass budget that already 
C    contains, at this point, snow & evaporation (of snow & ice)
C    but are not meant to be part of ice/ocean fresh-water flux.
C  fix: a) like below or b) by making the budget before snow/evap is added
c     fresh = (mwater0 - (rhos*(hs) + rhoi*(hi)))/dt
c    &      + snow(i,j,bi,bj)*rhos - evpAtm*rhofw
      fsalt = (msalt0 - rhoi*hi*saltice)/dt

      IF (dBug) WRITE(6,1020)'ThSI_THERM: dH2O,Evap[kg],fresh,fsalt',
     &   (mwater0-(rhos*hs+rhoi*hi))/dt,evap*rhofw,fresh,fsalt
      IF (dBug) WRITE(6,1020)'ThSI_THERM: Qleft,Fbot,extend/dt=',
     &            Qleft,Fbot,(etope+ebote)/dt

C-- note: at this point, compact has not been changed (unless reset to zero)
C      and it can only be reduced by lateral melting in the following part:

C calculate extent changes
      extend=etope+ebote
      if (compact.gt.0. _d 0.and.extend.gt.0. _d 0) then
         rq =  rhoi * 0.5 _d 0*(qicen(1)+qicen(2))
         rs =  rhos * qsnow
         rqh = rq * hi + rs * hs
         freshe=(rhos*hs+rhoi*hi)/dt
         salte=(rhoi*hi*saltice)/dt
         if (extend .lt. rqh) then
           compact=(1. _d 0-extend/rqh)*compact
           fresh=fresh+extend/rqh*freshe
           fsalt=fsalt+extend/rqh*salte
         else
           compact=0. _d 0
           hi=0. _d 0 
           hs=0. _d 0 
           qleft=qleft+(extend-rqh)/dt
           fresh=fresh+freshe
           fsalt=fsalt+salte
         endif
      elseif (extend.gt.0. _d 0) then
         qleft=qleft+extend/dt
      endif
 
#endif  /* ALLOW_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END       ! main thermo routine
C --------------------------------------------------------------------

CStartofinterface
      SUBROUTINE NEW_LAYERS_WINTON(hs,hi,hlyr,hnew,qicen)

C Repartition into equal-thickness layers, conserving energy.
C This is the 2-layer version.

      IMPLICIT NONE
C     == Global data ==
#include "THSICE_PARAMS.h"

      _RL  hnew(*)    ! new ice layer thickness (m)
      _RL  hs 
      _RL  hi
      _RL  hlyr          ! individual ice layer thickness (m)
      _RL  qicen(*)   ! ice enthalpy (J m-3)

#ifdef ALLOW_THSICE
C Local variables
      _RL  f1            ! Fraction of upper layer ice in new layer
      _RL  qh1, qh2      ! qice*h for layers 1 and 2
      _RL  qhtot         ! qh1 + qh2
      _RL  q2tmp         ! Temporary value of qice for layer 2

      if (hnew(1).gt.hnew(2)) then  ! Layer 1 gives ice to layer 2
         f1 = (hnew(1)-hlyr)/hlyr
         q2tmp = f1*qicen(1) + (1. _d 0-f1)*qicen(2)
         if (q2tmp.gt.Lfresh) then
            qicen(2) = q2tmp
         else            ! Keep q2 fixed to avoid q2<Lfresh and T2>0
            qh2 = hlyr*qicen(2)
            qhtot = hnew(1)*qicen(1) + hnew(2)*qicen(2)
            qh1 = qhtot - qh2
            qicen(1) = qh1/hlyr
         endif
      else               ! Layer 2 gives ice to layer 1
         f1 = hnew(1)/hlyr
         qicen(1) = f1*qicen(1) + (1. _d 0-f1)*qicen(2)
      endif

#endif  /* ALLOW_THSICE */

      RETURN
      END
