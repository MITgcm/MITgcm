cswdice
c
#include "CPP_OPTIONS.h"

      SUBROUTINE ICE_THERM(
     I                     i,j,bi,bj,qleft,fsalt,fresh,compact,myThid)
C     /==========================================================\
C     | S/R  ICE_MODEL                                           |
C     |==========================================================|
C     | Calculate thermodynamic changes to ice column            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_THERM_SEAICE
#include "ICE.h"
#include "BULKF_ICE_CONSTANTS.h"
#endif
#ifdef ALLOW_BULK_FORCE
#include "BULKF.h"
#endif

C     === Routine arguments ===
C     myThid :: Thread no. that called this routine.
      INTEGER myThid
      INTEGER i,j, bi,bj
      _RL qleft   ! net heat flux to ocean    (W/m^2)
      _RL fsalt   ! salt flux to ocean        (kg/m^2/s)
      _RL fresh   ! fresh water flux to ocean (kg/m^2/s)
      _RL compact ! fraction of grid area covered in ice

c ADAPTED FROM:
c LANL CICE.v2.0.2
c-----------------------------------------------------------------------
c.. thermodynamics (vertical physics) based on M. Winton 3-layer model
c.. See Bitz, C. M. and W. H. Lipscomb, 1999:  "An energy-conserving 
c..       thermodynamic sea ice model for climate study."  J. Geophys. 
c..       Res., 104, 15669 - 15677.
c..     Winton, M., 1999:  "A reformulated three-layer sea ice model."  
c..       Submitted to J. Atmos. Ocean. Technol.  

c.. authors Elizabeth C. Hunke and William Lipscomb
c..         Fluid Dynamics Group, Los Alamos National Laboratory
c-----------------------------------------------------------------------
cc****subroutine thermo_winton(n,fice,fsnow,dqice,dTsfc)
c.. Compute temperature change using Winton model with 2 ice layers, of
c.. which only the top layer has a variable heat capacity.

#ifdef ALLOW_THERM_SEAICE

c
c Local variables
c
      integer  k, nitmax
      parameter(nitmax=20)

      _RL  area          ! fraction of grid covered by ice (either 0 or 1)
      _RL  hi            ! ice height
      _RL  hs            ! snow height
      _RL  frsnow        ! fractional snow cover

      _RL  fswabs        ! net SW down at surface (W m-2)
      _RL  fswpen        ! SW penetrating beneath surface (W m-2)
      _RL  fswdn         ! SW absorbed at surface (W m-2)
      _RL  fswint        ! SW absorbed in ice (W m-2)
      _RL  fswocn        ! SW passed through ice to ocean (W m-2)
      _RL  albedo        ! surface albedo

      _RL  flwup         ! upward LW at surface (W m-2)
      _RL  fsh           ! surface downward sensible heat (W m-2)
      _RL  flh           ! surface downward latent heat (W m-2)
      _RL  flx0          ! net surf flux, w/out conduction (W m-2)

      _RL  df0dT         ! deriv of flx0 wrt Tsf (W m-2 deg-1)

      _RL  fct           ! heat conducted to top surface
      _RL  fcb           ! heat conducted to bottom surface
      _RL  Fbot          ! oceanic heat flux
      _RL  frzmlt        ! freezing/melting potential

      _RL  Tice(nlyr)    ! internal ice temperatures

      _RL  k12, k32      ! thermal conductivity terms
      _RL  a10, b10      ! coefficients in quadratic eqn for T1
      _RL  a1, b1, c1    ! coefficients in quadratic eqn for T1
      _RL  Tsf           ! surface (ice or snow) temperature
      _RL  dTsf          ! change in Tsf
      _RL  Tsf_start     ! old value of Tsf
      _RL  Tf            ! freezing temperature (C)
 
      _RL  etop          ! energy for top melting (J m-2)
      _RL  ebot          ! energy for bottom melting (J m-2)
      _RL  hnew(nlyr)    ! new ice layer thickness (m)
      _RL  hlyr          ! individual ice layer thickness (m)
      _RL  dhi           ! change in ice thickness
      _RL  dhs           ! change in snow thickness
      _RL  rq            ! rho * q for a layer
      _RL  rqh           ! rho * q * h for a layer
      _RL  qbot          ! q for new ice at bottom surf (J m-3)
      _RL  dt            ! timestep
      _RL  esurp         ! surplus energy from melting (J m-2)
      _RL  qicen (nlyr)  ! ice enthalpy (J m-3)
      _RL  mwater0       ! fresh water mass gained/lost (kg/m^2)
      _RL  msalt0        ! salt gained/lost  (kg/m^2)

      _RL  ustar, cphm, cpchr
      _RL  ust, vst, evap, ssq

      _RL  hilim, hslim, chi, chs
      integer iceornot

      _RL test

      dt  = deltaTtracer
      hilim = 5. _d 0
      hslim = 1.5 _d 0

      hi = 0.      ! initialize ice state
      hs = 0.
      Tsf = 0.
      do k=1,nlyr
        qicen(k) = 0.
        Tice(k)  = 0.
      enddo
      flh = 0.     ! initialize fluxes
      fsh = 0.
      Fbot = 0.d0
      flwup = 0.
      fswocn = 0.
      esurp = 0.   ! initialize energies
      etop = 0.
      ebot = 0.
      mwater0 = 0. ! initialize water, salt mass
      msalt0 = 0.
      frsnow = 0.
      chi = 0.
      chs = 0.
      flx0 = 0.
      df0dT = 0.
      evap = 0.
      ssq = 0.
      ust = 0.



cBB debugging
cBB   print*,'    '
cBB   print*,'Q *** START *** i=',i,' j=',j
cBB   print*,'Q1 ti1,ti2,tsf,Ta',
cBB  &         Tice1(i,j,bi,bj), Tice2(i,j,bi,bj), Tsrf(i,j,bi,bj),
cBB  &         Tair(i,j,bi,bj)
cBB   print*,'Q qice1, qice2',qice1(i,j,bi,bj), qice2(i,j,bi,bj)
cBB   print*,'Q1 hi, hs, rain',iceHeight(i,j,bi,bj),
cBB  &         snowHeight(i,j,bi,bj), rain(i,j,bi,bj)
cBB   print*,'Q1 theta, salt', theta(i,j,1,bi,bj), 
cBB  &         salt(i,j,1,bi,bj)

c.. specific melting temperature for grid cell
      Tf = -mu_Tf*salt(i,j,1,bi,bj)    ! melt temperaure

cQQ from V3.0
      cphm = cpwater*rhosw*delz(1)
      frzmlt = (Tf-theta(i,j,1,bi,bj))*cphm/dt
cQQ from v2
cQQ   frzmlt = (Tf-theta(i,j,1,bi,bj))*cpwater*30.d0/dt
cQQQQQQQQQQQQQQQQQQQQQQQQQQQQ reduced to 1000 re v3QQQQQQQQQQQQQQQQQQQQ
cQQ   frzmlt = min(max(frzmlt,-1.d4),1.d4)
cBB debugging
cBB   print*,'frzmlt, Tf',frzmlt, Tf

      if (iceheight(i,j,bi,bj).ge.himin) then

         if (frzmlt.ge. 0.) then
      !-----------------------------------------------------------------
      ! freezing conditions
      !-----------------------------------------------------------------
            Fbot = 0.d0
         else
      !-----------------------------------------------------------------
      ! melting conditions
      !-----------------------------------------------------------------
cSSSSSSSS try get this done
cQQQQ what is this
            ustar = 5.e-2        !for no currents
c frictional velocity between ice and water
            ustar = sqrt(0.00536*(uvel(i,j,1,bi,bj)**2 +
     &                            vvel(i,j,1,bi,bj)**2) )
            ustar=max(5e-3,ustar)
            cpchr =cpwater*rhosw*transcoef 
            Fbot = cpchr*(Tf-theta(i,j,1,bi,bj))*ustar  ! < 0
            Fbot = max(Fbot,frzmlt)    ! frzmlt < Fbot < 0
            Fbot = min(Fbot,0.d0)
cBB debugging
cBB         print*,'Fbot,frzmlt',Fbot, frzmlt
!!! use all frzmlt for standalone runs
cQQQQ used in this case ??
cQQQQ       Fbot = min(0.d0,frzmlt)    ! frzmlt = Fbot < 0
         endif

        hi = iceheight(i,j,bi,bj)
        hs = snowheight(i,j,bi,bj)
        area = icemask(i,j,bi,bj)
        Tsf = Tsrf(i,j,bi,bj)
        qicen(1) = qice1(i,j,bi,bj) 
        qicen(2) = qice2(i,j,bi,bj)
c       qicen(2) = qicen(2)  + Lfresh   !!!! note !!!! only if using transp
        hlyr = hi / rnlyr

c  mass of fresh water and salt initially present in ice
        mwater0 = rhos*hs + rhoi*hi
        msalt0  = rhoi*hi*saltice 

c fractional snow cover
        frsnow = 0.
        if (hs .gt. 0.) frsnow = 1.
c
c Compute SW flux absorbed at surface and penetrating to layer 1.
c
        call sfc_albedo(hi,hs,Tsf,sage(i,j,bi,bj),albedo)
c positive for dasilva, negative for ncep
        fswabs  = -solar(i,j,bi,bj) * (1. - albedo)
        fswpen  = fswabs * (1. - frsnow) * i0
        fswocn = fswpen * exp(-ksolar*hi)
        fswint = fswpen - fswocn

        fswdn = fswabs - fswpen
c
c Compute conductivity terms at layer interfaces.
c
      k12 = 4*kice*ksnow / (ksnow*hi+4*kice*hs)
      k32 = 2*kice  / hi
c
c compute ice temperatures
      a1 = cpice
      b1 = qicen(1) + (cpwater-cpice )*Tmlt1 - Lfresh
      c1 = Lfresh * Tmlt1
      Tice(1) = 0.5 * (-b1 - sqrt(b1*b1-4.*a1*c1)) / a1
      Tice(2) = (Lfresh-qicen(2)) / cpice

cBB debugging
      if (Tice(1).gt.0.or.Tice(2).gt.0.) then
          write (6,*) 'BBerr Tice(1) > 0 = ',Tice(1)
          write (6,*) 'BBerr Tice(2) > 0 = ',Tice(2)
      endif
c
c Compute coefficients used in quadratic formula.
c
        a10 = rhoi*cpice *hi/(2.d0*dt) +
     $       k32 * (4.d0*dt*k32 + rhoi*cpice *hi)
     $       / (6.d0*dt*k32 + rhoi*cpice *hi)
        b10 = -hi*
     $       (rhoi*cpice *Tice(1)+rhoi*Lfresh*Tmlt1/Tice(1))/(2.d0*dt)
     $       - k32 * (4.d0*dt*k32*Tf+rhoi*cpice *hi*Tice(2))
     $       / (6.d0*dt*k32 + rhoi*cpice *hi) - fswint
        c1 = rhoi*Lfresh*hi*Tmlt1 / (2.d0*dt)
c
c Compute new surface and internal temperatures; iterate until
c Tsfc converges.
c
c ----- begin iteration  -----
       do 100 k = 1,nitmax
c
c Save temperatures at start of iteration.
         Tsf_start = Tsf
c

c Compute top surface flux.
          if (hs.gt.3.d-1) then
              iceornot=2
          else
              iceornot=1
          endif
          call bulkf_formula_lanl(uwind(i,j,bi,bj), vwind(i,j,bi,bj),
     &        wspeed(i,j,bi,bj),
     &        Tair(i,j,bi,bj), Qair(i,j,bi,bj), cloud(i,j,bi,bj), Tsf,
     &        flwup, flh, fsh, df0dT, ust, vst, evap, ssq, iceornot,
     &        readwindstress)

cQQ use lw data
          flwup=(flwup-flw(i,j,bi,bj))

          flx0 = (fswdn + flwup + fsh + flh)

c
c Compute new top layer and surface temperatures.
c If Tsfc is computed to be > 0 C, fix Tsfc = 0 and recompute T1
c with different coefficients. 
c           
          a1 = a10 - k12*df0dT / (k12-df0dT)
          b1 = b10 - k12*(flx0-df0dT*Tsf) / (k12-df0dT)
          Tice(1) = -(b1 + sqrt(b1*b1-4.*a1*c1)) / (2.*a1)
cBB
cBB       print*,'Tice(1)',Tice(1), a1, b1, c1, k12, df0dT 
cBB 
          dTsf = (flx0 + k12*(Tice(1)-Tsf)) / (k12-df0dT)
          Tsf = Tsf + dTsf
cBB
cBB       print*,'Tsf,dTsf',Tsf,dTsf
          if (Tsf .gt. 0.) then
             a1 = a10 + k12
             b1 = b10          ! note b1 = b10 - k12*Tf0
             Tice(1) = (-b1 - sqrt(b1*b1-4.*a1*c1)) / (2.*a1)
cBB
cBB          print*,'Tsf>0, Tice(1)',Tice(1),a1,b1,c1
cBB
             Tsf = 0.
             if (hs.gt.3.d-1) then
                  iceornot=2
             else
                  iceornot=1
             endif
             call bulkf_formula_lanl(uwind(i,j,bi,bj), vwind(i,j,bi,bj),
     &         wspeed(i,j,bi,bj),
     &         Tair(i,j,bi,bj), Qair(i,j,bi,bj), cloud(i,j,bi,bj),
     &         Tsf,
     &         flwup, flh, fsh, df0dT, ust, vst, evap, ssq, iceornot, 
     &         readwindstress)
cQQ use lw data
               flwup=(flwup-flw(i,j,bi,bj))

cBB 
cBB             print*,'(b)heat flux, ',fswdn,flwup,fsh,flh
             flx0 = (fswdn + flwup + fsh + flh)
             dTsf = 0.d0
             Tsf  = 0.d0
          endif
c
c Check for convergence.  If no convergence, then repeat.
c
c Convergence test: Make sure Tsfc has converged, within prescribed error.  
c (Energy conservation is guaranteed within machine roundoff, even
c if Tsfc has not converged.)
c If no convergence, then repeat.
c
         if (abs(dTsf).lt.Terrmax) then
            goto 150
         elseif (k.eq.nitmax) then
            write (6,*) 'BB: thermw conv err ',i,j,dTsf
            write (6,*) 'BB: thermw conv err, iceheight ',
     &                 iceheight(i,j,bi,bj)
            write (6,*) 'BB: thermw conv err: Tsf, flx0', Tsf,flx0
            if (Tsf.lt.-70.d0) stop  !QQQQ fails
         endif

100     continue  ! surface temperature iteration
150     continue
c ------ end iteration ------------

cQQ
         if (Tsf.lt.-70.d0) then
cQQ        print*,'QQQQQ Tsf =',Tsf
cQQ        stop  !QQQQ fails
         endif
c
c Compute new bottom layer temperature.
c
         Tice(2) = (2.d0*dt*k32*(Tice(1)+2.*Tf)
     $           + rhoi*cpice *hi*Tice(2))
     $           / (6.d0*dt*k32 + rhoi*cpice *hi)

cBB     
cBB    print*,'**********************************'
cBB    print*,'Q final Tice1,Tice2,Tsf,sst',
cBB  &         Tice(1),Tice(2),Tsf,theta(i,j,1,bi,bj)

c
c Compute final flux values at surfaces.
c
      fct = k12*(Tsf-Tice(1))
      fcb = 4.d0*kice *(Tice(2)-Tf)/hi
      if (Tsf.lt.0.) flx0 = flx0 + df0dT*dTsf
c
c Compute new enthalpy for each layer.
c
      qicen(1) = -cpwater*Tmlt1 + cpice *(Tmlt1-Tice(1)) + 
     $              Lfresh*(1.-Tmlt1/Tice(1))
      qicen(2) = -cpice *Tice(2) + Lfresh
c
cBB debugging
C Make sure internal ice temperatures do not exceed Tmlt.
c (This should not happen for reasonable values of i0.)
c
      if (Tice(1) .ge. Tmlt1) then 
             write (6,*)  'BBerr - Bug: IceT(1) > Tmlt',
     &                         i,j,Tice(1),Tmlt1
      end if
      if (Tice(2) .ge. 0.0) then
             write (6,*)  'BBerr - Bug: IceT(2) > 0',
     &                         i,j,Tice(2)
      end if


c *******************************************************************
c......................................................................
c
c.. Compute growth and/or melting at the top and bottom surfaces.......
c......................................................................
c
c Compute enthalpy of new ice growing at bottom surface.
c
      qbot =  -cpice *Tf + Lfresh
c     
c Compute energy available for melting/growth.
c
      if (Tsf .eq. 0.) etop = (flx0 - fct) * dt
      ebot = (fcb-Fbot) * dt
cBBB
cBBB  if (ebot.gt.0) 
cBBB &     print*, 'fcb,fbot,ebot',fcb,fbot,ebot,frzmlt
c
c Initialize layer thicknesses.
c Make sure internal ice temperatures do not exceed Tmlt.
c If they do, then eliminate the layer.  (Dont think this will happen
c for reasonable values of i0.)
c
      do k = 1, nlyr
         hnew(k) = hlyr
      enddo
c
c Top melt: snow, then ice.
c
      if (etop .gt. 0.) then
         if (hs. gt. 0.) then
            rq =  rhos * qsnow
            rqh = rq * hs
            if (etop .lt. rqh) then
               hs = hs - etop/rq
               etop = 0.
            else
               etop = etop - rqh 
               hs = 0.
            endif
         endif
                  
         do k = 1, nlyr
            if (etop .gt. 0.) then
               rq =  rhoi * qicen(k)
               rqh = rq * hnew(k)
               if (etop .lt. rqh) then
                  hnew(k) = hnew(k) - etop / rq
                  etop = 0.
               else
                  etop = etop - rqh
                  hnew(k) = 0.
               endif
            endif
         enddo
      endif
c
c If ice is gone and melting energy remains
c      
      if (etop .gt. 0.) then
         write (6,*)  'All ice melts from top  ', i,j
         go to 200
      endif
c
c Bottom melt/growth. 
c
      if (ebot .lt. 0.) then
         dhi = -ebot / (qbot * rhoi)
         ebot = 0.
         k = nlyr
         qicen(k) = (hnew(k)*qicen(k)+dhi*qbot) / (hnew(k)+dhi)
         hnew(k) = hnew(k) + dhi
      else               
         do k = nlyr, 1, -1
            if (ebot .gt. 0. .and. hnew(k) .gt. 0.) then
               rq =  rhoi * qicen(k)
               rqh = rq * hnew(k)
               if (ebot .lt. rqh) then
                  hnew(k) = hnew(k) - ebot / rq
                  ebot = 0.
               else
                  ebot = ebot - rqh
                  hnew(k) = 0.
               endif
            endif
         enddo
c
c If ice melts completely and snow is left, remove the snow with 
c energy from the mixed layer
c         
         if (ebot .gt. 0. .and. hs .gt. 0.) then
            rq =  rhos * qsnow
            rqh = rq * hs
            if (ebot .lt. rqh) then
               hs = hs - ebot / rq
               esurp = esurp - rhos*qsnow*hs
               ebot = 0.
            else
               ebot = ebot - rqh
               hs = 0.
            endif
         endif
                  
         if (ebot .gt. 0.) then
cBB         write (6,*)  'All ice melts from bottom', i,j
            go to 200
         endif

      endif
c
c Compute new total ice thickness.
c
      hi = 0.
      do k = 1, nlyr
         hi = hi + hnew(k)
      enddo

c If hi < himin, melt the ice. 
      if (hi .lt. himin) then
         esurp = esurp - rhos*qsnow*hs
         do k = 1, nlyr
            esurp = esurp - rhoi*qicen(k)*hnew(k)
         enddo
         hi = 0.
         hs = 0.
         Tsf=0.
         do k = 1, nlyr
           qicen(k) = 0.
         enddo
         go to 200
      endif

c
c Let it snow
c
cQQm check
        frsnow = 0.  ! temporarily indicates whether fresh snow accumulates
        if (Tsf.lt.0.and.Tair(i,j,bi,bj).le.Tf0kel.and.hi.gt.0.d0) then
          frsnow = 1.
cQQ change if reading in snow -- including left over value of rain
          sage(i,j,bi,bj)=(dt+sage(i,j,bi,bj)*(1-dt/(50*24*60*60)))
     &                      *exp(rain(i,j,bi,bj))
          snow(i,j,bi,bj)=-rain(i,j,bi,bj)*1000/rhos
          rain(i,j,bi,bj)=0.0
          hs = hs + snow(i,j,bi,bj)*dt
        else
          snow(i,j,bi,bj)=0.0
          sage(i,j,bi,bj)=(dt+sage(i,j,bi,bj)*(1-dt/(50*24*60*60)))
        endif
cBB
        if (sage(i,j,bi,bj).gt.50*24*60*60) then
           print*,'sage',i,j,sage(i,j,bi,bj)
        endif

cswd  QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
c If ice evap is used to sublimate surface snow/ice or
c if no ice pass on to ocean
      if (hs.gt.0.d0) then
        if (-evap*1000/rhos *dt.gt.hs) then
           evap=evap+hs*rhos/1000/dt
           hs=0.d0
        else
           hs = hs + evap*1000/rhos *dt
           evap=0.d0
        endif
      endif
      if (hi.gt.0.d0.and.evap.lt.0.d0) then
        if (-evap*1000/rhoi *dt.gt.hi) then
           evap=evap+hi*rhoi/1000/dt
           hi=0.d0
        else
           hi = hi + evap*1000/rhoi *dt
           evap=0.d0
        endif
      endif


c If there is enough snow to lower the ice/snow interface below 
c freeboard, convert enough snow to ice to bring the interface back 
c to sea-level.  Adjust enthalpy of top ice layer accordingly.
c
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

cswd
c If ice evap is used to sublimate surface snow/ice or
c if no ice pass on to ocean
      

cQQQQQQ
c     if (hi.gt.hilim) then
c        print*,'BBerr, hi>hilim',i,j,hi
c        chi=hi-hilim
c        hi=hilim
c     endif
c     if (hs.gt.hslim) then
c        print*,'BBerr, hs>hslim',i,j,hs
c        chs=hs-hslim
c        hs=hslim
c     endif
cQQQQQQ


       hlyr = hi/rnlyr
       call new_layers_winton(hs,hi,hlyr,hnew,qicen)

200   continue
c     
c Compute surplus energy left over from melting.
c            
       esurp = esurp + etop + ebot 
c.. heat fluxes left over for ocean
cQQ    qleft = -fswocn
cQQ    if (frzmlt.le.0.) qleft =qleft -(Fbot+esurp/dt) ! W/m^2
      else   ! 0 < hi < himin
c If hi < himin, melt the ice.
         esurp = esurp - rhos*qsnow*snowheight(i,j,bi,bj)
         do k = 1, nlyr
            esurp = esurp - rhoi*qicen(k)*iceheight(i,j,bi,bj)*0.5
            qicen(k) = 0.
         enddo
         hi = 0.
         hs = 0.
         Tsf = 0.
cQQ      call bulkf_formula_lanl(uwind(i,j,bi,bj), vwind(i,j,bi,bj),
cQQ  &        wspeed(i,j,bi,bj),
cQQ  &        Tair(i,j,bi,bj), Qair(i,j,bi,bj), cloud(i,j,bi,bj), Tsf,
cQQ  &        flwup, flh, fsh, df0dT, ust, vst, evap, ssq, iceornot,
cQQ  &        readwindstress)
cQQ use lw data
cQQ      flwup=(flwup-flw(i,j,bi,bj))
cQQ      albedo=0.1   !QQQQ need to unhardwire
cQQ      flx0 = (-solar(i,j,bi,bj)*(1.d0-albedo) + flwup + fsh + flh)
c.. heat fluxes left over for ocean
cQQ      qleft=-(flx0+esurp/dt) ! W/m^2         
       endif  ! hi 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c.. fresh and salt fluxes
c QQQQQ
      fresh=0.0
      fsalt=0.0
      fresh = (mwater0 - (rhos*(hs+chs) + rhoi*(hi+chi)))/dt+evap
      fsalt = (msalt0 - rhoi*hi*saltice)/35.d0/dt  ! for same units as fresh
c.. heat fluxes left over for ocean
      qleft = -fswocn
      if (frzmlt.le.0.) qleft =qleft -(Fbot+esurp/dt) ! W/m^2
cBB debug
cBB   print*,'Q qnet, qleft, Fbot,esurp/dt', qnet(i,j,bi,bj),
cBB  &        qleft, Fbot, esurp/dt
cQQQQQQQQQQQQ
c     if (Tsf.lt.-50.d0) then
c        Tsf=-50.d0
c     endif
cQQ   if (Tsf.lt.Tair(i,j,bi,bj)-Tf0kel-3.d0) then
cQQ      Tsf=Tair(i,j,bi,bj)-Tf0kel-3.d0
cQQ   endif
cQQQQQQ
      if (hi.gt.hilim) then
cBBB     print*,'BBerr, hi>hilim',i,j,hi
         chi=hi-hilim 
         fresh=fresh+rhoi*chi/dt
         hi=hilim
      endif 
      if (hs.gt.hslim) then 
cBB      print*,'BBerr, hs>hslim',i,j,hs
         chs=hs-hslim
         fresh=fresh+rhos*chs/dt
         hs=hslim
      endif 
cQQQQQQ

   
c
c .. store new values
      iceheight(i,j,bi,bj)=hi     
c     if (hi.gt.0) then
c       icemask(i,j,bi,bj)=1
c     else
c       icemask(i,j,bi,bj)=0
c     endif
      if (hi.gt.0.d0) then
          compact=iceMask(i,j,bi,bj)
      else
          compact=0.d0
      endif
      snowheight(i,j,bi,bj)=hs
      tsrf(i,j,bi,bj)=Tsf
      tice1(i,j,bi,bj)=Tice(1)
      tice2(i,j,bi,bj)=Tice(2)
      qice1(i,j,bi,bj)=qicen(1)
      qice2(i,j,bi,bj)=qicen(2)

cBB debugging
cBB     print*,'Q4 i,j,hi,hs,Tsf',i,j,hi,hs,Tsf
cBB     print*,'Q4 qleft,fsalt,fresh', qleft,fsalt,fresh

#endif

      return
      end       ! main thermo routine
c --------------------------------------------------------------------

#include "CPP_OPTIONS.h"

c --------------------------------------------------------------------

CStartofinterface
      subroutine new_layers_winton(hs,hi,hlyr,hnew,qicen)
c
c Repartition into equal-thickness layers, conserving energy.
c This is the 2-layer version.
c
      implicit none
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_THERM_SEAICE
#include "ICE.h"   
#include "BULKF_ICE_CONSTANTS.h"
#endif

#ifndef ALLOW_THERM_SEAICE
      integer nlyr
      parameter(nlyr=1)
#endif

      _RL  hnew(nlyr)    ! new ice layer thickness (m)
      _RL  hs
      _RL  hi
      _RL  hlyr          ! individual ice layer thickness (m)
      _RL  qicen (nlyr)  ! ice enthalpy (J m-3)
c
#ifdef ALLOW_THERM_SEAICE
c Local variables
      _RL  f1            ! Fraction of upper layer ice in new layer
      _RL  qh1, qh2      ! qice*h for layers 1 and 2
      _RL  qhtot         ! qh1 + qh2
      _RL  q2tmp         ! Temporary value of qice for layer 2

      if (hnew(1).gt.hnew(2)) then  ! Layer 1 gives ice to layer 2 
         f1 = (hnew(1)-hlyr)/hlyr
         q2tmp = f1*qicen(1) + (1.-f1)*qicen(2)
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
         qicen(1) = f1*qicen(1) + (1.-f1)*qicen(2)
      endif

#endif

      return
      end

c --------------------------------------------------------------------
CStartofinterface
      subroutine sfc_albedo(hi,hs,Tsf,age,albedo)
c.. Compute surface albedo 
      implicit none
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_THERM_SEAICE
#include "ICE.h"          
#include "BULKF_ICE_CONSTANTS.h"
#endif

      _RL  frsnow              ! fractional snow cover
      _RL  hi                  ! ice height
      _RL  hs                  ! snow height
      _RL  Tsf                 ! surface temperature
      _RL  albedo              ! surface albedo

      _RL frsnalb, salb  
      _RL albsno, albice, age

#ifdef ALLOW_THERM_SEAICE
c     frsnow = 0.
c     if (hs .gt. 0.) frsnow = 1.

c     if (Tsf .lt. 0.) then
c        albedo = frsnow*albsnodry +
c    $     (1.-frsnow)*(albicemin + (albicemax - albicemin)
c    $                *(1.-exp(-hi/halb)))
c     else
c        albedo = frsnow*albsnowet +
c    $     (1.-frsnow)*(albicemin + (albicemax - albicemin)
c    $                *(1.-exp(-hi/halb)))
c     endif
c GISS model albedo calculation
      if (Tsf.lt.-10.d0) then
         frsnalb=0.3
      elseif (Tsf.le.0.d0) then
         frsnalb=0.3-0.015*(Tsf+10)
      else
         frsnalb=0.15
      endif
 
      albice=0.7
      albsno=0.55+frsnalb*exp(-0.2*age/(24*60*60))  !make age units into days
      albedo=albice*exp(-hs/0.3)+albsno*(1-exp(-hs/0.3))

      if (albedo.gt.1.d0.or.albedo.lt..5d0) then
        print*,'QQ - albedo problem', albedo, age, hs, albsno
        stop
      endif
#endif

      return
      end
c --------------------------------------------------------------------
