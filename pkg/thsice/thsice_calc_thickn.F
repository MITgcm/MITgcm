C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_calc_thickn.F,v 1.3 2004/05/07 21:33:34 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_CALC_THICKN
C     !INTERFACE:
      SUBROUTINE THSICE_CALC_THICKN(
     I                     frzmlt, Tf, oceTs, oceV2s, snowPr,
     I                     sHeating, flxCnB, evpAtm,
     U                     compact, hi, hs, Tsf, qicen, qleft, 
     O                     fresh, fsalt, Fbot,
     I                     dBugFlag, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_CALC_THICKN
C     | o Calculate ice & snow thickness changes
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     frzmlt   :: ocean mixed-layer freezing/melting potential [W/m2]
C     Tf       :: sea-water freezing temperature [oC] (function of S)
C     oceTs    :: surface level oceanic temperature [oC]
C     oceV2s   :: square of ocean surface-level velocity [m2/s2]
C     snowPr   :: snow precipitation                [kg/m2/s]
C     sHeating :: surf heating flux left to melt snow or ice (= Atmos-conduction)
C     flxCnB   :: heat flux conducted through the ice to bottom surface
C     evpAtm   :: evaporation to the atmosphere [kg/m2/s] (>0 if evaporate)
C     compact  :: fraction of grid area covered in ice
C     hi       :: ice height
C     hs       :: snow height
C     Tsf      :: surface (ice or snow) temperature
C     qicen    :: ice enthalpy (J m-3)
C     qleft    :: net heat flux to ocean    [W/m2]          (> 0 downward)
C     fresh    :: Total fresh water flux to ocean [kg/m2/s] (> 0 downward)
C     fsalt    :: salt flux to ocean        [g/m2/s]        (> 0 downward)
C     Fbot     :: oceanic heat flux used to melt/form ice [W/m2]
C     dBugFlag :: allow to print debugging stuff (e.g. on 1 grid point).
C     myThid   :: Thread no. that called this routine.
      _RL  frzmlt
      _RL  Tf
      _RL oceTs, oceV2s, snowPr
      _RL sHeating
      _RL flxCnB
      _RL evpAtm
      _RL compact
      _RL  hi
      _RL  hs
      _RL  Tsf
      _RL  qicen(nlyr)

      _RL qleft
      _RL fresh
      _RL fsalt
      _RL  Fbot
      LOGICAL dBugFlag
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
      INTEGER  k

      _RL  rnlyr         ! maximum number of ice layers (real value)
C     evap           ::  evaporation over snow/ice [kg/m2/s] (>0 if evaporate)
      _RL  evap
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

      _RL  ustar, cpchr

      _RL  chi, chs
      _RL  frace, rs, hq
      LOGICAL dBug

 1010 FORMAT(A,I3,3F8.3)
 1020 FORMAT(A,1P4E11.3)

      rnlyr = nlyr
      dt  = thSIce_deltaT
      dBug = .FALSE.
c     dBug = dBugFlag

C     initialize energies
      esurp = 0. _d 0

      evap = evpAtm

C......................................................................
C.. Compute growth and/or melting at the top and bottom surfaces.......
C......................................................................

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

      IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: evpAtm, frzmlt, Fbot =',
     &                                       evpAtm, frzmlt, Fbot

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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

c     if (Tsf .eq. 0. _d 0 .and. sHeating.gt.0. _d 0) then
      if ( sHeating.gt.0. _d 0 ) then
          etop = (1. _d 0-frace)*sHeating * dt
          etope = frace*sHeating * dt
      else
          etop =  0. _d 0
          etope = 0. _d 0
C jmc: found few cases where Tsf=0 & sHeating < 0 : add this line to conserv energy:
          esurp = sHeating * dt
      endif
C--   flux at the base of sea-ice: 
C     conduction H.flx= flxCnB (+ =down); oceanic turbulent H.flx= Fbot (+ =down).
C-    ==> energy available(+ => melt)= (flxCnB-Fbot)*dt
c     if (frzmlt.lt.0. _d 0) then
c         ebot = (1. _d 0-frace)*(flxCnB-Fbot) * dt
c         ebote = frace*(flxCnB-Fbot) * dt
c     else
c         ebot = (flxCnB-Fbot) * dt
c         ebote = 0. _d 0
c     endif
C- original formulation(above): Loose energy when flxCnB < Fbot < 0
      ebot = (flxCnB-Fbot) * dt
      if (ebot.gt.0. _d 0) then
         ebote = frace*ebot
         ebot  = ebot-ebote
      else
         ebote = 0. _d 0
      endif
      IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: etop,etope,ebot,ebote=',
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
      IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH:   etop, ebot, hi, hs =',
     &                                         etop, ebot, hi, hs    

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
         IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: -1 : esurp=',esurp
      endif

C--   do a mass-budget of sea-ice to compute "fresh" = the fresh-water flux
C     that is returned to the ocean ; needs to be done before snow/evap
      fresh = (mwater0 - (rhos*hs + rhoi*hi))/dt

C- note : was not supposed to modify snowPr in THSICE_CALC_TH ;
C         but to reproduce old results, reset to zero if Tsf >= 0
c     IF ( Tsf.ge.0. _d 0 ) snowPr = 0.

      IF ( hi .LE. 0. _d 0 ) THEN 
C-    return snow to the ocean (account for Latent heat of freezing)
        fresh = fresh + snowPr
        qleft = qleft - snowPr*Lfresh
        GOTO 200
      ENDIF

C Let it snow

        hs = hs + dt*snowPr/rhos

C If ice evap is used to sublimate surface snow/ice or
C if no ice pass on to ocean
      if (hs.gt.0. _d 0) then
        if (evap/rhos *dt.gt.hs) then
           evap=evap-hs*rhos/dt
           hs=0. _d 0
        else
           hs = hs - evap/rhos *dt
           evap=0. _d 0
        endif
      endif
      if (hi.gt.0. _d 0.and.evap.gt.0. _d 0) then
         do k = 1, nlyr
            if (evap .gt. 0. _d 0) then
C-- original scheme, does not care about ice temp.
C-  this can produce small error (< 1.W/m2) in the Energy budget
c              if (evap/rhoi *dt.gt.hnew(k)) then
c                evap=evap-hnew(k)*rhoi/dt
c                hnew(k)=0. _d 0
c              else
c                hnew(k) = hnew(k) - evap/rhoi *dt
c                evap=0. _d 0
c              endif
C-- modified scheme. taking into account Ice enthalpy
               dhi = evap/rhoi*dt 
               if (dhi.ge.hnew(k)) then
                 evap=evap-hnew(k)*rhoi/dt
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
         IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: -2 : esurp,fresh=',
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
c        print*,'BBerr, hs>hsMax',i,j,hs
         chs=hs-hsMax
         hs=hsMax
         fresh = fresh + chs*rhos/dt
      endif

C Compute new total ice thickness.

      hi = 0. _d 0
      do k = 1, nlyr
         hi = hi + hnew(k)
      enddo
 
      IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: b-Winton: hnew, qice =',
     &                                                 hnew, qicen

       hlyr = hi/rnlyr
       CALL THSICE_RESHAPE_LAYERS(
     U                            qicen,
     I                            hlyr, hnew, myThid )

      IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: compact,hi, qtot, hs =',
     &                  compact,hi,(qicen(1)+qicen(2))*0.5, hs

200   continue

C-  Compute surplus energy left over from melting.

      if (hi.le.0. _d 0) compact=0. _d 0

C.. heat fluxes left over for ocean
       qleft = qleft + (Fbot+(esurp+etop+ebot)/dt)
       IF(dBug) WRITE(6,1020)'ThSI_CALC_TH: [esurp,etop+ebot]/dt ='
     &                        ,esurp/dt,etop/dt,ebot/dt

C-- Evaporation left to the ocean :
       fresh = fresh - evap
C-  Correct Atmos. fluxes for this different latent heat:
C   evap was computed over freezing surf.(Tsf<0), latent heat = Lvap+Lfresh
C   but should be Lvap only for the fraction "evap" that is left to the ocean. 
       qleft = qleft + evap*Lfresh

C fresh and salt fluxes
c     fresh = (mwater0 - (rhos*(hs) + rhoi*(hi)))/dt-evap
c     fsalt = (msalt0 - rhoi*hi*saltice)/35. _d 0/dt  ! for same units as fresh
C note (jmc): fresh is computed from a sea-ice mass budget that already 
C    contains, at this point, snow & evaporation (of snow & ice)
C    but are not meant to be part of ice/ocean fresh-water flux.
C  fix: a) like below or b) by making the budget before snow/evap is added
c     fresh = (mwater0 - (rhos*(hs) + rhoi*(hi)))/dt
c    &      + snow(i,j,bi,bj)*rhos - evpAtm
      fsalt = (msalt0 - rhoi*hi*saltice)/dt

      IF (dBug) WRITE(6,1020)'ThSI_CALC_TH:dH2O,Ev[kg],fresh,fsalt',
     &   (mwater0-(rhos*hs+rhoi*hi))/dt,evap,fresh,fsalt
      IF (dBug) WRITE(6,1020)'ThSI_CALC_TH: Qleft,Fbot,extend/dt =',
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
      END
