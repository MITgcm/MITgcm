C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_calc_thickn.F,v 1.5 2006/02/10 00:24:12 jmc Exp $
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
#include "EEPARAMS.h"
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
C     qicen    :: ice enthalpy (J/kg)
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
      _RL  qbot          ! enthalpy for new ice at bottom surf (J/kg)
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

      IF (frzmlt.GE. 0. _d 0) THEN
C     !-----------------------------------------------------------------
C     ! freezing conditions
C     !-----------------------------------------------------------------
C if higher than hihig, use all frzmlt energy to grow extra ice
        IF (hi.GT.hihig .AND. compact.LE.iceMaskmax) THEN
          Fbot=0. _d 0
        ELSE
          Fbot=frzmlt
        ENDIF
      ELSE
C     !-----------------------------------------------------------------
C     ! melting conditions
C     !-----------------------------------------------------------------
         ustar = 5. _d -2        !for no currents
C frictional velocity between ice and water
         ustar = SQRT(0.00536 _d 0*oceV2s)
         ustar=max(5. _d -3,ustar)
         cpchr =cpwater*rhosw*transcoef 
         Fbot = cpchr*(Tf-oceTs)*ustar  ! < 0
         Fbot = max(Fbot,frzmlt)    ! frzmlt < Fbot < 0
         Fbot = min(Fbot,0. _d 0)
      ENDIF

C  mass of fresh water and salt initially present in ice
      mwater0 = rhos*hs + rhoi*hi
      msalt0  = rhoi*hi*saltice 

      IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: evpAtm, frzmlt, Fbot =',
     &                                       evpAtm, frzmlt, Fbot

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C Compute energy available for melting/growth.

      IF (hi.LT.himin0) THEN
C below a certain height, all energy goes to changing ice extent
       frace=1. _d 0
      ELSE
       frace=frac_energy
      ENDIF
      IF (hi.GT.hihig) THEN
C above certain height only melt from top
       frace=0. _d 0
      ELSE
       frace=frac_energy
      ENDIF
C force this when no ice fractionation
      IF (frac_energy.EQ.0. _d 0) frace=0. _d 0

c     IF (Tsf .EQ. 0. _d 0 .AND. sHeating.GT.0. _d 0) THEN
      IF ( sHeating.GT.0. _d 0 ) THEN
          etop = (1. _d 0-frace)*sHeating * dt
          etope = frace*sHeating * dt
      ELSE
          etop =  0. _d 0
          etope = 0. _d 0
C jmc: found few cases where Tsf=0 & sHeating < 0 : add this line to conserv energy:
          esurp = sHeating * dt
      ENDIF
C--   flux at the base of sea-ice: 
C     conduction H.flx= flxCnB (+ =down); oceanic turbulent H.flx= Fbot (+ =down).
C-    ==> energy available(+ => melt)= (flxCnB-Fbot)*dt
c     IF (frzmlt.LT.0. _d 0) THEN
c         ebot = (1. _d 0-frace)*(flxCnB-Fbot) * dt
c         ebote = frace*(flxCnB-Fbot) * dt
c     ELSE
c         ebot = (flxCnB-Fbot) * dt
c         ebote = 0. _d 0
c     ENDIF
C- original formulation(above): Loose energy when flxCnB < Fbot < 0
      ebot = (flxCnB-Fbot) * dt
      IF (ebot.GT.0. _d 0) THEN
         ebote = frace*ebot
         ebot  = ebot-ebote
      ELSE
         ebote = 0. _d 0
      ENDIF
      IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: etop,etope,ebot,ebote=',
     &            etop,etope,ebot,ebote

C Initialize layer thicknesses.
C Make sure internal ice temperatures do not exceed Tmlt.
C If they do, then eliminate the layer.  (Dont think this will happen
C for reasonable values of i0.)

      hlyr = hi / rnlyr
      DO k = 1, nlyr
         hnew(k) = hlyr
      ENDDO

C Top melt: snow, then ice.

      IF (etop .GT. 0. _d 0) THEN
         IF (hs. gt. 0. _d 0) THEN
            rq =  rhos * qsnow
            rqh = rq * hs
            IF (etop .LT. rqh) THEN
               hs = hs - etop/rq
               etop = 0. _d 0
            ELSE
               etop = etop - rqh 
               hs = 0. _d 0
            ENDIF
         ENDIF
                  
         DO k = 1, nlyr
            IF (etop .GT. 0. _d 0) THEN
               rq =  rhoi * qicen(k)
               rqh = rq * hnew(k)
               IF (etop .LT. rqh) THEN
                  hnew(k) = hnew(k) - etop / rq
                  etop = 0. _d 0
               ELSE
                  etop = etop - rqh
                  hnew(k) = 0. _d 0
               ENDIF
            ENDIF
         ENDDO
      ELSE
        etop=0. _d 0
      ENDIF
C If ice is gone and melting energy remains
c     IF (etop .GT. 0. _d 0) THEN
c        WRITE (6,*)  'QQ All ice melts from top  ', i,j
c        hi=0. _d 0
c        go to 200
c     ENDIF


C Bottom melt/growth. 

      IF (ebot .LT. 0. _d 0) THEN
C Compute enthalpy of new ice growing at bottom surface.
         qbot =  -cpice *Tf + Lfresh
         dhi = -ebot / (qbot * rhoi)
         ebot = 0. _d 0
         k = nlyr
         qicen(k) = (hnew(k)*qicen(k)+dhi*qbot) / (hnew(k)+dhi)
         hnew(k) = hnew(k) + dhi
      ELSE               
         DO k = nlyr, 1, -1
            IF (ebot.GT.0. _d 0 .AND. hnew(k).GT.0. _d 0) THEN
               rq =  rhoi * qicen(k)
               rqh = rq * hnew(k)
               IF (ebot .LT. rqh) THEN
                  hnew(k) = hnew(k) - ebot / rq
                  ebot = 0. _d 0
               ELSE
                  ebot = ebot - rqh
                  hnew(k) = 0. _d 0
               ENDIF
            ENDIF
         ENDDO

C If ice melts completely and snow is left, remove the snow with 
C energy from the mixed layer

         IF (ebot.GT.0. _d 0 .AND. hs.GT.0. _d 0) THEN
            rq =  rhos * qsnow
            rqh = rq * hs
            IF (ebot .LT. rqh) THEN
               hs = hs - ebot / rq
               ebot = 0. _d 0
            ELSE
               ebot = ebot - rqh
               hs = 0. _d 0
            ENDIF
         ENDIF
c        IF (ebot .GT. 0. _d 0) THEN
c           IF (dBug) WRITE(6,*) 'All ice (& snow) melts from bottom'
c           hi=0. _d 0
c           go to 200
c        ENDIF
      ENDIF

C Compute new total ice thickness.
      hi = 0. _d 0
      DO k = 1, nlyr
         hi = hi + hnew(k)
      ENDDO
      IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH:   etop, ebot, hi, hs =',
     &                                         etop, ebot, hi, hs    

C If hi < himin, melt the ice. 
      IF ( hi.LT.himin .AND. (hi+hs).GT.0. _d 0 ) THEN
         esurp = esurp - rhos*qsnow*hs
         DO k = 1, nlyr
            esurp = esurp - rhoi*qicen(k)*hnew(k)
         ENDDO
         hi = 0. _d 0
         hs = 0. _d 0
         Tsf=0. _d 0
         compact = 0. _d 0
         DO k = 1, nlyr
           qicen(k) = 0. _d 0
         ENDDO
         IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: -1 : esurp=',esurp
      ENDIF

C--   do a mass-budget of sea-ice to compute "fresh" = the fresh-water flux
C     that is returned to the ocean ; needs to be done before snow/evap
      fresh = (mwater0 - (rhos*hs + rhoi*hi))/dt

C- note : was not supposed to modify snowPr in THSICE_CALC_TH ;
C         but to reproduce old results, reset to zero if Tsf >= 0
c     IF ( Tsf.GE.0. _d 0 ) snowPr = 0.

      IF ( hi .LE. 0. _d 0 ) THEN 
C-    return  snow to the ocean (account for Latent heat of freezing)
        fresh = fresh + snowPr
        qleft = qleft - snowPr*Lfresh

      ELSE
C-    else: hi > 0

C Let it snow

        hs = hs + dt*snowPr/rhos

C If ice evap is used to sublimate surface snow/ice or
C if no ice pass on to ocean
       IF (hs.GT.0. _d 0) THEN
         IF (evap/rhos *dt.GT.hs) THEN
           evap=evap-hs*rhos/dt
           hs=0. _d 0
         ELSE
           hs = hs - evap/rhos *dt
           evap=0. _d 0
         ENDIF
       ENDIF
       IF (hi.GT.0. _d 0.AND.evap.GT.0. _d 0) THEN
         DO k = 1, nlyr
            IF (evap .GT. 0. _d 0) THEN
C-- original scheme, does not care about ice temp.
C-  this can produce small error (< 1.W/m2) in the Energy budget
c              IF (evap/rhoi *dt.GT.hnew(k)) THEN
c                evap=evap-hnew(k)*rhoi/dt
c                hnew(k)=0. _d 0
c              ELSE
c                hnew(k) = hnew(k) - evap/rhoi *dt
c                evap=0. _d 0
c              ENDIF
C-- modified scheme. taking into account Ice enthalpy
               dhi = evap/rhoi*dt 
               IF (dhi.GE.hnew(k)) THEN
                 evap=evap-hnew(k)*rhoi/dt
                 esurp = esurp - hnew(k)*rhoi*(qicen(k)-Lfresh)
                 hnew(k)=0. _d 0
               ELSE
                 hq = hnew(k)*qicen(k)-dhi*Lfresh
                 hnew(k) = hnew(k) - dhi
                 qicen(k)=hq/hnew(k)
                 evap=0. _d 0
               ENDIF
C-------
            ENDIF
         ENDDO
       ENDIF
c     IF (evap .GT. 0. _d 0) THEN
c           WRITE (6,*)  'BB All ice sublimates', i,j
c           hi=0. _d 0
c           go to 200
c     ENDIF

C Compute new total ice thickness.

       hi = 0. _d 0
       DO k = 1, nlyr
         hi = hi + hnew(k)
       ENDDO

C If hi < himin, melt the ice. 
       IF ( hi.GT.0. _d 0 .AND. hi.LT.himin ) THEN
         fresh = fresh + (rhos*hs + rhoi*hi)/dt
         esurp = esurp - rhos*qsnow*hs
         DO k = 1, nlyr
            esurp = esurp - rhoi*qicen(k)*hnew(k)
         ENDDO
         hi = 0. _d 0
         hs = 0. _d 0
         Tsf=0. _d 0
         compact = 0. _d 0
         DO k = 1, nlyr
           qicen(k) = 0. _d 0
         ENDDO
         IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: -2 : esurp,fresh=',
     &                   esurp, fresh
       ENDIF

C-    else hi > 0: end
      ENDIF

      IF ( hi .GT. 0. _d 0 ) THEN

C If there is enough snow to lower the ice/snow interface below 
C freeboard, convert enough snow to ice to bring the interface back 
C to sea-level.  Adjust enthalpy of top ice layer accordingly.

       IF ( hs .GT. hi*rhoiw/rhos ) THEN
cBB               WRITE (6,*)  'Freeboard adjusts'
         dhi = (hs * rhos - hi * rhoiw) / rhosw
         dhs = dhi * rhoi / rhos
         rqh = rhoi*qicen(1)*hnew(1) + rhos*qsnow*dhs
         hnew(1) = hnew(1) + dhi
         qicen(1) = rqh / (rhoi*hnew(1))
         hi = hi + dhi
         hs = hs - dhs
       ENDIF


C limit ice height
C- NOTE: this part does not conserve Energy ;
C        but surplus of fresh water and salt are taken into account.
       IF (hi.GT.hiMax) THEN
cBB      print*,'BBerr, hi>hiMax',i,j,hi
         chi=hi-hiMax
         DO k=1,nlyr
            hnew(k)=hnew(k)-chi/2. _d 0
         ENDDO
         fresh = fresh + chi*rhoi/dt
       ENDIF
       IF (hs.GT.hsMax) THEN
c        print*,'BBerr, hs>hsMax',i,j,hs
         chs=hs-hsMax
         hs=hsMax
         fresh = fresh + chs*rhos/dt
       ENDIF

C Compute new total ice thickness.

       hi = 0. _d 0
       DO k = 1, nlyr
         hi = hi + hnew(k)
       ENDDO
 
       IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: b-Winton: hnew, qice =',
     &                                                  hnew, qicen

       hlyr = hi/rnlyr
       CALL THSICE_RESHAPE_LAYERS(
     U                            qicen,
     I                            hlyr, hnew, myThid )

       IF (dBug) WRITE(6,1020) 'ThSI_CALC_TH: compact,hi, qtot, hs =',
     &                  compact,hi,(qicen(1)+qicen(2))*0.5, hs

C-    if hi > 0 : end
      ENDIF
 200  CONTINUE

C-  Compute surplus energy left over from melting.

      IF (hi.LE.0. _d 0) compact=0. _d 0

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
      IF (compact.GT.0. _d 0.AND.extend.GT.0. _d 0) THEN
         rq =  rhoi * 0.5 _d 0*(qicen(1)+qicen(2))
         rs =  rhos * qsnow
         rqh = rq * hi + rs * hs
         freshe=(rhos*hs+rhoi*hi)/dt
         salte=(rhoi*hi*saltice)/dt
         IF (extend .LT. rqh) THEN
           compact=(1. _d 0-extend/rqh)*compact
           fresh=fresh+extend/rqh*freshe
           fsalt=fsalt+extend/rqh*salte
         ELSE
           compact=0. _d 0
           hi=0. _d 0 
           hs=0. _d 0 
           qleft=qleft+(extend-rqh)/dt
           fresh=fresh+freshe
           fsalt=fsalt+salte
         ENDIF
      ELSEIF (extend.GT.0. _d 0) THEN
         qleft=qleft+extend/dt
      ENDIF
 
#endif  /* ALLOW_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
