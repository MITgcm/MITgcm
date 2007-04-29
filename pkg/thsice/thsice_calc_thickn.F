C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_calc_thickn.F,v 1.8 2007/04/29 23:48:44 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_CALC_THICKN
C     !INTERFACE:
      SUBROUTINE THSICE_CALC_THICKN(
     I                  bi, bj, siLo, siHi, sjLo, sjHi,
     I                  iMin,iMax, jMin,jMax, dBugFlag,
     I                  iceMask, tFrz, tOce, v2oc,
     I                  snowP, prcAtm, sHeat, flxCnB,
     U                  icFrac, hIce, hSnow, tSrf, qIc1, qIc2,
     U                  frwAtm, fzMlOc, flx2oc,
     O                  frw2oc, fsalt,
     I                  myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_CALC_THICKN
C     | o Calculate ice & snow thickness changes
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
#ifdef ALLOW_AUTODIFF_TAMC
# include "SIZE.h"
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     siLo,siHi   :: size of input/output array: 1rst dim. lower,higher bounds
C     sjLo,sjHi   :: size of input/output array: 2nd  dim. lower,higher bounds
C     bi,bj       :: tile indices
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C     dBugFlag    :: allow to print debugging stuff (e.g. on 1 grid point).
C---  Input:
C         iceMask :: sea-ice fractional mask [0-1]
C  tFrz    (Tf)   :: sea-water freezing temperature [oC] (function of S)
C  tOce   (oceTs) :: surface level oceanic temperature [oC]
C  v2oc  (oceV2s) :: square of ocean surface-level velocity [m2/s2]
C  snowP (snowPr) :: snow precipitation                [kg/m2/s]
C  prcAtm   (=)   :: total precip from the atmosphere [kg/m2/s]
C  sHeat(sHeating):: surf heating flux left to melt snow or ice (= Atmos-conduction)
C  flxCnB   (=)   :: heat flux conducted through the ice to bottom surface
C---  Modified (input&output):
C  icFrac(iceFrac):: fraction of grid area covered in ice
C  hIce    (hi)   :: ice height [m]
C  hSnow   (hs)   :: snow height [m]
C  tSrf    (Tsf)  :: surface (ice or snow) temperature
C  qIc1   (qicen) :: ice enthalpy (J/kg), 1rst level
C  qIc2   (qicen) :: ice enthalpy (J/kg), 2nd level
C  frwAtm (evpAtm):: evaporation to the atmosphere [kg/m2/s] (>0 if evaporate)
C  fzMlOc (frzmlt):: ocean mixed-layer freezing/melting potential [W/m2]
C  flx2oc (qleft) :: net heat flux to ocean    [W/m2]          (> 0 downward)
C---  Output
C  frw2oc (fresh) :: Total fresh water flux to ocean [kg/m2/s] (> 0 downward)
C  fsalt    (=)   :: salt flux to ocean        [g/m2/s]        (> 0 downward)
C---  Input:
C     myTime      :: current Time of simulation [s]
C     myIter      :: current Iteration number in simulation
C     myThid      :: my Thread Id number
      INTEGER siLo, siHi, sjLo, sjHi
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      LOGICAL dBugFlag
      _RL iceMask(siLo:siHi,sjLo:sjHi)
      _RL tFrz   (siLo:siHi,sjLo:sjHi)
      _RL tOce   (siLo:siHi,sjLo:sjHi)
      _RL v2oc   (siLo:siHi,sjLo:sjHi)
      _RL snowP  (siLo:siHi,sjLo:sjHi)
      _RL prcAtm (siLo:siHi,sjLo:sjHi)
      _RL sHeat  (siLo:siHi,sjLo:sjHi)
      _RL flxCnB (siLo:siHi,sjLo:sjHi)
      _RL icFrac (siLo:siHi,sjLo:sjHi)
      _RL hIce   (siLo:siHi,sjLo:sjHi)
      _RL hSnow  (siLo:siHi,sjLo:sjHi)
      _RL tSrf   (siLo:siHi,sjLo:sjHi)
      _RL qIc1   (siLo:siHi,sjLo:sjHi)
      _RL qIc2   (siLo:siHi,sjLo:sjHi)
      _RL frwAtm (siLo:siHi,sjLo:sjHi)
      _RL fzMlOc (siLo:siHi,sjLo:sjHi)
      _RL flx2oc (siLo:siHi,sjLo:sjHi)
      _RL frw2oc (siLo:siHi,sjLo:sjHi)
      _RL fsalt  (siLo:siHi,sjLo:sjHi)
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C     !LOCAL VARIABLES:
C---  local copy of input/output argument list variables (see description above)
      _RL  frzmlt
      _RL  Tf
      _RL oceTs, oceV2s, snowPr
      _RL sHeating
c     _RL flxCnB
c     _RL evpAtm
      _RL iceFrac
      _RL  hi
      _RL  hs
      _RL  Tsf
      _RL  qicen(nlyr)
      _RL qleft
      _RL fresh
c     _RL fsalt
C     == Local Variables ==
      INTEGER  i,j,k     ! loop indices
      _RL  rec_nlyr      ! reciprocal of number of ice layers (real value)
C     evap     :: evaporation over snow/ice [kg/m2/s] (>0 if evaporate)
C     Fbot     :: oceanic heat flux used to melt/form ice [W/m2]
      _RL  evap
      _RL  Fbot
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

C-    define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"

 1010 FORMAT(A,I3,3F8.3)
 1020 FORMAT(A,1P4E11.3)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
#endif /* ALLOW_AUTODIFF_TAMC */

      rec_nlyr = nlyr
      rec_nlyr = 1. _d 0 / rec_nlyr
      dt  = thSIce_deltaT

      DO j = jMin, jMax
       DO i = iMin, iMax
#ifdef ALLOW_AUTODIFF_TAMC
          ikey_1 = i
     &         + sNx*(j-1)
     &         + sNx*sNy*act1
     &         + sNx*sNy*max1*act2
     &         + sNx*sNy*max1*max2*act3
     &         + sNx*sNy*max1*max2*max3*act4
#endif /* ALLOW_AUTODIFF_TAMC */
C--
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE frwatm(i,j) = comlev1_thsice_1, key=ikey_1
CADJ STORE fzmloc(i,j) = comlev1_thsice_1, key=ikey_1
CADJ STORE hice(i,j) = comlev1_thsice_1, key=ikey_1
CADJ STORE hsnow(i,j) = comlev1_thsice_1, key=ikey_1
CADJ STORE icfrac(i,j) = comlev1_thsice_1, key=ikey_1
CADJ STORE qic1(i,j) = comlev1_thsice_1, key=ikey_1
CADJ STORE qic2(i,j) = comlev1_thsice_1, key=ikey_1
#endif

        IF (iceMask(i,j).GT.0. _d 0) THEN
          Tf      = tFrz(i,j)
          oceTs   = tOce(i,j)
          oceV2s  = v2oc(i,j)
          snowPr  = snowP(i,j)
c         prcAtm  = prcAtm(i,j)
          sHeating= sHeat(i,j)
c         flxCnB  = flxCnB(i,j)
          iceFrac = icFrac(i,j)
          hi      = hIce(i,j)
          hs      = hSnow(i,j)
          Tsf     = tSrf(i,j)
          qicen(1)= qIc1(i,j)
          qicen(2)= qIc2(i,j)
c         evpAtm  = frwAtm(i,j)
          frzmlt  = fzMlOc(i,j)
          qleft   = flx2oc(i,j)
c         fresh   = frw2oc(i,j)
c         fsalt   = fsalt(i,j)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     initialize energies
      esurp = 0. _d 0

      evap = frwAtm(i,j)

C......................................................................
C.. Compute growth and/or melting at the top and bottom surfaces.......
C......................................................................

      IF (frzmlt.GE. 0. _d 0) THEN
C     !-----------------------------------------------------------------
C     ! freezing conditions
C     !-----------------------------------------------------------------
C if higher than hThickIce, use all frzmlt energy to grow extra ice
        IF (hi.GT.hThickIce .AND. iceFrac.LE.iceMaskMax) THEN
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
         cpchr =cpWater*rhosw*bMeltCoef
         Fbot = cpchr*(Tf-oceTs)*ustar  ! < 0
         Fbot = max(Fbot,frzmlt)    ! frzmlt < Fbot < 0
         Fbot = min(Fbot,0. _d 0)
      ENDIF

C  mass of fresh water and salt initially present in ice
      mwater0 = rhos*hs + rhoi*hi
      msalt0  = rhoi*hi*saltIce

#ifdef ALLOW_DBUG_THSICE
      IF (dBug(i,j,bi,bj) ) WRITE(6,1020) 
     &  'ThSI_CALC_TH: evpAtm, frzmlt, Fbot =', frwAtm(i,j),frzmlt,Fbot
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C Compute energy available for melting/growth.

      IF (hi.LT.hThinIce) THEN
C below a certain height, all energy goes to changing ice extent
       frace=1. _d 0
      ELSE
       frace=fracEnMelt
      ENDIF
      IF (hi.GT.hThickIce) THEN
C above certain height only melt from top
       frace=0. _d 0
      ELSE
       frace=fracEnMelt
      ENDIF
C force this when no ice fractionation
      IF (fracEnMelt.EQ.0. _d 0) frace=0. _d 0

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
      ebot = (flxCnB(i,j)-Fbot) * dt
      IF (ebot.GT.0. _d 0) THEN
         ebote = frace*ebot
         ebot  = ebot-ebote
      ELSE
         ebote = 0. _d 0
      ENDIF
#ifdef ALLOW_DBUG_THSICE
      IF (dBug(i,j,bi,bj) ) WRITE(6,1020) 
     &  'ThSI_CALC_TH: etop,etope,ebot,ebote=', etop,etope,ebot,ebote
#endif

C Initialize layer thicknesses.
C Make sure internal ice temperatures do not exceed Tmlt.
C If they do, then eliminate the layer.  (Dont think this will happen
C for reasonable values of i0.)

      hlyr = hi * rec_nlyr
      DO k = 1, nlyr
         hnew(k) = hlyr
      ENDDO

C Top melt: snow, then ice.

      IF (etop .GT. 0. _d 0) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE etop = comlev1_thsice_1, key=ikey_1
#endif
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
#ifdef ALLOW_AUTODIFF_TAMC
          ikey_2 = k
     &         + nlyr*(i-1)
     &         + nlyr*sNx*(j-1)
     &         + nlyr*sNx*sNy*act1
     &         + nlyr*sNx*sNy*max1*act2
     &         + nlyr*sNx*sNy*max1*max2*act3
     &         + nlyr*sNx*sNy*max1*max2*max3*act4
#endif
C--
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE etop = comlev1_thsice_2, key=ikey_2
CADJ STORE hnew(k) = comlev1_thsice_2, key=ikey_2
#endif
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
         qbot =  -cpIce *Tf + Lfresh
         dhi = -ebot / (qbot * rhoi)
         ebot = 0. _d 0
cph         k = nlyr
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hnew(:) = comlev1_thsice_1, key=ikey_1
CADJ STORE qicen(:) = comlev1_thsice_1, key=ikey_1
#endif
         qicen(nlyr) = (hnew(nlyr)*qicen(nlyr)+dhi*qbot) / 
     &        (hnew(nlyr)+dhi)
         hnew(nlyr) = hnew(nlyr) + dhi
      ELSE
         DO k = nlyr, 1, -1
#ifdef ALLOW_AUTODIFF_TAMC
          ikey_2 = (nlyr-k+1)
     &         + nlyr*(i-1)
     &         + nlyr*sNx*(j-1)
     &         + nlyr*sNx*sNy*act1
     &         + nlyr*sNx*sNy*max1*act2
     &         + nlyr*sNx*sNy*max1*max2*act3
     &         + nlyr*sNx*sNy*max1*max2*max3*act4
#endif
C--
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ebot = comlev1_thsice_2, key=ikey_2
CADJ STORE hnew(k) = comlev1_thsice_2, key=ikey_2
CADJ STORE qicen(k) = comlev1_thsice_2, key=ikey_2
#endif
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
#ifdef ALLOW_DBUG_THSICE
      IF (dBug(i,j,bi,bj) ) WRITE(6,1020) 
     &  'ThSI_CALC_TH:   etop, ebot, hi, hs =', etop, ebot, hi, hs
#endif

C If hi < hIceMin, melt the ice.
      IF ( hi.LT.hIceMin .AND. (hi+hs).GT.0. _d 0 ) THEN
         esurp = esurp - rhos*qsnow*hs
         DO k = 1, nlyr
            esurp = esurp - rhoi*qicen(k)*hnew(k)
         ENDDO
         hi = 0. _d 0
         hs = 0. _d 0
         Tsf=0. _d 0
         iceFrac = 0. _d 0
         DO k = 1, nlyr
           qicen(k) = 0. _d 0
         ENDDO
#ifdef ALLOW_DBUG_THSICE
        IF (dBug(i,j,bi,bj) ) WRITE(6,1020) 
     &    'ThSI_CALC_TH: -1 : esurp=',esurp
#endif
      ENDIF

C--   do a mass-budget of sea-ice to compute "fresh" = the fresh-water flux
C     that is returned to the ocean ; needs to be done before snow/evap
      fresh = (mwater0 - (rhos*hs + rhoi*hi))/dt

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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE evap = comlev1_thsice_1, key=ikey_1
CADJ STORE hs = comlev1_thsice_1, key=ikey_1
#endif
       IF (hs.GT.0. _d 0) THEN
         IF (evap/rhos *dt.GT.hs) THEN
           evap=evap-hs*rhos/dt
           hs=0. _d 0
         ELSE
           hs = hs - evap/rhos *dt
           evap=0. _d 0
         ENDIF
       ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE evap = comlev1_thsice_1, key=ikey_1
CADJ STORE hi = comlev1_thsice_1, key=ikey_1
CADJ STORE hnew(:) = comlev1_thsice_1, key=ikey_1
CADJ STORE qicen(:) = comlev1_thsice_1, key=ikey_1
#endif
       IF (hi.GT.0. _d 0.AND.evap.GT.0. _d 0) THEN
         DO k = 1, nlyr
#ifdef ALLOW_AUTODIFF_TAMC
          ikey_2 = k
     &         + nlyr*(i-1)
     &         + nlyr*sNx*(j-1)
     &         + nlyr*sNx*sNy*act1
     &         + nlyr*sNx*sNy*max1*act2
     &         + nlyr*sNx*sNy*max1*max2*act3
     &         + nlyr*sNx*sNy*max1*max2*max3*act4
#endif
C--
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE evap = comlev1_thsice_2, key=ikey_2
CADJ STORE hnew(k) = comlev1_thsice_2, key=ikey_2
CADJ STORE qicen(k) = comlev1_thsice_2, key=ikey_2
#endif
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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hnew(k) = comlev1_thsice_2, key=ikey_2
#endif
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

C If hi < hIceMin, melt the ice.
       IF ( hi.GT.0. _d 0 .AND. hi.LT.hIceMin ) THEN
         fresh = fresh + (rhos*hs + rhoi*hi)/dt
         esurp = esurp - rhos*qsnow*hs
         DO k = 1, nlyr
            esurp = esurp - rhoi*qicen(k)*hnew(k)
         ENDDO
         hi = 0. _d 0
         hs = 0. _d 0
         Tsf=0. _d 0
         iceFrac = 0. _d 0
         DO k = 1, nlyr
           qicen(k) = 0. _d 0
         ENDDO
#ifdef ALLOW_DBUG_THSICE
         IF (dBug(i,j,bi,bj) ) WRITE(6,1020) 
     &    'ThSI_CALC_TH: -2 : esurp,fresh=', esurp, fresh
#endif
       ENDIF

C-    else hi > 0: end
      ENDIF

      IF ( hi .GT. 0. _d 0 ) THEN

C If there is enough snow to lower the ice/snow interface below
C freeboard, convert enough snow to ice to bring the interface back
C to sea-level.  Adjust enthalpy of top ice layer accordingly.

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hi = comlev1_thsice_1, key=ikey_1
CADJ STORE hs = comlev1_thsice_1, key=ikey_1
CADJ STORE hnew(:) = comlev1_thsice_1, key=ikey_1
CADJ STORE qicen(:) = comlev1_thsice_1, key=ikey_1
#endif
       IF ( hs .GT. hi*rhoiw/rhos ) THEN
cBB               WRITE (6,*)  'Freeboard adjusts'
         dhi = (hs * rhos - hi * rhoiw) / rhosw
         dhs = dhi * rhoi / rhos
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hnew(:) = comlev1_thsice_1, key=ikey_1
#endif
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

#ifdef ALLOW_DBUG_THSICE
       IF (dBug(i,j,bi,bj) ) WRITE(6,1020) 
     &  'ThSI_CALC_TH: b-Winton: hnew, qice =', hnew, qicen
#endif

       hlyr = hi * rec_nlyr
       CALL THSICE_RESHAPE_LAYERS(
     U                            qicen,
     I                            hlyr, hnew, myThid )

#ifdef ALLOW_DBUG_THSICE
       IF (dBug(i,j,bi,bj) ) WRITE(6,1020)
     &  'ThSI_CALC_TH: iceFrac,hi, qtot, hs =', iceFrac,hi,
     &                 (qicen(1)+qicen(2))*0.5, hs
#endif

C-    if hi > 0 : end
      ENDIF
 200  CONTINUE

C-  Compute surplus energy left over from melting.

      IF (hi.LE.0. _d 0) iceFrac=0. _d 0

C.. heat fluxes left over for ocean
       qleft = qleft + (Fbot+(esurp+etop+ebot)/dt)
#ifdef ALLOW_DBUG_THSICE
       IF (dBug(i,j,bi,bj) ) WRITE(6,1020) 
     &  'ThSI_CALC_TH: [esurp,etop+ebot]/dt =',esurp/dt,etop/dt,ebot/dt
#endif

C-- Evaporation left to the ocean :
       fresh = fresh - evap
C-  Correct Atmos. fluxes for this different latent heat:
C   evap was computed over freezing surf.(Tsf<0), latent heat = Lvap+Lfresh
C   but should be Lvap only for the fraction "evap" that is left to the ocean.
       qleft = qleft + evap*Lfresh

C fresh and salt fluxes
c     fresh = (mwater0 - (rhos*(hs) + rhoi*(hi)))/dt-evap
c     fsalt = (msalt0 - rhoi*hi*saltIce)/35. _d 0/dt  ! for same units as fresh
C note (jmc): fresh is computed from a sea-ice mass budget that already
C    contains, at this point, snow & evaporation (of snow & ice)
C    but are not meant to be part of ice/ocean fresh-water flux.
C  fix: a) like below or b) by making the budget before snow/evap is added
c     fresh = (mwater0 - (rhos*(hs) + rhoi*(hi)))/dt
c    &      + snow(i,j,bi,bj)*rhos - frwAtm
      fsalt(i,j) = (msalt0 - rhoi*hi*saltIce)/dt

#ifdef ALLOW_DBUG_THSICE
      IF (dBug(i,j,bi,bj) ) THEN
        WRITE(6,1020)'ThSI_CALC_TH:dH2O,Ev[kg],fresh,fsalt',
     &   (mwater0-(rhos*hs+rhoi*hi))/dt,evap,fresh,fsalt(i,j)
        WRITE(6,1020)'ThSI_CALC_TH: Qleft,Fbot,extend/dt =',
     &            Qleft,Fbot,(etope+ebote)/dt
      ENDIF
#endif

C-- add remaining liquid Precip (rain+RunOff) directly to ocean:
      fresh = fresh + (prcAtm(i,j)-snowPr)

C-- note: at this point, iceFrac has not been changed (unless reset to zero)
C      and it can only be reduced by lateral melting in the following part:

C calculate extent changes
      extend=etope+ebote
      IF (iceFrac.GT.0. _d 0.AND.extend.GT.0. _d 0) THEN
         rq =  rhoi * 0.5 _d 0*(qicen(1)+qicen(2))
         rs =  rhos * qsnow
         rqh = rq * hi + rs * hs
         freshe=(rhos*hs+rhoi*hi)/dt
         salte=(rhoi*hi*saltIce)/dt
         IF (extend .LT. rqh) THEN
           iceFrac=(1. _d 0-extend/rqh)*iceFrac
           fresh=fresh+extend/rqh*freshe
           fsalt(i,j)=fsalt(i,j)+extend/rqh*salte
         ELSE
           iceFrac=0. _d 0
           hi=0. _d 0
           hs=0. _d 0
           qleft=qleft+(extend-rqh)/dt
           fresh=fresh+freshe
           fsalt(i,j)=fsalt(i,j)+salte
         ENDIF
      ELSEIF (extend.GT.0. _d 0) THEN
         qleft=qleft+extend/dt
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Update output variables :

C-  Diagnostic of Atmos. fresh water flux (E-P) over sea-ice :
C     substract precip from Evap (<- stored in frwAtm array)
          frwAtm(i,j) = frwAtm(i,j) - prcAtm(i,j)

C-  update Mixed-Layer Freezing potential heat flux by substracting the
C    part which has already been accounted for (Fbot):
          fzMlOc(i,j) = frzmlt - Fbot*iceMask(i,j)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef ALLOW_DBUG_THSICE
       IF (dBug(i,j,bi,bj) ) WRITE(6,1020) 
     &  'ThSI_CALC_TH: iceFrac,flx2oc,fsalt,frw2oc=',
     &   iceFrac, qleft, fsalt(i,j), fresh
#endif
#ifdef CHECK_ENERGY_CONSERV
          CALL THSICE_CHECK_CONSERV( dBugFlag, i, j, bi, bj, 0,
     I            iceMask(i,j), iceFrac, hi, hs, qicen,
     I            qleft, fresh, fsalt,
     I            myTime, myIter, myThid )
#endif /* CHECK_ENERGY_CONSERV */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Update Sea-Ice state output:
          icFrac(i,j) = iceFrac
          hIce(i,j)   = hi
          hSnow(i,j ) = hs
          tSrf(i,j)   = Tsf
          qIc1(i,j)   = qicen(1)
          qIc2(i,j)   = qicen(2)
C-- Update oceanic flux output:
          flx2oc(i,j) = qleft
          frw2oc(i,j) = fresh
c         fsalt(i,j)  = fsalt
        ENDIF
       ENDDO
      ENDDO
#endif  /* ALLOW_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
