#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_seaicev4(myThid)

c     ==================================================================
c     SUBROUTINE cost_gencost_seaicev4
c     ==================================================================
c
c     o Evaluate cost function contributions of ice concentration.
c
c     ==================================================================
c     SUBROUTINE cost_gencost_seaicev4
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE_COST.h"
# include "SEAICE_PARAMS.h"
#endif

c     == routine arguments ==
      integer myThid

#ifdef ALLOW_SEAICE
#ifdef ALLOW_GENCOST_CONTRIBUTION

c     == local variables ==

      integer nnzsiv4, nnzbar
      parameter (nnzsiv4 = 1 , nnzbar = 1)
      integer nrecloc
      integer localstartdate(4)

catn changing names to make more self-explanatory
c old:sst  -> model has deficiency in iceconc -> new:deconc
c old:heff -> model has excess of iceconc     -> new:exconc

      _RL areabar    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL thetabar  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL heffbar  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localweight  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
#ifdef READIN_EXCONC_DECONC_SIGMA
      _RL localweight_exconc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localweight_deconc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
#endif
      _RL dummy
      _RL localperiod
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc

      character*(MAX_LEN_FNAM) areabarfile
      character*(MAX_LEN_FNAM) thetabarfile
      character*(MAX_LEN_FNAM) heffbarfile
      character*(MAX_LEN_FNAM) localobsfile

      integer igen_conc, igen_deconc, igen_exconc

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec, jrec
      integer  il, k2
      integer localrec
      integer obsrec
      logical dosumsq, dovarwei

      integer preproc_i(NGENPPROC)
      _RL preproc_r(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc_c(NGENPPROC)

      _RL localmask  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)

      _RL localobs   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzsiv4,nSx,nSy)
      _RL localtmp   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzsiv4,nSx,nSy)
      _RL localdif   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzsiv4,nSx,nSy)
      _RL difmask    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzsiv4,nSx,nSy)
      _RL difmask1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzsiv4,nSx,nSy)

      character*(128) fname0, fname0w, fname1

      character*(MAX_LEN_FNAM) localobswfile
      logical exst

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c=============== PART 0: initilization ===================

c-- detect the relevant gencost indices
      igen_conc=0
      igen_deconc=0
      igen_exconc=0
      do k=1,NGENCOST
        if (gencost_name(k).EQ.'siv4-conc') igen_conc=k
        if (gencost_name(k).EQ.'siv4-deconc') igen_deconc=k
        if (gencost_name(k).EQ.'siv4-exconc') igen_exconc=k
      enddo

c-- Dependency:
c A) igen_conc can exist on its own
c B) igen_deconc needs igen_conc
c C) igen_exconc needs both igen_conc and igen_deconc
      if (igen_conc.NE.0) then

c-- initialize objf and num:
      do bj = jtlo,jthi
        do bi = itlo,ithi
          objf_gencost(bi,bj,igen_conc) = 0. _d 0
          num_gencost(bi,bj,igen_conc)  = 0. _d 0
          if(igen_deconc.ne.0) then
            objf_gencost(bi,bj,igen_deconc) = 0. _d 0
            num_gencost(bi,bj,igen_deconc)  = 0. _d 0
          endif
          if(igen_exconc.ne.0) then
            objf_gencost(bi,bj,igen_exconc) = 0. _d 0
            num_gencost(bi,bj,igen_exconc)  = 0. _d 0
          endif
        enddo
      enddo

c--   Initialise local variables.
      nrecloc=0
      localperiod=0.

      areabarfile=gencost_barfile(igen_conc)
      if(igen_deconc.ne.0) thetabarfile=gencost_barfile(igen_deconc)
      if(igen_exconc.ne.0) heffbarfile=gencost_barfile(igen_exconc)

      localobsfile=gencost_datafile(igen_conc)
      localobswfile=gencost_errfile(igen_conc)
      dummy=gencost_dummy(igen_conc)
      localstartdate(1)=modelstartdate(1)
      localstartdate(2)=modelstartdate(2)
      localstartdate(3)=modelstartdate(3)
      localstartdate(4)=modelstartdate(4)
      spminloc=gencost_spmin(igen_conc)
      spmaxloc=gencost_spmax(igen_conc)
      spzeroloc=gencost_spzero(igen_conc)

      localperiod=gencost_period(igen_conc)
      nrecloc=gencost_nrec(igen_conc)

c-- flag to add cost: true=(obs-mod)*(obs-mod)*weight
      dosumsq=.TRUE.
      dovarwei=.FALSE.
      do k2 = 1, NGENPPROC
        preproc(k2)=gencost_preproc(k2,igen_conc)
        preproc_i(k2)=gencost_preproc_i(k2,igen_conc)
        preproc_c(k2)=gencost_preproc_c(k2,igen_conc)
        preproc_r(k2)=gencost_preproc_r(k2,igen_conc)
        if (preproc(k2).EQ.'variaweight') dovarwei=.TRUE.
        if (preproc(k2).EQ.'nosumsq') dosumsq=.FALSE.
      enddo

c-- initialize arrays, copy maskC to localmask
      call ecco_zero(localobs,1,spzeroloc,myThid)
      call ecco_zero(localweight,1,zeroRL,myThid)
#ifdef READIN_EXCONC_DECONC_SIGMA
      call ecco_zero(localweight_exconc,1,zeroRL,myThid)
      call ecco_zero(localweight_deconc,1,zeroRL,myThid)
#endif
      call ecco_zero(localmask,Nr,zeroRL,myThid)
      call ecco_cprsrl(maskC,Nr,localmask,Nr,myThid)

c=============== PART 1: main loop ===================
      if ( .NOT. ( localobsfile.EQ.' ' ) ) then

c--   Loop over records for the second time.
      do irec = 1, nrecloc

c====================================================
c--------- PART 1.1 read weights --------------------
c====================================================
        exst=.FALSE.
        jrec=1
        if( dovarwei ) jrec = irec
        call cost_gencal(areabarfile,gencost_errfile(igen_conc),
     &     jrec, localstartdate, localperiod, fname1,
     &     fname0w, localrec, obsrec, exst, myThid)
        call ecco_zero(localweight,nnzsiv4,zeroRL,myThid)
#ifdef READIN_EXCONC_DECONC_SIGMA
        call ecco_zero(localweight_exconc,nnzsiv4,zeroRL,myThid)
        call ecco_zero(localweight_deconc,nnzsiv4,zeroRL,myThid)
#endif
#ifdef SEAICECOST_JPL
       fname0w=gencost_errfile(igen_conc)
       call ecco_readwei(fname0w,localweight,localrec,
     &      nnzsiv4,myThid)
       call ecco_readwei(gencost_errfile(igen_deconc),
     &      gencost_weight(1,1,1,1,igen_deconc),localrec,
     &      nnzsiv4,myThid)
       call ecco_readwei(gencost_errfile(igen_exconc),
     &      gencost_weight(1,1,1,1,igen_exconc),localrec,
     &      nnzsiv4,myThid)
#else
        if ( (localrec. GT. 0).AND.(obsrec .GT. 0).AND.(exst) ) then
          call ecco_readwei(fname0w,localweight,localrec,
     &      nnzsiv4,myThid)
#ifdef READIN_EXCONC_DECONC_SIGMA
       call ecco_readwei(gencost_errfile(igen_deconc),
     &      localweight_deconc,localrec,
     &      nnzsiv4,myThid)
       call ecco_readwei(gencost_errfile(igen_exconc),
     &      localweight_exconc,localrec,
     &      nnzsiv4,myThid)
#endif
        else
          WRITE(standardMessageUnit,'(A)')
     &     'siv4cost WARNING: ALL WEIGHTS ZEROS! NO CONTRIBUTION'
        endif
#endif

c====================================================
c--------- PART 1.2 read barfiles ------------------
c====================================================
c-- set all bars to zeros:
        call ecco_zero(areabar,nnzbar,zeroRL,myThid)
        call ecco_zero(thetabar,nnzbar,zeroRL,myThid)
        call ecco_zero(heffbar, nnzbar,zeroRL,myThid)

c--1.2.A sea-ice concentration barfile
        exst=.FALSE.
        call cost_gencal(areabarfile,gencost_datafile(igen_conc),
     &   irec,localstartdate,localperiod,fname1,
     &   fname0,localrec,obsrec,exst,myThid)
CADJ INCOMPLETE areabar
        call cost_genread(fname1,areabar,localtmp,irec,nnzbar,
     &       nrecloc,preproc,preproc_c,preproc_i,preproc_r,
     &       dummy,myThid)

c--1.2.B sst as proxy for deconc barfile, needs igen_conc
        if(igen_deconc.ne.0) then
         exst=.FALSE.
         call cost_gencal(thetabarfile,gencost_datafile(igen_conc),
     &    irec,localstartdate,localperiod,fname1,
     &    fname0,localrec,obsrec,exst,myThid)
         call cost_genread(fname1,thetabar,localtmp,irec,nnzbar,
     &       nrecloc,preproc,preproc_c,preproc_i,preproc_r,
     &       dummy,myThid)
        endif

c--1.2.C heff as proxy for exconc barfile, need igen_conc and igen_exconc
        if(igen_deconc.ne.0 .and. igen_exconc.ne.0) then
         exst=.FALSE.
         call cost_gencal(heffbarfile,gencost_datafile(igen_conc),
     &    irec,localstartdate,localperiod,fname1,
     &    fname0,localrec,obsrec,exst,myThid)
         call cost_genread(fname1,heffbar,localtmp,irec,nnzbar,
     &       nrecloc,preproc,preproc_c,preproc_i,preproc_r,
     &       dummy,myThid)
        endif

c====================================================
c--------- PART 1.3 read data --------------------
c====================================================
c-- initialize to spzerloc = -9999.
        call ecco_zero(localobs,nnzsiv4,spzeroloc,myThid)
        if ( (localrec .GT. 0).AND.(obsrec .GT. 0).AND.(exst) ) then
         CALL READ_REC_3D_RL( fname0, cost_iprec, nnzsiv4,
     &                        localobs, localrec, 0, myThid )
        else
          il=ilnblnk( fname0 )
          WRITE(standardMessageUnit,'(2A)')
     &     'siv4cost WARNING: DATA MISING! NO CONTRIBUTION, ',
     &     fname0(1:il)
        endif

c====================================================
c--------- PART 1.4 Cost calculation -------------
c====================================================
c compute obs minus bar (localdif) and mask (difmask)
        call ecco_zero(localdif,nnzsiv4,zeroRL,myThid)
        call ecco_zero(difmask,nnzsiv4,zeroRL,myThid)
        call ecco_diffmsk(
     I     areabar, nnzbar, localobs, nnzsiv4, localmask,
     I     spminloc, spmaxloc, spzeroloc,
     O     localdif, difmask,
     I     myThid )

c---1.4.A area term:
        call ecco_addcost(
     I     localdif,localweight,difmask,nnzsiv4,dosumsq,
     O     objf_gencost(1,1,igen_conc),num_gencost(1,1,igen_conc),
     I     myThid)

c---1.4.B defficient ice term: (old: sst term, new: deconc)
c Add ice: model_A==0 but obs_A > 0, calc enthalpy E:
        if(igen_deconc.ne.0) then
         call ecco_zero(difmask1,nnzsiv4,zeroRL,myThid)
         call ecco_zero(localdif,nnzsiv4,zeroRL,myThid)
         call ecco_zero(localtmp,nnzsiv4,zeroRL,myThid)
#ifdef READIN_EXCONC_DECONC_SIGMA
         call ecco_cp(localweight_deconc,nnzsiv4,
     O        localtmp,nnzsiv4,myThid)
#endif
         call get_exconc_deconc(
     I    localobs,nnzsiv4,areabar,heffbar,thetabar,nnzbar,
     I    difmask,'de',
     O    localdif,difmask1,localtmp,
     I    myThid )
#ifdef SEAICECOST_JPL
        call ecco_cp(gencost_weight(1,1,1,1,igen_deconc),nnzsiv4,
     O       localtmp,nnzsiv4,myThid)
#endif
        call ecco_addcost(
     I      localdif,localtmp,difmask1,nnzsiv4,dosumsq,
     O      objf_gencost(1,1,igen_deconc),num_gencost(1,1,igen_deconc),
     I      myThid)
        endif

c---1.4.C excessive ice term:  (old: heff and sst term, new: exconc)
c Removing ice: model_A > 0 but obs_A==0, calc enthalpy E:
        if(igen_deconc.ne.0 .and. igen_exconc.ne.0) then
         call ecco_zero(difmask1,nnzsiv4,zeroRL,myThid)
         call ecco_zero(localdif,nnzsiv4,zeroRL,myThid)
         call ecco_zero(localtmp,nnzsiv4,zeroRL,myThid)
#ifdef READIN_EXCONC_DECONC_SIGMA
         call ecco_cp(localweight_exconc,nnzsiv4,
     O        localtmp,nnzsiv4,myThid)
#endif
         call get_exconc_deconc(
     I    localobs,nnzsiv4,areabar,heffbar,thetabar,nnzbar,
     I    difmask,'ex',
     O    localdif,difmask1,localtmp,
     I    myThid )
#ifdef SEAICECOST_JPL
        call ecco_cp(gencost_weight(1,1,1,1,igen_exconc),nnzsiv4,
     O       localtmp,nnzsiv4,myThid)
#endif
        call ecco_addcost(
     I      localdif,localtmp,difmask1,nnzsiv4,dosumsq,
     O      objf_gencost(1,1,igen_exconc),num_gencost(1,1,igen_exconc),
     I      myThid)
        endif

      enddo

      endif !if ( .NOT. ( localobsfile.EQ.' ' ) ) then
      endif !if (igen_conc.NE.0)

#endif /* ALLOW_GENCOST_CONTRIBUTION */
#endif /* ALLOW_SEAICE */

      RETURN
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "ECCO_OPTIONS.h"

      subroutine get_exconc_deconc(
     I    localobs,nnzobs,concbar,heffbar,thetabar,nnzbar,
     I    localmask,flag_exconc_deconc,
     O    localfld,localfldmsk,localfldweight,
     I    myThid )

C     !DESCRIPTION: \bv
c     Routine to calculate Enthalpy for the case of
c     defficient/excessive model seaice
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE_COST.h"
# include "SEAICE_PARAMS.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      integer myThid, nnzbar, nnzobs
      _RL localmask     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)

      _RL localobs      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL concbar       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL thetabar      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL heffbar       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localfld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localfldmsk   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localfldweight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)

      character*2 flag_exconc_deconc

#ifdef ALLOW_GENCOST_CONTRIBUTION
#ifdef ALLOW_SEAICE

c    == local variables ==
      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer i,j,k

C- atn: lastly, define 2 cutoff values for cost to be read in from data.seaice
C      and initialized in seaice_readparms: SEAICE_cutoff_[area,heff]
C      Reason: some iceconc data set have "bogus" mask with area>0
C      at climatological max locations -> not real data.  So either need
C      to clean up the data or take SEAICE_cutoff_area>=0.15 for example.
C      Might need to migrate into pkg/ecco instead of in pkg/seaice.

      _RL SEAICE_freeze, epsilonTemp, epsilonHEFF
      _RL localnorm, localnormsq
      _RL beta_T, delta_hi, const0, const1, epsilon_h, epsilon_T
      _RL heff_local
CEOP

cigf  nominal seawater freezing point, needed for energy calculation
cigf  because we define 0 energy as ocean grid cell without ice at T=T_f
cigf  T_f = seawater freezing point, SEAICE_freeze
      SEAICE_freeze  = -1.96  _d 0

cigf  target ice thickness for deconc proxy term
cigf  a value of 0.15 m is safe to ensure that the gradient of the 
cigf  deconc proxy cost term with respect to temperature is positive
cigf  when drF(1) = 10 m.
      epsilon_h = 0.15 _d 0
      
cigf  target seawater temperature for exconc proxy term  
      epsilon_T = 1 _d 0
      
      
cigf  hard-coded uncertainty for exconc and deconc proxy terms
cigf  costs are formulated in terms of ocean temperature,
cigf  so uncertainty is also in terms of temperature
cigf  a reasonable value is
cigf  \sigma_T = 0.1 degrees Celsius
cigf  localnorm = 1/\sigma_T = 10 C^-1
      localnorm = 10 _d 0 

cigf  localnormsq is weight [1/ sigma_t]^2
      localnormsq=localnorm*localnorm

cigf  beta_T is a constant: the ocean temperature per unit 
cigf  ocean depth required to melt sea-ice of equivalent thickness
cigf  [J kg-1][kg m-3] / ([J kg-1 K-1] [kg m-3]) --> K
cigf  numerically, about 73 degrees Celsius
      beta_T = SEAICE_lhFusion*SEAICE_rhoIce / (HeatCapacity_Cp*rhoNil)
      
cigf  temperature of uppermost ocean grid cell of thickness drF(1)
cigf  required to melt \epsilon_h meters of sea-ice.  
cigf  suggested to make const1 about 1 degree Celsius to ensure
cigf  the gradient of deconc cost term with respect to temperature is 
cigf  positive with respect to ocean temperature.
cigf  note: since all of these terms are constants, we could also just set
cigf  const1 = 1 and be done with it.
      const0 = beta_T / drF(1)
      const1 = const0 * epsilon_h
      
      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1-OLy
      jmax = sNy+OLy
      imin = 1-OLx
      imax = sNx+OLx

c intialize
      call ecco_zero(localfld,nnzobs,zeroRL,myThid)
      call ecco_zero(localfldmsk,nnzobs,zeroRL,myThid)
#ifndef READIN_EXCONC_DECONC_SIGMA
      call ecco_zero(localfldweight,nnzobs,zeroRL,myThid)
#endif

c----------------------DECONC-------------------------------

c Proxy sea-ice term for the case when observations show 
c sea-ice and the model shows no sea-ice.

c goal is to make penalty the same order of magnitude 
c as an SST misfit.  Cost will be formulated in terms of 
c thermal energy required to reduce seawater temperature 
c to the freezing point and then freeze $\epsilon_h$ meters 
c starting from the current condition of zero ice and 
c seawater temperatures, T_ocn, at or above the 
c freezing point, T_f.

c We expect that the adjoint model forcing from deconc
c will be **positive** to ad_theta -- a positive 
c perturbation to ocean temperatures should increase
c the cost function (we're trying to modify the control
c parameters so that ice forms and to do that we want to
c decrease ocean temperatures).
c To decrease the cost function, a negative ocean temperature
c perturbation is required.

c needs localconcbar and thetabar

c the deconc cost function can be written in terms of a 
c penalty between a) the current energy of the uppermost ocean 
c grid cell plus the sea ice in that grid cell 
c and b) a target energy, scaled by some weight:
c 
c   J_deconc = \sigma_U^-2 (U_current - U_target)^2
c
c the current energy in ocean upermost grid cell + sea-ice:
c
c   U_current = (T_ocn -T_f) c_p \rho_sw \Delta z
c
c with c_p      : seawater heat capacity, HeatCapacity_Cp
c      \Delta z : uppermost ocean grid cell thickness, drF(1)
c
c the target energy is seawater at freezing point, T_ocn - T_f = 0,
c and some nonzero sea-ice thickness:
c
c   U_target  = -(epsilon_h L_i \rho_ice)
c  
c with L_i: sea-ice latent heat of fusion, SEAICE_lhFusion
c
c we can reframe deconc in terms of upper ocean grid cell 
c temperature by defining the energy uncertainty as
c
c  \sigma_E = c_p \rho_sw \Delta z \sigma_T
c
c  which then gives us,
c
c    J_deconc = \sigma_T^-2 [T_ocn - T_f + \beta_T \Delta z^-1 \epsilon_h]
c
c  with \beta_T = L_i \rho_i / (c_p \rho_sw)
c
c  check the gradient w.r.t. T_ocn:
c  ================================
c
c  \partial_T J_deconc = \sigma_T^-2 x 2 x [T_ocn - T_f + c1][1]
c
c  [T_ocn -T_f + c1] > 0 so long as c1 > T_ocn - T_f
c  --> the gradient is positive with respect to ocean temperature,
c      as expected
c 
c  Note: normally T_ocn - T_f > 0 because the seawater temperature 
c  is above the freezing point.  However, T_ocn can sometimes fall
c  below the freezing point for numerical or other reasons 
c  so choose c1 to be large enough to handle those events
c  using c1 = 1 degree Celsius is a safe bet.

      if(flag_exconc_deconc.EQ.'de') then
        do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nnzobs
            do j = jmin,jmax
             do i = imin,imax

cigf          test for deconc: model has no ice (concbar .le. 0)
c             but observation has nonzero ice (localobs  .gt. 0)
              if ( (concbar(i,j,k,bi,bj) .LE. 0.).AND.
     &             (localobs(i,j,k,bi,bj) .GT. 0.) ) then

               localfldmsk(i,j,k,bi,bj) = localmask(i,j,k,bi,bj)

#ifndef READIN_EXCONC_DECONC_SIGMA
cigf           weight is 1/sigma_T^2, defined above localnormsq
c              suggested value is 100 1/degreesC^2 
               localfldweight(i,j,k,bi,bj) = localnormsq
#endif

cigf           J = [T_ocn - T_f + \beta_T \Delta z^-1 \ep_h]^2 sigma_T^-2
c              J = [T_ocn - T_f + const1]^2 sigma_T^-2
c              localfld is the [T_o - T_f + const1] term
               localfld(i,j,k,bi,bj) =
     &           thetabar(i,j,k,bi,bj) - SEAICE_freeze + const1

              endif
             enddo
            enddo
           enddo
          enddo
        enddo
      endif

c----------------------EXCONC-------------------------------

c Proxy sea-ice term for the case when observations show 
c no sea-ice and the model shows some sea-ice.

c goal is to make penalty the same order of magnitude 
c as an SST misfit.  Cost will be formulated in terms of 
c thermal energy required to melt the ice and raise
c seawater temperature to some target \epsilon_T
c degrees Celsius above the freezing point, T_f.

c We expect that the adjoint model forcing from exconc
c will be **negative** to ad_theta -- a positive 
c perturbation to ocean temperatures should decrease
c the cost function (we're trying to modify the control
c parameters to eliminate existing ice). 
c
c In addition, we expect the adjoint model forcing from 
c exconc will be **positive** to heff because a positive 
c perturbation to heff would exacerbate
c our problem (we're trying to eliminate existing ice)

c needs localconcbar, thetabar, and heffbar

c the exconc cost function can be written in terms of a 
c penalty between a) the current energy of the uppermost ocean 
c grid cell plus the sea ice in that grid cell 
c and b) a target energy, scaled by some weight:
c 
c   J_exconc = \sigma_U^-2 (U_current - U_target)^2
c
c the current energy in ocean upermost grid cell + sea-ice:
c
c   U_current = (T_ocn -T_f) c_p \rho_sw \Delta z 
c                 - h_i L_i \rho_ice
c
c with c_p      : seawater heat capacity, HeatCapacity_Cp
c      \Delta z : uppermost ocean grid cell thickness, drF(1)
c      h_i      : sea ice thickness, heff
c      L_i      : sea-ice latent heat of fusion, SEAICE_lhFusion

c the target energy is seawater \epsilon_T degrees above
c the freezing point and zero sea-ice thickness:
c
c   U_target  =  \epsilon_T c_p \rho_sw \Delta z 
c  
c
c we can reframe exconc in terms of upper ocean grid cell 
c temperature by defining the energy uncertainty as
c
c  \sigma_E = c_p \rho_sw \Delta z \sigma_T
c
c  which then gives us,
c
c    J_exconc = \sigma_T^-2 [-\beta_T \Delta z^-1 h_i + T_ocn - T_f - \ep_T]^2
c
c  after factoring out constants and using
c  \beta_T = L_i \rho_i / (c_p \rho_sw)
c
c  check the gradient w.r.t. T_ocn:
c  ================================
c
c  \partial_T J_deconc = \sigma_T^-2 x 2 x 
c                      [-\beta_T \Delta z^-1 h_i + T_ocn - T_f - \ep_T] x 
c                      [-\beta_T \Delta z^-1 \partial_T h_i + 1]
c
c  consider terms in []
c  1. -\beta_T \Delta z^-1 h_i < 0 for all h_i
c  2. T_ocn - T_f -\epsilon_T  < 0 so long as \epsilon_T > T_ocn - T_f
c  3. -\beta_T \Delta z^-1 \partial_T h_i > 0 because
c          \partial_T h_i < 0 (increase ocean temperature thins ice)
c
c  [(-) + (-)][(+) + (+)] --> [-]  
c  --> the gradient of exconc is negative with respect to ocean temperature,
c      as expected
c 
c  check the gradient w.r.t. h_i:
c  ================================
c
c  \partial_h_i J_deconc = \sigma_T^-2 x 2 x 
c                      [-\beta_T \Delta z^-1 h_i + T_ocn - T_f - \ep_T] x 
c                      [-\beta_T \Delta z^-1]
c
c  consider terms in []
c  1. -\beta_T \Delta z^-1 h_i < 0 for all h_i
c  2. T_ocn - T_f -\epsilon_T  < 0 so long as \epsilon_T > T_ocn - T_f
c  3. -\beta_T \Delta z^-1  < 0  
c
c  [(-) + (-)][-] --> [+]  
c  --> the gradient of exconc is positive with respect to ice thickness,
c      as expected

c  Note: normally when exconc is activate T_ocn - T_f = 0 
c  because the seawater temperature is at or very near the freezing point.
c  When that is the case, our gradient conditions hold because 
c  \epsilon_T > T_ocn - T_f so long as \epsilon_T > 0.
c  However, T_ocn can sometimes fall below the freezing point 
c  for numerical or other reasons and so one should choose
c  \epsilon_T to be large enough to ensure that the T_ocn -T_f - \epsilon_T 
c  is negative.
c  using \epsilon_T = 1 degree Celsius is a safe bet.



      if(flag_exconc_deconc.EQ.'ex') then
        do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nnzobs
            do j = jmin,jmax
             do i = imin,imax

c             observations are zero and 
              if ((localobs(i,j,k,bi,bj) .LE. 0.).AND.
     &            (concbar(i,j,k,bi,bj)  .GT. 0.)) then

               localfldmsk(i,j,k,bi,bj) = localmask(i,j,k,bi,bj)

#ifndef READIN_EXCONC_DECONC_SIGMA
cigf           weight is 1/sigma_T^2, defined above localnormsq
c              suggested value is 100 1/degreesC^2 
               localfldweight(i,j,k,bi,bj) = localnormsq
#endif

               heff_local = max(heffbar(i,j,k,bi,bj), epsilon_h)

               localfld(i,j,k,bi,bj) =
     &            -const0 * heff_local 
     &            + (thetabar(i,j,k,bi,bj) - SEAICE_freeze - epsilon_T)
     &            
              endif
             enddo
            enddo
           enddo
          enddo
        enddo
      endif

#endif /* ALLOW_GENCOST_CONTRIBUTION */
#endif /* ALLOW_SEAICE */
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
