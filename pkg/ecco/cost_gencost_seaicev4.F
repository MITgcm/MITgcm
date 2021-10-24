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
# include "ECCO_SIZE.h"
# include "ECCO.h"
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

      integer nrecloc
      integer localstartdate(4)

catn changing names to make more self-explanatory
c old:sst  -> model has deficiency in iceconc -> new:deconc
c old:heff -> model has excess of iceconc     -> new:exconc

      _RL areabar    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL deconcbar  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL exconcbar  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localweight  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL dummy
      _RL localperiod
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc

      character*(MAX_LEN_FNAM) areabarfile
      character*(MAX_LEN_FNAM) deconcbarfile
      character*(MAX_LEN_FNAM) exconcbarfile
      character*(MAX_LEN_FNAM) localobsfile

      integer igen_conc, igen_deconc, igen_exconc

      integer bi,bj
      integer k
      integer irec, jrec
      integer  il, k2
      integer localrec
      integer obsrec
      logical dosumsq, dovarwei

      integer preproc_i(NGENPPROC)
      _RL preproc_r(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc_c(NGENPPROC)

      _RL localmask  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localobs   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localtmp   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localdif   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL difmask    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL difmask1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)

      character*(128) fname0, fname0w, fname1

      character*(MAX_LEN_FNAM) localobswfile
      logical exst

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

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
      do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
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
      if(igen_deconc.ne.0) deconcbarfile=gencost_barfile(igen_deconc)
      if(igen_exconc.ne.0) exconcbarfile=gencost_barfile(igen_exconc)

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

c--   initialize arrays, copy 2D maskInC to 2D localmask; this means
c     that sea ice cost function contributions on open boundary points
c     are masked out, probably something that eccov4 can live
c     with. Alternatively, on has to copy the first level of maskC to
c     localmask explicitly.
      call ecco_zero(localobs,1,spzeroloc,myThid)
      call ecco_zero(localweight,1,zeroRL,myThid)
      call ecco_zero(localmask,1,zeroRL,myThid)
      call ecco_cprsrl(maskInC,localmask,1,1,myThid)

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
        call ecco_zero(localweight,1,zeroRL,myThid)
#ifdef SEAICECOST_JPL
       fname0w=gencost_errfile(igen_conc)
       call ecco_readwei(fname0w,localweight,localrec,
     &      1,1,dosumsq,myThid)
       call ecco_readwei(gencost_errfile(igen_deconc),
     &      gencost_weight(1-OLx,1-OLy,1,1,igen_deconc),localrec,
     &      1,1,dosumsq,myThid)
       call ecco_readwei(gencost_errfile(igen_exconc),
     &      gencost_weight(1-OLx,1-OLy,1,1,igen_exconc),localrec,
     &      1,1,dosumsq,myThid)
#else
        if ( (localrec. GT. 0).AND.(obsrec .GT. 0).AND.(exst) ) then
          call ecco_readwei(fname0w,localweight,localrec,
     &      1,1,dosumsq,myThid)
        else
          WRITE(standardMessageUnit,'(A)')
     &     'siv4cost WARNING: ALL WEIGHTS ZEROS! NO CONTRIBUTION'
        endif
#endif

c====================================================
c--------- PART 1.2 read barfiles ------------------
c====================================================
c-- set all bars to zeros:
        call ecco_zero(areabar,1,zeroRL,myThid)
        call ecco_zero(deconcbar,1,zeroRL,myThid)
        call ecco_zero(exconcbar,1,zeroRL,myThid)

c--1.2.A sea-ice concentration barfile
        exst=.FALSE.
        call cost_gencal(areabarfile,gencost_datafile(igen_conc),
     &   irec,localstartdate,localperiod,fname1,
     &   fname0,localrec,obsrec,exst,myThid)
        call cost_genread(fname1,areabar,localtmp,irec,1,1,
     &       nrecloc,preproc,preproc_c,preproc_i,preproc_r,
     &       dummy,myThid)

c--1.2.B sst as proxy for deconc barfile, needs igen_conc
        if(igen_deconc.ne.0) then
         exst=.FALSE.
         call cost_gencal(deconcbarfile,gencost_datafile(igen_conc),
     &    irec,localstartdate,localperiod,fname1,
     &    fname0,localrec,obsrec,exst,myThid)
         call cost_genread(fname1,deconcbar,localtmp,
     &        irec,1,1,
     &        nrecloc,preproc,preproc_c,preproc_i,preproc_r,
     &        dummy,myThid)
        endif

c--1.2.C heff as proxy for exconc barfile, need igen_conc and igen_exconc
        if(igen_deconc.ne.0 .and. igen_exconc.ne.0) then
         exst=.FALSE.
         call cost_gencal(exconcbarfile,gencost_datafile(igen_conc),
     &    irec,localstartdate,localperiod,fname1,
     &    fname0,localrec,obsrec,exst,myThid)
         call cost_genread(fname1,exconcbar,localtmp,
     &        irec,1,1,
     &        nrecloc,preproc,preproc_c,preproc_i,preproc_r,
     &        dummy,myThid)
        endif

c====================================================
c--------- PART 1.3 read data --------------------
c====================================================
c-- initialize to spzerloc = -9999.
        call ecco_zero(localobs,1,spzeroloc,myThid)
        if ( (localrec .GT. 0).AND.(obsrec .GT. 0).AND.(exst) ) then
         CALL READ_REC_3D_RL( fname0, cost_iprec, 1,
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
        call ecco_zero(localdif,1,zeroRL,myThid)
        call ecco_zero(difmask,1,zeroRL,myThid)
        call ecco_diffmsk(
     I     areabar, localobs, localmask,
     I     1, 1, spminloc, spmaxloc, spzeroloc,
     O     localdif, difmask,
     I     myThid )

c---1.4.A area term:
        call ecco_addcost(
     I     localdif,localweight,difmask,1,1,dosumsq,
     O     objf_gencost(1,1,igen_conc),num_gencost(1,1,igen_conc),
     I     myThid)

c---1.4.B defficient ice term: (old: sst term, new: deconc)
c Add ice: model_A==0 but obs_A > 0, calc enthalpy E:
        if(igen_deconc.ne.0) then
         call ecco_zero(difmask1,1,zeroRL,myThid)
         call ecco_zero(localdif,1,zeroRL,myThid)
         call ecco_zero(localtmp,1,zeroRL,myThid)

         call get_exconc_deconc(
     I    localobs,1,areabar,exconcbar,deconcbar,1,
     I    difmask,'de',
     O    localdif,difmask1,localtmp,
     I    myThid )
#ifdef SEAICECOST_JPL
        call ecco_cp( gencost_weight(1-OLx,1-OLy,1,1,igen_deconc),
     O       localtmp,1,1,myThid)
#endif
        call ecco_addcost(
     I      localdif,localtmp,difmask1,1,1,dosumsq,
     O      objf_gencost(1,1,igen_deconc),num_gencost(1,1,igen_deconc),
     I      myThid)
        endif

c---1.4.C excessive ice term:  (old: heff and sst term, new: exconc)
c Removing ice: model_A > 0 but obs_A==0, calc enthalpy E:
        if(igen_deconc.ne.0 .and. igen_exconc.ne.0) then
         call ecco_zero(difmask1,1,zeroRL,myThid)
         call ecco_zero(localdif,1,zeroRL,myThid)
         call ecco_zero(localtmp,1,zeroRL,myThid)

         call get_exconc_deconc(
     I    localobs,1,areabar,exconcbar,deconcbar,1,
     I    difmask,'ex',
     O    localdif,difmask1,localtmp,
     I    myThid )
#ifdef SEAICECOST_JPL
        call ecco_cp( gencost_weight(1-OLx,1-OLy,1,1,igen_exconc),
     O       localtmp,1,1,myThid)
#endif
        call ecco_addcost(
     I      localdif,localtmp,difmask1,1,1,dosumsq,
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

      subroutine get_exconc_deconc(
     I    localobs,nnzobs,concbar,exconcbar,deconcbar,nnzbar,
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
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

c     == routine arguments ==

      integer myThid, nnzbar, nnzobs
      _RL localmask     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)

      _RL localobs      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL concbar       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL deconcbar     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL exconcbar     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localfld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localfldmsk   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL localfldweight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)

      character*2 flag_exconc_deconc

#ifdef ALLOW_GENCOST_CONTRIBUTION
#ifdef ALLOW_SEAICE

c    == local variables ==
      integer bi,bj
      integer i,j,k

C- jmc: params SEAICE_freeze has been retired; set it locally until someone
C       who knows what this cost-cointribution means fix it.
C- atn: also adding 1 normalizing factor same order of magnitude as
C       rhoNil*HeatCapacity_cp*dz = SEAICE_rhoice*SEAICE_lhFusion*heff
C       = 1e3*1e3*1e1=1e7
C- atn: lastly, define 2 cutoff values for cost to be read in from data.seaice
C      and initialized in seaice_readparms: SEAICE_cutoff_[area,heff]
C      Reason: some iceconc data set have "bogus" mask with area>0
C      at climatological max locations -> not real data.  So either need
C      to clean up the data or take SEAICE_cutoff_area>=0.15 for example.
C      Might need to migrate into pkg/ecco instead of in pkg/seaice.
      _RL SEAICE_freeze, epsilonTemp, epsilonHEFF
      _RL localnorm, localnormsq
      _RL const1,const2
CEOP

      SEAICE_freeze  = -1.96  _d 0
      epsilonTemp = 0.0001 _d 0
#ifdef SEAICECOST_JPL
      epsilonHEFF = 0.3 _d 0
#else
      epsilonHEFF = 0.01 _d 0
#endif
      localnorm = 1. _d -07
      localnormsq=localnorm*localnorm

      const1=HeatCapacity_Cp*rhoNil*drF(1)
      const2=SEAICE_lhFusion*SEAICE_rhoIce

c intialize
      call ecco_zero(localfld,nnzobs,zeroRL,myThid)
      call ecco_zero(localfldmsk,nnzobs,zeroRL,myThid)
      call ecco_zero(localfldweight,nnzobs,zeroRL,myThid)

c----------------------DECONC-------------------------------
catn-- old: sst term, new: deconc
c needs localconcbar and localsstbar
c Add ice: model_A==0 but obs_A > 0, calc enthalpy E:
c E_current = (deconcbar(i,j,k,bi,bj)-Tfreeze)
c             *HeatCapacity_Cp*rhoNil*drF(1)
c HEFF_target = epsilon_HEFF [m]
c E_target  = -(HEFF_target*SEAICE_lhFusion*SEAICE_rhoIce)
c cost=(Model-data)^2
      if(flag_exconc_deconc.EQ.'de') then
        do bj = myByLo(myThid), myByHi(myThid)
          do bi = myBxLo(myThid), myBxHi(myThid)
           do k = 1,nnzobs
            do j = 1-OLy,sNy+OLy
             do i = 1-OLx,sNx+OLx

              if ( (concbar(i,j,k,bi,bj) .LE. 0.).AND.
     &               (localobs(i,j,k,bi,bj) .GT. 0.) ) then

               localfldmsk(i,j,k,bi,bj) = localmask(i,j,k,bi,bj)

#ifndef SEAICECOST_JPL
               localfldweight(i,j,k,bi,bj) = localnormsq
#endif
               localfld(i,j,k,bi,bj) =
     &          (deconcbar(i,j,k,bi,bj)-SEAICE_freeze)*const1
     &               - (-1. _d 0 *epsilonHEFF*const2)
              endif
             enddo
            enddo
           enddo
          enddo
        enddo
      endif

c----------------------EXCONC-------------------------------
catn-- old: heff and sst term, new: exconc
c needs localconcbar, localsstbar, and localheffbar
c Removing ice: model_A > 0 but obs_A==0, calc enthalpy E:
c E_current = [(deconcbar-SEAICE_freeze)*HeatCapacity_Cp*rhoNil*drF(1)
c            - (exconcbar * SEAICE_lhFusion * SEAICE_rhoIce)
c            - (HSNOW * SEAICE_lhFusion * SEAICE_rhoSnow)]
c E_target = (epsilonTemp) * HeatCapacity_Cp * rhoNil * drF(1)
c cost(Model-data)^2

      if(flag_exconc_deconc.EQ.'ex') then
        do bj = myByLo(myThid), myByHi(myThid)
          do bi = myBxLo(myThid), myBxHi(myThid)
           do k = 1,nnzobs
            do j = 1-OLy,sNy+OLy
             do i = 1-OLx,sNx+OLx

              if ((localobs(i,j,k,bi,bj) .LE. SEAICE_cutoff_area).AND.
     &        (exconcbar(i,j,k,bi,bj) .GT. SEAICE_cutoff_heff)) then

               localfldmsk(i,j,k,bi,bj) = localmask(i,j,k,bi,bj)
#ifndef SEAICECOST_JPL
               localfldweight(i,j,k,bi,bj) = localnormsq
#endif
               localfld(i,j,k,bi,bj) =
     &        ( (deconcbar(i,j,k,bi,bj)-SEAICE_freeze)*const1
#ifdef SEAICECOST_JPL
     &         - max(exconcbar(i,j,k,bi,bj),epsilonHEFF)*
#else
     &         - exconcbar(i,j,k,bi,bj)*
#endif
     &           const2 ) - (epsilonTemp*const1)
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
