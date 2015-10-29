C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_seaicev4.F,v 1.15 2015/10/29 00:21:58 atn Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_seaicev4(mythid)

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
      integer mythid

#ifdef ALLOW_SEAICE
#ifdef ALLOW_GENCOST_CONTRIBUTION

c     == local variables ==

      integer nnzsiv4, nnzbar
      parameter (nnzsiv4 = 1 , nnzbar = 1)
      integer nrecloc
      integer localstartdate(4)

catn changing names to make more self-explanatory
c old:heff -> model has excess of iceconc     -> new:exconc
c old:sst  -> model has deficiency in iceconc -> new:deconc

      _RL areabbbar   (1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL exconcbbbar  (1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL deconcbbbar  (1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL localweight(1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL dummy
      _RL localperiod
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc

      character*(MAX_LEN_FNAM) areabbbarfile
      character*(MAX_LEN_FNAM) exconcbbbarfile
      character*(MAX_LEN_FNAM) deconcbbbarfile
      character*(MAX_LEN_FNAM) localobsfile

      integer igen_conc, igen_deconc, igen_exconc

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec, jrec
      integer  il
      integer localrec
      integer obsrec

      integer preproc_i(NGENPPROC)
      _RL preproc_r(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc_c(NGENPPROC)

      _RL localwww
      _RL junk

      _RL localmask  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localobs   (1-olx:snx+olx,1-oly:sny+oly,nnzsiv4,nsx,nsy)
      _RL localtmp   (1-olx:snx+olx,1-oly:sny+oly,nnzsiv4,nsx,nsy)
      _RL localdif   (1-olx:snx+olx,1-oly:sny+oly,nnzsiv4,nsx,nsy)
      _RL difmask    (1-olx:snx+olx,1-oly:sny+oly,nnzsiv4,nsx,nsy)

      character*(128) fname0, fname0w, fname1

      character*(MAX_LEN_FNAM) localobswfile
      logical exst

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

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
      _RL localnorm
      SEAICE_freeze  = -1.96  _d 0
      epsilonTemp = 0.0001 _d 0
      epsilonHEFF = 0.01 _d 0
      localnorm = 1. _d -07

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c-- detect the relevant gencost indices
      igen_conc=0
      igen_deconc=0
      igen_exconc=0
      do k=1,NGENCOST
        if (gencost_name(k).EQ.'siv4-conc') igen_conc=k
        if (gencost_name(k).EQ.'siv4-deconc') igen_deconc=k
        if (gencost_name(k).EQ.'siv4-exconc') igen_exconc=k
      enddo

      if ((igen_conc.NE.0).AND.(igen_deconc.NE.0)
     &    .AND.(igen_exconc.NE.0)) then

c-- initialize objf and num:
      do bj = jtlo,jthi
        do bi = itlo,ithi
          objf_gencost(bi,bj,igen_conc) = 0. _d 0
          objf_gencost(bi,bj,igen_exconc) = 0. _d 0
          objf_gencost(bi,bj,igen_deconc) = 0. _d 0

          num_gencost(bi,bj,igen_conc) = 0. _d 0
          num_gencost(bi,bj,igen_exconc) = 0. _d 0
          num_gencost(bi,bj,igen_deconc) = 0. _d 0
        enddo
      enddo

      call ecco_zero(gencost_weight(:,:,1,1,igen_conc),1,zeroRL,myThid)
      if ( gencost_errfile(igen_conc) .NE. ' ' ) 
     &  call ecco_readwei(gencost_errfile(igen_conc),
     &  gencost_weight(:,:,1,1,igen_conc),1,1,mythid)

c--   Initialise local variables.
      localwww = 0. _d 0
      nrecloc=0
      localperiod=0.

      areabbbarfile=gencost_barfile(igen_conc)
      exconcbbbarfile=gencost_barfile(igen_exconc)
      deconcbbbarfile=gencost_barfile(igen_deconc)
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
c-atn need to set local period, else always enter monthly avg
      localperiod=gencost_period(igen_conc)
      nrecloc=gencost_nrec(igen_conc)

      preproc(1)=gencost_preproc(1,igen_conc)
      preproc_i(1)=gencost_preproc_i(1,igen_conc)
      preproc_c(1)=gencost_preproc_c(1,igen_conc)
      preproc_r(1)=gencost_preproc_r(1,igen_conc)
c-atn
      call ecco_zero(localobs,1,spzeroloc,myThid)
      call ecco_zero(localweight,1,zeroRL,myThid)
      call ecco_zero(localmask,Nr,zeroRL,myThid)
      call ecco_cprsrl(maskC,Nr,localmask,Nr,myThid)

      if ( .NOT. ( localobsfile.EQ.' ' ) ) then

c--   Loop over records for the second time.
      do irec = 1, nrecloc

c-- read weights
        exst=.FALSE.
        jrec=1
        if(preproc(1).EQ.'variaweight') jrec = irec
        call cost_gencal(areabbbarfile,gencost_errfile(igen_conc),
     &     jrec, localstartdate, localperiod, fname1,
     &     fname0w, localrec, obsrec, exst, mythid)
        call ecco_zero(localweight,nnzsiv4,zeroRL,myThid)
        if ( (localrec. GT. 0).AND.(obsrec .GT. 0).AND.(exst) ) then
          call ecco_readwei(fname0w,localweight,localrec,nnzsiv4,mythid)
        else
          WRITE(standardMessageUnit,'(A)')
     &     'siv4cost WARNING: ALL WEIGHTS ZEROS! NO CONTRIBUTION'
        endif

c-- read barfiles
c-- sea-ice concentration barfile
        exst=.FALSE.
        call cost_gencal(areabbbarfile,gencost_datafile(igen_conc),
     &   irec, localstartdate, localperiod, fname1,
     &   fname0, localrec, obsrec, exst, mythid)
        call ecco_zero(areabbbar,nnzbar,zeroRL,myThid)
        call cost_genread( fname1, areabbbar, localtmp, irec, nnzbar,
     &       nrecloc, preproc, preproc_c, preproc_i, preproc_r, 
     &       dummy, mythid )

c-- heff as proxy for exconc barfile
        exst=.FALSE.
        call cost_gencal(exconcbbbarfile,gencost_datafile(igen_conc),
     &   irec, localstartdate, localperiod, fname1,
     &   fname0, localrec, obsrec, exst, mythid)
        call ecco_zero(exconcbbbar,nnzbar,zeroRL,myThid)
        call cost_genread( fname1, exconcbbbar, localtmp, irec, nnzbar,
     &       nrecloc, preproc, preproc_c, preproc_i, preproc_r, 
     &       dummy, mythid )

c-- sst as proxy for deconc barfile
        exst=.FALSE.
        call cost_gencal(deconcbbbarfile,gencost_datafile(igen_conc),
     &   irec, localstartdate, localperiod, fname1,
     &   fname0, localrec, obsrec, exst, mythid)
        call ecco_zero(deconcbbbar,nnzbar,zeroRL,myThid)
        call cost_genread( fname1, deconcbbbar, localtmp, irec, nnzbar,
     &       nrecloc, preproc, preproc_c, preproc_i, preproc_r, 
     &       dummy, mythid )

c-- read in data, first initialize to spzerloc = -9999.
        call ecco_zero(localobs,nnzsiv4,spzeroloc,mythid)
        if ( (localrec .GT. 0).AND.(obsrec .GT. 0).AND.(exst) ) then
          call mdsreadfield( fname0, cost_iprec, cost_yftype, nnzsiv4,
     &         localobs, localrec, mythid )
        else
          il=ilnblnk( fname0 )
          WRITE(standardMessageUnit,'(2A)')
     &     'siv4cost WARNING: DATA MISING! NO CONTRIBUTION, ',
     &     fname0(1:il)
        endif

c compute obs minus bar (localdif) and mask (difmask) 
        call ecco_zero(localdif,nnzsiv4,zeroRL,mythid)
        call ecco_zero(difmask,nnzsiv4,zeroRL,mythid)
        call ecco_diffmsk(
     I     areabbbar, nnzbar, localobs, nnzsiv4, localmask,
     I     spminloc, spmaxloc, spzeroloc,
     O     localdif, difmask,
     I     myThid )

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnzsiv4
             do j = jmin,jmax
              do i = imin,imax

c area term
                localwww = localweight(i,j,k,bi,bj)*difmask(i,j,k,bi,bj)
                junk     = ( areabbbar(i,j,k,bi,bj) -
     &                        localobs(i,j,k,bi,bj) )
                objf_gencost(bi,bj,igen_conc) =
     &            objf_gencost(bi,bj,igen_conc) + junk*junk*localwww

                if ( localwww .ne. 0. )
     &               num_gencost(bi,bj,igen_conc) =
     &                 num_gencost(bi,bj,igen_conc) + 1. _d 0

catn-- old: heff and sst term, new: exconc
c Removing ice: model_A > 0 but obs_A==0, calc enthalpy E:
c E_current = [(deconcbbbar-SEAICE_freeze)*HeatCapacity_Cp*rhoNil*drF(1)
c            - (exconcbbbar * SEAICE_lhFusion * SEAICE_rhoIce) 
c            - (HSNOW * SEAICE_lhFusion * SEAICE_rhoSnow)]
c E_target = (epsilonTemp) * HeatCapacity_Cp * rhoNil * drF(1)
c cost(Model-data)^2
                if ((localobs(i,j,k,bi,bj) .LE. SEAICE_cutoff_area)
     &            .AND.
     &              (exconcbbbar(i,j,k,bi,bj) .GT. SEAICE_cutoff_heff))
     &             then

                 localwww=localnorm*localnorm*difmask(i,j,k,bi,bj)
                 junk=
     &              (( (deconcbbbar(i,j,k,bi,bj)-SEAICE_freeze)*
     &                 HeatCapacity_Cp*rhoNil*drF(1)
     &              - exconcbbbar(i,j,k,bi,bj)*
     &                 SEAICE_lhFusion*SEAICE_rhoIce )
     &              - (epsilonTemp*HeatCapacity_Cp*rhoNil*drF(1)))
                  if( localwww .ne. 0. )
     &               num_gencost(bi,bj,igen_exconc) =
     &                 num_gencost(bi,bj,igen_exconc) + 1. _d 0
                else
                   junk = 0. _d 0
                   localwww = 0. _d 0
                     num_gencost(bi,bj,igen_exconc) =
     &                 num_gencost(bi,bj,igen_exconc) + 0. _d 0
                endif

                objf_gencost(bi,bj,igen_exconc) =
     &            objf_gencost(bi,bj,igen_exconc) + 
     &            junk*junk*localwww

catn-- old: sst term, new: deconc
c Add ice: model_A==0 but obs_A > 0, calc enthalpy E:
c E_current = (deconcbbbar(i,j,k,bi,bj)-SEAICE_freeze)
c             *HeatCapacity_Cp*rhoNil*drF(1)
c HEFF_target = epsilon_HEFF [m]
c E_target  = -(HEFF_target*SEAICE_lhFusion*SEAICE_rhoIce)
c cost=(Model-data)^2
                if ( (areabbbar(i,j,1,bi,bj) .LE. 0.).AND.
     &               (localobs(i,j,1,bi,bj) .GT. 0.) ) then

                 localwww=localnorm*localnorm*difmask(i,j,k,bi,bj)
                 junk=
     &               (( (deconcbbbar(i,j,k,bi,bj)-SEAICE_freeze)*
     &                  HeatCapacity_Cp*rhoNil*drF(1) )
     &               - (-1. _d 0 *epsilonHEFF*
     &                  SEAICE_lhFusion*SEAICE_rhoIce))
                 if( localwww .ne. 0. )
     &               num_gencost(bi,bj,igen_deconc) =
     &                 num_gencost(bi,bj,igen_deconc) + 1. _d 0
                else
                 junk = 0. _d 0
                 localwww = 0. _d 0
                     num_gencost(bi,bj,igen_deconc) =
     &                 num_gencost(bi,bj,igen_deconc) + 0. _d 0
                endif

                objf_gencost(bi,bj,igen_deconc) =
     &            objf_gencost(bi,bj,igen_deconc) + 
     &            junk*junk*localwww

              enddo
             enddo
            enddo

          enddo
        enddo

      enddo

      endif !if ( .NOT. ( localobsfile.EQ.' ' ) ) then
      endif !if (igen_[conc,deconc,exconc].NE.0)

#endif /* ALLOW_GENCOST_CONTRIBUTION */
#endif /* ALLOW_SEAICE */

      end
