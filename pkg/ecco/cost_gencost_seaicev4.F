C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_seaicev4.F,v 1.11 2015/02/17 14:34:33 gforget Exp $
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

      integer nnzsiv4
      parameter (nnzsiv4 = 1 )
      integer nrecloc
      integer localstartdate(4)

catn changing names to make more self-explanatory
c old:heff -> model has excess of iceconc     -> new:exconc
c old:sst  -> model has deficiency in iceconc -> new:deconc

      _RL areabbbar   (1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL exconcbbbar  (1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL deconcbbbar  (1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL localweight(1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL xx_areabbbar_mean_dummy
      _RL xx_exconcbbbar_mean_dummy
      _RL xx_deconcbbbar_mean_dummy
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
      integer irec
      integer  il
      integer localrec
      integer obsrec

      logical doglobalread
      logical ladinit

      _RL spval
      parameter (spval = -9999. )
      _RL localwww
      _RL junk

      _RL localmask  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localobs   (1-olx:snx+olx,1-oly:sny+oly,nnzsiv4,nsx,nsy)
      _RL cmask (1-olx:snx+olx,1-oly:sny+oly,nnzsiv4)

      character*(128) fname0, fname1, fname2, fname3

#ifdef ALLOW_GENCOST_TIMEVARY_WEIGHT
      character*(MAX_LEN_FNAM) localobswfile
      character*(128) fname0w
      _RL localobsweight (1-olx:snx+olx,1-oly:sny+oly,nnzsiv4,nsx,nsy)
      _RL tmpobsweight (1-olx:snx+olx,1-oly:sny+oly,nnzsiv4,nsx,nsy)
#endif /* ALLOW_GENCOST_TIMEVARY_WEIGHT */

      _RL daytime
      _RL diffsecs
      integer dayiter
      integer daydate(4)
      integer difftime(4)
      integer middate(4)
      integer yday, ymod
      integer md, dd, sd, ld, wd
      integer mody, modm
      integer beginmodel, beginlocal
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

c--   Initialise local variables.

      localwww = 0. _d 0
      nrecloc=0
      localperiod=0.

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nnzsiv4
            do j = jmin,jmax
              do i = imin,imax
                localobs(i,j,k,bi,bj) = 0. _d 0
#ifdef ALLOW_GENCOST_TIMEVARY_WEIGHT
                if(gencost_timevaryweight(igen_conc)) then
                  localobsweight(i,j,k,bi,bj) = 0. _d 0
                  tmpobsweight(i,j,k,bi,bj) = 0. _d 0
                endif
#endif /* ALLOW_GENCOST_TIMEVARY_WEIGHT */
              enddo
            enddo
          enddo
        enddo
      enddo

c--   Assign mask
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,Nr
            do j = 1-oly,sny+oly
              do i = 1-olx,snx+olx
         localmask(i,j,k,bi,bj) = maskC(i,j,k,bi,bj)
              enddo
            enddo
          enddo
        enddo
      enddo

      areabbbarfile=gencost_barfile(igen_conc)
      exconcbbbarfile=gencost_barfile(igen_exconc)
      deconcbbbarfile=gencost_barfile(igen_deconc)
      localobsfile=gencost_datafile(igen_conc)
#ifdef ALLOW_GENCOST_TIMEVARY_WEIGHT
      localobswfile=gencost_errfile(igen_conc)
#endif /* ALLOW_GENCOST_TIMEVARY_WEIGHT */
      xx_areabbbar_mean_dummy=gencost_dummy(igen_conc)
      xx_exconcbbbar_mean_dummy=gencost_dummy(igen_exconc)
      xx_deconcbbbar_mean_dummy=gencost_dummy(igen_deconc)
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
c-atn

c--   First, read tiled data.
      doglobalread = .false.
      ladinit      = .false.

      write(fname1(1:128),'(80a)') ' '
      il=ilnblnk( areabbbarfile )
      write(fname1(1:128),'(2a,i10.10)')
     &     areabbbarfile(1:il),'.',eccoiter

      il=ilnblnk( exconcbbbarfile )
      write(fname2(1:128),'(2a,i10.10)')
     &     exconcbbbarfile(1:il),'.',eccoiter

      il=ilnblnk( deconcbbbarfile )
      write(fname3(1:128),'(2a,i10.10)')
     &     deconcbbbarfile(1:il),'.',eccoiter

      if ( .NOT. ( localobsfile.EQ.' ' ) ) then

c--   Loop over records for the second time.
      do irec = 1, nrecloc

           call active_read_xy( fname1, areabbbar, irec, doglobalread,
     &                      ladinit, eccoiter, mythid,
     &                      xx_areabbbar_mean_dummy )

           call active_read_xy( fname2, exconcbbbar, irec,doglobalread,
     &                      ladinit, eccoiter, mythid,
     &                      xx_exconcbbbar_mean_dummy )

           call active_read_xy( fname3, deconcbbbar, irec,doglobalread,
     &                      ladinit, eccoiter, mythid,
     &                      xx_deconcbbbar_mean_dummy )

        if ( localperiod .EQ. 86400. ) then
c-- assume daily fields
           obsrec = irec
           daytime = FLOAT(secondsperday*(irec-1))
           dayiter = hoursperday*(irec-1)
           call cal_getdate( dayiter, daytime, daydate, mythid )
           call cal_convdate( daydate,yday,md,dd,sd,ld,wd,mythid )
           ymod = localstartdate(1)/10000
           if ( ymod .EQ. yday ) then
              middate(1) = modelstartdate(1)
           else
              middate(1) = yday*10000+100+1
           endif
           middate(2) = 0
           middate(3) = modelstartdate(3)
           middate(4) = modelstartdate(4)
           call cal_TimePassed( middate, daydate, difftime, mythid )
           call cal_ToSeconds( difftime, diffsecs, mythid )
           localrec = int(diffsecs/localperiod) + 1
        else
c-- assume monthly fields
           beginlocal = localstartdate(1)/10000
           beginmodel = modelstartdate(1)/10000
           obsrec =
     &           ( beginmodel - beginlocal )*nmonthyear
     &         + ( mod(modelstartdate(1)/100,100)
     &            -mod(localstartdate(1)/100,100) )
     &         + irec
           mody   = modelstartdate(1)/10000
           modm   = modelstartdate(1)/100 - mody*100
           yday   = mody + INT((modm-1+irec-1)/12)
           localrec = 1 + MOD(modm-1+irec-1,12)
        endif

        il=ilnblnk(localobsfile)
        write(fname0(1:128),'(2a,i4)')
     &       localobsfile(1:il), '_', yday
        inquire( file=fname0, exist=exst )
        if (.NOT. exst) then
           write(fname0(1:128),'(a)') localobsfile(1:il)
c to use the data in a repreated cycle, comment next line?
           localrec = obsrec
        endif

        if ( localrec .GT. 0 ) then
          call mdsreadfield( fname0, cost_iprec, cost_yftype, nnzsiv4,
     &         localobs, localrec, mythid )
        else
          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nnzsiv4
                do j = jmin,jmax
                  do i = imin,imax
                     localobs(i,j,k,bi,bj) = spval
c not sure why this is not spzeroloc
                  enddo
                enddo
              enddo
            enddo
          enddo
        endif

#ifdef ALLOW_GENCOST_TIMEVARY_WEIGHT
catn--- reading time-variable weights -----------
cph        if(gencost_timevaryweight(igen_conc)) then

          il=ilnblnk(localobswfile)
          write(fname0w(1:128),'(2a,i4)')
     &       localobswfile(1:il), '_', yday
          inquire( file=fname0w, exist=exst )

          if (.NOT. exst) then
           write(fname0w(1:128),'(a)') localobswfile(1:il)
c to use the data in a repreated cycle, comment next line?
           localrec = obsrec
          endif

          if ( localrec .GT. 0 ) then
          call mdsreadfield( fname0w, cost_iprec, cost_yftype, nnzsiv4,
     &         tmpobsweight, localrec, mythid )
          else
          WRITE(standardMessageUnit,'(A)')
     &     'siv4cost WARNING: ALL WEIGHTS ZEROS! NO CONTRIBUTION'
          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nnzsiv4
                do j = jmin,jmax
                  do i = imin,imax
                     tmpobsweight(i,j,k,bi,bj) = 0. _d 0
                  enddo
                enddo
              enddo
            enddo
          enddo
          endif
cph        endif
catn---------------------------------------------
#endif /* ALLOW_GENCOST_TIMEVARY_WEIGHT */

        do bj = jtlo,jthi
          do bi = itlo,ithi

c--           Determine the mask on weights
            do k = 1,nnzsiv4
             do j = jmin,jmax
              do i = imin,imax
               cmask(i,j,k) = localmask(i,j,k,bi,bj)
                if ( localobs(i,j,k,bi,bj) .lt. spminloc .or.
     &               localobs(i,j,k,bi,bj) .gt. spmaxloc .or.
     &               localobs(i,j,k,bi,bj) .eq. spzeroloc ) then
                   cmask(i,j,k) = 0. _d 0
                endif
catn----------------------------------------------
#ifdef ALLOW_GENCOST_TIMEVARY_WEIGHT
                if (gencost_timevaryweight(igen_conc)) then
c--    Test for missing values.
                  if (tmpobsweight(i,j,k,bi,bj) .lt. -9900.) then
                    tmpobsweight(i,j,k,bi,bj) = 0. _d 0
                  endif
                  if (tmpobsweight(i,j,k,bi,bj) .ne. 0.) then
                    localweight(i,j,k,bi,bj) =
     &                1./tmpobsweight(i,j,k,bi,bj)/
     &                   tmpobsweight(i,j,k,bi,bj)
                  else
                    localweight(i,j,k,bi,bj) = 0.0 _d 0
                  endif
                else
                  localweight(i,j,k,bi,bj)=
     &              gencost_weight(i,j,bi,bj,igen_conc)
                endif
#else  /* ALLOW_GENCOST_TIMEVARY_WEIGHT */
                localweight(i,j,k,bi,bj)=
     &            gencost_weight(i,j,bi,bj,igen_conc)
#endif /* ALLOW_GENCOST_TIMEVARY_WEIGHT */
catn----------------------------------------------
              enddo
             enddo
            enddo
c--
            do k = 1,nnzsiv4
             do j = jmin,jmax
              do i = imin,imax

c area term
                localwww  = localweight(i,j,k,bi,bj)*cmask(i,j,k)
                junk      = ( areabbbar(i,j,k,bi,bj) -
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
                   junk=1. _d 0 *cmask(i,j,k)*
     &              (( (deconcbbbar(i,j,k,bi,bj)-SEAICE_freeze)*
     &                 HeatCapacity_Cp*rhoNil*drF(1)
     &              - exconcbbbar(i,j,k,bi,bj)*
     &                 SEAICE_lhFusion*SEAICE_rhoIce )
     &              - (epsilonTemp*HeatCapacity_Cp*rhoNil*drF(1)))
                     num_gencost(bi,bj,igen_exconc) =
     &                 num_gencost(bi,bj,igen_exconc) + 1. _d 0
                else
                   junk = 0. _d 0
                     num_gencost(bi,bj,igen_exconc) =
     &                 num_gencost(bi,bj,igen_exconc) + 0. _d 0
                endif

                objf_gencost(bi,bj,igen_exconc) =
     &            objf_gencost(bi,bj,igen_exconc) + 
     &            junk*junk*localnorm*localnorm

catn-- old: sst term, new: deconc
c Add ice: model_A==0 but obs_A > 0, calc enthalpy E:
c E_current = (deconcbbbar(i,j,k,bi,bj)-SEAICE_freeze)
c             *HeatCapacity_Cp*rhoNil*drF(1)
c HEFF_target = epsilon_HEFF [m]
c E_target  = -(HEFF_target*SEAICE_lhFusion*SEAICE_rhoIce)
c cost=(Model-data)^2
                if ( (areabbbar(i,j,1,bi,bj) .LE. 0.).AND.
     &               (localobs(i,j,1,bi,bj) .GT. 0.) ) then
                 junk=1. _d 0 *cmask(i,j,k)*
     &               (( (deconcbbbar(i,j,k,bi,bj)-SEAICE_freeze)*
     &                  HeatCapacity_Cp*rhoNil*drF(1) )
     &               - (-1. _d 0 *epsilonHEFF*
     &                  SEAICE_lhFusion*SEAICE_rhoIce))
                     num_gencost(bi,bj,igen_deconc) =
     &                 num_gencost(bi,bj,igen_deconc) + 1. _d 0
                else
                 junk = 0. _d 0
                     num_gencost(bi,bj,igen_deconc) =
     &                 num_gencost(bi,bj,igen_deconc) + 0. _d 0
                endif

                objf_gencost(bi,bj,igen_deconc) =
     &            objf_gencost(bi,bj,igen_deconc) + 
     &            junk*junk*localnorm*localnorm

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
