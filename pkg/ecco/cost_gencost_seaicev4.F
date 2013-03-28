C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_seaicev4.F,v 1.2 2013/03/28 13:40:09 jmc Exp $
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
#ifdef ALLOW_COST
# include "ecco_cost.h"
# include "optim.h"
# ifdef ALLOW_SEAICE
#  include "SEAICE_COST.h"
#  include "SEAICE_PARAMS.h"
# endif
#endif

c     == routine arguments ==
      integer mythid

#ifdef ALLOW_SEAICE_COST_CONTRIBUTION
#ifdef ALLOW_GENCOST_CONTRIBUTION

c     == local variables ==

      integer nnzobs
      parameter (nnzobs = 1 )
      integer nrecloc
      integer localstartdate(4)

      _RL areabbbar   (1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL heffbbbar  (1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL sstbbbar  (1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL localweight(1-olx:snx+olx,1-oly:sny+oly,1,nsx,nsy)
      _RL xx_areabbbar_mean_dummy
      _RL xx_heffbbbar_mean_dummy
      _RL xx_sstbbbar_mean_dummy
      _RL mult_local
      _RL localperiod
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL objf_local(nsx,nsy)
      _RL num_local(nsx,nsy)

      character*(MAX_LEN_FNAM) areabbbarfile
      character*(MAX_LEN_FNAM) heffbbbarfile
      character*(MAX_LEN_FNAM) sstbbbarfile
      character*(MAX_LEN_FNAM) localobsfile

      integer igen_conc, igen_sst, igen_vol

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
      _RL localobs   (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL cmask (1-olx:snx+olx,1-oly:sny+oly,nnzobs)

      character*(128) fname0, fname1, fname2, fname3
      character*(MAX_LEN_MBUF) msgbuf

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
      igen_sst=0
      igen_vol=0
      do k=1,NGENCOST
        if (gencost_name(k).EQ.'siv4-conc') igen_conc=k
        if (gencost_name(k).EQ.'siv4-sst') igen_sst=k
        if (gencost_name(k).EQ.'siv4-vol') igen_vol=k
      enddo

      if ((igen_conc.NE.0).AND.(igen_sst.NE.0).AND.(igen_vol.NE.0))
     &   then

c--   Initialise local variables.

      localwww = 0. _d 0

      do bj = jtlo,jthi
        do bi = itlo,ithi
          objf_gencost(bi,bj,igen_conc) = 0. _d 0
          objf_gencost(bi,bj,igen_vol) = 0. _d 0
          objf_gencost(bi,bj,igen_sst) = 0. _d 0
          num_gencost(bi,bj,igen_conc) = 0. _d 0
          num_gencost(bi,bj,igen_vol) = 0. _d 0
          num_gencost(bi,bj,igen_sst) = 0. _d 0
          do k = 1,nnzobs
            do j = jmin,jmax
              do i = imin,imax
                localobs(i,j,k,bi,bj) = 0. _d 0
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
      heffbbbarfile=gencost_barfile(igen_vol)
      sstbbbarfile=gencost_barfile(igen_sst)
      localobsfile=gencost_name(igen_conc)
      xx_areabbbar_mean_dummy=xx_genbar_dummy(igen_conc)
      xx_heffbbbar_mean_dummy=xx_genbar_dummy(igen_vol)
      xx_sstbbbar_mean_dummy=xx_genbar_dummy(igen_sst)
      localperiod=gencost_period(igen_conc)
      nrecloc=gencost_nrec(igen_conc)
      localstartdate(1)=modelstartdate(1)
      localstartdate(2)=modelstartdate(2)
      localstartdate(3)=modelstartdate(3)
      localstartdate(4)=modelstartdate(4)
      spminloc=gencost_spmin(igen_conc)
      spmaxloc=gencost_spmax(igen_conc)
      spzeroloc=gencost_spzero(igen_conc)

c--   First, read tiled data.
      doglobalread = .false.
      ladinit      = .false.

      write(fname1(1:128),'(80a)') ' '
      il=ilnblnk( areabbbarfile )
      write(fname1(1:128),'(2a,i10.10)')
     &     areabbbarfile(1:il),'.',optimcycle

      il=ilnblnk( heffbbbarfile )
      write(fname2(1:128),'(2a,i10.10)')
     &     heffbbbarfile(1:il),'.',optimcycle

      il=ilnblnk( sstbbbarfile )
      write(fname3(1:128),'(2a,i10.10)')
     &     sstbbbarfile(1:il),'.',optimcycle

      if ( .NOT. ( localobsfile.EQ.' ' ) ) then

c--   Loop over records for the second time.
      do irec = 1, nrecloc

           call active_read_xy( fname1, areabbbar, irec, doglobalread,
     &                      ladinit, optimcycle, mythid,
     &                      xx_areabbbar_mean_dummy )

           call active_read_xy( fname2, heffbbbar, irec, doglobalread,
     &                      ladinit, optimcycle, mythid,
     &                      xx_heffbbbar_mean_dummy )

           call active_read_xy( fname3, sstbbbar, irec, doglobalread,
     &                      ladinit, optimcycle, mythid,
     &                      xx_sstbbbar_mean_dummy )

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
          call mdsreadfield( fname0, cost_iprec, cost_yftype, nnzobs,
     &         localobs, localrec, mythid )
        else
          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nnzobs
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

        do bj = jtlo,jthi
          do bi = itlo,ithi

c--           Determine the mask on weights
            do k = 1,nnzobs
             do j = jmin,jmax
              do i = imin,imax
               cmask(i,j,k) = cosphi(i,j,bi,bj)*localmask(i,j,k,bi,bj)
                if ( localobs(i,j,k,bi,bj) .lt. spminloc .or.
     &               localobs(i,j,k,bi,bj) .gt. spmaxloc .or.
     &               localobs(i,j,k,bi,bj) .eq. spzeroloc ) then
                   cmask(i,j,k) = 0. _d 0
                endif
               localweight(i,j,k,bi,bj)=
     &           gencost_weight(i,j,bi,bj,igen_conc)
              enddo
             enddo
            enddo
c--
            do k = 1,nnzobs
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

c heff term
                if ( (localobs(i,j,k,bi,bj) .EQ. 0.).AND.
     &               (heffbbbar(i,j,k,bi,bj) .GT. 0.) ) then
                   junk=10. _d 0 *cmask(i,j,k)*heffbbbar(i,j,k,bi,bj)
                     num_gencost(bi,bj,igen_vol) =
     &                 num_gencost(bi,bj,igen_vol) + 1. _d 0
                else
                   junk = 0. _d 0
                     num_gencost(bi,bj,igen_vol) =
     &                 num_gencost(bi,bj,igen_vol) + 0. _d 0
                endif

                objf_gencost(bi,bj,igen_vol) =
     &            objf_gencost(bi,bj,igen_vol) + junk

c sst term
                if ( (areabbbar(i,j,1,bi,bj) .LE. 0.).AND.
     &               (localobs(i,j,1,bi,bj) .GT. 0.) ) then
                 junk=1. _d 0 *cmask(i,j,k)*
     &               ( 3. _d 0 + sstbbbar(i,j,k,bi,bj) )
                     num_gencost(bi,bj,igen_sst) =
     &                 num_gencost(bi,bj,igen_sst) + 1. _d 0
                else
                 junk = 0. _d 0
                     num_gencost(bi,bj,igen_sst) =
     &                 num_gencost(bi,bj,igen_sst) + 0. _d 0
                endif

                objf_gencost(bi,bj,igen_sst) =
     &            objf_gencost(bi,bj,igen_sst) + junk

              enddo
             enddo
            enddo

          enddo
        enddo

      enddo

      endif !if ( .NOT. ( localobsfile.EQ.' ' ) ) then
      endif !if ((igen_conc.NE.0).AND.(igen_sst.NE.0).AND.(igen_vol.NE.0)) then

#endif /* ALLOW_GENCOST_CONTRIBUTION */
#endif /* ALLOW_SEAICE_COST_CONTRIBUTION */

      end
