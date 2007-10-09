C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_ctds.F,v 1.5 2007/10/09 00:02:50 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_CTDS(
     I                     myiter,
     I                     mytime,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_CTDS
c     ==================================================================
c
c     o Evaluate cost function contribution of CTD temperature data.
c
c     started:  Elisabeth Remy eremy@ucsd.edu 30-Aug-2000
c
c     ==================================================================
c     SUBROUTINE cost_CTDS
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "cal.h"
#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer nrec
      integer irec
      integer ilu

      _RL fctile_ctds
      _RL fcthread_ctds
      _RL www (1-olx:snx+olx,1-oly:sny+oly)
      _RL wtmp (1-olx:snx+olx,1-oly:sny+oly)
      _RL tmpobs (1-olx:snx+olx,1-oly:sny+oly)
      _RL tmpbar (1-olx:snx+olx,1-oly:sny+oly)
      _RL cmask (1-olx:snx+olx,1-oly:sny+oly)
      _RL spval
      _RL spmax

      character*(80) fnamesalt

      logical doglobalread
      logical ladinit

      character*(MAX_LEN_MBUF) msgbuf

cnew(
      integer  il
      integer mody, modm
      integer iyear, imonth
      character*(80) fnametmp
      logical exst
cnew)

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

      spval = 25.
      spmax = 40.

c--   Read state record from global file.
      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_CTDS_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        ilu=ilnblnk( sbarfile )
        write(fnamesalt(1:80),'(2a,i10.10)')
     &       sbarfile(1:ilu),'.',optimcycle
      endif

      fcthread_ctds = 0. _d 0

cnew(
      mody = modelstartdate(1)/10000
      modm = modelstartdate(1)/100 - mody*100
cnew)

c--   Loop over records.
      do irec = 1,nmonsrec

c--     Read time averages and the monthly mean data.
        call active_read_xyz( fnamesalt, sbar, irec,
     &          doglobalread, ladinit,
     &          optimcycle, mythid, xx_sbar_mean_dummy )

cnew(
        iyear = mody + INT((modm-1+irec-1)/12)
        imonth = 1 + MOD(modm-1+irec-1,12)
        il=ilnblnk(ctdsfile)
        write(fnametmp(1:80),'(2a,i4)')
     &       ctdsfile(1:il), '_', iyear
        inquire( file=fnametmp, exist=exst )
        if (.NOT. exst) then
           write(fnametmp(1:80),'(a)') ctdsfile(1:il)
           imonth = irec
        endif

        call mdsreadfield( fnametmp, cost_iprec, 'RL', nr, ctdsobs,
     &                     imonth, mythid)
cnew)

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Loop over the model layers

            fctile_ctds = 0. _d 0

            do k = 1,nr

c--           Determine the weights to be used.
              do j = jmin,jmax
                do i = imin,imax
                  cmask(i,j) = 1. _d 0
                  if (ctdsobs(i,j,k,bi,bj) .lt. spval .or.
     &                ctdsobs(i,j,k,bi,bj) .gt. spmax .or.
     &                ctdsobs(i,j,k,bi,bj) .eq. 0. ) then
                    cmask(i,j) = 0. _d 0
                  endif

cph(
cph               print *, 'WARNING: SPECIFIC SETUP FOR ECCO'
cph               below statement could be replaced by following
cph               to make it independnet of Nr:
cph
cph               if ( rC(K) .GT. -1000. ) then
cph)
                  if (
     &             (_hFacC(i,j,13,bi,bj) .ne. 0.).and.
     &             (_hFacC(i,j,k,bi,bj) .ne. 0.)) then

                     www(i,j)    = cosphi(i,j,bi,bj)*cmask(i,j)
                     tmpobs(i,j) = ctdsobs(i,j,k,bi,bj)
                     tmpbar(i,j) = sbar(i,j,k,bi,bj)
                     wtmp(i,j) = wsalt2(i,j,k,bi,bj)

                     fctile_ctds = fctile_ctds +
     &                             (wtmp(i,j)*www(i,j))*
     &                             (tmpbar(i,j)-tmpobs(i,j))*
     &                             (tmpbar(i,j)-tmpobs(i,j))
                     if ( wtmp(i,j)*www(i,j) .ne. 0. )
     &                    num_ctds(bi,bj) = num_ctds(bi,bj) + 1. _d 0
		  endif
                enddo
              enddo
            enddo
c--         End of loop over layers.

            fcthread_ctds    = fcthread_ctds    + fctile_ctds
            objf_ctds(bi,bj) = objf_ctds(bi,bj) + fctile_ctds

#ifdef ECCO_VERBOSE
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' COST_CTDS: irec,bi,bj            =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        ' COST_CTDS: cost function         = ', fctile_ctds
            call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
#endif

         enddo
        enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_R8( fcthread_ctds , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_CTDS: irec =  ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' ( CTD sal. )  = ',fcthread_ctds
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

      enddo
c--   End of second loop over records.

#else
c--   Do not enter the calculation of the CTD temperature contribution
c--   to the final cost function.

      fctile_ctds   = 0. _d 0
      fcthread_ctds = 0. _d 0

crg
      nrec = 1
crg

      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    ' cost_CTDS: no contribution of CTD temperature ',
     &                    ' to cost function.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,i9.8)')
     &    ' cost_CDTS: number of records that would have',
     &                      ' been processed: ',nrec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      return
      end
