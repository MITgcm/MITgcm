C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_xbt.F,v 1.7 2007/10/09 00:02:51 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_XBT(
     I                     myiter,
     I                     mytime,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_XBT
c     ==================================================================
c
c     o Evaluate cost function contribution of XBT temperature data.
c
c     started:  Elisabeth Remy eremy@ucsd.edu 30-Aug-2000
c
c
c     ==================================================================
c     SUBROUTINE cost_XBT
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

      _RL fctile_xbt
      _RL fcthread_xbt
      _RL www (1-olx:snx+olx,1-oly:sny+oly)
      _RL tmpobs (1-olx:snx+olx,1-oly:sny+oly)
      _RL cmask (1-olx:snx+olx,1-oly:sny+oly)
      _RL spval
      _RL spmax
      _RL ztop,rl_35,rl_0

      character*(80) fnametheta

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
      _RL SW_PTMP
      external SW_PTMP

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      spval = -1.8
      spmax = 40.
      ztop  = -.981*1.027
      rl_35 = 35.0
      rl_0  = 0.0

c--   Read state record from global file.
      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_XBT_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        ilu=ilnblnk( tbarfile )
        write(fnametheta(1:80),'(2a,i10.10)')
     &       tbarfile(1:ilu),'.',optimcycle
      endif

      fcthread_xbt = 0. _d 0

cnew(
      mody = modelstartdate(1)/10000
      modm = modelstartdate(1)/100 - mody*100
cnew)

c--   Loop over records.
      do irec = 1,nmonsrec

c--     Read time averages and the monthly mean data.
        call active_read_xyz( fnametheta, tbar, irec,
     &          doglobalread, ladinit,
     &          optimcycle, mythid
     &   , xx_tbar_mean_dummy )

cnew(
        iyear = mody + INT((modm-1+irec-1)/12)
        imonth = 1 + MOD(modm-1+irec-1,12)
        il=ilnblnk(xbtfile)
        write(fnametmp(1:80),'(2a,i4)')
     &       xbtfile(1:il), '_', iyear
        inquire( file=fnametmp, exist=exst )
        if (.NOT. exst) then
           write(fnametmp(1:80),'(a)') xbtfile(1:il)
            imonth = irec
        endif

        call mdsreadfield( fnametmp, cost_iprec, 'RL', nr, xbtobs,
     &                     imonth, mythid)
cnew)

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Loop over the model layers

            fctile_xbt = 0. _d 0

            do k = 1,nr

c--           Determine the weights to be used.
              do j = jmin,jmax
                do i = imin,imax
                  cmask(i,j) = cosphi(i,j,bi,bj)
                  if ( xbtobs(i,j,k,bi,bj) .lt. spval .or.
     &		       xbtobs(i,j,k,bi,bj) .gt. spmax .or.
     &                 xbtobs(i,j,k,bi,bj) .eq. 0. ) then
                    cmask(i,j) = 0. _d 0
                  endif

cph(
cph               print *, 'WARNING: SPECIFIC SETUP FOR ECCO'
cph               below statement could be replaced by following
cph               to make it independnet of Nr:
cph
cph               if ( rC(K) .GT. -1000. ) then
cph)

                enddo
              enddo

              do j = jmin,jmax
                do i = imin,imax
                  if ( _hFacC(i,j,k,bi,bj) .ne. 0. .AND.
     &                 _hFacC(i,j,13,bi,bj) .ne. 0. ) then
                    tmpobs(i,j) = SW_PTMP(rl_35,
     $                   xbtobs(i,j,k,bi,bj),ztop*rc(k),rl_0)
c--               The array ctdtobs contains CTD temperature.

                    fctile_xbt = fctile_xbt +
     &                   (wtheta2(i,j,k,bi,bj)*cmask(i,j))*
     &                   (tbar(i,j,k,bi,bj)-tmpobs(i,j))*
     &                   (tbar(i,j,k,bi,bj)-tmpobs(i,j))
                    if ( wtheta2(i,j,k,bi,bj)*cmask(i,j) .ne. 0. )
     &                   num_xbt(bi,bj) = num_xbt(bi,bj) + 1. _d 0
                  endif
                enddo
              enddo
c--         End of loop over layers.
            enddo

            fcthread_xbt    = fcthread_xbt    + fctile_xbt
            objf_xbt(bi,bj) = objf_xbt(bi,bj) + fctile_xbt

#ifdef ECCO_VERBOSE
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' COST_XBT: irec,bi,bj            =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        ' COST_XBT: cost function         = ', fctile_xbt
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
        _GLOBAL_SUM_R8( fcthread_xbt , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_XBT: irec =  ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' ( XBT temp. )  = ',fcthread_xbt
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

      enddo
c--   End of second loop over records.

#else
c--   Do not enter the calculation of the XBT temperature contribution
c--   to the final cost function.

      fctile_xbt   = 0. _d 0
      fcthread_xbt = 0. _d 0

crg
      nrec = 1
crg

      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    ' cost_XBT: no contribution of XBT temperature ',
     &                    ' to cost function.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,i9.8)')
     &    ' cost_XBT: number of records that would have',
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
