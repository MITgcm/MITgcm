C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_curmtr.F,v 1.3 2007/10/09 00:02:50 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"

      subroutine cost_CurMtr(
     I                     myiter,
     I                     mytime,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_CurMtr
c     ==================================================================
c
c     o Evaluate cost function contribution of Vector-Measuring
c       Current Meters.
c
c     started:  Elisabeth Remy eremy@ucsd.edu 30-Aug-2000
c     modified: G. Gebbie, gebbie@mit.edu, 3- May- 2002.
c
c     ==================================================================
c     SUBROUTINE cost_CurMtr
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

      _RL fctile_curmtr
      _RL fcthread_curmtr
      _RL wwwu (1-olx:snx+olx,1-oly:sny+oly)
      _RL wwwv (1-olx:snx+olx,1-oly:sny+oly)
      _RL wu (1-olx:snx+olx,1-oly:sny+oly)
      _RL wv (1-olx:snx+olx,1-oly:sny+oly)
      _RL umask (1-olx:snx+olx,1-oly:sny+oly)
      _RL vmask (1-olx:snx+olx,1-oly:sny+oly)
      _RL tmpuobs (1-olx:snx+olx,1-oly:sny+oly)
      _RL tmpubar (1-olx:snx+olx,1-oly:sny+oly)
      _RL tmpvobs (1-olx:snx+olx,1-oly:sny+oly)
      _RL tmpvbar (1-olx:snx+olx,1-oly:sny+oly)
      _RL spval

      character*(80) fnameuvel
      character*(80) fnamevvel

      logical doglobalread
      logical ladinit

      character*(MAX_LEN_MBUF) msgbuf

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

      spval = -9990.

c--   Read state record from global file.
      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_CURMTR_COST_CONTRIBUTION

#ifdef ECCO_VERBOSE
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i8.8)')
     &  ' cost_Curmtr: number of records to process =', nmonsrec
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

      if (optimcycle .ge. 0) then
        ilu=ilnblnk( ubarfile )
        write(fnameuvel(1:80),'(2a,i10.10)')
     &       ubarfile(1:ilu), '.', optimcycle
      endif

      if (optimcycle .ge. 0) then
        ilu=ilnblnk( vbarfile )
        write(fnamevvel(1:80),'(2a,i10.10)')
     &       vbarfile(1:ilu), '.', optimcycle
      endif

      fcthread_curmtr = 0. _d 0

c--   Loop over records.
      do irec = 1,nmonsrec

cgg    First do the Zonal Velocity.
c--     Read time averages and the monthly mean data.
        call active_read_xyz( fnameuvel, ubar, irec,
     &          doglobalread, ladinit,
     &          optimcycle, mythid, xx_ubar_mean_dummy )


cgg    Then meridional velocity.
        call active_read_xyz( fnamevvel, vbar, irec,
     &          doglobalread, ladinit,
     &          optimcycle, mythid, xx_vbar_mean_dummy )

cgg( fixed precision of file.)
        call mdsreadfield( curmtrufile,cost_iprec, 'RL', nr, curmtruobs,
     &                           irec,   mythid)

        call mdsreadfield( curmtrvfile,cost_iprec, 'RL', nr, curmtrvobs,
     &                           irec,   mythid)


c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Loop over the model layers

            fctile_curmtr = 0. _d 0

            do k = 1,nr

c--           Determine the weights to be used.
              do j = jmin,jmax
                do i = imin,imax
                  umask(i,j) = 1. _d 0
                  vmask(i,j) = 1. _d 0
                  if (curmtruobs(i,j,k,bi,bj) .eq. 0.) then
                    umask(i,j) = 0. _d 0
                  endif

                  if (curmtrvobs(i,j,k,bi,bj) .eq. 0.) then
                    vmask(i,j) = 0. _d 0
                  endif

                  if (curmtruobs(i,j,k,bi,bj) .lt. spval) then
                    umask(i,j) = 0. _d 0
                  endif

                  if (curmtrvobs(i,j,k,bi,bj) .lt. spval) then
                    vmask(i,j) = 0. _d 0
                  endif

cph(
cph               print *, 'WARNING: SPECIFIC SETUP FOR ECCO'
cph               below statement could be replaced by following
cph               to make it independnet of Nr:
cph
cph               if ( rC(K) .GT. -1000. ) then
cph)
c                 set cmask=0 in areas shallower than 1000m
cgg                  if (_hFacC(i,j,13,bi,bj) .eq. 0.) then
cgg                    cmask(i,j) = 0. _d 0
cgg                  endif

                  if (_hFacW(i,j,k,bi,bj) .eq. 0.) then
                    umask(i,j) = 0. _d 0
                  endif

                  if (_hFacS(i,j,k,bi,bj) .eq. 0.) then
                    vmask(i,j) = 0. _d 0
                  endif

                enddo
              enddo

              do j = jmin,jmax
                do i = imin,imax
                  wwwu(i,j)    = cosphi(i,j,bi,bj)*umask(i,j)
                  wwwv(i,j)    = cosphi(i,j,bi,bj)*vmask(i,j)

                  tmpuobs(i,j) = curmtruobs(i,j,k,bi,bj)
                  tmpubar(i,j) = ubar(i,j,k,bi,bj)

                  tmpvobs(i,j) = curmtrvobs(i,j,k,bi,bj)
                  tmpvbar(i,j) = vbar(i,j,k,bi,bj)

#ifdef SUBEX_2DEG
cgg(  Perform the comparison in observation space.
                  if ( (i.eq.5) .and. (j.eq.13)) then
         tmpubar(i,j) = ubar(5,12,k,bi,bj)
         tmpvbar(i,j) = (vbar(4,12,k,bi,bj) + vbar(5,12,k,bi,bj)
     &                 +vbar(4,13,k,bi,bj) + vbar(5,13,k,bi,bj))/4.
                  endif
                  if ( (i .eq. 11) .and. (j .eq. 13)) then
         tmpubar(i,j) = ubar(11,12,k,bi,bj)
         tmpvbar(i,j) = (vbar(10,12,k,bi,bj) + vbar(11,12,k,bi,bj)
     &                 +vbar(10,13,k,bi,bj) + vbar(11,13,k,bi,bj))/4.
                  endif
                  if ( (i .eq. 8) .and. (j .eq. 9)) then
         tmpubar(i,j) = (ubar(7,8,k,bi,bj) + ubar(8,8,k,bi,bj)
     &                 +ubar(7,9,k,bi,bj) + ubar(8,9,k,bi,bj))/4.
         tmpvbar(i,j) = vbar(7,9,k,bi,bj)
                  endif
                  if ( (i .eq. 5) .and. (j .eq. 5)) then
         tmpubar(i,j) = (ubar(5,4,k,bi,bj) + ubar(5,5,k,bi,bj))/2.
         tmpvbar(i,j) = (vbar(4,5,k,bi,bj) + vbar(5,5,k,bi,bj))/2.
                  endif
                  if ( (i .eq. 11) .and. (j .eq. 5)) then
         tmpubar(i,j) = (ubar(11,4,k,bi,bj)+ubar(11,5,k,bi,bj))/2.
         tmpvbar(i,j) = (vbar(10,5,k,bi,bj)+vbar(11,5,k,bi,bj))/2.
                  endif
#endif

cgg(  Weights are subject to individual's preference.
                  wu(i,j) = wcurrent2(i,j,k,bi,bj)
                  wv(i,j) = wcurrent2(i,j,k,bi,bj)
cgg                  wtmp(i,j) = wtheta2(i,j,k,bi,bj)
                enddo
              enddo

              do j = jmin,jmax
                do i = imin,imax
c--               The array tmpuobs contains current meter velocities.
cgg                  if(umask(i,j).eq.1.0 .and.tmpubar(i,j) .ne. 0. ) then
cgg                   print *,'cost_curmtr U',i,j,tmpubar(i,j),tmpuobs(i,j)
cgg                  endif

cgg                  if(vmask(i,j).eq.1.0.and.tmpvbar(i,j) .ne. 0.) then
cgg                   print *,'cost_curmtr V',i,j,tmpvbar(i,j),tmpvobs(i,j)
cgg                  endif

cgg( Add one more zero check.
                  if (tmpubar(i,j) .ne. 0.) then
                    fctile_curmtr = fctile_curmtr +
     &                             (wu(i,j)*wwwu(i,j))*
     &                             (tmpubar(i,j)-tmpuobs(i,j))*
     &                             (tmpubar(i,j)-tmpuobs(i,j))
                    if ( wu(i,j)*wwwu(i,j)) .ne. 0. )
     &                  num_curmtr(bi,bj) = num_curmtr(bi,bj) + 1. _d 0
                  endif

cgg( Add one more zero check.
                  if (tmpvbar(i,j) .ne. 0.) then
                    fctile_curmtr = fctile_curmtr +
     &                             (wv(i,j)*wwwv(i,j))*
     &                             (tmpvbar(i,j)-tmpvobs(i,j))*
     &                             (tmpvbar(i,j)-tmpvobs(i,j))
                    if ( wv(i,j)*wwwv(i,j)) .ne. 0. )
     &                  num_curmtr(bi,bj) = num_curmtr(bi,bj) + 1. _d 0
                  endif
                enddo
              enddo
            enddo
c--         End of loop over layers.

            fcthread_curmtr    = fcthread_curmtr    + fctile_curmtr
            objf_curmtr(bi,bj) = objf_curmtr(bi,bj) + fctile_curmtr

#ifdef ECCO_VERBOSE
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' COST_curmtr: irec,bi,bj            =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        ' COST_Curmtr: cost function         = ', fctile_curmtr
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
        _GLOBAL_SUM_R8( fcthread_curmtr , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_Curmtr: irec =  ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' ( CurMtr )  = ',fcthread_curmtr
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

      fctile_curmtr   = 0. _d 0
      fcthread_curmtr = 0. _d 0

crg
      nrec = 1
crg

      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    ' cost_Curmtr: no contribution of CTD temperature ',
     &                    ' to cost function.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,i9.8)')
     &    ' cost_Curmtr: number of records that would have',
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







