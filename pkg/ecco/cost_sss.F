C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_sss.F,v 1.4 2005/03/28 19:40:59 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_sss(
     I                       myiter,
     I                       mytime,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_sss
c     ==================================================================
c
c     o Evaluate cost function contribution of sea surface salinity.
c
c     started: Elisabeth Remy 19-mar-2001 copy from cost_sst.F
c
c     ==================================================================
c     SUBROUTINE cost_sss
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
      integer irec
      integer levmon
      integer levoff
      integer ilsalt

      _RL fctile_sss
      _RL fcthread_sss

      _RL cmask (1-olx:snx+olx,1-oly:sny+oly)  
      _RL spval
      _RL spmax

      character*(80) fnamesalt

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

      spval = 25.0
      spmax = 40.0

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_SSS_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        ilsalt = ilnblnk( sbarfile )
        write(fnamesalt(1:80),'(2a,i10.10)')
     &    sbarfile(1:ilsalt),'.',optimcycle
      endif

      fcthread_sss = 0. _d 0

c--   Loop over records.
      do irec = 1,nmonsrec

c--     Read time averages and the monthly mean data.
        call active_read_xyz( fnamesalt, sbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_sbar_mean_dummy )

        call cost_ReadSSSFields( irec, mythid )

        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Loop over the model layers
            fctile_sss = 0. _d 0
            k = 1

c--           Determine the mask on weights
              do j = jmin,jmax
                do i = imin,imax
                  cmask(i,j) = 1. _d 0
                  if (sssdat(i,j,bi,bj) .lt. spval .or.
     &		      sssdat(i,j,bi,bj) .gt. spmax .or.
     &                sssdat(i,j,bi,bj) .eq. 0.) then
                    cmask(i,j) = 0. _d 0
                  endif
                  
cph(
cph               print *, 'WARNING: SPECIFIC SETUP FOR ECCO'
cph               below statement could be replaced by following
cph               to make it independnet of Nr:
cph
cph               if ( rC(K) .GT. -1000. ) then
cph)
c                 set cmask=0 in areas shallower than 1000m 
cph                  if (_hFacC(i,j,13,bi,bj) .eq. 0.) then
cph                    cmask(i,j) = 0. _d 0
cph                  endif

c--         Compute cost rel. to monthly SSS field.
                if (_hFacC(i,j,k,bi,bj) .ne. 0.) then
                   fctile_sss = fctile_sss +
     &                  wsss(i,j,bi,bj)*cosphi(i,j,bi,bj)*cmask(i,j)*
     &                  ( (sbar(i,j,k,bi,bj)-sssdat(i,j,bi,bj))*
     &                    (sbar(i,j,k,bi,bj)-sssdat(i,j,bi,bj))*
     &                           sssmask(i,j,bi,bj) )
                endif
              enddo
            enddo


            fcthread_sss     = fcthread_sss + fctile_sss
            objf_sss(bi,bj)  = objf_sss(bi,bj)  + fctile_sss

#ifdef ECCO_VERBOSE
c--         Print cost function for each tile in each thread.
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' cost_sss: irec,bi,bj          =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        '     cost function (sss)         = ',
     &        fctile_sss
            call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

          enddo
        enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_R8( fcthread_sss   , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_sss: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (    SST    ) = ',fcthread_sss
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

      enddo
c--   End of loop over records.

#else
c--   Do not enter the calculation of the temperature contribution to
c--   the final cost function.

      fctile_sss     = 0. _d 0
      fcthread_sss   = 0. _d 0

#ifdef ECCO_VERBOSE
      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    ' cost_sss: no contribution of temperature field ',
     &                 'to cost function.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,i9.8)')
     &    ' cost_sss: number of records that would have',
     &                ' been processed: ',nmonsrec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

#endif

      return
      end

