C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_tmi.F,v 1.4 2005/03/28 23:49:49 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_tmi(
     I                       myiter,
     I                       mytime,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_tmi
c     ==================================================================
c
c     o Evaluate cost function contribution of temperature.
c
c     started: Ralf Giering 17-Jan-2001 copy from cost_theta.F
c
c     ==================================================================
c     SUBROUTINE cost_tmi
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
      integer iltheta

      _RL fctile_tmi
      _RL fcthread_tmi

      _RL cmask (1-olx:snx+olx,1-oly:sny+oly)  
      _RL spval
      _RL spmax

      character*(80) fnametheta

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

      spval = -1.8
      spmax = 40.

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_TMI_SST_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        iltheta = ilnblnk( tbarfile )
        write(fnametheta(1:80),'(2a,i10.10)')
     &    tbarfile(1:iltheta),'.',optimcycle
      endif

      fcthread_tmi = 0. _d 0

c--   Loop over records.
      do irec = 1,nmonsrec

c--     Read time averages and the monthly mean data.
        call active_read_xyz( fnametheta, tbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_theta_dummy )

        call cost_ReadTMIFields( irec, mythid )

        do bj = jtlo,jthi
          do bi = itlo,ithi

            fctile_tmi = 0. _d 0
            k = 1

c--         Compute cost rel. to monthly TMI SST climatology field.

c--           Determine the mask on weights
              do j = jmin,jmax
                do i = imin,imax
                  cmask(i,j) = cosphi(i,j,bi,bj)
                  if (tmidat(i,j,bi,bj) .eq. 0.) then
                    cmask(i,j) = 0. _d 0
                  else if (tmidat(i,j,bi,bj) .lt. spval) then
                    cmask(i,j) = 0. _d 0
                  else if (tmidat(i,j,bi,bj) .gt. spmax) then
                    cmask(i,j) = 0. _d 0
                  endif
                  
c--   set cmask=0 in areas shallower than 1000m 
                  if (_hFacC(i,j,13,bi,bj) .eq. 0.) then
                    cmask(i,j) = 0. _d 0
                  endif
                enddo
              enddo

            do j = jmin,jmax
              do i = imin,imax
                if (_hFacC(i,j,k,bi,bj) .ne. 0.) then
                   fctile_tmi = fctile_tmi +
     &                  wsst(i,j,bi,bj)*cmask(i,j)*
     &                  ( (tbar(i,j,k,bi,bj)-tmidat(i,j,bi,bj))*
     &                    (tbar(i,j,k,bi,bj)-tmidat(i,j,bi,bj)) )
                   if ( wsst(i,j,bi,bj)*cmask(i,j) .ne. 0. )
     &                  num_tmi(bi,bj)  = num_tmi(bi,bj) + 1. _d 0
                endif
              enddo
            enddo

            fcthread_tmi     = fcthread_tmi + fctile_tmi
            objf_tmi(bi,bj)  = objf_tmi(bi,bj)  + fctile_tmi

#ifdef ECCO_VERBOSE
c--         Print cost function for each tile in each thread.
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' cost_tmi: irec,bi,bj          =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        '     cost function (tmi)         = ',
     &        fctile_tmi
            call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

          enddo
        enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_R8( fcthread_tmi   , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_tmi: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (    TMI    ) = ',fcthread_tmi
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

      fctile_tmi     = 0. _d 0
      fcthread_tmi   = 0. _d 0

      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    ' cost_tmi: no contribution of temperature field ',
     &                 'to cost function.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,i9.8)')
     &    ' cost_tmi: number of records that would have',
     &                ' been processed: ',nmonsrec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      return
      end

