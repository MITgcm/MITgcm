
#include "COST_CPPOPTIONS.h"


      subroutine cost_mean_heatflux(
     I                       myiter,
     I                       mytime,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_mean_heatflux
c     ==================================================================
c
c     o Evaluate cost function contribution of sea surface salinity.
c
c     started: Elisabeth Remy 19-mar-2001 copy from cost_sst.F
c
c     ==================================================================
c     SUBROUTINE cost_mean_heatflux
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PARAMS.h"

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
      integer i,j,kk
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer levmon
      integer levoff
      integer ilsalt

      _RL fctilemm
      _RL fcthreadmm
      _RL tmpx
      _RL sumcos


      character*(80) fnamehflux

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

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        ilsalt = ilnblnk( hfluxbarfile )
        write(fnamehflux(1:80),'(2a,i10.10)')
     &    hfluxbarfile(1:ilsalt),'.',optimcycle
      endif

      fcthreadmm       = 0. _d 0

      irec = 1

c--     Read time averages and the monthly mean data.
        call active_read_xy( fnamehflux, tmpfld2d, irec,
     &                       doglobalread, ladinit,
     &                       optimcycle, mythid,
     &                       xx_hflux_mean_dummy )

        do bj = jtlo,jthi
          do bi = itlo,ithi
            kk = 1
            fctilemm = 0. _d 0
            sumcos   = 0. _d 0
            do j = jmin,jmax
              do i = imin,imax
                tmpx=tmpfld2d(i,j,bi,bj)
                if (maskC(i,j,kk,bi,bj) .ne. 0.) then
                  fctilemm = fctilemm + tmpx
     $                  *cos(yc(i,j,bi,bj)*deg2rad)
                  sumcos = sumcos + cos(yc(i,j,bi,bj)*deg2rad)
                endif
              enddo
            enddo
            if(sumcos.eq.0) sumcos=1
            fctilemm = (fctilemm / sumcos)
            fctilemm = whfluxmm(bi,bj) * (fctilemm )

            objf_hfluxmm(bi,bj) = fctilemm
            fcthreadmm          = fcthreadmm + fctilemm

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_R8( fcthreadmm , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' ecco_cost.heatflux:                       irec =  ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,d22.15)')
     &    '                 global cost function value = ',
     &    fcthreadmm
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

          enddo
        enddo


#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_R8( fcthreadmm       , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (        ) = ',fcthreadmm    
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

#else
c--   Do not enter the calculation of the temperature contribution to
c--   the final cost function.

      fctilemm         = 0. _d 0
      fcthreadmm       = 0. _d 0

      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    ' cost_: no contribution of temperature field ',
     &                 'to cost function.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,i9.8)')
     &    ' cost_: number of records that would have',
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

