
#include "COST_CPPOPTIONS.h"


      subroutine cost_mean_saltflux(
     I                       myiter,
     I                       mytime,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_mean_saltflux
c     ==================================================================
c
c     o Evaluate cost function contribution of sea surface salinity.
c
c     started: Elisabeth Remy 19-mar-2001 copy from cost_sst.F
c
c     ==================================================================
c     SUBROUTINE cost_mean_saltflux
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


      character*(80) fnamesflux

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

#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        ilsalt = ilnblnk( sfluxbarfile )
        write(fnamesflux(1:80),'(2a,i10.10)')
     &    sfluxbarfile(1:ilsalt),'.',optimcycle
      endif

      fcthreadmm       = 0. _d 0

      irec = 1

c--     Read time averages and the monthly mean data.
        call active_read_xy( fnamesflux, tmpfld2d, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_sflux_mean_dummy )

        do bj = jtlo,jthi
          do bi = itlo,ithi
            kk = 1
            fctilemm = 0. _d 0
            sumcos   = 0. _d 0
            do j = jmin,jmax
              do i = imin,imax
                tmpx=tmpfld2d(i,j,bi,bj)
                if (maskC(i,j,kk,bi,bj) .ne. 0.) then
                   fctilemm = fctilemm+tmpx*cos(yc(i,j,bi,bj)*deg2rad)
                  sumcos = sumcos + cos(yc(i,j,bi,bj)*deg2rad)
                endif
              enddo
            enddo

            if(sumcos.eq.0) sumcos=1.0
            fctilemm = (fctilemm / sumcos)
            fctilemm = wsfluxmm(bi,bj) * (fctilemm )

            objf_sfluxmm(bi,bj) = fctilemm
            fcthreadmm          = fcthreadmm + fctilemm

          enddo
        enddo

#endif

      return
      end

