
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

      _RL tmpx
      _RL sumtot
      _RL fctiletot


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

      do irec = 1, MAX(1,nyearsrec)

c--     Read time averages and the monthly mean data.
        call active_read_xy( fnamesflux, tmpfld2d, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_sflux_mean_dummy )

        sumtot    = 0.
        fctiletot = 0.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            kk = 1
            do j = jmin,jmax
              do i = imin,imax
                tmpx=tmpfld2d(i,j,bi,bj)
                if (maskC(i,j,kk,bi,bj) .ne. 0.) then
                   fctiletot = fctiletot
     &                + tmpx*cos(yc(i,j,bi,bj)*deg2rad)
                   sumtot = sumtot
     &                + cos(yc(i,j,bi,bj)*deg2rad)
                   num_sfluxmm(bi,bj) = num_sfluxmm(bi,bj) + 1
                endif
              enddo
            enddo
          enddo
        enddo

       _GLOBAL_SUM_R8( sumtot , myThid )
       _GLOBAL_SUM_R8( fctiletot , myThid )
       
       if (sumtot.eq.0.) sumtot = 1.

       if ( wmean_sflux .NE. 0. ) then
          objf_sfluxmm = objf_sfluxmm
     &        + (fctiletot/sumtot/wmean_sflux)**2
       else
          objf_sfluxmm = 0. _d 0
       endif

c-- diagnostic: imbalance per year:
       write(standardmessageunit,'(A,I5,2(X,D22.14))')
     &      ' --> bal_sfluxmm    =', irec,
     &      fctiletot/sumtot,
     &      (fctiletot/sumtot/wmean_sflux)**2

      enddo

#endif

      return
      end

