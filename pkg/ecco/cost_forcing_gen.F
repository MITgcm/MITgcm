C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_forcing_gen.F,v 1.5 2006/03/02 02:53:23 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_forcing_gen(
     I                       myiter,
     I                       mytime,
     I                       startrec,
     I                       endrec,
     I                       xx_gen_file,
     I                       xx_gen_dummy,
     I                       xx_gen_period,
     I                       wmean_gen,
     I                       wgen,
     O                       num_gen_anom,
     O                       num_gen_mean,
     O                       objf_gen_anom,
     O                       objf_gen_mean,
     O                       objf_gen_smoo,
     I                       xx_gen_remo_intercept, 
     I                       xx_gen_remo_slope,
     I                       genmask,
     I                       mythid
     &                         )

c     ==================================================================
c     SUBROUTINE cost_forcing_gen
c     ==================================================================
c
c     o Generic routine for all forcing penalty terms (flux and bulk)
c
c     ==================================================================
c     SUBROUTINE cost_forcing_gen
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer startrec
      integer endrec
      character*(MAX_LEN_FNAM) xx_gen_file
      _RL xx_gen_dummy
      _RL xx_gen_period
      _RL wmean_gen
      _RL wgen(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL num_gen_anom(nsx,nsy)
      _RL num_gen_mean(nsx,nsy)
      _RL num_gen_smoo(nsx,nsy)
      _RL objf_gen_anom(nsx,nsy)
      _RL objf_gen_mean(nsx,nsy)
      _RL objf_gen_smoo(nsx,nsy)
      _RL xx_gen_remo_intercept
      _RL xx_gen_remo_slope
      _RS genmask(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,kk
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer nrec
      integer irec
      integer ilfld

      _RL fctile
      _RL fctilem
      _RL fctilemm
      _RL tmpx
      _RL sumcos
      _RL lengthscale

      _RL xx_mean(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      logical doglobalread
      logical ladinit

      character*(80) fnamefld

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

      lengthscale = 1. _d 0

c--   Read state record from global file.
      doglobalread = .false.
      ladinit      = .false.

c     Number of records to be used.
      nrec = endrec-startrec+1

      if (optimcycle .ge. 0) then
        ilfld=ilnblnk( xx_gen_file )
        write(fnamefld(1:80),'(2a,i10.10)') 
     &       xx_gen_file(1:ilfld),'.',optimcycle
      endif

c--   >>> Loop 1 to compute mean forcing:
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_mean(i,j,bi,bj)   = 0. _d 0
            enddo
          enddo
          num_gen_anom(bi,bj)  = 0. _d 0
          num_gen_mean(bi,bj)  = 0. _d 0
          num_gen_smoo(bi,bj)  = 0. _d 0
          objf_gen_anom(bi,bj) = 0. _d 0
          objf_gen_mean(bi,bj) = 0. _d 0
          objf_gen_smoo(bi,bj) = 0. _d 0
        enddo
      enddo

      do irec = 1,nrec

        call active_read_xy_loc( 
     &        fnamefld, tmpfld2d, irec, doglobalread,
     &        ladinit, optimcycle, mythid, xx_gen_dummy )

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
                xx_mean(i,j,bi,bj) = xx_mean(i,j,bi,bj)
     &                + tmpfld2d(i,j,bi,bj)
     &                - ( xx_gen_remo_intercept +
     &                    xx_gen_remo_slope*(irec-1)*xx_gen_period )
              enddo
            enddo
          enddo
        enddo

      enddo

      do bj = jtlo,jthi
        do bi = itlo,ithi

c--     Determine the weights to be used.
        kk = 1
        fctilem = 0. _d 0
        do j = jmin,jmax
          do i = imin,imax
            xx_mean(i,j,bi,bj)
     &            = xx_mean(i,j,bi,bj)/float(nrec)
            tmpx = xx_mean(i,j,bi,bj)/wmean_gen
            if (genmask(i,j,kk,bi,bj) .ne. 0.) then
              if ( ABS(R_low(i,j,bi,bj)) .LT. 100. )
     &              tmpx = tmpx*ABS(R_low(i,j,bi,bj))/100.
              fctilem = fctilem + cosphi(i,j,bi,bj)*tmpx*tmpx
              if ( cosphi(i,j,bi,bj) .ne. 0. )
     &             num_gen_mean(bi,bj) = num_gen_mean(bi,bj) + 1. _d 0
            endif
          enddo
        enddo

        objf_gen_mean(bi,bj) = objf_gen_mean(bi,bj) + fctilem

        enddo
      enddo

c--   >>> Loop 2 over records.
      do irec = 1,nrec

        call active_read_xy_loc( 
     &        fnamefld, tmpfld2d, irec, doglobalread,
     &        ladinit, optimcycle, mythid, xx_gen_dummy )

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            kk = 1
            fctile = 0. _d 0
            do j = jmin,jmax
              do i = imin,imax
                if (genmask(i,j,kk,bi,bj) .ne. 0.) then
                  tmpx = tmpfld2d(i,j,bi,bj)-xx_mean(i,j,bi,bj)
     &                   - ( xx_gen_remo_intercept +
     &                       xx_gen_remo_slope*(irec-1)*xx_gen_period )
                  if ( ABS(R_low(i,j,bi,bj)) .LT. 100. )
     &              tmpx = tmpx*ABS(R_low(i,j,bi,bj))/100.
                  fctile = fctile
     &                 + wgen(i,j,bi,bj)*cosphi(i,j,bi,bj)
     &                 *tmpx*tmpx
                  if ( wgen(i,j,bi,bj)*cosphi(i,j,bi,bj) .ne. 0. )
     &                 num_gen_anom(bi,bj) = num_gen_anom(bi,bj) 
     &                 + 1. _d 0
                endif
              enddo
            enddo

            objf_gen_anom(bi,bj) = objf_gen_anom(bi,bj) + fctile

          enddo
        enddo

c--   End of loop over records.
      enddo

#ifdef ALLOW_SMOOTH_BC_COST_CONTRIBUTION

c--   >>> Loop 2 over records.
      do irec = 1,nrec

        call active_read_xy_loc( 
     &        fnamefld, tmpfld2d, irec, doglobalread,
     &        ladinit, optimcycle, mythid, xx_gen_dummy )

        _EXCH_XY_R8(tmpfld2d, mythid)

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            kk = 1
            fctile = 0. _d 0
            do j = jmin,jmax
              do i = imin,imax
                if (genmask(i,j,kk,bi,bj) .ne. 0.) then
                  tmpx = 
     &                 ( tmpfld2d(i+2,j,bi,bj)-tmpfld2d(i+1,j,bi,bj) )
     &                   *maskW(i+1,j,kk,bi,bj)*maskW(i+2,j,kk,bi,bj)
     &               + ( tmpfld2d(i+1,j,bi,bj)-tmpfld2d(i,j,bi,bj) )
     &                   *maskW(i+1,j,kk,bi,bj)
     &               + ( tmpfld2d(i,j+2,bi,bj)-tmpfld2d(i,j+1,bi,bj) )
     &                   *maskS(i,j+1,kk,bi,bj)*maskS(i,j+2,kk,bi,bj)
     &               + ( tmpfld2d(i,j+1,bi,bj)-tmpfld2d(i,j,bi,bj) )
     &                   *maskS(i,j+1,kk,bi,bj)
                  if ( ABS(R_low(i,j,bi,bj)) .LT. 100. )
     &              tmpx = tmpx*ABS(R_low(i,j,bi,bj))/100.
                  fctile = fctile
     &               + wgen(i,j,bi,bj)*cosphi(i,j,bi,bj)
     *                 *0.0161*lengthscale/4.0
     &                 *tmpx*tmpx
                  if ( wgen(i,j,bi,bj)*cosphi(i,j,bi,bj) .ne. 0. )
     &                 num_gen_smoo(bi,bj) = num_gen_smoo(bi,bj) 
     &                 + 1. _d 0
                endif
              enddo
            enddo

            objf_gen_smoo(bi,bj) = objf_gen_smoo(bi,bj) + fctile

          enddo
        enddo

c--   End of loop over records.
      enddo

#endif

      return
      end

