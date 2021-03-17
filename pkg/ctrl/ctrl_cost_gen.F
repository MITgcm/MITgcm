c ----------------------------------------------------------------
c --- ctrl_cost_gen2d
c --- ctrl_cost_gen3d
c ----------------------------------------------------------------

c ----------------------------------------------------------------

#include "CTRL_OPTIONS.h"
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ctrl_cost_gen2d
C     !INTERFACE:
      subroutine ctrl_cost_gen2d(
     I                       startrec,
     I                       endrec,
     I                       xx_gen_file,
     I                       xx_gen_dummy,
     I                       xx_gen_period,
     I                       xx_gen_weight,
     I                       dodimensionalcost,
     O                       num_gen_anom,
     O                       objf_gen_anom,
#ifdef ECCO_CTRL_DEPRECATED
     I                       xx_gen_wmean,
     O                       num_gen_mean,
     O                       objf_gen_mean,
     O                       objf_gen_smoo,
     I                       xx_gen_remo_intercept,
     I                       xx_gen_remo_slope,
#endif /* ECCO_CTRL_DEPRECATED */
     I                       xx_gen_mask2D,
     I                       myThid
     &                         )

C     !DESCRIPTION: \bv
C     Generic routine for all 2D control penalty terms
C     \ev

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_ECCO
#  include "ecco.h"
#endif
#ifdef ALLOW_CTRL
# include "ctrl.h"
# include "optim.h"
#endif

c     == routine arguments ==

      integer startrec
      integer endrec
      character*(MAX_LEN_FNAM) xx_gen_file
      _RL xx_gen_dummy
      _RL xx_gen_period
      _RL xx_gen_weight(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      logical dodimensionalcost
      _RL num_gen_anom(nsx,nsy)
      _RL objf_gen_anom(nsx,nsy)
#ifdef ECCO_CTRL_DEPRECATED
      _RL xx_gen_wmean
      _RL num_gen_mean(nsx,nsy)
      _RL num_gen_smoo(nsx,nsy)
      _RL objf_gen_mean(nsx,nsy)
      _RL objf_gen_smoo(nsx,nsy)
      _RL xx_gen_remo_intercept
      _RL xx_gen_remo_slope
#endif /* ECCO_CTRL_DEPRECATED */
      _RS xx_gen_mask2D(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer myThid

#ifdef ALLOW_CTRL

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
      _RL tmpx
      _RL lengthscale

#ifdef ECCO_CTRL_DEPRECATED
      _RL xx_mean(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif /* ECCO_CTRL_DEPRECATED */

      logical doglobalread
      logical ladinit

      character*(80) fnamefld

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

CEOP

      jtlo = mybylo(myThid)
      jthi = mybyhi(myThid)
      itlo = mybxlo(myThid)
      ithi = mybxhi(myThid)
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
          num_gen_anom(bi,bj)  = 0. _d 0
          objf_gen_anom(bi,bj) = 0. _d 0
#ifdef ECCO_CTRL_DEPRECATED
          do j = jmin,jmax
            do i = imin,imax
              xx_mean(i,j,bi,bj)   = 0. _d 0
            enddo
          enddo
          num_gen_mean(bi,bj)  = 0. _d 0
          num_gen_smoo(bi,bj)  = 0. _d 0
          objf_gen_mean(bi,bj) = 0. _d 0
          objf_gen_smoo(bi,bj) = 0. _d 0
#endif /* ECCO_CTRL_DEPRECATED */
        enddo
      enddo

#ifndef ECCO_CTRL_DEPRECATED
c--   >>> Loop over records.
      do irec = 1,nrec

#ifdef ALLOW_AUTODIFF
        call active_read_xy(
     &        fnamefld, tmpfld2d, irec, doglobalread,
     &        ladinit, optimcycle, myThid, xx_gen_dummy )
#else
        CALL READ_REC_XY_RL( fnamefld, tmpfld2d, iRec, 1, myThid )
#endif

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            fctile = 0. _d 0
            do j = jmin,jmax
              do i = imin,imax
                if (xx_gen_mask2D(i,j,bi,bj) .ne. 0. _d 0) then

                tmpx = tmpfld2d(i,j,bi,bj)
                IF ( dodimensionalcost ) THEN
                  fctile = fctile + xx_gen_weight(i,j,bi,bj)*tmpx*tmpx
                ELSE
                  fctile = fctile + tmpx*tmpx
                ENDIF
                if ( xx_gen_weight(i,j,bi,bj) .ne. 0. _d 0 )
     &            num_gen_anom(bi,bj) = num_gen_anom(bi,bj)
     &            + 1. _d 0
                endif

              enddo
            enddo

            objf_gen_anom(bi,bj) = objf_gen_anom(bi,bj) + fctile

          enddo
        enddo

c--   End of loop over records.
      enddo

#else /* ECCO_CTRL_DEPRECATED */

      IF ( dodimensionalcost ) THEN
      do irec = 1,nrec

#ifdef ALLOW_AUTODIFF
        call active_read_xy(
     &        fnamefld, tmpfld2d, irec, doglobalread,
     &        ladinit, optimcycle, myThid, xx_gen_dummy )
#else
        CALL READ_REC_XY_RL( fnamefld, tmpfld2d, iRec, 1, myThid )
#endif

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

      if ( xx_gen_wmean .NE. 0. ) then
       do bj = jtlo,jthi
        do bi = itlo,ithi
c--     Determine the weights to be used.
        fctilem = 0. _d 0
        do j = jmin,jmax
          do i = imin,imax
            xx_mean(i,j,bi,bj)
     &            = xx_mean(i,j,bi,bj)/float(nrec)
            tmpx = xx_mean(i,j,bi,bj)/xx_gen_wmean
            if (xx_gen_mask2d(i,j,bi,bj) .ne. 0. _d 0) then
#ifdef ALLOW_ECCO
              if ( ABS(R_low(i,j,bi,bj)) .LT. 100.  _d 0)
     &              tmpx = tmpx*ABS(R_low(i,j,bi,bj))/100.  _d 0
              fctilem = fctilem + cosphi(i,j,bi,bj)*tmpx*tmpx
              if ( cosphi(i,j,bi,bj) .ne. 0. _d 0)
     &             num_gen_mean(bi,bj) = num_gen_mean(bi,bj) + 1. _d 0
#else
              fctilem = fctilem + tmpx*tmpx
                   num_gen_mean(bi,bj) = num_gen_mean(bi,bj) + 1. _d 0
#endif
            endif
          enddo
        enddo
        objf_gen_mean(bi,bj) = objf_gen_mean(bi,bj) + fctilem
        enddo
       enddo
      endif
      ENDIF !IF ( dodimensionalcost ) THEN

c--   >>> Loop 2 over records.
      do irec = 1,nrec

#ifdef ALLOW_AUTODIFF
        call active_read_xy(
     &        fnamefld, tmpfld2d, irec, doglobalread,
     &        ladinit, optimcycle, myThid, xx_gen_dummy )
#else
        CALL READ_REC_XY_RL( fnamefld, tmpfld2d, iRec, 1, myThid )
#endif

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            fctile = 0. _d 0
            do j = jmin,jmax
              do i = imin,imax
                if (xx_gen_mask2d(i,j,bi,bj) .ne. 0. _d 0) then

                IF ( dodimensionalcost ) THEN
                  tmpx = tmpfld2d(i,j,bi,bj)
     &                   - xx_mean(i,j,bi,bj)
     &                   - ( xx_gen_remo_intercept +
     &                       xx_gen_remo_slope*(irec-1)*xx_gen_period )
#ifdef ALLOW_ECCO
                  if ( ABS(R_low(i,j,bi,bj)) .LT. 100. _d 0 )
     &              tmpx = tmpx*ABS(R_low(i,j,bi,bj))/100. _d 0
                  fctile = fctile + xx_gen_weight(i,j,bi,bj)*tmpx*tmpx
     &                   * cosphi(i,j,bi,bj)
#else
                  fctile = fctile + xx_gen_weight(i,j,bi,bj)*tmpx*tmpx
#endif
                ELSE !IF ( dodimensionalcost ) THEN
                  tmpx = tmpfld2d(i,j,bi,bj)
                  fctile = fctile + tmpx*tmpx
                ENDIF !IF ( dodimensionalcost ) THEN
#ifdef ALLOW_ECCO
                  if ( xx_gen_weight(i,j,bi,bj)
     &                *cosphi(i,j,bi,bj) .ne. 0. _d 0 )
#else
                  if ( xx_gen_weight(i,j,bi,bj) .ne. 0. _d 0 )
#endif
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

      IF ( dodimensionalcost ) THEN
#ifdef ALLOW_SMOOTH_BC_COST_CONTRIBUTION

c--   >>> Loop 2 over records.
      do irec = 1,nrec

#ifdef ALLOW_AUTODIFF
        call active_read_xy(
     &        fnamefld, tmpfld2d, irec, doglobalread,
     &        ladinit, optimcycle, myThid, xx_gen_dummy )
#else
        CALL READ_REC_XY_RL( fnamefld, tmpfld2d, iRec, 1, myThid )
#endif

        _EXCH_XY_RL(tmpfld2d, myThid)

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            kk = 1
            fctile = 0. _d 0
            do j = jmin,jmax
              do i = imin,imax
                if (xx_gen_mask2d(i,j,bi,bj) .ne. 0. _d 0) then
                  tmpx =
     &                 ( tmpfld2d(i+2,j,bi,bj)-tmpfld2d(i+1,j,bi,bj) )
     &                   *maskW(i+1,j,kk,bi,bj)*maskW(i+2,j,kk,bi,bj)
     &               + ( tmpfld2d(i+1,j,bi,bj)-tmpfld2d(i,j,bi,bj) )
     &                   *maskW(i+1,j,kk,bi,bj)
     &               + ( tmpfld2d(i,j+2,bi,bj)-tmpfld2d(i,j+1,bi,bj) )
     &                   *maskS(i,j+1,kk,bi,bj)*maskS(i,j+2,kk,bi,bj)
     &               + ( tmpfld2d(i,j+1,bi,bj)-tmpfld2d(i,j,bi,bj) )
     &                   *maskS(i,j+1,kk,bi,bj)
#ifdef ALLOW_ECCO
                  if ( ABS(R_low(i,j,bi,bj)) .LT. 100. _d 0 )
     &              tmpx = tmpx*ABS(R_low(i,j,bi,bj))/100. _d 0
                  fctile = fctile
     &               + xx_gen_weight(i,j,bi,bj)*cosphi(i,j,bi,bj)
#else
                  fctile = fctile
     &               + xx_gen_weight(i,j,bi,bj)
#endif
     *                 *0.0161 _d 0*lengthscale/4.0 _d 0
     &                 *tmpx*tmpx
#ifdef ALLOW_ECCO
                  if ( xx_gen_weight(i,j,bi,bj)*cosphi(i,j,bi,bj)
     &                 .ne. 0.  _d 0 )
#else
                  if ( xx_gen_weight(i,j,bi,bj) .ne. 0. _d 0 )
#endif
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

#endif /* ALLOW_SMOOTH_BC_COST_CONTRIBUTION */
      ENDIF !IF ( dodimensionalcost ) THEN

#endif /* ECCO_CTRL_DEPRECATED */

#endif /* ALLOW_CTRL */

      return
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ctrl_cost_gen3d
C     !INTERFACE:
      subroutine ctrl_cost_gen3d(
     I                       xx_gen_file,
     I                       xx_gen_dummy,
     I                       xx_gen_weight,
     I                       dodimensionalcost,
     O                       num_gen,
     O                       objf_gen,
     I                       xx_gen_mask,
     I                       myThid
     &                         )

C     !DESCRIPTION: \bv
C     Generic routine for all 3D control penalty terms
C     \ev

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_ECCO
#  include "ecco.h"
#endif
#ifdef ALLOW_CTRL
# include "ctrl.h"
# include "optim.h"
#endif

c     == routine arguments ==

      character*(MAX_LEN_FNAM) xx_gen_file
      _RL xx_gen_dummy
      _RL xx_gen_weight(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      logical dodimensionalcost
      _RL num_gen(nsx,nsy)
      _RL objf_gen(nsx,nsy)
      _RS xx_gen_mask(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      INTEGER myThid

#ifdef ALLOW_CTRL

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer ilfld

      _RL tmpx

      logical doglobalread
      logical ladinit

      character*(80) fnamefld

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

CEOP

      jtlo = mybylo(myThid)
      jthi = mybyhi(myThid)
      itlo = mybxlo(myThid)
      ithi = mybxhi(myThid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c--   Read state record from global file.
      doglobalread = .false.
      ladinit      = .false.

      if (optimcycle .ge. 0) then
        ilfld = ilnblnk( xx_gen_file )
        write(fnamefld(1:80),'(2a,i10.10)')
     &       xx_gen_file(1:ilfld),'.',optimcycle
      endif

c--   >>> Loop 1 to compute mean forcing:
      do bj = jtlo,jthi
        do bi = itlo,ithi
          num_gen(bi,bj)  = 0. _d 0
          objf_gen(bi,bj) = 0. _d 0
        enddo
      enddo

      irec = 1

#ifdef ALLOW_AUTODIFF
      call active_read_xyz( fnamefld, tmpfld3d, irec, doglobalread,
     &                       ladinit, optimcycle, myThid
     &        , xx_gen_dummy )
#else
        CALL READ_REC_XYZ_RL( fnamefld, tmpfld3d, iRec, 1, myThid )
#endif

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

            num_gen(bi,bj)  = 0. _d 0
            objf_gen(bi,bj) = 0. _d 0

            do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                if (xx_gen_mask(i,j,k,bi,bj) .ne. 0. _d 0) then
                  tmpx = tmpfld3d(i,j,k,bi,bj)
                IF ( dodimensionalcost ) THEN
                  objf_gen(bi,bj) = objf_gen(bi,bj)
     &                 + xx_gen_weight(i,j,k,bi,bj)
     &                 *tmpx*tmpx
                ELSE
                  objf_gen(bi,bj) = objf_gen(bi,bj) + tmpx*tmpx
                ENDIF
                if ( xx_gen_weight(i,j,k,bi,bj) .ne. 0. _d 0 )
     &            num_gen(bi,bj) = num_gen(bi,bj) + 1. _d 0
                endif
              enddo
            enddo
            enddo

          enddo
        enddo

#endif

      return
      end

