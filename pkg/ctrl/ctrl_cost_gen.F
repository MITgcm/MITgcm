c ----------------------------------------------------------------
c --- ctrl_cost_gen2d
c --- ctrl_cost_gen3d
c ----------------------------------------------------------------

#include "CTRL_OPTIONS.h"

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
     I                       xx_gen_mask2D,
     I                       myThid )

C     !DESCRIPTION: \bv
C     Generic routine for all 2D control penalty terms
C     \ev

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_CTRL
# include "CTRL.h"
# include "optim.h"
#endif

c     == routine arguments ==

      integer startrec
      integer endrec
      character*(MAX_LEN_FNAM) xx_gen_file
      _RL xx_gen_dummy
      _RL xx_gen_period
      _RL xx_gen_weight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      logical dodimensionalcost
      _RL num_gen_anom(nSx,nSy)
      _RL objf_gen_anom(nSx,nSy)
      _RS xx_gen_mask2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      integer myThid

#ifdef ALLOW_CTRL

c     == local variables ==

      integer bi,bj
      integer i, j
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer nrec
      integer irec
      integer ilfld,ilDir

      _RL fctile
      _RL tmpx
      _RL lengthscale

      _RL tmpfld2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)

      logical doglobalread
      logical ladinit

      character*(MAX_LEN_FNAM) fnamefld

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

      lengthscale = 1. _d 0

c--   Read state record from global file.
      doglobalread = .false.
      ladinit      = .false.

c--   Number of records to be used.
      nrec = endrec-startrec+1

c--   Find ctrlDir length
      ilDir=ilnblnk( ctrlDir )

      do bj = jtlo,jthi
       do bi = itlo,ithi
        do j = 1-OLy,sNy+OLy
         do i = 1-OLx,sNx+OLx
          tmpfld2d(i,j,bi,bj) = 0. _d 0
         enddo
        enddo
       enddo
      enddo

      if (optimcycle .ge. 0) then
        ilfld=ilnblnk( xx_gen_file )
        write(fnamefld,'(2a,i10.10)')
     &       ctrlDir(1:ilDir)//xx_gen_file(1:ilfld),'.',optimcycle
      endif

c--   >>> Loop 1 to compute mean forcing:
      do bj = jtlo,jthi
        do bi = itlo,ithi
          num_gen_anom(bi,bj)  = 0. _d 0
          objf_gen_anom(bi,bj) = 0. _d 0
        enddo
      enddo

c--   >>> Loop over records.
      do irec = startrec,endrec

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
     I                       myThid )

C     !DESCRIPTION: \bv
C     Generic routine for all 3D control penalty terms
C     \ev

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_CTRL
# include "CTRL.h"
# include "optim.h"
#endif

c     == routine arguments ==

      character*(MAX_LEN_FNAM) xx_gen_file
      _RL xx_gen_dummy
      _RL xx_gen_weight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      logical dodimensionalcost
      _RL num_gen(nSx,nSy)
      _RL objf_gen(nSx,nSy)
      _RS xx_gen_mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
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
      integer ilfld,ilDir

      _RL tmpx

      logical doglobalread
      logical ladinit
      _RL     tmpfld3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      character*(MAX_LEN_FNAM) fnamefld

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c--   Read state record from global file.
      doglobalread = .false.
      ladinit      = .false.

c--   Find ctrlDir length
      ilDir=ilnblnk( ctrlDir )

      do bj = jtlo,jthi
       do bi = itlo,ithi
        do k = 1,Nr
         do j = 1-OLy,sNy+OLy
          do i = 1-OLx,sNx+OLx
           tmpfld3d(i,j,k,bi,bj) = 0. _d 0
          enddo
         enddo
        enddo
       enddo
      enddo

      if (optimcycle .ge. 0) then
        ilfld = ilnblnk( xx_gen_file )
        write(fnamefld,'(2a,i10.10)')
     &       ctrlDir(1:ilDir)//xx_gen_file(1:ilfld),'.',optimcycle
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
      call active_read_xyz(
     &      fnamefld, tmpfld3d, irec, doglobalread,
     &      ladinit, optimcycle, myThid, xx_gen_dummy )
#else
      CALL READ_REC_XYZ_RL( fnamefld, tmpfld3d, iRec, 1, myThid )
#endif

c--   Loop over this thread tiles.
      do bj = jtlo,jthi
        do bi = itlo,ithi

          num_gen(bi,bj)  = 0. _d 0
          objf_gen(bi,bj) = 0. _d 0

          do k = 1,Nr
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

#endif /* ALLOW_CTRL */

      return
      end
