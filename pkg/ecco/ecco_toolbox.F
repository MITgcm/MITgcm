#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C--  File ecco_toolbox.F: Routines to handle basic operations common in ecco.
C--   Contents
C--   o ecco_zero
C--   o ecco_cp
C--   o ecco_cprsrl
C--   o ecco_diffmsk
C--   o ecco_diffanommsk
C--   o ecco_obsmsk
C--   o ecco_addcost
C--   o ecco_add
C--   o ecco_subtract
C--   o ecco_addmask
C--   o ecco_div
C--   o ecco_divfield
C--   o ecco_mult
C--   o ecco_multfield
C--   o ecco_readbar
C--   o ecco_readwei

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_zero
C     !INTERFACE:
      subroutine ecco_zero( fld, nnzloc, zeroloc, myThid )
C     !DESCRIPTION: \bv
C     fill a field with zeroloc
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      INTEGER myThid
      INTEGER nnzloc
      _RL zeroloc
      _RL fld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzloc,nSx,nSy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1-OLy
      jmax = sNy+OLy
      imin = 1-OLx
      imax = sNx+OLx

c--     Determine the model-data difference mask
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnzloc
             do j = jmin,jmax
              do i = imin,imax
                fld(i,j,k,bi,bj) = zeroloc
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_diffmsk
C     !INTERFACE:
      subroutine ecco_diffmsk(
     I                   localbar, nnzbar, localobs, nnzobs, localmask,
     I                   spminloc, spmaxloc, spzeroloc,
     O                   localdif, difmask,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     compute masked difference between model and observations
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      INTEGER myThid
      INTEGER nnzobs, nnzbar

      _RL localbar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL localobs   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL localmask  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL localdif   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL difmask    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)

      _RL spminloc, spmaxloc, spzeroloc

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c--     Determine the model-data difference mask
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnzobs
             do j = jmin,jmax
              do i = imin,imax
#ifdef ECCO_CTRL_DEPRECATED
               difmask(i,j,k,bi,bj) = cosphi(i,j,bi,bj)*
     &                           localmask(i,j,k,bi,bj)
#else
               difmask(i,j,k,bi,bj) = localmask(i,j,k,bi,bj)
#endif
                if ( localobs(i,j,k,bi,bj) .lt. spminloc .or.
     &               localobs(i,j,k,bi,bj) .gt. spmaxloc .or.
     &               localobs(i,j,k,bi,bj) .eq. spzeroloc ) then
                   difmask(i,j,k,bi,bj) = 0. _d 0
                endif
               localdif(i,j,k,bi,bj) = difmask(i,j,k,bi,bj)*
     &             (localbar(i,j,k,bi,bj)-localobs(i,j,k,bi,bj))
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_diffanommsk
C     !INTERFACE:
      subroutine ecco_diffanommsk(
     I                   localbar, localbarmean, nnzbar,
     I                   localobs, localobsmean, nnzobs,
     I                   localmask,
     I                   spminloc, spmaxloc, spzeroloc,
     O                   localdif, difmask,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     compute masked difference between time-anomaly model and observations
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      INTEGER myThid
      INTEGER nnzobs, nnzbar

      _RL localbar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL localbarmean (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL localobs   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL localobsmean (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL localmask  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL localdif   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL difmask    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)

      _RL spminloc, spmaxloc, spzeroloc

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c--     Determine the model-data difference mask
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnzobs
             do j = jmin,jmax
              do i = imin,imax
#ifdef ECCO_CTRL_DEPRECATED
               difmask(i,j,k,bi,bj) = cosphi(i,j,bi,bj)*
     &                           localmask(i,j,k,bi,bj)
#else
               difmask(i,j,k,bi,bj) = localmask(i,j,k,bi,bj)
#endif
                if ( localobs(i,j,k,bi,bj) .lt. spminloc .or.
     &               localobs(i,j,k,bi,bj) .gt. spmaxloc .or.
     &               localobs(i,j,k,bi,bj) .eq. spzeroloc ) then
                   difmask(i,j,k,bi,bj) = 0. _d 0
                endif
               localdif(i,j,k,bi,bj) = difmask(i,j,k,bi,bj)*
     &           ( (localbar(i,j,k,bi,bj)-localbarmean(i,j,k,bi,bj))
     &            -(localobs(i,j,k,bi,bj)-localobsmean(i,j,k,bi,bj)) )
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_obsmsk
C     !INTERFACE:
      subroutine ecco_obsmsk(
     I                   localbar, nnzbar, localobs, nnzobs, localmask,
     I                   spminloc, spmaxloc, spzeroloc,
     O                   localout, obsmask,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     mask (model) fieds if observation is out-of-bound or missing.
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      INTEGER myThid
      INTEGER nnzobs, nnzbar

      _RL localbar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL localobs   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL localmask  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
      _RL localout   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL obsmask    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)

      _RL spminloc, spmaxloc, spzeroloc

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c--     Determine the model-data difference mask
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnzobs
             do j = jmin,jmax
              do i = imin,imax
#ifdef ECCO_CTRL_DEPRECATED
               obsmask(i,j,k,bi,bj) = cosphi(i,j,bi,bj)*
     &                           localmask(i,j,k,bi,bj)
#else
               obsmask(i,j,k,bi,bj) = localmask(i,j,k,bi,bj)
#endif
                if ( localobs(i,j,k,bi,bj) .lt. spminloc .or.
     &               localobs(i,j,k,bi,bj) .gt. spmaxloc .or.
     &               localobs(i,j,k,bi,bj) .eq. spzeroloc ) then
                   obsmask(i,j,k,bi,bj) = 0. _d 0
                endif
               localout(i,j,k,bi,bj) = obsmask(i,j,k,bi,bj)*
     &             localbar(i,j,k,bi,bj)
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_cp
C     !INTERFACE:
      subroutine ecco_cp(
     I                   fldIn, nzIn, fldOut, nzOut,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     copy a field to another array
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      INTEGER myThid
      INTEGER nzOut, nzIn

      _RL fldIn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzOut,nSx,nSy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c--     Determine the model-data difference mask
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nzOut
             do j = jmin,jmax
              do i = imin,imax
               fldOut(i,j,k,bi,bj) = fldIn(i,j,k,bi,bj)
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_cprsrl
C     !INTERFACE:
      subroutine ecco_cprsrl(
     I                   fldIn, nzIn, fldOut, nzOut,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     copy a field to another array, switching from _RS to _RL
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      INTEGER myThid
      INTEGER nzOut, nzIn

      _RS fldIn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzOut,nSx,nSy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c--     Determine the model-data difference mask
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nzOut
             do j = jmin,jmax
              do i = imin,imax
               fldOut(i,j,k,bi,bj) = fldIn(i,j,k,bi,bj)
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_addcost
C     !INTERFACE:
      subroutine ecco_addcost(
     I               localdif, localweight, difmask, nnzobs, dosumsq,
     U               objf_local, num_local,
     I               myThid
     &               )

C     !DESCRIPTION: \bv
C     adds to a cost function term
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      INTEGER myThid
      INTEGER nnzobs

      _RL localdif   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL localweight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL difmask    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)

      _RL objf_local(nSx,nSy)
      _RL num_local(nSx,nSy)

      logical dosumsq

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      _RL localwww
      _RL localcost
      _RL junk

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

      localwww = 0. _d 0

c--     Compute normalized model-obs cost function
        do bj = jtlo,jthi
          do bi = itlo,ithi
            localcost    = 0. _d 0
            do k = 1,nnzobs
             do j = jmin,jmax
              do i = imin,imax
                localwww  = localweight(i,j,k,bi,bj)
     &                    * difmask(i,j,k,bi,bj)
                junk      = localdif(i,j,k,bi,bj)
                if(dosumsq) then
                  localcost = localcost + junk*junk*localwww
                else
                  localcost = localcost + junk*localwww
                endif
                if ( localwww .ne. 0. )
     &               num_local(bi,bj) = num_local(bi,bj) + 1. _d 0
              enddo
             enddo
            enddo
            objf_local(bi,bj) = objf_local(bi,bj) + localcost
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_add
C     !INTERFACE:
      subroutine ecco_add(
     I                   fldIn, nzIn, fldOut, nzOut,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     add a field (fldIn) to another field (fldOut)
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      INTEGER myThid
      INTEGER nzOut, nzIn

      _RL fldIn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzOut,nSx,nSy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c--     Determine the model-data difference mask
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nzOut
             do j = jmin,jmax
              do i = imin,imax
               fldOut(i,j,k,bi,bj) = fldOut(i,j,k,bi,bj)
     &                             + fldIn(i,j,k,bi,bj)
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_subtract
C     !INTERFACE:
      subroutine ecco_subtract(
     I                   fldIn, nzIn, fldOut, nzOut,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     subtract a field (fldIn) from another field (fldOut)
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      INTEGER myThid
      INTEGER nzOut, nzIn

      _RL fldIn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzOut,nSx,nSy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nzOut
             do j = jmin,jmax
              do i = imin,imax
               fldOut(i,j,k,bi,bj) = fldOut(i,j,k,bi,bj)
     &                             - fldIn(i,j,k,bi,bj)
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_addmask
C     !INTERFACE:
      subroutine ecco_addmask(
     I                   fldIn, fldInmask, nzIn, fldOut, fldOutnum,
     I                   nzOut, myThid
     &                   )

C     !DESCRIPTION: \bv
C     add a field to another array only grids where the mask is non-zero.
C     Also increase the counter by one one those girds.
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      INTEGER myThid
      INTEGER nzOut, nzIn

      _RL fldIn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldInmask   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzOut,nSx,nSy)
      _RL fldOutnum   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzOut,nSx,nSy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c--     Determine the model-data difference mask
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nzOut
             do j = jmin,jmax
              do i = imin,imax
               if(fldInmask(i,j,k,bi,bj) .NE. 0. _d 0) then
                fldOut(i,j,k,bi,bj) = fldOut(i,j,k,bi,bj)
     &                              + fldIn(i,j,k,bi,bj)
                fldOutnum(i,j,k,bi,bj) = fldOutnum(i,j,k,bi,bj)
     &           + 1. _d 0
               endif
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_div
C     !INTERFACE:
      subroutine ecco_div( fld, nnzloc, numerloc, myThid )
C     !DESCRIPTION: \bv
C     divide a field with RL constant
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      INTEGER myThid
      INTEGER nnzloc
      _RL numerloc
      _RL fld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzloc,nSx,nSy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnzloc
             do j = jmin,jmax
              do i = imin,imax
                fld(i,j,k,bi,bj) = fld(i,j,k,bi,bj)/numerloc
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_divfield
C     !INTERFACE:
      subroutine ecco_divfield( fld, nnzloc, flddenom, myThid )
C     !DESCRIPTION: \bv
C     divide a field by another field
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      INTEGER myThid
      INTEGER nnzloc
      _RL fld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzloc,nSx,nSy)
      _RL flddenom (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzloc,nSx,nSy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnzloc
             do j = jmin,jmax
              do i = imin,imax
                if(flddenom(i,j,k,bi,bj) .NE. 0. _d 0) then
                 fld(i,j,k,bi,bj) = fld(i,j,k,bi,bj)/
     &                   flddenom(i,j,k,bi,bj)
                else
                 fld(i,j,k,bi,bj) = 0. _d 0
                endif
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_mult
C     !INTERFACE:
      subroutine ecco_mult( fld, nnzloc, multloc, myThid )
C     !DESCRIPTION: \bv
C     multiply a field with RL constant
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      INTEGER myThid
      INTEGER nnzloc
      _RL multloc
      _RL fld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzloc,nSx,nSy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnzloc
             do j = jmin,jmax
              do i = imin,imax
                fld(i,j,k,bi,bj) = fld(i,j,k,bi,bj)*multloc
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_multfield
C     !INTERFACE:
      subroutine ecco_multfield( fld, nnzloc, fld2, myThid )
C     !DESCRIPTION: \bv
C     multiply a field by another field, fld2 is updated on output
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      INTEGER myThid
      INTEGER nnzloc
      _RL fld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzloc,nSx,nSy)
      _RL fld2  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzloc,nSx,nSy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnzloc
             do j = jmin,jmax
              do i = imin,imax
                fld2(i,j,k,bi,bj) = fld(i,j,k,bi,bj)*
     &                   fld2(i,j,k,bi,bj)
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_readbar
C     !INTERFACE:
      subroutine ecco_readbar(
     I                           active_var_file,
     O                           active_var,
     I                           iRec,
     I                           nnzbar,
     I                           dummy,
     I                           myThid
     &                         )

C     !DESCRIPTION: \bv
C     reads one record from averaged time series ("bar file")
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

C     active_var_file: filename
C     active_var:      array
C     iRec:            record number
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      INTEGER iRec
      INTEGER myThid
      INTEGER nnzbar
      _RL     dummy

#ifdef ALLOW_ECCO

c     == local variables ==

      LOGICAL doglobalread
      LOGICAL lAdInit

CEOP

      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_AUTODIFF
        if ( nnzbar .EQ. 1 ) then
           call active_read_xy( active_var_file, active_var,
     &                      irec, doglobalread,
     &                      ladinit, eccoiter, myThid,
     &                      dummy )
        else
           call active_read_xyz( active_var_file, active_var,
     &                       irec, doglobalread,
     &                       ladinit, eccoiter, myThid,
     &                       dummy )
        endif
#else
        if ( nnzbar .EQ. 1 ) then
           CALL READ_REC_XY_RL( active_var_file, active_var,
     &                          iRec, 1, myThid )
        else
           CALL READ_REC_XYZ_RL( active_var_file, active_var,
     &                          iRec, 1, myThid )
        endif
#endif

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ecco_readwei
C     !INTERFACE:
      subroutine ecco_readwei(
     I                           localerr_file,
     O                           localweight,
     I                           iRec,
     I                           nnzbar,
     I                           dosumsq,
     I                           myThid
     &                         )

C     !DESCRIPTION: \bv
C     reads uncertainty field and compute weight as squared inverse
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

C     localerr_file: filename
C     localweight:      array
C     iRec:            record number
      CHARACTER*(*) localerr_file
      _RL     localweight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      INTEGER iRec
      INTEGER myThid
      INTEGER nnzbar
      LOGICAL dosumsq

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

      CALL READ_REC_3D_RL( localerr_file, cost_iprec, nnzbar,
     &                     localweight, iRec, 1, myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j = jmin,jmax
         DO i = imin,imax
          DO k = 1,nnzbar
c--        Test for missing values.
           if (localweight(i,j,k,bi,bj) .lt. -9900.) then
             localweight(i,j,k,bi,bj) = 0. _d 0
c--        Convert to weight
           elseif ( localweight(i,j,k,bi,bj).ne.0. .AND. dosumsq ) then
              localweight(i,j,k,bi,bj) =
     &          oneRL/localweight(i,j,k,bi,bj)/localweight(i,j,k,bi,bj)
           elseif ( localweight(i,j,k,bi,bj).ne.0. ) then
              localweight(i,j,k,bi,bj) =
     &           oneRL/localweight(i,j,k,bi,bj)
           endif
          enddo
         enddo
        enddo
       enddo
      enddo

#endif /* ALLOW_ECCO */

      RETURN
      END
