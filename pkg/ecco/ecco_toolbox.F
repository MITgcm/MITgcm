C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_toolbox.F,v 1.6 2014/12/09 15:51:55 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

C--  File ecco_toolbox.F: Routines to handle basic operations common in ecco.
C--   Contents
C--   o ecco_zero
C--   o ecco_cp
C--   o ecco_cprsrl
C--   o ecco_diffmsk
C--   o ecco_addcost
C--   o ecco_add
C--   o ecco_div
C--   o ecco_readbar

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
      _RL fld   (1-olx:snx+olx,1-oly:sny+oly,nnzloc,nsx,nsy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx


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

      _RL localbar   (1-olx:snx+olx,1-oly:sny+oly,nnzbar,nsx,nsy)
      _RL localobs   (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL localmask  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localdif   (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL difmask    (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)

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

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx


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

      _RL fldIn   (1-olx:snx+olx,1-oly:sny+oly,nzIn,nsx,nsy)
      _RL fldOut   (1-olx:snx+olx,1-oly:sny+oly,nzOut,nsx,nsy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx


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

      _RS fldIn   (1-olx:snx+olx,1-oly:sny+oly,nzIn,nsx,nsy)
      _RL fldOut   (1-olx:snx+olx,1-oly:sny+oly,nzOut,nsx,nsy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx


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
     I                   localdif, localweight, difmask, nnzobs,
     I                   objf_local, num_local,
     I                   myThid
     &                   )

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

      _RL localdif   (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL localweight(1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL difmask    (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)

      _RL objf_local(nsx,nsy)
      _RL num_local(nsx,nsy)

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

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

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
                localcost = localcost + junk*junk*localwww
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
C     add a field to another array
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

      _RL fldIn   (1-olx:snx+olx,1-oly:sny+oly,nzIn,nsx,nsy)
      _RL fldOut   (1-olx:snx+olx,1-oly:sny+oly,nzOut,nsx,nsy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx


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
      _RL fld   (1-olx:snx+olx,1-oly:sny+oly,nnzloc,nsx,nsy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

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

        if ( nnzbar .EQ. 1 ) then
           call active_read_xy( active_var_file, active_var,
     &                      irec, doglobalread,
     &                      ladinit, eccoiter, mythid,
     &                      dummy )
        else
           call active_read_xyz( active_var_file, active_var,
     &                       irec, doglobalread,
     &                       ladinit, eccoiter, mythid,
     &                       dummy )
        endif

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

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

      call mdsreadfield( localerr_file, cost_iprec,
     &  cost_yftype, nnzbar, localweight, iRec, mythid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j = 1-Oly,sNy+Oly
         DO i = 1-Olx,sNx+Olx
          DO k = 1,nnzbar
c--        Test for missing values.
           if (localweight(i,j,k,bi,bj) .lt. -9900.) then
             localweight(i,j,k,bi,bj) = 0. _d 0
           endif
c--        Convert to weight
           if (localweight(i,j,k,bi,bj) .ne. 0.) then
              localweight(i,j,k,bi,bj) =
     &             1./localweight(i,j,k,bi,bj)/
     &                localweight(i,j,k,bi,bj)
           endif
          enddo
         enddo
        enddo
       enddo
      enddo

#endif /* ALLOW_ECCO */

      RETURN
      END





