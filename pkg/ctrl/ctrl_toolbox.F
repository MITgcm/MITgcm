#include "CTRL_OPTIONS.h"

C--  File ctrl_toolbox.F: Routines to handle basic operations common in ecco.
C--  Note: basically same as ecco_toolbox routines, but copied here for package
C--  independence (pkg/ecco doesn't work with openad)
C--   Contents
C--   o ctrl_zero
C--   o ctrl_cprlrl
C--   o ctrl_cprsrl

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ctrl_zero
C     !INTERFACE:
      subroutine ctrl_zero( fld, nnzloc, zeroloc, myThid )
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

#ifdef ALLOW_CTRL

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

#endif /* ALLOW_CTRL */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ctrl_cprlrl
C     !INTERFACE:
      subroutine ctrl_cprlrl(
     I                   fldIn, nzIn, fldOut, nzOut,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     copy a field (RL) to another array (RL)
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

c     == routine arguments ==

      INTEGER myThid
      INTEGER nzOut, nzIn

      _RL fldIn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzOut,nSx,nSy)

#ifdef ALLOW_CTRL

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
               fldOut(i,j,k,bi,bj) = fldIn(i,j,k,bi,bj)
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_CTRL */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ctrl_cprsrl
C     !INTERFACE:
      subroutine ctrl_cprsrl(
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

c     == routine arguments ==

      INTEGER myThid
      INTEGER nzOut, nzIn

      _RS fldIn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzOut,nSx,nSy)

#ifdef ALLOW_CTRL

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

#endif /* ALLOW_CTRL */

      RETURN
      END

