C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_tendency_apply.F,v 1.2 2006/08/07 21:13:52 edhill Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_TENDENCY_APPLY_U

C !INTERFACE:
      subroutine MYPACKAGE_TENDENCY_APPLY_U(
     &     iMin, iMax, jMin, jMax,
     &     bi,bj,kLev,myTime,myThid)

C     !DESCRIPTION:
C     Add mypackage tendency terms to U tendency.  Routine works for one
C     level at a time Assumes that U and V tendencies are already on
C     C-Grid

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime
CEOP

C     !LOCAL VARIABLES:
      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj) + 
     &   maskW(i,j,kLev,bi,bj) * mypaGUvel(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_TENDENCY_APPLY_V

C !INTERFACE:
      SUBROUTINE MYPACKAGE_TENDENCY_APPLY_V(
     &     iMin, iMax, jMin, jMax,
     &     bi,bj,kLev,myTime,myThid)

C     !DESCRIPTION:
C     Add mypackage tendency terms to V tendency.  Routine works for one
C     level at a time Assumes that U and V tendencies are already on
C     C-Grid

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime
CEOP

C     !LOCAL VARIABLES:
      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj) + 
     &   maskS(i,j,kLev,bi,bj) * mypaGVvel(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_TENDENCY_APPLY_T

C !INTERFACE:
      SUBROUTINE MYPACKAGE_TENDENCY_APPLY_T(
     &     iMin, iMax, jMin, jMax,
     &     bi,bj,kLev,myTime,myThid)

C     !DESCRIPTION:
C     Add mypackage tendency terms to T (theta) tendency.  Routine works
C     for one level at a time

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime
CEOP

C     !LOCAL VARIABLES:
      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gT(i,j,kLev,bi,bj) = gT(i,j,kLev,bi,bj) + 
     &   maskC(i,j,kLev,bi,bj) * mypaGScal1(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_TENDENCY_APPLY_S

C !INTERFACE:
      SUBROUTINE MYPACKAGE_TENDENCY_APPLY_S(
     &     iMin, iMax, jMin, jMax,
     &     bi,bj,kLev,myTime,myThid)

C     !DESCRIPTION:
C     Add mypackage tendency terms to S tendency.  Routine works for one
C     level at a time

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime
CEOP

C     !LOCAL VARIABLES:
      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gS(i,j,kLev,bi,bj) = gS(i,j,kLev,bi,bj) + 
     &   maskC(i,j,kLev,bi,bj) * mypaGScal2(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end
