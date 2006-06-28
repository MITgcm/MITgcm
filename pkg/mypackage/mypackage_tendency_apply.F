C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_tendency_apply.F,v 1.1 2006/06/28 21:26:21 heimbach Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

      subroutine mypackage_tendency_apply_u(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: mypackage_tendency_apply_u
C     add mypackage tendency terms to U tendency.
C   
C INPUT: 
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C        Assumes that U and V tendencies are already on C-Grid
C=======================================================================
      implicit none

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj) + 
     &   maskW(i,j,kLev,bi,bj) * mypaGUvel(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end

      subroutine mypackage_tendency_apply_v(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: mypackage_tendency_apply_v
C     add mypackage tendency terms to V tendency.
C   
C INPUT: 
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C        Assumes that U and V tendencies are already on C-Grid
C=======================================================================
      implicit none

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj) + 
     &   maskS(i,j,kLev,bi,bj) * mypaGVvel(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end

      subroutine mypackage_tendency_apply_t(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: mypackage_tendency_apply_t
C     add mypackage tendency terms to T (theta) tendency.
C   
C INPUT: 
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C=======================================================================
      implicit none

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gT(i,j,kLev,bi,bj) = gT(i,j,kLev,bi,bj) + 
     &   maskC(i,j,kLev,bi,bj) * mypaGScal1(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end

      subroutine mypackage_tendency_apply_s(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: mypackage_tendency_apply_s
C     add mypackage tendency terms to S tendency.
C   
C INPUT: 
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C=======================================================================
      implicit none

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gS(i,j,kLev,bi,bj) = gS(i,j,kLev,bi,bj) + 
     &   maskC(i,j,kLev,bi,bj) * mypaGScal2(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end
