C $Header: /u/gcmpack/MITgcm/verification/fizhi-gridalt-hs/code/fizhi_tendency_apply.F,v 1.1 2004/12/07 22:14:35 edhill Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      subroutine fizhi_tendency_apply_u(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: fizhi_tendency_apply_u
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to U tendency.
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
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj) + 
     .      maskW(i,j,kLev,bi,bj) * guphy(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end
      subroutine fizhi_tendency_apply_v(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: fizhi_tendency_apply_v
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to V tendency.
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
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj) + 
     .      maskS(i,j,kLev,bi,bj) * gvphy(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end
      subroutine fizhi_tendency_apply_t(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: fizhi_tendency_apply_t
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to T (theta) tendency.
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
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gT(i,j,kLev,bi,bj) = maskC(i,j,kLev,bi,bj)
     .       *( gT(i,j,kLev,bi,bj) + gthphy(i,j,kLev,bi,bj) )
       enddo
      enddo

      return
      end
      subroutine fizhi_tendency_apply_s(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: fizhi_tendency_apply_s
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to S tendency.
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
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gS(i,j,kLev,bi,bj) = maskC(i,j,kLev,bi,bj)
     .       *( gS(i,j,kLev,bi,bj) + gsphy(i,j,kLev,bi,bj) )
       enddo
      enddo

      return
      end
