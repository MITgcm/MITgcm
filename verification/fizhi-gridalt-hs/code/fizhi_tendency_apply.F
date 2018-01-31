#include "FIZHI_OPTIONS.h"
      subroutine fizhi_tendency_apply_u(
     U                        gU_arr,
     I                        iMin,iMax,jMin,jMax, kLev, bi, bj,
     I                        myTime, myIter, myThid )
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

      _RL     gU_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER kLev, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gU_arr(i,j) = gU_arr(i,j) +
     .      maskW(i,j,kLev,bi,bj) * guphy(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end
      subroutine fizhi_tendency_apply_v(
     U                        gV_arr,
     I                        iMin,iMax,jMin,jMax, kLev, bi, bj,
     I                        myTime, myIter, myThid )
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

      _RL     gV_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER kLev, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gV_arr(i,j) = gV_arr(i,j) +
     .      maskS(i,j,kLev,bi,bj) * gvphy(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end
      subroutine fizhi_tendency_apply_t(
     U                        gT_arr,
     I                        iMin,iMax,jMin,jMax, kLev, bi, bj,
     I                        myTime, myIter, myThid )
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

      _RL     gT_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER kLev, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gT_arr(i,j) = maskC(i,j,kLev,bi,bj)
     .       *( gT_arr(i,j) + gthphy(i,j,kLev,bi,bj) )
       enddo
      enddo

      return
      end
      subroutine fizhi_tendency_apply_s(
     U                        gS_arr,
     I                        iMin,iMax,jMin,jMax, kLev, bi, bj,
     I                        myTime, myIter, myThid )
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

      _RL     gS_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER kLev, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gS_arr(i,j) = maskC(i,j,kLev,bi,bj)
     .       *( gS_arr(i,j) + gsphy(i,j,kLev,bi,bj) )
       enddo
      enddo

      return
      end
