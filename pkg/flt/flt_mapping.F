C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_mapping.F,v 1.4 2010/12/22 21:21:24 jahn Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

C--   Contents
C--   o FLT_MAP_XY2IJLOCAL
C--   o FLT_MAP_IJLOCAL2XY
C--   o FLT_MAP_R2K  (Function)
C--   o FLT_MAP_K2R  (Function)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE FLT_MAP_XY2IJLOCAL(
     O                               ix, jy,
     I                               xx, yy, bi, bj, myThid )

C     ==================================================================
C     SUBROUTINE FLT_MAP_XY2IJLOCAL
C     ==================================================================
C     o Converts global x,y-coordinates (grid) to corresponding
C       local fractional horizontal indices for specific tile
C     Range: [1/2 , sNx+1/2] , [1/2 , sNy+1/2]
C           Center  (Tracer Pt) <-> integer , integer
C           U-velocity Pt <-> half integer , integer
C           V-velocity Pt <-> integer , half integer
C           Vorticity  Pt <-> half integer , half integer
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     == routine arguments ==
      _RL ix, jy
      _RL xx, yy
      INTEGER bi, bj, myThid

C     == local variables ==
      _RL fm, dist
      INTEGER i, j

C     == end of interface ==

      IF ( usingCartesianGrid .OR.
     &     usingSphericalPolarGrid .AND. .NOT.rotateGrid
     &   ) THEN

        ix = -1. _d 0
        jy = -1. _d 0

        j = 1
        DO i=0,sNx+1
          IF ( ix.EQ.-1. _d 0 ) THEN
           IF ( xG(i,j,bi,bj).LE.xx .AND. xx.LT.xG(i+1,j,bi,bj) ) THEN
             dist = xG(i+1,j,bi,bj) - xG(i,j,bi,bj)
             fm = ( xx - xG(i,j,bi,bj) ) / dist
             ix = DFLOAT(i)+fm-0.5 _d 0
           ENDIF
          ENDIF
        ENDDO

        i = 1
        DO j=0,sNy+1
          IF ( jy.EQ.-1. _d 0 ) THEN
           IF ( yG(i,j,bi,bj).LE.yy .AND. yy.LT.yG(i,j+1,bi,bj) ) THEN
             dist = yG(i,j+1,bi,bj) - yG(i,j,bi,bj)
             fm = ( yy - yG(i,j,bi,bj) ) / dist
             jy = DFLOAT(j)+fm-0.5 _d 0
           ENDIF
          ENDIF
        ENDDO

      ELSE
        STOP 'FLT_MAP_XY2IJLOCAL: not yet coded for this grid'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE FLT_MAP_IJLOCAL2XY(
     O                               xx, yy,
     I                               ix, jy, bi, bj, myThid )

C     ==================================================================
C     SUBROUTINE FLT_MAP_IJLOCAL2XY
C     ==================================================================
C     o Converts local fractional horizontal indices for specific tile
C       to corresponding global x,y-coordinates (grid)
C     Range: [1/2 , sNx+1/2] , [1/2 , sNy+1/2]
C           Center  (Tracer Pt) <-> integer , integer
C           U-velocity Pt <-> half integer , integer
C           V-velocity Pt <-> integer , half integer
C           Vorticity  Pt <-> half integer , half integer
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     == routine arguments ==
      _RL xx, yy
      _RL ix, jy
      INTEGER bi, bj, myThid

C     == local variables ==
      _RL ddx, ddy
      INTEGER i, j

C     == end of interface ==

      IF ( usingCartesianGrid .OR.
     &     usingSphericalPolarGrid .AND. .NOT.rotateGrid
     &   ) THEN

        i = NINT(ix)
        j = NINT(jy)
        ddx = 0.5 _d 0 + ix - DFLOAT(i)
        ddy = 0.5 _d 0 + jy - DFLOAT(j)

        xx = xG(i,j,bi,bj) + ddx*( xG(i+1,j,bi,bj) - xG(i,j,bi,bj) )
        yy = yG(i,j,bi,bj) + ddy*( yG(i,j+1,bi,bj) - yG(i,j,bi,bj) )

      ELSEIF ( usingCurvilinearGrid ) THEN

        i = NINT(ix)
        j = NINT(jy)
        ddx = 0.5 _d 0 + ix - DFLOAT(i)
        ddy = 0.5 _d 0 + jy - DFLOAT(j)

C       bilinear interpolation within grid cell (should use arcs instead?)
        xx = xG(i,j,bi,bj) + ddx*( xG(i+1,j,bi,bj) - xG(i,j,bi,bj) )
     &                     + ddy*( xG(i,j+1,bi,bj) - xG(i,j,bi,bj) )
     &                 + ddx*ddy*( xG(i+1,j+1,bi,bj) - xG(i+1,j,bi,bj)
     &                           - xG(i,j+1,bi,bj) + xG(i,j,bi,bj) )
        yy = yG(i,j,bi,bj) + ddx*( yG(i+1,j,bi,bj) - yG(i,j,bi,bj) )
     &                     + ddy*( yG(i,j+1,bi,bj) - yG(i,j,bi,bj) )
     &                 + ddx*ddy*( yG(i+1,j+1,bi,bj) - yG(i+1,j,bi,bj)
     &                           - yG(i,j+1,bi,bj) + yG(i,j,bi,bj) )

      ELSE
        STOP 'FLT_MAP_IJLOCAL2XY: not yet coded for this grid'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _RL FUNCTION FLT_MAP_R2K(
     I                          rr, bi, bj, myThid )

C     ==================================================================
C     FUNCTION FLT_MAP_R2K
C     ==================================================================
C     o Converts r-coordinate (grid) to corresponding
C       fractional vertical index
C     Range: [1/2 , Nr+1/2],
C           Center  (Tracer Pt) <-> integer
C           Interface (wVel Pt) <-> half integer
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"

C     == routine arguments ==
      _RL rr
      INTEGER bi, bj, myThid

C     == local variables ==
      _RL fm
      INTEGER k

C     == end of interface ==

      FLT_MAP_R2K = 0. _d 0
      DO k=1,Nr
        IF ( FLT_MAP_R2K .EQ. 0. _d 0 ) THEN
C-    r decreases when k increases (rkSign < 0):
          IF ( rF(k) .GE. rr .AND. rr.GT.rF(k+1) ) THEN
            fm = ( rr - rF(k) ) * recip_drF(k)*rkSign
            FLT_MAP_R2K = DFLOAT(k)+fm-0.5 _d 0
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _RL FUNCTION FLT_MAP_K2R(
     I                          kr, bi, bj, myThid )

C     ==================================================================
C     FUNCTION FLT_MAP_K2R
C     ==================================================================
C     o Converts fractional vertical index to corresponding
C           r-coordinate (grid)
C     Range: [1/2 , Nr+1/2],
C           Center  (Tracer Pt) <-> integer
C           Interface (wVel Pt) <-> half integer
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"

C     == routine arguments ==
      _RL kr
      INTEGER bi, bj, myThid

C     == local variables ==
      _RL ddz
      INTEGER k

C     == end of interface ==

      k = NINT(kr)
      IF ( k.LT.1 ) THEN
        FLT_MAP_K2R = rF(1)
      ELSEIF ( k.GT.Nr ) THEN
        FLT_MAP_K2R = rF(Nr+1)
      ELSE
        ddz = 0.5 _d 0 + kr - DFLOAT(k)
        FLT_MAP_K2R = rF(k) + ddz*drF(k)*rkSign
      ENDIF

      RETURN
      END
