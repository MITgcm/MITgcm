C $Header: /u/gcmpack/MITgcm/pkg/autodiff/Attic/exch_ad.F,v 1.8 2010/05/04 15:08:45 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C--  File exch_ad.F: adjoint routines to exchange routines
C--   Contents
C--   o DUMMY_EXCH_AD
C--   o ADEXCH_XY_RS
C--   o ADEXCH_XY_RL
C--   o ADEXCH_XYZ_RS
C--   o ADEXCH_XYZ_RL
C--   o ADEXCH_XZ_RS
C--   o ADEXCH_XZ_RL
C--   o ADEXCH_YZ_RS
C--   o ADEXCH_YZ_RL

C     *==========================================================*
C     | These routines essentially are calling the original
C     | routines in reverse mode.
C     | These are regular adjoint routines with no innermost
C     | dimension added.
C     | Calls to these routines will be generated automatically
C     | by TAMC if correct flow directives are given
C     |   tamc -reverse ....
C     |
C     | written and tested by Ralf Giering, Jan. 14th 1999
C     | added new S/R for _xz_ , _yz_ heimbach@mit.edu May-01
C     *==========================================================*

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_EXCH2

C     For exch2 all exch_xy* routines are shown to TAF
C     and hand-written code below is not needed.

      SUBROUTINE DUMMY_EXCH_AD

      RETURN
      END

#else

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_XY_RS(
     I                         myThid,
     U                         adphi )
#else
      SUBROUTINE ADEXCH_XY_RS(
     U                         adphi,
     I                         myThid )
#endif
C     *==========================================================*
C     | SUBROUTINE ADEXCH_XY_RS   adjoint of  EXCH_XY_RS
C     | o Handle exchanges for _RS, two-dimensional arrays.
C     *==========================================================*
C     === Global data ===
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array which overlap regions are to be exchanged
C     myThid - My thread id.
      _RS     adphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
CEndOfInterface
C     == Local variables ==
C     OL[wens]       - Overlap extents in west, east, north, south.
C     exchWidth[XY]  - Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
      INTEGER mysNx, mysNy, myNz

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      mysNx      = sNx
      mysNy      = sNy
      myNz       = 1
      IF (useCubedSphereExchange) THEN
       CALL EXCH_RS_CUBE( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ELSE
       CALL EXCH_RS( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_XY_RL(
     I                         myThid,
     U                         adphi )
#else
      SUBROUTINE ADEXCH_XY_RL(
     U                         adphi,
     I                         myThid )
#endif
C     *==========================================================*
C     | SUBROUTINE ADEXCH_XY_RL   adjoint of  EXCH_XY_RL
C     | o Handle exchanges for _RL, two-dimensional arrays.
C     *==========================================================*
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array which overlap regions are to be exchanged
C     myThid - My thread id.
      _RL     adphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
CEndOfInterface
C     == Local variables ==
C     OL[wens]       - Overlap extents in west, east, north, south.
C     exchWidth[XY]  - Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
      INTEGER mysNx, mysNy, myNz

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      mysNx      = sNx
      mysNy      = sNy
      myNz       = 1
      IF (useCubedSphereExchange) THEN
       CALL EXCH_RL_CUBE( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ELSE
       CALL EXCH_RL( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_XYZ_RS(
     I                          myThid,
     U                          adphi )
#else
      SUBROUTINE ADEXCH_XYZ_RS(
     U                          adphi,
     I                          myThid )
#endif
C     *==========================================================*
C     | SUBROUTINE ADEXCH_XYZ_RS   adjoint of  EXCH_XYZ_RS
C     | o Handle exchanges for _RS, tree-dimensional arrays.
C     *==========================================================*
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array which overlap regions are to be exchanged
C     myThid - My thread id.
      _RS     adphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      INTEGER myThid
CEndOfInterface
C     == Local variables ==
C     OL[wens]       - Overlap extents in west, east, north, south.
C     exchWidth[XYZ]  - Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
      INTEGER mysNx, mysNy, myNz

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      mysNx      = sNx
      mysNy      = sNy
      myNz       = Nr
      IF (useCubedSphereExchange) THEN
       CALL EXCH_RS_CUBE( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ELSE
       CALL EXCH_RS( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_XYZ_RL(
     I                          myThid,
     U                          adphi )
#else
      SUBROUTINE ADEXCH_XYZ_RL(
     U                          adphi,
     I                          myThid )
#endif
C     *==========================================================*
C     | SUBROUTINE ADEXCH_XYZ_RL   adjoint of  EXCH_XYZ_RL
C     | o Handle exchanges for _RL, tree-dimensional arrays.
C     *==========================================================*
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array which overlap regions are to be exchanged
C     myThid - My thread id.
      _RL     adphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      INTEGER myThid
CEndOfInterface
C     == Local variables ==
C     OL[wens]       - Overlap extents in west, east, north, south.
C     exchWidth[XYZ]  - Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
      INTEGER mysNx, mysNy, myNz

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      mysNx      = sNx
      mysNy      = sNy
      myNz       = Nr
      IF (useCubedSphereExchange) THEN
       CALL EXCH_RL_CUBE( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ELSE
       CALL EXCH_RL( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_XZ_RS(
     I                         myThid,
     U                         adphi )
#else
      SUBROUTINE ADEXCH_XZ_RS(
     U                         adphi,
     I                         myThid )
#endif
C     *==========================================================*
C     | SUBROUTINE ADEXCH_XZ_RS   adjoint of  EXCH_XZ_RS
C     | o Handle exchanges for _RS, two-dimensional arrays.
C     *==========================================================*
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array which overlap regions are to be exchanged
C     myThid - My thread id.
      _RS     adphi(1-OLx:sNx+OLx,1,1:Nr,nSx,nSy)
      INTEGER myThid
CEndOfInterface
C     == Local variables ==
C     OL[wens]       - Overlap extents in west, east, north, south.
C     exchWidth[XYZ]  - Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
      INTEGER mysNx, mysNy, myNz

      OLw        = OLx
      OLe        = OLx
      OLn        = 0
      OLs        = 0
      exchWidthX = OLx
      exchWidthY = 0
      mysNx      = sNx
      mysNy      = 1
      myNz       = Nr
      CALL EXCH_RS( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_XZ_RL(
     I                         myThid,
     U                         adphi )
#else
      SUBROUTINE ADEXCH_XZ_RL(
     U                         adphi,
     I                         myThid )
#endif
C     *==========================================================*
C     | SUBROUTINE ADEXCH_XZ_RL   adjoint of  EXCH_XZ_RL
C     | o Handle exchanges for _RL, two-dimensional arrays.
C     *==========================================================*
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array which overlap regions are to be exchanged
C     myThid - My thread id.
      _RL     adphi(1-OLx:sNx+OLx,1,1:Nr,nSx,nSy)
      INTEGER myThid
CEndOfInterface
C     == Local variables ==
C     OL[wens]       - Overlap extents in west, east, north, south.
C     exchWidth[XYZ]  - Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
      INTEGER mysNx, mysNy, myNz

      OLw        = OLx
      OLe        = OLx
      OLn        = 0
      OLs        = 0
      exchWidthX = OLx
      exchWidthY = 0
      mysNx      = sNx
      mysNy      = 1
      myNz       = Nr
      CALL EXCH_RL( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_YZ_RS(
     I                         myThid,
     U                         adphi )
#else
      SUBROUTINE ADEXCH_YZ_RS(
     U                         adphi,
     I                         myThid )
#endif
C     *==========================================================*
C     | SUBROUTINE ADEXCH_YZ_RS   adjoint of  EXCH_YZ_RS
C     | o Handle exchanges for _RS, two-dimensional arrays.
C     *==========================================================*
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array which overlap regions are to be exchanged
C     myThid - My thread id.
      _RS     adphi(1,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      INTEGER myThid
CEndOfInterface
C     == Local variables ==
C     OL[wens]       - Overlap extents in west, east, north, south.
C     exchWidth[XYZ]  - Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
      INTEGER mysNx, mysNy, myNz

      OLw        = 0
      OLe        = 0
      OLn        = OLy
      OLs        = OLy
      exchWidthX = 0
      exchWidthY = OLy
      mysNx      = 1
      mysNy      = sNy
      myNz       = Nr
      CALL EXCH_RS( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_YZ_RL(
     I                         myThid,
     U                         adphi )
#else
      SUBROUTINE ADEXCH_YZ_RL(
     U                         adphi,
     I                         myThid )
#endif
C     *==========================================================*
C     | SUBROUTINE ADEXCH_YZ_RL   adjoint of  EXCH_YZ_RL
C     | o Handle exchanges for _RL, two-dimensional arrays.
C     *==========================================================*
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array which overlap regions are to be exchanged
C     myThid - My thread id.
      _RL     adphi(1,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      INTEGER myThid
CEndOfInterface
C     == Local variables ==
C     OL[wens]       - Overlap extents in west, east, north, south.
C     exchWidth[XYZ]  - Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
      INTEGER mysNx, mysNy, myNz

      OLw        = 0
      OLe        = 0
      OLn        = OLy
      OLs        = OLy
      exchWidthX = 0
      exchWidthY = OLy
      mysNx      = 1
      mysNy      = sNy
      myNz       = Nr
      CALL EXCH_RL( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )

      RETURN
      END

#endif /* ALLOW_EXCH2 */
