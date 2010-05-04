C $Header: /u/gcmpack/MITgcm/pkg/autodiff/Attic/exch_3d_ad.F,v 1.6 2010/05/04 16:00:18 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C--  File exch_3d_ad.F: adjoint routines to 3-D exchange routines
C--   Contents
C--   o DUMMY_EXCH_3D_AD
C--   o ADEXCH_3D_RS
C--   o ADEXCH_3D_RL
C--   o ADEXCH_UV_3D_RS
C--   o ADEXCH_UV_3D_RL
C--   o ADEXCH_Z_3D_RS
C--   o ADEXCH_Z_3D_RL

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

      SUBROUTINE DUMMY_EXCH_3D_AD

      RETURN
      END

#else

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADEXCH_3D_RS

C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_3D_RS(
     I                         myNz, myThid,
     U                         adphi )
#else
      SUBROUTINE ADEXCH_3D_RS(
     U                         adphi,
     I                         myNz, myThid )
#endif

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ADEXCH_3D_RS
C     | o Handle exchanges for _RS, three-dim scalar arrays.
C     *==========================================================*
C     | Invoke appropriate exchange routine depending on type
C     | of grid (cube or globally indexed) to be operated on.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     adphi    :: Array with overlap regions are to be exchanged
C     myNz   :: 3rd dimension of array to exchange
C     myThid :: My thread id.
      INTEGER myNz
      _RS adphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:myNz,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     OL[wens]      :: Overlap extents in west, east, north, south.
C     exchWidth[XY] :: Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
CEOP

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
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
CBOP
C     !ROUTINE: ADEXCH_3D_RL

C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_3D_RL(
     I                         myNz, myThid,
     U                         adphi )
#else
      SUBROUTINE ADEXCH_3D_RL(
     U                         adphi,
     I                         myNz, myThid )
#endif

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ADEXCH_3D_RL
C     | o Handle exchanges for _RL, three-dim scalar arrays.
C     *==========================================================*
C     | Invoke appropriate exchange routine depending on type
C     | of grid (cube or globally indexed) to be operated on.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     adphi    :: Array with overlap regions are to be exchanged
C     myNz   :: 3rd dimension of array to exchange
C     myThid :: My thread id.
      INTEGER myNz
      _RL adphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:myNz,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     OL[wens]      :: Overlap extents in west, east, north, south.
C     exchWidth[XY] :: Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
CEOP

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
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
CBOP
C     !ROUTINE: ADEXCH_UV_3D_RS

C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_UV_3D_RS(
     I                            withSigns, myNz, myThid,
     U                            adUphi, adVphi )
#else
      SUBROUTINE ADEXCH_UV_3D_RS(
     U                            adUphi, adVphi,
     I                            withSigns, myNz, myThid )
#endif

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ADEXCH_UV_3D_RS
C     | o Handle exchanges for _RS, 3-dimensional vector arrays.
C     *==========================================================*
C     | Vector arrays need to be rotated and interchaged for
C     | exchange operations on some grids. This driver routine
C     | branches to support this.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     phi    :: Array with overlap regions are to be exchanged
C               Note - The interface to EXCH_RS assumes that
C               the standard Fortran 77 sequence association rules
C               apply.
C     myNz   :: 3rd dimension of array to exchange
C     myThid :: My thread id.
      INTEGER myNz
      _RS adUphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz,nSx,nSy)
      _RS adVphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz,nSx,nSy)
      LOGICAL withSigns
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     OL[wens]      :: Overlap extents in west, east, north, south.
C     exchWidth[XY] :: Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
CEOP

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      IF (useCubedSphereExchange) THEN
       CALL EXCH_UV_RS_CUBE( adUphi, adVphi, withSigns,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ELSE
       CALL EXCH_RS( adUphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
       CALL EXCH_RS( adVphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADEXCH_UV_3D_RL

C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_UV_3D_RL(
     I                          withSigns, myNz, myThid,
     U                          adUphi, adVphi )
#else
      SUBROUTINE ADEXCH_UV_3D_RL(
     U                          adUphi, adVphi,
     I                          withSigns, myNz, myThid )
#endif

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ADEXCH_UV_3D_RL
C     | o Handle exchanges for _RL, 3-dimensional vector arrays.
C     *==========================================================*
C     | Vector arrays need to be rotated and interchaged for
C     | exchange operations on some grids. This driver routine
C     | branches to support this.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     phi    :: Array with overlap regions are to be exchanged
C               Note - The interface to EXCH_RL assumes that
C               the standard Fortran 77 sequence association rules
C               apply.
C     myNz   :: 3rd dimension of array to exchange
C     myThid :: My thread id.
      INTEGER myNz
      _RL adUphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz,nSx,nSy)
      _RL adVphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz,nSx,nSy)
      LOGICAL withSigns
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     OL[wens]      :: Overlap extents in west, east, north, south.
C     exchWidth[XY] :: Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
CEOP

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      IF (useCubedSphereExchange) THEN
       CALL EXCH_UV_RL_CUBE( adUphi, adVphi, withSigns,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ELSE
       CALL EXCH_RL( adUphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
       CALL EXCH_RL( adVphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ADEXCH_Z_3D_RS

C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_Z_3D_RS(
     I                           myNz, myThid,
     U                           adphi )
#else
      SUBROUTINE ADEXCH_Z_3D_RS(
     U                           adphi,
     I                           myNz, myThid )
#endif

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ADEXCH_Z_3D_RS   adjoint of  EXCH_Z_3D_RS
C     | o Handle exchanges for _RS, two-dimensional arrays.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     phi    :: Array with overlap regions are to be exchanged
C     myNz   :: 3rd dimension of array to exchange
C     myThid :: My thread id.
      _RS     adphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz,nSx,nSy)
      INTEGER myNz
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     OL[wens]       :: Overlap extents in west, east, north, south.
C     exchWidth[XYZ] :: Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
CEOP

#ifdef ALLOW_EXCH2
      STOP 'ABNORMAL END: Should never enter ADEXCH_Z_3D_RS'
#endif
      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      IF (useCubedSphereExchange) THEN
       CALL EXCH_Z_RS_CUBE( adphi,
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

CBOP
C     !ROUTINE: ADEXCH_UV_3D_RL

C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_Z_3D_RL(
     I                         myNz, myThid,
     U                         adphi )
#else
      SUBROUTINE ADEXCH_Z_3D_RL(
     U                         adphi,
     I                         myNz, myThid )
#endif

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ADEXCH_Z_3D_RL   adjoint of  EXCH_Z_3D_RL
C     | o Handle exchanges for _RL, two-dimensional arrays.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     phi    :: Array with overlap regions are to be exchanged
C     myNz   :: 3rd dimension of array to exchange
C     myThid :: My thread id.
      _RL     adphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz,nSx,nSy)
      INTEGER myNz
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     OL[wens]       :: Overlap extents in west, east, north, south.
C     exchWidth[XYZ] :: Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY
CEOP

#ifdef ALLOW_EXCH2
      STOP 'ABNORMAL END: Should never enter ADEXCH_Z_3D_RS'
#endif
      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      IF (useCubedSphereExchange) THEN
       CALL EXCH_Z_RL_CUBE( adphi,
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

#endif /* ALLOW_EXCH2 */
