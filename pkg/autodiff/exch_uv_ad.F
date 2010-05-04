C $Header: /u/gcmpack/MITgcm/pkg/autodiff/Attic/exch_uv_ad.F,v 1.6 2010/05/04 15:08:45 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C--  File exch_uv_ad.F: adjoint routines to exchange vector routines
C--   Contents
C--   o DUMMY_EXCH_UV_AD
C--   o ADEXCH_UV_XY_RS
C--   o ADEXCH_UV_XY_RL
C--   o ADEXCH_UV_XYZ_RS
C--   o ADEXCH_UV_XYZ_RL

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

      SUBROUTINE DUMMY_EXCH_UV_AD

      RETURN
      END

#else

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_UV_XY_RS(
     I                        withSigns, myThid,
     U                        aduphi, advphi )
#else
      SUBROUTINE ADEXCH_UV_XY_RS(
     U                        aduphi, advphi,
     I                        withSigns, myThid )
#endif
C     *==========================================================*
C     | SUBROUTINE ADEXCH_UV_XY_RS   adjoint of  EXCH_UV_XY_RS
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
      _RS aduphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS advphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      LOGICAL withSigns
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
      IF (useCubedSphereExchange) THEN
       CALL EXCH_UV_RS_CUBE( aduphi, advphi, withSigns,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ELSE
       CALL EXCH_RS( aduphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
       CALL EXCH_RS( advphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
c_jmc: for JAM compatibility, replace the 2 CALLs above by the 2 CPP_MACROs:
c       _EXCH_XY_RS( aduphi, myThid )
c       _EXCH_XY_RS( advphi, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_UV_XY_RL(
     I                        withSigns, myThid,
     U                        aduphi, advphi )
#else
      SUBROUTINE ADEXCH_UV_XY_RL(
     U                        aduphi, advphi,
     I                        withSigns, myThid )
#endif

C     *==========================================================*
C     | SUBROUTINE ADEXCH_UV_XY_RL   adjoint of  EXCH_UV_XY_RL
C     | o Handle exchanges for _RL, two-dimensional arrays.
C     *==========================================================*
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array which overlap regions are to be exchanged
C     myThid - My thread id.
      _RL aduphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL advphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      LOGICAL withSigns
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
      IF (useCubedSphereExchange) THEN
       CALL EXCH_UV_RL_CUBE( aduphi, advphi, withSigns,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ELSE
       CALL EXCH_RL( aduphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
       CALL EXCH_RL( advphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
c_jmc: for JAM compatibility, replace the 2 CALLs above by the 2 CPP_MACROs:
c       _EXCH_XY_RL( aduphi, myThid )
c       _EXCH_XY_RL( advphi, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_UV_XYZ_RS(
     I                         withSigns, myThid,
     U                         aduphi, advphi )
#else
      SUBROUTINE ADEXCH_UV_XYZ_RS(
     U                         aduphi, advphi,
     I                         withSigns, myThid )
#endif
C     *==========================================================*
C     | SUBROUTINE ADEXCH_UV_XYZ_RS   adjoint of  EXCH_UV_XYZ_RS
C     | o Handle exchanges for _RS, two-dimensional arrays.
C     *==========================================================*
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array which overlap regions are to be exchanged
C     myThid - My thread id.
      _RS aduphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS advphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      LOGICAL withSigns
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
      IF (useCubedSphereExchange) THEN
       CALL EXCH_UV_RS_CUBE( aduphi, advphi, withSigns,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ELSE
       CALL EXCH_RS( aduphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
       CALL EXCH_RS( advphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
c_jmc: for JAM compatibility, replace the 2 CALLs above by the 2 CPP_MACROs:
c       _EXCH_XY_RS( aduphi, myThid )
c       _EXCH_XY_RS( advphi, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE ADEXCH_UV_XYZ_RL(
     I                         withSigns, myThid,
     U                         aduphi, advphi )
#else
      SUBROUTINE ADEXCH_UV_XYZ_RL(
     U                         aduphi, advphi,
     I                         withSigns, myThid )
#endif

C     *==========================================================*
C     | SUBROUTINE ADEXCH_UV_XYZ_RL   adjoint of  EXCH_UV_XYZ_RL
C     | o Handle exchanges for _RL, two-dimensional arrays.
C     *==========================================================*
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array which overlap regions are to be exchanged
C     myThid - My thread id.
      _RL aduphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL advphi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      LOGICAL withSigns
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
      IF (useCubedSphereExchange) THEN
       CALL EXCH_UV_RL_CUBE( aduphi, advphi, withSigns,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      ELSE
       CALL EXCH_RL( aduphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
       CALL EXCH_RL( advphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
c_jmc: for JAM compatibility, replace the 2 CALLs above by the 2 CPP_MACROs:
c       _EXCH_XY_RL( aduphi, myThid )
c       _EXCH_XY_RL( advphi, myThid )
      ENDIF

      RETURN
      END

#endif /* ALLOW_EXCH2 */
