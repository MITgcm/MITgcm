C $Header: /u/gcmpack/MITgcm/pkg/autodiff/Attic/exch_ad.F,v 1.5 2008/06/14 16:15:03 heimbach Exp $
C $Name:  $

C     /==========================================================\
C     | SUBROUTINEs  adexch_xy_rs adexch_xyz_rs                  |
C     |              adexch_xy_rl adexch_xyz_rl                  |
C     |              adexch_xz_rs adexch_yz_rs                   |
C     |              adexch_xz_rl adexch_yz_rl                   |
C     | o adjoint routines to exchange routines                  |
C     |==========================================================|
C     | These routines essentially are calling the original      |
C     | routines in reverse mode.                                |
C     | These are regular adjoint routines with no innermost     |
C     | dimension added.                                         |
C     | Calls to these routines will be generated automatically  |
C     | by TAMC if correct flow directives are given             |
C     |   tamc -reverse ....                                     |
C     |                                                          |
C     | written and tested by Ralf Giering, Jan. 14th 1999       |
C     | added new S/R's for _xz_ , _yz_ heimbach@mit.edu May-01  |
C     \==========================================================/

#include "CPP_OPTIONS.h"

#ifdef ALLOW_EXCH2

C     For exch2 all exch_xy* routines are shown to TAF
C     and hand-written code below is not needed.

      SUBROUTINE DUMMY_EXCH_AD

      RETURN
      END

#else

CStartOfInterface
      SUBROUTINE adexch_xy_rs(
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_XY_RS   adjoint of  EXCH_XY_RS         |
C     | o Handle exchanges for real*4, two-dimensional arrays.   |
C     |==========================================================|
C     === Global data ===
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
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


CStartOfInterface
      SUBROUTINE adexch_xy_rl(
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_XY_RL   adjoint of  EXCH_XY_RL         |
C     | o Handle exchanges for real*8, two-dimensional arrays.   |
C     |==========================================================|
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
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


CStartOfInterface
      SUBROUTINE adexch_xyz_rs(
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_XYZ_RS   adjoint of  EXCH_XYZ_RS       |
C     | o Handle exchanges for real*4, two-dimensional arrays.   |
C     |==========================================================|
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
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


CStartOfInterface
      SUBROUTINE adexch_xyz_rl(
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_XYZ_RL   adjoint of  EXCH_XYZ_RL       |
C     | o Handle exchanges for real*8, two-dimensional arrays.   |
C     |==========================================================|
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
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


CStartOfInterface
      SUBROUTINE adexch_xz_rs(
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_XZ_RS   adjoint of  EXCH_XZ_RS         |
C     | o Handle exchanges for real*4, two-dimensional arrays.   |
C     |==========================================================|
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
      CALL EXCH_RS( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )

      RETURN
      END


CStartOfInterface
      SUBROUTINE adexch_xz_rl(
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_XZ_RL   adjoint of  EXCH_XZ_RL         |
C     | o Handle exchanges for real*8, two-dimensional arrays.   |
C     |==========================================================|
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
      CALL EXCH_RL( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )

      RETURN
      END


CStartOfInterface
      SUBROUTINE adexch_yz_rs(
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_YZ_RS   adjoint of  EXCH_YZ_RS         |
C     | o Handle exchanges for real*4, two-dimensional arrays.   |
C     |==========================================================|
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
      CALL EXCH_RS( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )

      RETURN
      END


CStartOfInterface
      SUBROUTINE adexch_yz_rl(
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_YZ_RL   adjoint of  EXCH_YZ_RL         |
C     | o Handle exchanges for real*8, two-dimensional arrays.   |
C     |==========================================================|
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
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
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
      CALL EXCH_RL( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )

      RETURN
      END

#endif /* ALLOW_EXCH2 */
