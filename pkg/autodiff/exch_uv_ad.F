C     /==========================================================\
C     | SUBROUTINEs  adexch_uv_xy_rs adexch_uv_xyz_rs            |
C     |              adexch_uv_xy_rl adexch_uv_xyz_rl            |
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

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE adexch_uv_xy_rs( 
     I                       withSigns, myThid,
     U                       aduphi, advphi 
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_UV_XY_RS   adjoint of  EXCH_UV_XY_RS   |
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


CStartOfInterface
      SUBROUTINE adexch_uv_xy_rl( 
     I                       withSigns, myThid,
     U                       aduphi, advphi 
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_UV_XY_RL   adjoint of  EXCH_UV_XY_RL   |
C     | o Handle exchanges for real*8, two-dimensional arrays.   |
C     |==========================================================|
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
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


CStartOfInterface
      SUBROUTINE adexch_uv_xyz_rs( 
     I                       withSigns, myThid,
     U                       aduphi, advphi 
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_UV_XYZ_RS   adjoint of  EXCH_UV_XYZ_RS |
C     | o Handle exchanges for real*4, two-dimensional arrays.   |
C     |==========================================================|
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
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


CStartOfInterface
      SUBROUTINE adexch_uv_xyz_rl( 
     I                       withSigns, myThid,
     U                       aduphi, advphi 
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_UV_XYZ_RL   adjoint of  EXCH_UV_XYZ_RL |
C     | o Handle exchanges for real*8, two-dimensional arrays.   |
C     |==========================================================|
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
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

