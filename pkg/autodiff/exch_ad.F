C     /==========================================================\
C     | SUBROUTINEs  adexch_xy_r4 adexch_xyz_r4                  |
C     |              adexch_xy_r8 adexch_xyz_r8                  |
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
C     \==========================================================/

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE adexch_xy_r4( 
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_XY_R4   adjoint of  EXCH_XY_R4         |
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
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY, myNz

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      myNz       = 1
C     ** NOTE ** The exchange routine we use here does not 
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface 
C                that is calling it here is meant to ensure 
C                that threads are synchronised before exchanges
C                begine.
      _BARRIER
      CALL EXCH_RS( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      _BARRIER

      RETURN
      END


#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE adexch_xy_r8( 
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_XY_R8   adjoint of  EXCH_XY_R8         |
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
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY, myNz

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      myNz       = 1
C     ** NOTE ** The exchange routine we use here does not 
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface 
C                that is calling it here is meant to ensure 
C                that threads are synchronised before exchanges
C                begine.
      _BARRIER
      CALL EXCH_RL( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      _BARRIER

      RETURN
      END



#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE adexch_xyz_r4( 
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_XYZ_R4   adjoint of  EXCH_XYZ_R4       |
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
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY, myNz

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      myNz       = Nr
C     ** NOTE ** The exchange routine we use here does not 
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface 
C                that is calling it here is meant to ensure 
C                that threads are synchronised before exchanges
C                begine.
      _BARRIER
      CALL EXCH_RS( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      _BARRIER

      RETURN
      END



#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE adexch_xyz_r8( 
     I                         myThid
     U                       , adphi
     &                       )
C     /==========================================================\
C     | SUBROUTINE ADEXCH_XYZ_R8   adjoint of  EXCH_XYZ_R8       |
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
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY, myNz

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
      myNz       = Nr
C     ** NOTE ** The exchange routine we use here does not 
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface 
C                that is calling it here is meant to ensure 
C                that threads are synchronised before exchanges
C                begine.
      _BARRIER
      CALL EXCH_RL( adphi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            REVERSE_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      _BARRIER

      RETURN
      END
