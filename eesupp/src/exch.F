C--   File exch.F: Routines that perform atomic communication operations 
C                  i.e. communication operations that are completed by
C                  time the routines return. 
C      Contents
C      o exch_xy_r4   - Does overlap updates for REAL*4 2d XY fields
C      o exch_xy_r8   - Does overlap updates for REAL*8 2d XY fields
C      o exch_xyz_r4  - Does overlap updates for REAL*4 3d XYZ fields
C      o exch_xyz_r8  - Does overlap updates for REAL*8 3d XYZ fields
#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE EXCH_XY_R4( 
     U                       phi, 
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_XY_R4                                    |
C     | o Handle exchanges for real*4, two-dimensional arrays.   |
C     |==========================================================|
C     | Do true shared-memory data transfers and "messaging"     |
C     | tranfers for blocking case of data transfers.            |
C     | Applications call this routine using                     |
C     |  CALL EXCH..( x, myThid )                                |
C     | where x is a two-dimensional array with overlaps.        |
C     | This routine does true-shared-memory copies for blocks   |
C     | within a thread. It will also do MPI meesaging between   |
C     | different processes.                                     |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array with overlap regions are to be exchanged
C     myThid - My thread id.
      Real*4 phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
      CALL EXCH_RS( phi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            FORWARD_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      _BARRIER

      RETURN
      END

CStartOfInterface
      SUBROUTINE EXCH_XYZ_R4( 
     U                       phi, 
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_XYZ_R4                                   |
C     | o Handle exchanges for real*4, three-dimensional arrays. |
C     |==========================================================|
C     | Do true shared-memory data transfers and "messaging"     |
C     | tranfers for blocking case of data transfers.            |
C     | Applications call this routine using                     |
C     |  CALL EXCH..( x, myThid )                                |
C     | where x is a three-dimensional array with overlaps.      |
C     | This routine does true-shared-memory copies for blocks   |
C     | within a thread. It will also do MPI meesaging between   |
C     | different processes.                                     |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array with overlap regions are to be exchanged
C     myThid - My thread id.
      Real*4 phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
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
      myNz       = Nr
C     ** NOTE ** The exchange routine we use here does not 
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface 
C                that is calling it here is meant to ensure 
C                that threads are synchronised before exchanges
C                begine.
      _BARRIER
      CALL EXCH_RS( phi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            FORWARD_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      _BARRIER

      RETURN
      END

CStartOfInterface
      SUBROUTINE EXCH_XY_R8( 
     U                       phi, 
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_XY_R8                                    |
C     | o Handle exchanges for real*8, two-dimensional arrays.   |
C     |==========================================================|
C     | Do true shared-memory data transfers and "messaging"     |
C     | tranfers for blocking case of data transfers.            |
C     | Applications call this routine using                     |
C     |  CALL EXCH..( x, myThid )                                |
C     | where x is a two-dimensional array with overlaps.        |
C     | This routine does true-shared-memory copies for blocks   |
C     | within a thread. It will also do MPI meesaging between   |
C     | different processes.                                     |
C     | Note:                                                    |
C     | =====                                                    |
C     | This routine is a default mode interface to the more     |
C     | general exchange routine. Under default mode the         |
C     | exchange is for an edge width of OLx, and OLy and        |
C     | updates corner points as well as edges ( slightly more   |
C     | synchronisation required).                               |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array with overlap regions are to be exchanged
C              Note - The interface to EXCH_RL assumes that
C              the standard Fortran 77 sequence association rules
C              apply.
C     myThid - My thread id.
      Real*8 phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
      CALL EXCH_RL( phi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            FORWARD_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      _BARRIER

      RETURN
      END

CStartOfInterface
      SUBROUTINE EXCH_XYZ_R8( 
     U                       phi, 
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_XYZ_R8                                   |
C     | o Handle exchanges for real*8, three-dimensional arrays. |
C     |==========================================================|
C     | Do true shared-memory data transfers and "messaging"     |
C     | tranfers for blocking case of data transfers.            |
C     | Applications call this routine using                     |
C     |  CALL EXCH..( x, myThid )                                |
C     | where x is a three-dimensional array with overlaps.      |
C     | This routine does true-shared-memory copies for blocks   |
C     | within a thread. It will also do MPI meesaging between   |
C     | different processes.                                     |
C     | Note:                                                    |
C     | =====                                                    |
C     | If it is used, "asynchronous" messaging in which         |
C     | communication overlaps computation is handled elsewhere  |
C     | - see recv.F and send.F.                                 |
C     | In practice MPI implementations may not be completely    |
C     | thread safe. In principle this code is correct even for  |
C     | mixed MPI and multi-threaded execution.                  |
C     | In multi-thread execution data managed by other threads  |
C     | is only automatically visible to another thread if it    |
C     | existed before the thread was created. This means that   |
C     | for Fortran we declare arrays with overlap regions in    |
C     | COMMON blocks.                                           |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array with overlap regions are to be exchanged
C     myThid - My thread id.
      Real*8 phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
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
      myNz       = Nr
C     ** NOTE ** The exchange routine we use here does not 
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface 
C                that is calling it here is meant to ensure 
C                that threads are synchronised before exchanges
C                begine.
      _BARRIER
      CALL EXCH_RL( phi,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            FORWARD_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      _BARRIER

      RETURN
      END

C $Id: exch.F,v 1.9 1999/05/24 15:15:11 adcroft Exp $
