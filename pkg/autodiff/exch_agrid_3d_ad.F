C     /==========================================================\
C     | SUBROUTINEs  
C     |              adexch_uv_agrid_3d_rs                             |
C     |              adexch_uv_agrid_3d_rl                             |
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

CBOP
C     !ROUTINE: ADEXCH_UV_AGRID_3D_RS

C     !INTERFACE:
      SUBROUTINE ADEXCH_UV_AGRID_3D_RS(
     I                          withSigns, myNz, myThid,
     U                          adUphi, adVphi )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ADEXCH_UV_AGRID_3D_RS
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
#include "EESUPPORT.h"
#include "EXCH.h"

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
C     OL[wens]       :: Overlap extents in west, east, north, south.
C     exchWidth[XY]  :: Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY

CEOP
#ifdef ALLOW_EXCH2
      CALL EXCH2_UV_3D_RS(
     U                     adUphi, adVphi,
     I                     withSigns, myNz, myThid )
      RETURN
#endif

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
      IF (useCubedSphereExchange) THEN
       STOP 'EXCH_AGRID_3D_AD: AGRID EXCH not implemented for c.s.'
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

CBOP
C     !ROUTINE: ADEXCH_UV_AGRID_3D_RL

C     !INTERFACE:
      SUBROUTINE ADEXCH_UV_AGRID_3D_RL(
     I                          withSigns, myNz, myThid,
     U                          adUphi, adVphi )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ADEXCH_UV_AGRID_3D_RL
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
#include "EESUPPORT.h"
#include "EXCH.h"

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
C     OL[wens]       :: Overlap extents in west, east, north, south.
C     exchWidth[XY]  :: Extent of regions that will be exchanged.
      INTEGER OLw, OLe, OLn, OLs, exchWidthX, exchWidthY

CEOP
#ifdef ALLOW_EXCH2
      CALL EXCH2_UV_3D_RL(
     U                     adUphi, adVphi,
     I                     withSigns, myNz, myThid )
      RETURN
#endif

      OLw        = OLx
      OLe        = OLx
      OLn        = OLy
      OLs        = OLy
      exchWidthX = OLx
      exchWidthY = OLy
C     ** NOTE ** The exchange routine we use here does not
C                require the preceeding and following barriers.
C                However, the slow, simple exchange interface
C                that is calling it here is meant to ensure
C                that threads are synchronised before exchanges
C                begine.
      IF (useCubedSphereExchange) THEN
       STOP 'EXCH_AGRID_3D_AD: AGRID EXCH not implemented for c.s.'
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
