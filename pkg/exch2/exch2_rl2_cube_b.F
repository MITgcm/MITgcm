#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: EXCH2_RL2_CUBE_B

C     !INTERFACE:
      SUBROUTINE EXCH2_RL2_CUBE_B(
     U            array1, array1b,
     U            array2, array2b,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )

C     !DESCRIPTION:
C     Two components vector field AD-Exchange:
C     Tile-edge overlap-region of a 2 component vector field is added to
C     corresponding near-edge interior data point and then zero out.

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_BUFFER.h"

C     !INPUT/OUTPUT PARAMETERS:
C     array1      :: 1rst component array with edges to exchange.
C     array2      :: 2nd  component array with edges to exchange.
C     signOption  :: Flag controlling whether vector is signed.
C     fieldCode   :: field code (position on staggered grid)
C     myOLw,myOLe :: West and East overlap region sizes.
C     myOLs,myOLn :: South and North overlap region sizes.
C     exchWidthX  :: Width of data region exchanged in X.
C     exchWidthY  :: Width of data region exchanged in Y.
C     cornerMode  :: halo-corner-region treatment: update/ignore corner region
C     myThid      :: Thread number of this instance of S/R EXCH...

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RL array1(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      _RL array2(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      _RL array1b(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      _RL array2b(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      LOGICAL signOption
      CHARACTER*2 fieldCode
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP
      CALL EXCH2_RL2_CUBE_AD(
     U            array1b, array2b,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )
      RETURN
      END
