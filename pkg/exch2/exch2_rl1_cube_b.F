#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: EXCH_RL_CUBE_B

C     !INTERFACE:
      SUBROUTINE EXCH2_RL1_CUBE_B(
     U            array, arrayb,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )

C     !DESCRIPTION:
C     Scalar field (1 component) AD-Exchange:
C     Tile-edge overlap-region of a 1 component scalar field is added to
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
C     array       :: Array with edges to exchange.
C     signOption  :: Flag controlling whether field sign depends on orientation
C                 :: (signOption not yet implemented but needed for SM exch)
C     fieldCode   :: field code (position on staggered grid)
C     myOLw,myOLe :: West and East overlap region sizes.
C     myOLs,myOLn :: South and North overlap region sizes.
C     exchWidthX  :: Width of data regi exchanged in X.
C     exchWidthY  :: Width of data region exchanged in Y.
C     cornerMode  :: halo-corner-region treatment: update/ignore corner region
C     myThid      :: Thread number of this instance of S/R EXCH...

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RL array(1-myOLw:sNx+myOLe,
     &          1-myOLs:sNy+myOLn,
     &          myNZ, nSx, nSy)
      _RL arrayb(1-myOLw:sNx+myOLe,
     &          1-myOLs:sNy+myOLn,
     &          myNZ, nSx, nSy)
      LOGICAL signOption
      CHARACTER*2 fieldCode
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

C     !LOCAL VARIABLES:
C     e2_msgHandles :: Synchronization and coordination data structure used to
C                   :: coordinate access to e2Bufr1_RL or to regulate message
C                   :: buffering. In PUT communication sender will increment
C                   :: handle entry once data is ready in buffer. Receiver will
C                   :: decrement handle once data is consumed from buffer.
C                   :: For MPI MSG communication MPI_Wait uses handle to check
C                   :: Isend has cleared. This is done in routine after receives.
C     note: a) current implementation does not use e2_msgHandles for communication
C              between threads: all-threads barriers are used (see CNH note below).
C              For a 2-threads synchro communication (future version),
C              e2_msgHandles should be shared (in common block, moved to BUFFER.h)
C           b) 1rst dim=2 so that it could be used also by exch2_rx2_cube.
      INTEGER bi, bj
C     Variables for working through W2 topology
      INTEGER e2_msgHandles( 2, W2_maxNeighbours, nSx, nSy )
      INTEGER thisTile, farTile, N, nN, oN
      INTEGER tIlo, tIhi, tJlo, tJhi, tKlo, tKhi
      INTEGER tIStride, tJStride, tKStride
      INTEGER i1Lo, i1Hi, j1Lo, j1Hi, k1Lo, k1Hi
      LOGICAL updateCorners

#ifdef ALLOW_USE_MPI
      INTEGER iBufr, nri, nrj
C     MPI stuff (should be in a routine call)
      INTEGER mpiStatus(MPI_STATUS_SIZE)
      INTEGER mpiRc
      INTEGER wHandle
#endif
CEOP
      CALL EXCH2_RL1_CUBE_AD(
     U            arrayb,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
