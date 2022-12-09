#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: EXCH_RL2_CUBE_B

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
      INTEGER bi, bj
C     Variables for working through W2 topology
      INTEGER e2_msgHandles( 2, W2_maxNeighbours, nSx, nSy )
      INTEGER thisTile, farTile, N, nN, oN
      INTEGER tIlo1, tIhi1, tJlo1, tJhi1, oIs1, oJs1
      INTEGER tIlo2, tIhi2, tJlo2, tJhi2, oIs2, oJs2
      INTEGER tIStride, tJStride
      INTEGER tKlo, tKhi, tKStride
      INTEGER i1Lo, i1Hi, j1Lo, j1Hi, k1Lo, k1Hi
      INTEGER i2Lo, i2Hi, j2Lo, j2Hi, k2Lo, k2Hi
      LOGICAL updateCorners

#ifdef ALLOW_USE_MPI
      INTEGER iBufr1, iBufr2, nri, nrj
C     MPI stuff (should be in a routine call)
      INTEGER mpiStatus(MPI_STATUS_SIZE)
      INTEGER mpiRc
      INTEGER wHandle
#endif
CEOP
      CALL EXCH2_RL2_CUBE_AD(
     U            array1b, array2b,
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
