#include "CPP_EEOPTIONS.h"

C--  File exch2_rs_rl_12_d.F: Tapenade Tangent Linear EXCH2 routines
C--   Contents
C--   o  EXCH2_RS1_CUBE_D(
C--   o  EXCH2_RL1_CUBE_D(
C--   o  EXCH2_RS2_CUBE_D(
C--   o  EXCH2_RL2_CUBE_D(

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH2_RS1_CUBE_D(
     U            array, arrayd,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_BUFFER.h"

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RS array(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      _RS arrayd(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      LOGICAL signOption
      CHARACTER*2 fieldCode
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH2_RS1_CUBE(
     U            array,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )
      CALL EXCH2_RS1_CUBE(
     U            arrayd,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH2_RL1_CUBE_D(
     U            array, arrayd,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_BUFFER.h"

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RL array(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      _RL arrayd(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      LOGICAL signOption
      CHARACTER*2 fieldCode
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH2_RL1_CUBE(
     U            array,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )
      CALL EXCH2_RL1_CUBE(
     U            arrayd,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )

      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH2_RS2_CUBE_D(
     U            array1, array1d,
     U            array2, array2d,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_BUFFER.h"
      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RS array1(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      _RS array1d(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      _RS array2(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      _RS array2d(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)

      LOGICAL signOption
      CHARACTER*2 fieldCode
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH2_RS2_CUBE(
     U            array1, array2,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )

      CALL EXCH2_RS2_CUBE(
     U            array1d, array2d,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )

      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH2_RL2_CUBE_D(
     U            array1, array1d,
     U            array2, array2d,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_BUFFER.h"

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RL array1(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      _RL array1d(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      _RL array2(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)
      _RL array2d(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn,
     &           myNz, nSx, nSy)

      LOGICAL signOption
      CHARACTER*2 fieldCode
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH2_RL2_CUBE(
     U            array1, array2,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )

      CALL EXCH2_RL2_CUBE(
     U            array1d, array2d,
     I            signOption, fieldCode,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            cornerMode, myThid )

      END
