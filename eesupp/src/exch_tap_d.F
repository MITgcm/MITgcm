#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"

C--  File exch_tap_d.F: Tapenade Tangent Linear EXCH1 routines
C--   Contents
C--   o  EXCH1_UV_RL_CUBE_D
C--   o  EXCH1_UV_RS_CUBE_D
C--   o  EXCH1_RL_CUBE_D
C--   o  EXCH1_RS_CUBE_D
C--   o  EXCH1_RL_D
C--   o  EXCH1_RS_D

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_UV_RL_CUBE_D(
     U                 Uarray, Uarrayd, Varray, Varrayd,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RL     Uarray( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RL     Uarrayd( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RL     Varray( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RL     Varrayd( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      LOGICAL withSigns
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH1_UV_RL_CUBE(
     &                 Uarray, Varray,
     &                 withSigns,
     &                 myOLw, myOLe, myOLs, myOLn, myNz,
     &                 exchWidthX, exchWidthY,
     &                 cornerMode, myThid )

      CALL EXCH1_UV_RL_CUBE(
     &                 Uarrayd, Varrayd,
     &                 withSigns,
     &                 myOLw, myOLe, myOLs, myOLn, myNz,
     &                 exchWidthX, exchWidthY,
     &                 cornerMode, myThid )

      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_UV_RS_CUBE_D(
     U                 Uarray, Uarrayd, Varray, Varrayd,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RS     Uarray( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RS     Uarrayd( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RS     Varray( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RS     Varrayd( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      LOGICAL withSigns
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH1_UV_RS_CUBE(
     &                 Uarray, Varray,
     &                 withSigns,
     &                 myOLw, myOLe, myOLs, myOLn, myNz,
     &                 exchWidthX, exchWidthY,
     &                 cornerMode, myThid )

      CALL EXCH1_UV_RS_CUBE(
     &                 Uarrayd, Varrayd,
     &                 withSigns,
     &                 myOLw, myOLe, myOLs, myOLn, myNz,
     &                 exchWidthX, exchWidthY,
     &                 cornerMode, myThid )

      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_RL_CUBE_D(
     U                 array, arrayd,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RL     array( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      _RL     arrayd( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      LOGICAL withSigns
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH1_RL_CUBE(arrayd, withSigns,myOLw, myOLe, myOLs, myOLn,
     &     myNz, exchWidthX , exchWidthY, cornerMode, myThid )

      CALL EXCH1_RL_CUBE(array, withSigns,myOLw, myOLe, myOLs, myOLn,
     &     myNz, exchWidthX , exchWidthY, cornerMode, myThid )

      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_RS_CUBE_D(
     U                 array, arrayd,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RS     array( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      _RS     arrayd( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      LOGICAL withSigns
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH1_RS_CUBE(arrayd, withSigns,myOLw, myOLe, myOLs, myOLn,
     &     myNz, exchWidthX , exchWidthY, cornerMode, myThid )

      CALL EXCH1_RS_CUBE(array, withSigns,myOLw, myOLe, myOLs, myOLn,
     &     myNz, exchWidthX , exchWidthY, cornerMode, myThid )

      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_RL_D(
     U                 array, arrayd,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EXCH.h"

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RL     array( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      _RL     arrayd( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH1_RL(arrayd,myOLw, myOLe, myOLs, myOLn,
     &     myNz, exchWidthX , exchWidthY, cornerMode, myThid )
      CALL EXCH1_RL(array,myOLw, myOLe, myOLs, myOLn,
     &     myNz, exchWidthX , exchWidthY, cornerMode, myThid )
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_RS_D(
     U                 array, arrayd,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EXCH.h"

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RS     array( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      _RS     arrayd( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH1_RS(arrayd, myOLw, myOLe, myOLs,
     &     myOLn, myNz, exchWidthX, exchWidthY,
     &     cornerMode, myThid)

      CALL EXCH1_RS(array, myOLw, myOLe, myOLs,
     &     myOLn, myNz, exchWidthX, exchWidthY,
     &     cornerMode, myThid)

      END
