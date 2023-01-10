#include "CPP_EEOPTIONS.h"

      SUBROUTINE EXCH1_UV_RL_CUBE_B(
     U                          Uarray, Uarrayb, Varray, Varrayb, 
     I                          withSigns, 
     I                          myOLw, myOLe, myOLs, myOLn, 
     I                          myNz, exchwidthx,exchWidthY, 
     I                          cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

      _RL     Uarray( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RL     Uarrayb( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RL     Varray( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RL     Varrayb( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      LOGICAL withSigns
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid
      INTEGER myOLw, myOLe, myOLs, myOLn, myNz

      CALL EXCH1_UV_RL_CUBE_AD(
     U                 Uarrayb, Varrayb,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
      END

      SUBROUTINE EXCH1_UV_RS_CUBE_B(
     U                          Uarray, Uarrayb, Varray, Varrayb, 
     I                          withSigns, 
     I                          myOLw, myOLe, myOLs, myOLn, 
     I                          myNz, exchwidthx,exchWidthY, 
     I                          cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

      _RS     Uarray( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RS     Uarrayb( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RS     Varray( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      _RS     Varrayb( 1-myOLw:sNx+myOLe,
     &                1-myOLs:sNy+myOLn,
     &                myNz, nSx, nSy )
      LOGICAL withSigns
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid
      INTEGER myOLw, myOLe, myOLs, myOLn, myNz

      CALL EXCH1_UV_RS_CUBE_AD(
     U                 Uarrayb, Varrayb,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
      END

      SUBROUTINE EXCH1_RL_CUBE_B(
     U                 array, arrayb,
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
      _RL     arrayb( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      LOGICAL withSigns
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid
      
      CALL EXCH1_RL_CUBE_AD(
     U                 arrayb,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
      END


      SUBROUTINE EXCH1_RS_CUBE_B(
     U                 array, arrayb,
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
      _RS     arrayb( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      LOGICAL withSigns
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH1_RS_CUBE_AD(
     U                 arrayb,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
      END

      SUBROUTINE EXCH1_RL_B(array, arrayb,
     &        myOLw, myOLe, myOLs, myOLn,  myNz, 
     &        exchWidthX, exchWidthY, cornerMode, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EXCH.h"
      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RL     array( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      _RL     arrayb( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH1_RL_AD(arrayb,myOLw, myOLe, myOLs, myOLn, 
     &     myNz, exchWidthX , exchWidthY, cornerMode, myThid )
      END

      SUBROUTINE EXCH1_RS_B(array, arrayb,
     &      myOLw, myOLe,  myOLs, myOLn,  myNz, 
     &      exchWidthX, exchWidthY, 
     &      cornerMode, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EXCH.h"
      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RL     array( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      _RL     arrayb( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH1_RS_AD(arrayb, myOLw, myOLe, myOLs, 
     &     myOLn, myNz, exchWidthX, exchWidthY, 
     &     cornerMode, myThid)
      END


