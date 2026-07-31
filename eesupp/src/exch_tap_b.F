#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"

C--  File exch_tap_b.F: Tapenade Adjoint (Backward) EXCH1 routines
C--   Contents
C--   o  EXCH1_UV_RL_CUBE_B
C--   o  EXCH1_UV_RS_CUBE_B
C--   o  EXCH1_RL_CUBE_B
C--   o  EXCH1_RL_CUBE_FWD (for adjoint without checkpointing)
C--   o  EXCH1_RL_CUBE_BWD (for adjoint without checkpointing)
C--   o  EXCH1_RS_CUBE_B
C--   o  EXCH1_RL_B
C--   o  EXCH1_RL_FWD (for adjoint without checkpointing)
C--   o  EXCH1_RL_BWD (for adjoint without checkpointing)
C--   o  EXCH1_RS_B
C--   o  EXCH1_RS_FWD (for adjoint without checkpointing)
C--   o  EXCH1_RS_BWD (for adjoint without checkpointing)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_UV_RL_CUBE_B(
     U                          Uarray, Uarrayb, Varray, Varrayb,
     I                          withSigns,
     I                          myOLw, myOLe, myOLs, myOLn,
     I                          myNz, exchwidthx,exchWidthY,
     I                          cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
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

      CALL EXCH1_UV_RL_CUBE_AD(
     U                 Uarrayb, Varrayb,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_UV_RS_CUBE_B(
     U                          Uarray, Uarrayb, Varray, Varrayb,
     I                          withSigns,
     I                          myOLw, myOLe, myOLs, myOLn,
     I                          myNz, exchwidthx,exchWidthY,
     I                          cornerMode, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
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

      CALL EXCH1_UV_RS_CUBE_AD(
     U                 Uarrayb, Varrayb,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_RL_CUBE_FWD(
     U                 array,
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
      LOGICAL withSigns
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

#ifdef ALLOW_TAPENADE
c [llh] the following PUSHREAL8ARRAY could certainly be refined!
c [ssg] for halfpipe_streamice
c      CALL PUSHREAL8ARRAY(array, 26**2*2)
c [ssg] for tutorial_global_oce_biogeo
      CALL PUSHBOOLEAN(withSigns)
      CALL PUSHINTEGER4(myOLw)
      CALL PUSHINTEGER4(myOLe)
      CALL PUSHINTEGER4(myOLs)
      CALL PUSHINTEGER4(myOLn)
      CALL PUSHINTEGER4(myNz)
      CALL PUSHINTEGER4(exchWidthX)
      CALL PUSHINTEGER4(exchWidthY)
      CALL PUSHINTEGER4(cornerMode)
      CALL PUSHINTEGER4(myThid)
      CALL EXCH1_RL_CUBE(
     U                 array,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_RL_CUBE_BWD(
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

#ifdef ALLOW_TAPENADE
      CALL POPBOOLEAN(withSigns)
      CALL POPINTEGER4(myOLw)
      CALL POPINTEGER4(myOLe)
      CALL POPINTEGER4(myOLs)
      CALL POPINTEGER4(myOLn)
      CALL POPINTEGER4(myNz)
      CALL POPINTEGER4(exchWidthX)
      CALL POPINTEGER4(exchWidthY)
      CALL POPINTEGER4(cornerMode)
      CALL POPINTEGER4(myThid)
      CALL EXCH1_RL_CUBE_AD(
     U                 arrayb,
     I                 withSigns,
     I                 myOLw, myOLe, myOLs, myOLn, myNz,
     I                 exchWidthX, exchWidthY,
     I                 cornerMode, myThid )
c [ssg] for halfpipe_streamice
c      CALL POPREAL8ARRAY(array, 26**2*2)
c [ssg] for tutorial_global_oce_biogeo
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_RL_FWD(array,
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
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

#ifdef ALLOW_TAPENADE
c [llh] the following PUSHREAL8ARRAY could certainly be refined!
c [ssg] for halfpipe_streamice
c      CALL PUSHREAL8ARRAY(array, 26**2*2)
c [ssg] for tutorial_global_oce_biogeo
      CALL PUSHINTEGER4(myOLw)
      CALL PUSHINTEGER4(myOLe)
      CALL PUSHINTEGER4(myOLs)
      CALL PUSHINTEGER4(myOLn)
      CALL PUSHINTEGER4(myNz)
      CALL PUSHINTEGER4(exchWidthX)
      CALL PUSHINTEGER4(exchWidthY)
      CALL PUSHINTEGER4(cornerMode)
      CALL PUSHINTEGER4(myThid)
      CALL EXCH1_RL(array, myOLw, myOLe, myOLs, myOLn,
     &     myNz, exchWidthX , exchWidthY, cornerMode, myThid )
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_RL_BWD(array, arrayb,
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
#ifdef ALLOW_TAPENADE
      INTEGER tempCM

      CALL POPINTEGER4(myThid)
      CALL POPINTEGER4(tempCM)
      CALL POPINTEGER4(exchWidthY)
      CALL POPINTEGER4(exchWidthX)
      CALL POPINTEGER4(myNz)
      CALL POPINTEGER4(myOLn)
      CALL POPINTEGER4(myOLs)
      CALL POPINTEGER4(myOLe)
      CALL POPINTEGER4(myOLw)
      CALL EXCH1_RL_AD(arrayb, myOLw, myOLe, myOLs, myOLn,
     &     myNz, exchWidthX , exchWidthY, tempCM, myThid )
c [ssg] for halfpipe_streamice
c      CALL POPREAL8ARRAY(array, 26**2*2)
c [ssg] for tutorial_global_oce_biogeo
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_RS_B(array, arrayb,
     &      myOLw, myOLe,  myOLs, myOLn,  myNz,
     &      exchWidthX, exchWidthY,
     &      cornerMode, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EXCH.h"
      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RS     array( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      _RS     arrayb( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

      CALL EXCH1_RS_AD(arrayb, myOLw, myOLe, myOLs,
     &     myOLn, myNz, exchWidthX, exchWidthY,
     &     cornerMode, myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_RS_FWD(array,
     &        myOLw, myOLe, myOLs, myOLn,  myNz,
     &        exchWidthX, exchWidthY, cornerMode, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EXCH.h"
      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RS     array( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid

#ifdef ALLOW_TAPENADE
c [llh] the following PUSHREAL8ARRAY could certainly be refined!
c [ssg] for halfpipe_streamice
c      CALL PUSHREAL8ARRAY(array, 26**2*2)
c [ssg] for tutorial_global_oce_biogeo
      CALL PUSHINTEGER4(myOLw)
      CALL PUSHINTEGER4(myOLe)
      CALL PUSHINTEGER4(myOLs)
      CALL PUSHINTEGER4(myOLn)
      CALL PUSHINTEGER4(myNz)
      CALL PUSHINTEGER4(exchWidthX)
      CALL PUSHINTEGER4(exchWidthY)
      CALL PUSHINTEGER4(cornerMode)
      CALL PUSHINTEGER4(myThid)
      CALL EXCH1_RS(array, myOLw, myOLe, myOLs, myOLn,
     &     myNz, exchWidthX , exchWidthY, cornerMode, myThid )
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXCH1_RS_BWD(array, arrayb,
     &        myOLw, myOLe, myOLs, myOLn,  myNz,
     &        exchWidthX, exchWidthY, cornerMode, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EXCH.h"
      INTEGER myOLw, myOLe, myOLs, myOLn, myNz
      _RS     array( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      _RS     arrayb( 1-myOLw:sNx+myOLe,
     &               1-myOLs:sNy+myOLn,
     &               myNz, nSx, nSy )
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER cornerMode
      INTEGER myThid
#ifdef ALLOW_TAPENADE
      INTEGER tempCM

      CALL POPINTEGER4(myThid)
      CALL POPINTEGER4(tempCM)
      CALL POPINTEGER4(exchWidthY)
      CALL POPINTEGER4(exchWidthX)
      CALL POPINTEGER4(myNz)
      CALL POPINTEGER4(myOLn)
      CALL POPINTEGER4(myOLs)
      CALL POPINTEGER4(myOLe)
      CALL POPINTEGER4(myOLw)
      CALL EXCH1_RS_AD(arrayb, myOLw, myOLe, myOLs, myOLn,
     &     myNz, exchWidthX , exchWidthY, cornerMode, myThid )
c [ssg] for tutorial_global_oce_biogeo
#endif

      RETURN
      END
