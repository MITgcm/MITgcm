#include "GGL90_OPTIONS.h"

CBOP
C !ROUTINE: GGL90_ADD_STOKESDRIFT

C !INTERFACE: ==========================================================
      SUBROUTINE GGL90_ADD_STOKESDRIFT(
     O                 uRes, vRes,
     I                 uFld, vFld,
     I                 k, bi, bj, myThid )

C !DESCRIPTION:
C  Add Stokes-drift contribution to Eulerien velocity to get residual flow

C !USES: ===============================================================
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "GGL90.h"

C !INPUT/OUTPUT PARAMETERS: ===================================================
C   uRes, vRes   :: residual flow with Stokes-drift added
C   uFld, vFld   :: Eulerien horizontal velocity, 2 compon.
C   k            :: current vertical level
C   bi, bj       :: current tile indices
C   myThid       :: my Thread Id number
      _RL uRes   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vRes   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj
      INTEGER myThid

#ifdef ALLOW_GGL90_LANGMUIR
C !LOCAL VARIABLES: ====================================================
C   i, j         :: loop indices
C   uStar, vStar :: frictional velocity component
      INTEGER i,j
      _RL uStar, vStar, recip_Lasq, depthFac
      _RL stokesU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stokesV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

      IF ( useLANGMUIR ) THEN

C-    Calculate Stokes-Drift (fct of wind-stress)
C     surface Stokes-Drift velocity is just (uStar,vStar)/ LC_num^2
        recip_Lasq = 1. _d 0 / LC_num
        recip_Lasq = recip_Lasq * recip_Lasq
        depthFac = recip_Lasq*EXP( 4. _d 0 *PI/LC_lambda*rC(k) )
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           uStar = SIGN( SQRT(ABS(surfaceForcingU(i,j,bi,bj))),
     &                   surfaceForcingU(i,j,bi,bj) )
           stokesU(i,j) = uStar * depthFac
           vStar = SIGN( SQRT(ABS(surfaceForcingV(i,j,bi,bj))),
     &                   surfaceForcingV(i,j,bi,bj) )
           stokesV(i,j) = vStar * depthFac
         ENDDO
        ENDDO

C-    Add Stokes-Drift to Eulerien velocity to get residual flow:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           uRes(i,j) = uFld(i,j) + stokesU(i,j)*maskW(i,j,k,bi,bj)
           vRes(i,j) = vFld(i,j) + stokesV(i,j)*maskS(i,j,k,bi,bj)
         ENDDO
        ENDDO

      ENDIF

#endif /* ALLOW_GGL90_LANGMUIR */

      RETURN
      END
