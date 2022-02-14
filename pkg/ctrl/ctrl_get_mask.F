#include "CTRL_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

C--   File ctrl_get_mask.F:
C--    Contents:
C--    o CTRL_GET_MASK3D
C--    o CTRL_GET_MASK2D

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: CTRL_GET_MASK3D
C     !INTERFACE:
      SUBROUTINE CTRL_GET_MASK3D(
     I      xx_filename,
     U      mask3D,
     I      myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_GET_MASK3D
C     | o A simple routine to return the correct 3D mask for each ctrl
C     |   variable
C     | o Only important for a few cases: e.g.
C     |   - uvel/vvel need maskW/S rather than maskC
C     |   - in shelfice domain, need special 2D masks
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(MAX_LEN_FNAM) xx_filename
      _RS mask3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP

C --- Initial velocity
#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      IF (xx_filename(1:7).EQ.'xx_uvel') THEN
        CALL CTRL_CPRSRS(maskW,Nr,mask3D,Nr,myThid)
      ELSEIF (xx_filename(1:7).EQ.'xx_vvel') THEN
        CALL CTRL_CPRSRS(maskS,Nr,mask3D,Nr,myThid)
      ELSE
#else
      IF (.TRUE.) THEN
#endif
C --- Set to maskC by default
        CALL CTRL_CPRSRS(maskC, Nr, mask3D, Nr, myThid)
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     !ROUTINE: CTRL_GET_MASK2D
C     !INTERFACE:
      SUBROUTINE CTRL_GET_MASK2D(
     I      xx_filename,
     U      mask2D,
     I      myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_GET_MASK2D
C     | o A simple routine to return the correct 2D mask for each ctrl
C     |   variable.
C     | o 2D mask is in XY plane
C     | o Only important for a few cases: e.g.
C     |   - uvel/vvel need maskW/S rather than maskC
C     |   - in shelfice domain, need special 2D masks
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(MAX_LEN_FNAM) xx_filename
      _RS mask2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
      LOGICAL iAmDone
CEOP

      iAmDone = .FALSE.

C --- Shelfice
#ifdef ALLOW_SHELFICE
      IF ((xx_filename(1:11).EQ.'xx_shicoeff').OR.
     &    (xx_filename(1:11).EQ.'xx_shicdrag').OR.
     &    (xx_filename(1:11).EQ.'xx_shifwflx')) THEN
        CALL CTRL_CPRSRS(maskSHI, Nr, mask2D, 1, myThid)
        iAmDone = .TRUE.
      ENDIF
#endif

C --- Wind stress. Wind speed always uses maskC
#ifndef ALLOW_ROTATE_UV_CONTROLS
# ifdef ALLOW_EXF
      IF (stressIsOnCgrid) THEN
# endif
        IF (xx_filename(1:7).EQ.'xx_tauu') THEN
          CALL CTRL_CPRSRS(maskW, Nr, mask2D, 1, myThid)
          iAmDone = .TRUE.
        ELSEIF (xx_filename(1:7).EQ.'xx_tauv') THEN
          CALL CTRL_CPRSRS(maskS, Nr, mask2D, 1, myThid)
          iAmDone = .TRUE.
        ENDIF
# ifdef ALLOW_EXF
      ENDIF
# endif
#endif

C --- Set to maskC by default, if not set
      IF (.NOT.iAmDone) CALL CTRL_CPRSRS(maskC, Nr, mask2D, 1, myThid)

      RETURN
      END
