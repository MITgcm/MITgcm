#include "CTRL_OPTIONS.h"
C- note: although we just need CPP_EEOPTIONS.h here, prefer to
C        include the same option-file above as in the AD version.

C--  File ctrl_bound_tl.F:
C--   Contents
C--   o CTRL_BOUND_3D_TL
C--   o CTRL_BOUND_2D_TL

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CTRL_BOUND_3D_TL
C     !INTERFACE:
      SUBROUTINE CTRL_BOUND_3D_TL(
     U                fieldCur, fieldCur_tl,
     I                mask3D, boundsVec, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o tangent linear version of S/R CTRL_BOUND_3D. Call
C     |   S/R CTRL_BOUND_3D to apply specified bounds to control
C     |   variable, but leave tangent linear variable alone
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL fieldCur   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fieldCur_tl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS mask3D     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL boundsVec(5)
      INTEGER myThid

      CALL CTRL_BOUND_3D(fieldCur,mask3D,boundsVec,myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C
C     !ROUTINE: CTRL_BOUND_2D_TL
C     !INTERFACE:
      SUBROUTINE CTRL_BOUND_2D_TL(
     U                fieldCur, fieldCur_tl,
     I                mask2D, boundsVec, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o tangent linear version of S/R CTRL_BOUND_2D. Call
C     |   S/R CTRL_BOUND_2D to apply specified bounds to control
C     |   variable, but leave tangent linear variable alone
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL fieldCur   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fieldCur_tl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask2D     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL boundsVec(5)
      INTEGER myThid

      CALL CTRL_BOUND_2D(fieldCur,mask2D,boundsVec,myThid)

      RETURN
      END
