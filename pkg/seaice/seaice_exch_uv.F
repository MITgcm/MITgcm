C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/seaice_exch_uv.F,v 1.3 2006/08/24 01:01:47 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_EXCH_UV

C     !INTERFACE:
      SUBROUTINE SEAICE_EXCH_UV(
     U                           uFld, vFld,
     I                           myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE SEAICE_EXCH_UV
C     | o RL exchange routine with k=3
C     *==========================================================*
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine.
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP

C--   Update overlap regions
      CALL EXCH_UV_3D_RL(
     U                    uFld, vFld,
     I                    .TRUE., 3, myThid )

      RETURN
      END
