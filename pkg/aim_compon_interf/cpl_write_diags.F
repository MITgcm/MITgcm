C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/cpl_write_diags.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CStartOfInterFace
      SUBROUTINE CPL_WRITE_DIAGS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE CPL_WRITE_DIAGS                               |
C     | o Calculate CPL diagnostics                              |
C     | - Atmospheric version -                                  |
C     |==========================================================|
C     | Empty S/R for now.                                       |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid - Number of this instance of the routine
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE

C     == Local variables ==

#endif /* COMPONENT_MODULE */

      RETURN
      END



