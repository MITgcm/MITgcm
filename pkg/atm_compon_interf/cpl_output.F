C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/cpl_output.F,v 1.1 2009/06/17 14:53:11 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CStartOfInterFace
      SUBROUTINE CPL_OUTPUT( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE CPL_OUTPUT
C     | o Write CPL time-average output
C     | - Atmospheric version -
C     *==========================================================*
C     | Empty S/R for now.
C     *==========================================================*
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
