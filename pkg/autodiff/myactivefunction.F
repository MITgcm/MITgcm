C $Header: /u/gcmpack/MITgcm/pkg/autodiff/myactivefunction.F,v 1.1 2005/06/23 21:52:47 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !INTERFACE:
       _RL FUNCTION myActiveFunction ( mytmp3d, mythid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      _RL     mytmp3d(sNx,sNy,Nr,nSx,nSy)
      INTEGER myThid
      INTEGER myIter

C     !LOCAL VARIABLES:

c     == external ==

CEOP

      myActiveFunction = 1.0

      RETURN
      END
