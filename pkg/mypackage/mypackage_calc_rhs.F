C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_calc_rhs.F,v 1.1 2010/10/07 00:22:51 dimitri Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

CBOP
C !ROUTINE: MYPACKAGE_CALC_RHS

C !INTERFACE: ==========================================================
      SUBROUTINE MYPACKAGE_CALC_RHS(
     I        bi, bj, myTime, myIter, myThid )

C !DESCRIPTION:
C Calculate custom tendency terms outside k-loop in DO_OCEANIC_PHYS

C !USES: ===============================================================
      IMPLICIT NONE

C !INPUT PARAMETERS: ===================================================
C bi,bj     :: Tile indices
C myTime    :: Current time in simulation
C myIter    :: Current time-step number
C myThid    :: my Thread Id number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================

C !LOCAL VARIABLES: ====================================================
CEOP

C- Calculate custom tendency terms ...

      RETURN
      END
