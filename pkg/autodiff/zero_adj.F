
#include "CPP_OPTIONS.h"

C     !ROUTINE: ZERO_ADJ_3D
C     !INTERFACE:
      SUBROUTINE ZERO_ADJ_3D(
     I             bi, bj, fieldFwd,
     I             myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a 3D adjoint field
C     | ... nothing happens in forward mode
C     | author: Gael Forget
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j,k
      _RL fieldFwd(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)

      end


C     !ROUTINE: ZERO_ADJ_2D
C     !INTERFACE:
      SUBROUTINE ZERO_ADJ_2D(
     I             bi, bj, fieldFwd,
     I             myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a 2D adjoint field
C     | ... nothing happens in forward mode
C     | author: Gael Forget
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j
      _RL fieldFwd(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

      end


