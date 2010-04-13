C $Header: /u/gcmpack/MITgcm/pkg/autodiff/zero_adj.F,v 1.5 2010/04/13 04:35:53 gforget Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     !ROUTINE: ZERO_ADJ_LOC
C     !INTERFACE:
      SUBROUTINE ZERO_ADJ_LOC(
     I             NrIn,FieldFwd,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a local (no nsx,nsy dims.) adjoint field
C     | ... nothing happens in forward mode
C     | author: Gael Forget
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER NrIn, myThid
      _RL FieldFwd(1-Olx:sNx+Olx,1-Oly:sNy+Oly,NrIn)

      RETURN
      END


C     !ROUTINE: ZERO_ADJ
C     !INTERFACE:
      SUBROUTINE ZERO_ADJ(
     I             NrIn,FieldFwd,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a full (incl. nsx,nsy dims.) adjoint field
C     | ... nothing happens in forward mode
C     | author: Gael Forget
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER NrIn, myThid
      _RL FieldFwd(1-Olx:sNx+Olx,1-Oly:sNy+Oly,NrIn,nSx,nSy)

      RETURN
      END
