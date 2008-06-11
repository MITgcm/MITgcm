C $Header: /u/gcmpack/MITgcm/pkg/autodiff/zero_adj.F,v 1.3 2008/06/11 18:29:11 gforget Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     !ROUTINE: ZERO_ADJ_LOC
C     !INTERFACE:
      SUBROUTINE ZERO_ADJ_LOC(
     I             NrIn,myThid,FieldFwd)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a local (no nsx,nsy dims.) adjoint field
C     | ... nothing happens in forward mode
C     | author: Gael Forget
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j,k
      _RL FieldFwd(1-Olx:sNx+Olx,1-Oly:sNy+Oly,NrIn)

      end


C     !ROUTINE: ZERO_ADJ
C     !INTERFACE:
      SUBROUTINE ZERO_ADJ(
     I             NrIn,myThid,FieldFwd)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a full (incl. nsx,nsy dims.) adjoint field
C     | ... nothing happens in forward mode
C     | author: Gael Forget
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j,k
      _RL FieldFwd(1-Olx:sNx+Olx,1-Oly:sNy+Oly,NrIn,nSx,nSy)

      end

