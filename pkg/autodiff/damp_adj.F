C $Header: /u/gcmpack/MITgcm/pkg/autodiff/damp_adj.F,v 1.1 2010/04/17 20:47:24 gforget Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     !ROUTINE: DAMP_ADJ
C     !INTERFACE:
      SUBROUTINE DAMP_ADJ(
     I    NrIn,minRef,maxRef,timeDamp,
     I    fieldRef,fieldDamp,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | author: Gael Forget
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER NrIn, myThid
      _RL fieldRef(1-Olx:sNx+Olx,1-Oly:sNy+Oly,NrIn,nSx,nSy)
      _RL fieldDamp(1-Olx:sNx+Olx,1-Oly:sNy+Oly,NrIn,nSx,nSy)
      _RL minRef,maxRef,timeDamp
      integer bi,bj,i,j,k

      RETURN
      END
