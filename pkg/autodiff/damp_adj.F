C $Header: /u/gcmpack/MITgcm/pkg/autodiff/damp_adj.F,v 1.2 2012/08/12 18:49:09 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

CBOP
C     !ROUTINE: DAMP_ADJ
C     !INTERFACE:
      SUBROUTINE DAMP_ADJ(
     I       NrIn, minRef, maxRef, timeDamp,
     I       fieldRef, fieldDamp, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | author: Gael Forget
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER NrIn, myThid
      _RL minRef,maxRef,timeDamp
      _RL fieldRef(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn,nSx,nSy)
      _RL fieldDamp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn,nSx,nSy)
CEOP

      RETURN
      END
