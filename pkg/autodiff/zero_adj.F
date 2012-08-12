C $Header: /u/gcmpack/MITgcm/pkg/autodiff/zero_adj.F,v 1.7 2012/08/12 18:46:20 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

C--  File zero_adj.F:
C--   Contents
C--   o ZERO_ADJ_1D
C--   o ZERO_ADJ_LOC
C--   o ZERO_ADJ

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ZERO_ADJ_1D
C     !INTERFACE:
      SUBROUTINE ZERO_ADJ_1D(
     I                NrIn, fieldFwd, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a one dimensional adjoint field
C     | ... nothing happens in forward mode
C     | author: Gael Forget
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER NrIn, myThid
      _RL fieldFwd(NrIn)
CEOP

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ZERO_ADJ_LOC
C     !INTERFACE:
      SUBROUTINE ZERO_ADJ_LOC(
     I                NrIn, fieldFwd, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a local (no nsx,nsy dims.) adjoint field
C     | ... nothing happens in forward mode
C     | author: Gael Forget
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER NrIn, myThid
      _RL fieldFwd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn)
CEOP

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ZERO_ADJ
C     !INTERFACE:
      SUBROUTINE ZERO_ADJ(
     I                NrIn, fieldFwd, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a full (incl. nsx,nsy dims.) adjoint field
C     | ... nothing happens in forward mode
C     | author: Gael Forget
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER NrIn, myThid
      _RL fieldFwd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn,nSx,nSy)
CEOP

      RETURN
      END
