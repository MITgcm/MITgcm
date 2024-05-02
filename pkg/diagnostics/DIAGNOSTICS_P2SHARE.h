CBOP
C     !ROUTINE: DIAGNOSTICS_P2SHARE.h
C     !INTERFACE:
C     #include "DIAGNOSTICS_P2SHARE.h"

C     !DESCRIPTION:
C     *================================================================*
C     | DIAGNOSTICS_P2SHARE.h
C     | o Header file for pkg "diagnostics" parameters that are used
C     |   outside pkg/diagnostics (and need to be shared).
C     *================================================================*
CEOP

C  - DIAG_PARAMS_TO_SHARE common block:
C    useDiag4AdjOutp :: use diagnostics pkg for Adjoint variables
      LOGICAL useDiag4AdjOutp
      COMMON / DIAG_PARAMS_TO_SHARE_L / useDiag4AdjOutp

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
