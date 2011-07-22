C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/DIAGNOSTICS_CALC.h,v 1.2 2011/07/22 19:47:14 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: DIAGNOSTICS_CALC.h
C     !INTERFACE:
C     include "DIAGNOSTICS_CALC.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | DIAGNOSTICS_CALC.h
C     | o Hold parameters and variables used in post-processing
C     |   diagnostics
C     *==========================================================*
C     *==========================================================*
C     \ev
CEOP

C     DIAGNOSTICS_CALC common block:
C-- used in Velocity-Potential calculation:
C     diagCG_maxIters  :: max number of iterations in diag_cg2d solver
C     diagCG_prtResFrq :: frequency for printing residual in CG iterations
C     diagCG_resTarget :: residual target for diag_cg2d solver (no units)
C-- used in Stream-Function calculation:
C     iPsi0,jPsi0 :: indices of grid-point location where Psi == 0
C     xPsi0,yPsi0 :: grid-point coordinate where Psi == 0

      INTEGER diagCG_maxIters
      INTEGER diagCG_prtResFrq
      INTEGER iPsi0(nSx,nSy)
      INTEGER jPsi0(nSx,nSy)
      _RL     diagCG_resTarget
      _RS     xPsi0, yPsi0
      COMMON / DIAGNOSTICS_CALC_I /
     &     diagCG_maxIters, diagCG_prtResFrq,
     &     iPsi0, jPsi0
      COMMON / DIAGNOSTICS_CALC_RL /
     &     diagCG_resTarget
      COMMON / DIAGNOSTICS_CALC_RS /
     &     xPsi0, yPsi0

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
