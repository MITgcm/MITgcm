C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/DIAGNOSTICS_CALC.h,v 1.4 2014/11/18 23:53:04 jmc Exp $
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
C     prtFirstCall     :: print messages if first call
C-- used in Velocity-Potential calculation:
C     diagCG_maxIters  :: max number of iterations in diag_cg2d solver
C     diagCG_prtResFrq :: frequency for printing residual in CG iterations
C     diagCG_resTarget :: residual target for diag_cg2d solver (no units)
C     diagCG_pcOffDFac :: diag_cg2d preconditioner off-diagonal factor
C-- used in Stream-Function calculation:
C     iPsi0,jPsi0 :: indices of grid-point location where Psi == 0
C     xPsi0,yPsi0 :: grid-point coordinate where Psi == 0

      LOGICAL prtFirstCall
      INTEGER diagCG_maxIters
      INTEGER diagCG_prtResFrq
      INTEGER iPsi0(nSx,nSy)
      INTEGER jPsi0(nSx,nSy)
      _RL     diagCG_resTarget
      _RL     diagCG_pcOffDFac
      _RS     xPsi0, yPsi0
      COMMON / DIAGNOSTICS_CALC_L /
     &     prtFirstCall
      COMMON / DIAGNOSTICS_CALC_I /
     &     diagCG_maxIters, diagCG_prtResFrq,
     &     iPsi0, jPsi0
      COMMON / DIAGNOSTICS_CALC_RL /
     &     diagCG_resTarget, diagCG_pcOffDFac
      COMMON / DIAGNOSTICS_CALC_RS /
     &     xPsi0, yPsi0

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
