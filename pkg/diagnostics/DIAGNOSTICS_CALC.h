C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/DIAGNOSTICS_CALC.h,v 1.1 2011/07/06 01:43:49 jmc Exp $
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
C-- used in Stream-Function calculation:
C     iPsi0,jPsi0 :: indices of grid-point location where Psi == 0
C     xPsi0,yPsi0 :: grid-point coordinate where Psi == 0

      INTEGER iPsi0(nSx,nSy)
      INTEGER jPsi0(nSx,nSy)
      _RS     xPsi0, yPsi0
      COMMON / DIAGNOSTICS_CALC_I /
     &     iPsi0, jPsi0
      COMMON / DIAGNOSTICS_CALC_RS /
     &     xPsi0, yPsi0

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
