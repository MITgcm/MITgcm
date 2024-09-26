CBOP
C     !ROUTINE: CG2D.h
C     !INTERFACE:
C     include "CG2D.h"
C
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | CG2D.h
C     | o Two-dimensional conjugate gradient solver header.
C     *==========================================================*
C     | Internal (private) data structures.
C     *==========================================================*
C     \ev
CEOP

C--   COMMON /CG2D_I_L/ cg2dNormaliseRHS
C     cg2dNormaliseRHS :: flag set to TRUE if normalise RHS in the Solver
      COMMON /CG2D_I_L/ cg2dNormaliseRHS
      LOGICAL cg2dNormaliseRHS

C--   COMMON /CG2D_I_R/ DEL**2 Laplacian operators
C     aW2d    :: East-west operator.
C     aS2d    :: North-south operator.
C     aC2d    :: 2D operator main diagonal term.
C     pW      :: East-west off-diagonal term of preconditioner.
C     pS      :: North-south off-diagonal term of preconditioner.
C     pC      :: Main diagonal term of preconditioner.
C     cg2dNorm :: A matrix normalisation factor.
C     cg2dTolerance_sq :: square of cg2d solver Tolerance (units depends
C                 on cg2dNormaliseRHS, solver-unit ^2 = (m2/s2)^2 or no unit)
      COMMON /CG2D_I_RS/
     &      aW2d, aS2d, aC2d,
     &      pW, pS, pC
      COMMON /CG2D_I_RL/
     &      cg2dNorm, cg2dTolerance_sq
      _RS  aW2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aS2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aC2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pW   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pC   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2dNorm, cg2dTolerance_sq

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
