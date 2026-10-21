!BOP
! !ROUTINE: CG2D.h
! !INTERFACE:
! include "CG2D.h"
!
! !DESCRIPTION:
! \bv
! *==========================================================*
! | CG2D.h
! | o Two-dimensional conjugate gradient solver header.
! *==========================================================*
! | Internal (private) data structures.
! *==========================================================*
! \ev
!EOP

!--   COMMON /CG2D_I_L/ cg2dNormaliseRHS
! cg2dNormaliseRHS :: flag set to TRUE if normalise RHS in the Solver
      COMMON /CG2D_I_L/ cg2dNormaliseRHS
      LOGICAL :: cg2dNormaliseRHS

!--   COMMON /CG2D_I_R/ DEL**2 Laplacian operators
! aW2d    :: East-west operator.
! aS2d    :: North-south operator.
! aC2d    :: 2D operator main diagonal term.
! pW      :: East-west off-diagonal term of preconditioner.
! pS      :: North-south off-diagonal term of preconditioner.
! pC      :: Main diagonal term of preconditioner.
! cg2dNorm :: A matrix normalisation factor.
! cg2dTolerance_sq :: square of cg2d solver Tolerance (units depends
!             on cg2dNormaliseRHS, solver-unit ^2 = (m2/s2)^2 or no unit)
      COMMON /CG2D_I_RS/                                                          &
     &      aW2d, aS2d, aC2d,                                                     &
     &      pW, pS, pC
      COMMON /CG2D_I_RL/                                                          &
     &      cg2dNorm, cg2dTolerance_sq
      _RS  aW2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aS2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aC2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pW   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pC   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2dNorm, cg2dTolerance_sq

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
