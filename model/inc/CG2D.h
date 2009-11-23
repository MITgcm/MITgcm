C $Header: /u/gcmpack/MITgcm/model/inc/CG2D.h,v 1.16 2009/11/23 16:13:57 mlosch Exp $
C $Name:  $

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

C--   COMMON /CG2D_R/ DEL**2 Laplacian operators
C     aW2d    :: East-west operator.
C     aS2d    :: North-south operator.
C     aC2d    :: 2D operator main diagonal term.
C     pW      :: East-west off-diagonal term of preconditioner.
C     pS      :: North-south off-diagonal term of preconditioner.
C     pC      :: Main diagonal term of preconditioner.
C     cg2dNorm :: A matrix normalisation factor.
C     cg2dTolerance :: cg2d solver Tolerance (solver unit = m2/s2 or no unit,
C                                             depending on cg2dNormaliseRHS)
      COMMON /CG2D_I_R/
     &      aW2d, aS2d, aC2d,
     &      pW, pS, pC,
     &      cg2dNorm, cg2dTolerance
      _RS  aW2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aS2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aC2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pW   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pC   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2dNorm, cg2dTolerance

C--   COMMON /CG2D_I_WK_R/  Work array common block
C     cg2d_q :: Intermediate matrix-vector product term
C     cg2d_r ::   *same*
C     cg2d_s ::   *same*
      COMMON /CG2D_I_WK_R/
     & cg2d_q, cg2d_r, cg2d_s
      _RL  cg2d_q(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
#ifdef ALLOW_CG2D_NSA
      _RL  cg2d_r(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_s(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#else  /* ALLOW_CG2D_NSA */
      _RL  cg2d_r(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL  cg2d_s(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
#endif /* ALLOW_CG2D_NSA */

#ifdef ALLOW_CG2D_NSA
C--   COMMON /CG2D_I_WK_R2/  extra work array common block
C     cg2d_z :: Intermediate matrix-vector product term
C            :: reduces the number of recomputation in adjoint mode
C            :: this field is superfluous if your cg2d is self-adjoint.
      COMMON /CG2D_I_WK_R2/ cg2d_z
      _RL  cg2d_z(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
#endif /* ALLOW_CG2D_NSA */

#ifdef ALLOW_SRCG
C--   COMMON /CG2D_I_WK_R3/  Work array common block
C     cg2d_y :: residual scaled by preconditioner
C     cg2d_v :: z times the operator
C     cg2d_q :: Intermediate matrix-vector product term
C     cg2d_r ::   *same*
C     cg2d_s ::   *same*
C     sumPhi :: needed to call global_vec_sum_r8 when mutli-threaded 
      COMMON /CG2D_I_WK_R3/
     & cg2d_y,cg2d_v,sumPhi
      _RL  cg2d_y(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL  cg2d_v(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL  sumPhi(3,nSx,nSy)
#endif /* ALLOW_SRCG */
