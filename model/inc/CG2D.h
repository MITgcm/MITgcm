C $Header: /u/gcmpack/MITgcm/model/inc/CG2D.h,v 1.9 2001/09/21 03:54:36 cnh Exp $
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
C     cg2dNormaliseRHS - flag set to TRUE if normalise RHS in the Solver
      COMMON /CG2D_I_L/ cg2dNormaliseRHS
      LOGICAL cg2dNormaliseRHS

C--   COMMON /CG2D_R/ DEL**2 Laplacian operators
C     aW2d - East-west operator.
C     aS2d - North-south operator.
C     pW   - East-west off-diagonal term of preconditioner.
C     pS   - North-south off-diagonal term of preconditioner.
C     pC   - Main diagonal term of preconditioner.
C     etaNBuf - Shared buffers for accumulating con. grad vector product.
C     alphaBuf
C     errBuf
C     nrmBuf
C     cg2dNorm - A matrix normalisation factor.
C     cg2dTolerance - cg2d solver Tolerance (solver unit = m2/s2 or no unit,
C                                            depending on cg2dNormaliseRHS) 
      COMMON /CG2D_I_R/
     &      aW2d,
     &      aS2d,
     &      pW, pS, pC,
     &      cg2dNorm, cg2dTolerance
      _RS  aW2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aS2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pW   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pC   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2dNorm, cg2dTolerance

C--   COMMON /CG2D_I_WK_R/  Work array common block
C     cg2d_q - Intermediate matrix-vector product term
C     cg2d_r -   "
C     cg2d_s -   "
      COMMON /CG2D_I_WK_R/
     & cg2d_q, cg2d_r, cg2d_s
      _RL  cg2d_q(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_r(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL  cg2d_s(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
