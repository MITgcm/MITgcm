C $Header: /u/gcmpack/MITgcm/model/inc/Attic/CG2D_INTERNAL.h,v 1.2 1999/03/12 18:53:24 adcroft Exp $
C
C     /==========================================================\
C     | CG2D.h                                                   |
C     | o Two-dimensional conjugate gradient solver header.      |
C     |==========================================================|
C     | Internal (private) data structures.                      |
C     \==========================================================/

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
      COMMON /CG2D_I_R/
     &      aW2d,
     &      aS2d,
     &      pW, pS, pC,
     &      errBuf, nrmBuf, etaNbuf, etaNM1Buf, alphaBuf, sumRhsBuf,
     &      cg2dNorm, rhsMaxBuf, cg2dNBuf
      _RS  aW2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aS2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pW   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pC   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  etaNBuf(lShare8,MAX_NO_THREADS)
      _RL  etaNM1Buf(lShare8,MAX_NO_THREADS)
      _RL  alphaBuf(lShare8,MAX_NO_THREADS)
      _RL  errBuf(lShare8,MAX_NO_THREADS)
      _RL  nrmBuf(lShare8,MAX_NO_THREADS)
      _RL  sumRHSBuf(lShare8,MAX_NO_THREADS)
      _RS  cg2dNBuf(lShare8,MAX_NO_THREADS)
      _RL  rhsMaxBuf(lShare8,MAX_NO_THREADS)
      _RL  cg2dNorm

C--   COMMON /CG2D_I_WK_R/  Work array common block
C     cg2d_q - Intermediate matrix-vector product term
C     cg2d_r -   "
C     cg2d_s -   "
      COMMON /CG2D_I_WK_R/
     & cg2d_q, cg2d_r, cg2d_s
      _RL  cg2d_q(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_r(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL  cg2d_s(1-1:sNx+1,1-1:sNy+1,nSx,nSy)

