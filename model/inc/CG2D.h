C $Header: /u/gcmpack/MITgcm/model/inc/CG2D.h,v 1.4 1998/09/29 18:50:56 cnh Exp $
C
C     /==========================================================\
C     | CG2D.h                                                   |
C     | o Two-dimensional conjugate gradient solver header.      |
C     |==========================================================|
C     | The common blocks set up here are used in the elliptic   |
C     | equation inversion. They are also used as the interface  |
C     | to the rest of the model. To set the source term for the |
C     | solver set the appropriate array below. To read the      |
C     | solution read from the appropriate array below.          |
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
      COMMON /CG2D_R/
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

C--   COMMON /CG2D_WK_R/  Work array common block
C     cg2d_q - Intermediate matrix-vector product term
C     cg2d_r -   "
C     cg2d_s -   "
C     cg2d_x   Solution vector
C     cg2d_b   Right-hand side vector
      COMMON /CG2D_WK_R/
     & cg2d_b, cg2d_q, cg2d_r, cg2d_s, cg2d_x
      _RL  cg2d_q(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_r(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL  cg2d_s(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL  cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

