C $Header: /u/gcmpack/MITgcm/model/inc/CG3D.h,v 1.3 2001/02/04 14:38:44 cnh Exp $
C $Name:  $

#ifdef ALLOW_NONHYDROSTATIC
C     /==========================================================\
C     | CG3D.h                                                   |
C     | o Three-dimensional conjugate gradient solver header.    |
C     |==========================================================|
C     | The common blocks set up here are used in the elliptic   |
C     | equation inversion. They are also used as the interface  |
C     | to the rest of the model. To set the source term for the |
C     | solver set the appropriate array below. To read the      |
C     | solution read from the appropriate array below.          |
C     \==========================================================/

C--   COMMON /CG3D_R/ DEL**2 Laplacian operators
C     aW3d - East-west operator.
C     aS3d - North-south operator.
C     aV3d - Vertical operator.
C     etaNBuf - Shared buffers for accumulating con. grad vector product.
C     alphaBuf
C     errBuf
C     nrmBuf
C     cg3dNorm - A matrix normalisation factor.
      COMMON /CG3D_R/
     &      aW3d,
     &      aS3d,
     &      aV3d,
     &      zMC, zML, zMU,
     &      cg3dNorm
      _RS  aW3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  aS3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  aV3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zMC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zML  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zMU  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  cg3dNorm

C--   COMMON /CG3D_WK_R/  Work array common block
C     cg3d_q - Intermediate matrix-vector product term
C     cg3d_r -   "
C     cg3d_s -   "
C     cg3d_x   Solution vector
C     cg3d_b   Right-hand side vector
      COMMON /CG3D_WK_R/
     & cg3d_b, cg3d_q, cg3d_r, cg3d_s, cg3d_x
      _RL  cg3d_q(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL  cg3d_r(1-1  :sNx+1  ,1-1  :sNy+1  ,nR,nSx,nSy)
      _RL  cg3d_s(1-1  :sNx+1  ,1-1  :sNy+1  ,nR,nSx,nSy)
      _RL  cg3d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL  cg3d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)

#endif /* ALLOW_NONHYDROSTATIC */
