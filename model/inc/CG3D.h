C $Header: /u/gcmpack/MITgcm/model/inc/CG3D.h,v 1.10 2010/03/16 00:04:00 jmc Exp $
C $Name:  $

#ifdef ALLOW_NONHYDROSTATIC
CBOP
C     !ROUTINE: CG3D.h
C     !INTERFACE:
C     include "CG3D.h"
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | CG3D.h
C     | o Three-dimensional conjugate gradient solver header.
C     *==========================================================*
C     | The common blocks set up here are used in the elliptic
C     | equation inversion. They are also used as the interface
C     | to the rest of the model. To set the source term for the
C     | solver set the appropriate array below. To read the
C     | solution read from the appropriate array below.
C     *==========================================================*
C     \ev
CEOP

C--   COMMON /CG3D_R/ DEL**2 Laplacian operators
C     aW3d :: East-west operator.
C     aS3d :: North-south operator.
C     aV3d :: Vertical operator.
C     aC3d :: 3D operator main diagonal term.
C     zMC, zML, zMU :: preconditioner 3D solver
C     cg3dNorm - A matrix normalisation factor.
      COMMON /CG3D_R/
     &      aW3d, aS3d, aV3d,
     &      aC3d,
     &      zMC, zML, zMU,
     &      cg3dNorm
      _RS  aW3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  aS3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  aV3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  aC3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zMC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zML  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zMU  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  cg3dNorm

C--   COMMON /CG3D_WK_R/  Work array common block
C     cg3d_q - Intermediate matrix-vector product term
C     cg3d_r -   idem
C     cg3d_s -   idem
      COMMON /CG3D_WK_R/
     & cg3d_q, cg3d_r, cg3d_s
      _RL  cg3d_q(1-1  :sNx+1  ,1-1  :sNy+1  ,Nr,nSx,nSy)
      _RL  cg3d_r(1-1  :sNx+1  ,1-1  :sNy+1  ,Nr,nSx,nSy)
      _RL  cg3d_s(1-1  :sNx+1  ,1-1  :sNy+1  ,Nr,nSx,nSy)

#endif /* ALLOW_NONHYDROSTATIC */
