C $Header: /u/gcmpack/MITgcm/model/inc/Attic/CG2D_INTERNAL.h,v 1.6 2001/10/03 16:12:54 cnh Exp $
C $Name:  $
CBOP
C     !ROUTINE: CG2D_INTERNAL.h
C     !INTERFACE:
C     include "CG2D_INTERNAL.h"
C     !DESCRIPTION: 
C     \bv
C     *==========================================================*
C     | CG2D_INTERNAL.h :: Header file for two-dimensional 
C     |                 :: conjugate gradient solver common 
C     |                 :: blocks.
C     *==========================================================*
C     | Internal (private) data structures.                       
C     *==========================================================*
C     \ev
CEOP

C--   COMMON /CG2D_R/ DEL**2 Laplacian operators
C     aW2d :: Two-d con. grad solver east-west operator.
C     aS2d :: Two-d con. grad solver north-south operator.
C     pW   :: Two-d con. grad solver east-west off-diagonal term of preconditioner.
C     pS   :: Two-d con. grad solver north-south off-diagonal term of preconditioner.
C     pC   :: Two-d con. grad solver main diagonal term of preconditioner.
C     cg2dNorm :: Two-d con. grad solver A matrix normalisation factor.
      COMMON /CG2D_I_R/
     &      aW2d,
     &      aS2d,
     &      pW, pS, pC,
     &      cg2dNorm
      _RS  aW2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aS2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pW   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pC   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2dNorm

C--   COMMON /CG2D_I_WK_R/  Work array common block
C     cg2d_q, :: Two-d con. grad solver intermediate matrix-vector product term
C     cg2d_r, :: 
C     cg2d_s  ::
      COMMON /CG2D_I_WK_R/
     & cg2d_q, cg2d_r, cg2d_s
      _RL  cg2d_q(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_r(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL  cg2d_s(1-1:sNx+1,1-1:sNy+1,nSx,nSy)

