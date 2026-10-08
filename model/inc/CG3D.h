#ifdef ALLOW_NONHYDROSTATIC
!BOP
! !ROUTINE: CG3D.h
! !INTERFACE:
! include "CG3D.h"
! !DESCRIPTION: \bv
! *==========================================================*
! | CG3D.h
! | o Three-dimensional conjugate gradient solver header.
! *==========================================================*
! | The common blocks set up here are used in the elliptic
! | equation inversion. They are also used as the interface
! | to the rest of the model. To set the source term for the
! | solver set the appropriate array below. To read the
! | solution read from the appropriate array below.
! *==========================================================*
! \ev
!EOP

!--   COMMON /CG3D_L/ cg3dNormaliseRHS
! cg3dNormaliseRHS :: flag set to TRUE if normalise RHS in the Solver
      COMMON /CG3D_L/ cg3dNormaliseRHS
      LOGICAL :: cg3dNormaliseRHS

!--   COMMON /CG3D_R/ DEL**2 Laplacian operators
! aW3d :: East-west operator.
! aS3d :: North-south operator.
! aV3d :: Vertical operator.
! aC3d :: 3D operator main diagonal term.
! zMC, zML, zMU :: preconditioner 3D solver
! cg3dNorm :: A matrix normalisation factor.
! cg3dTolerance_sq :: square of cg3d solver Tolerance (units depends
!             on cg3dNormaliseRHS, solver-unit ^2 = (m2/s2)^2 or no unit)
      COMMON /CG3D_RS/                                                            &
     &      aW3d, aS3d, aV3d, aC3d,                                               &
     &      zMC, zML, zMU
      COMMON /CG3D_RL/                                                            &
     &      cg3dNorm, cg3dTolerance_sq
      _RS  aW3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  aS3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  aV3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  aC3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zMC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zML  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zMU  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  cg3dNorm, cg3dTolerance_sq

!--   COMMON /CG3D_WK_R/  Work array common block
! cg3d_q :: Intermediate matrix-vector product term
! cg3d_r ::   idem
! cg3d_s ::   idem
      COMMON /CG3D_WK_R/                                                          &
     &      cg3d_q, cg3d_r, cg3d_s
      _RL  cg3d_q( 0:sNx+1, 0:sNy+1, Nr,nSx,nSy)
      _RL  cg3d_r( 0:sNx+1, 0:sNy+1, Nr,nSx,nSy)
      _RL  cg3d_s( 0:sNx+1, 0:sNy+1, Nr,nSx,nSy)

#endif /* ALLOW_NONHYDROSTATIC */
