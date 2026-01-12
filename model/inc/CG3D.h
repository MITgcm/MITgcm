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

C--   COMMON /CG3D_L/ cg3dNormaliseRHS
C     cg3dNormaliseRHS :: flag set to TRUE if normalise RHS in the Solver
      COMMON /CG3D_L/ cg3dNormaliseRHS
      LOGICAL cg3dNormaliseRHS

C--   COMMON /CG3D_R/ DEL**2 Laplacian operators
C-    Matrix coeff units: before normalisation: wVel units ; no unit after.
C     aW3d     :: East-west operator.
C     aS3d     :: North-south operator.
C     aV3d     :: Vertical operator.
C     aC3d     :: 3D operator main diagonal term.
C     zMC, zML, zMU :: preconditioner 3D solver
C     cg3dNorm :: matrix normalisation factor, units: 1/wVel [s/rUnits]
C     cg3dTolerance_sq :: square of cg3d solver Tolerance (units depends on
C                 cg3dNormaliseRHS=F/T, solver-unit ^2 = (m^2)^2 or no unit)
      COMMON /CG3D_RS/
     &      aW3d, aS3d, aV3d, aC3d,
     &      zMC, zML, zMU
      COMMON /CG3D_RL/
     &      cg3dNorm, cg3dTolerance_sq
      _RS  aW3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  aS3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  aV3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  aC3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zMC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zML  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  zMU  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  cg3dNorm, cg3dTolerance_sq

C--   COMMON /CG3D_WK_R/  Work array common block
C     cg3d_q   :: Intermediate matrix-vector product term
C     cg3d_r   ::   idem
C     cg3d_s   ::   idem
      COMMON /CG3D_WK_R/
     & cg3d_q, cg3d_r, cg3d_s
      _RL  cg3d_q( 0:sNx+1, 0:sNy+1, Nr,nSx,nSy)
      _RL  cg3d_r( 0:sNx+1, 0:sNy+1, Nr,nSx,nSy)
      _RL  cg3d_s( 0:sNx+1, 0:sNy+1, Nr,nSx,nSy)

#endif /* ALLOW_NONHYDROSTATIC */
