C $Header: /u/gcmpack/MITgcm/model/inc/SOLVE_FOR_PRESSURE3D.h,v 1.2 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: SOLVE_FOR_PRESSURE3D.h
C    !INTERFACE:
C    include SOLVE_FOR_PRESSURE3D.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | SOLVE_FOR_PRESSURE3D.h                                    
C     | o Globals used by Fortran 3d pressure solver routine      
C     *==========================================================*
C     \ev
CEOP

#ifdef ALLOW_NONHYDROSTATIC
      COMMON / SFP3D_COMMON_R8 / cg3d_x, cg3d_b
      _RL  cg3d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL  cg3d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
#endif /* ALLOW_NONHYDROSTATIC */
