C $Header: /u/gcmpack/MITgcm/model/inc/SOLVE_FOR_PRESSURE3D.h,v 1.5 2005/11/08 01:54:53 jmc Exp $
C $Name:  $

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

C--   COMMON /SFP3D_COMMON_R/ variables (RL) used by 3-D pressure solver
C     cg3d_x  :: Solution vector of the 3-D solver equation A.x=B 
C     cg3d_b  :: Right Hand side vector of the 3-D solver equation A.x=B

      COMMON /SFP3D_COMMON_R/ 
     &                        cg3d_b
c    &                        cg3d_x, cg3d_b
c     _RL  cg3d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL  cg3d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)

#endif /* ALLOW_NONHYDROSTATIC */
