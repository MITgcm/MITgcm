C $Header: /u/gcmpack/MITgcm/model/inc/SOLVE_FOR_PRESSURE.h,v 1.2 2001/05/29 14:01:36 adcroft Exp $
C $Name:  $
C
C     /==========================================================\
C     | SOLVE_FOR_PRESSURE.h                                     |
C     | o Globals used by Fortran pressure solver routine        |
C     \==========================================================/
      COMMON / SFP_COMMON_R8 / cg2d_x, cg2d_b
      _RL cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
