C $Header: /u/gcmpack/MITgcm/model/inc/SOLVE_FOR_PRESSURE.h,v 1.4 2011/02/22 18:32:28 jmc Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: SOLVE_FOR_PRESSURE.h
C    !INTERFACE:
C    include SOLVE_FOR_PRESSURE.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | SOLVE_FOR_PRESSURE.h                                      
C     | o Globals used by Fortran pressure solver routine         
C     *==========================================================*
C     \ev
CEOP
c     COMMON / SFP_COMMON_R8 / cg2d_x, cg2d_b
      _RL cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
