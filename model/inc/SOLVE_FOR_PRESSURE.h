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
