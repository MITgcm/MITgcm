C $Header: /u/gcmpack/MITgcm/model/inc/Attic/CG2D_EXTERNAL.h,v 1.1 1998/10/28 03:11:35 cnh Exp $
C
C     /==========================================================\
C     | CG2D_EXTERNAL.h                                          |
C     | o Two-dimensional conjugate gradient solver header.      |
C     |==========================================================|
C     | The common blocks set up here are the external interface |
C     | (public) arrays for the cg solver.                       |
C     \==========================================================/


C--   COMMON /CG2D_E_WK_R/  Work array common block
C     cg2d_x   Solution vector
C     cg2d_b   Right-hand side vector
      COMMON /CG2D_E_WK_R/ cg2d_b, cg2d_x
      _RL  cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

