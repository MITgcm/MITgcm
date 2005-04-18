C $Header: /u/gcmpack/MITgcm/pkg/matrix/MATRIX.h,v 1.1 2005/04/18 19:38:23 spk Exp $
C $Name:  $

#ifdef ALLOW_MATRIX

      INTEGER expMatrixCounter, impMatrixCounter, expMatrixWriteCount, 
     &   impMatrixWriteCount
      _RL expMatrixWriteTime, impMatrixWriteTime
      COMMON /MATRIX_PARAMS/ expMatrixCounter, impMatrixCounter, 
     &   expMatrixWriteCount, impMatrixWriteCount,
     &   expMatrixWriteTime, impMatrixWriteTime
      
      NAMELIST /MATRIX_PARM01/ expMatrixWriteTime, impMatrixWriteTime

      _RL MATRIX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &	         PTRACERS_num,2)
      _RL PTRACERS_initial(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &                     PTRACERS_num)
      COMMON /MATRIX_FIELDS/ MATRIX, PTRACERS_initial
      
#endif /* ALLOW_MATRIX */
