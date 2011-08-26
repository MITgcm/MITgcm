C $Header: /u/gcmpack/MITgcm/pkg/matrix/MATRIX.h,v 1.4 2011/08/26 19:46:59 jmc Exp $
C $Name:  $

#ifdef ALLOW_MATRIX

      INTEGER expMatrixCounter(nSx,nSy), impMatrixCounter(nSx,nSy)
      INTEGER expMatrixWriteCount, impMatrixWriteCount
      _RL expMatrixWriteTime, impMatrixWriteTime
      COMMON /MATRIX_PARAMS_I/
     &   expMatrixCounter, impMatrixCounter,
     &   expMatrixWriteCount, impMatrixWriteCount
      COMMON /MATRIX_PARAMS_R/
     &   expMatrixWriteTime, impMatrixWriteTime

      _RL MATRIX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &           PTRACERS_num,2)
      _RL PTRACERS_initial(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &                     PTRACERS_num)
      COMMON /MATRIX_FIELDS/ MATRIX, PTRACERS_initial

#endif /* ALLOW_MATRIX */
