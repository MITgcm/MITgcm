C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/Attic/SHAP_FILT_TRACER.h,v 1.2 2001/05/29 14:01:40 adcroft Exp $
C $Name:  $
C
C     /==========================================================\
C     | SHAP_FILT_TRACER.h                                       |
C     | o Globals used by Fortran shapiro filter routine.        |
C     \==========================================================/
      COMMON / SHFT_COMMON_R8 / tmpFld
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
