C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/SHAP_FILT.h,v 1.2 2001/05/29 14:01:39 adcroft Exp $
C $Name:  $

#ifdef ALLOW_SHAP_FILT

C     Package flag 
C     Shap_funct : define which Shapiro Filter function is used
C        = 1  (S1) : [1 - d_xx^n - d_yy^n]
C        = 4  (S4) : [1 - d_xx^n][1- d_yy^n]
C        = 2  (S2) : [1 - (d_xx+d_yy)^n]
      INTEGER nShapT, nShapUV, Shap_funct
      COMMON /SHAP_FILT_INT/ nShapT, nShapUV, Shap_funct

C     Shapiro Filter parameters
      _RL Shap_Trtau, Shap_TrDamp, Shap_TrLength
      _RL Shap_uvtau, Shap_uvDamp, Shap_uvLength
      COMMON /SHAP_FILT_PARAMS/ 
     &                  Shap_Trtau, Shap_TrDamp, Shap_TrLength,
     &                  Shap_uvtau, Shap_uvDamp, Shap_uvLength

#endif /* ALLOW_SHAP_FILT */
