C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/SHAP_FILT.h,v 1.4 2001/12/11 14:31:59 jmc Exp $
C $Name:  $

#ifdef ALLOW_SHAP_FILT

C-    Package flag and logical parameters :
C     shap_filt_uvStar  :: filter applied to u*,v* (before SOLVE_FOR_P) 
C     shap_filt_TrStagg :: if using a Stager time-step, filter T,S before
C                           computing PhiHyd ;
C                           has no effect if syncr. time step is used

      LOGICAL shap_filt_uvStar, shap_filt_TrStagg
      COMMON /SHAP_FILT_PARM_L/ 
     &        shap_filt_uvStar, shap_filt_TrStagg

C-    Shapiro Filter integer parameters :  
C     Shap_funct :: define which Shapiro Filter function is used
C        = 1  (S1) : [1 - d_xx^n - d_yy^n]
C        = 4  (S4) : [1 - d_xx^n][1- d_yy^n]
C        = 2  (S2) : [1 - (d_xx+d_yy)^n]
C     nShap_Tr,UV :: (total) power of the filter for Tracer, Velocity
C  available only with Shap_funct=2 : 
C      combine filter in Physical space (power of nShap_Phys) 
C        and pure numerical filter (power nShap - nShap_Phys) 

      INTEGER  Shap_funct, nShapT, nShapUV, nShapTrPhys, nShapUVPhys
      COMMON /SHAP_FILT_PARM_I/ 
     &         Shap_funct, nShapT, nShapTrPhys, nShapUV, nShapUVPhys

C-    Shapiro Filter (real) parameters
      _RL Shap_Trtau, Shap_TrLength
      _RL Shap_uvtau, Shap_uvLength
      COMMON /SHAP_FILT_PARAMS/ 
     &                  Shap_Trtau, Shap_TrLength,
     &                  Shap_uvtau, Shap_uvLength

#endif /* ALLOW_SHAP_FILT */
