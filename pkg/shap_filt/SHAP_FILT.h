C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/SHAP_FILT.h,v 1.3 2001/06/15 15:14:56 jmc Exp $
C $Name:  $

#ifdef ALLOW_SHAP_FILT

C-    Package flag 
C     Shap_funct : define which Shapiro Filter function is used
C        = 1  (S1) : [1 - d_xx^n - d_yy^n]
C        = 4  (S4) : [1 - d_xx^n][1- d_yy^n]
C        = 2  (S2) : [1 - (d_xx+d_yy)^n]
C     nShap_Tr,UV : (total) power of the filter for Tracer, Velocity
C  available only with Shap_funct=2 : 
C     combine filter in Physical space (power of nShap__Phys) 
C        and pure numerical filter (power nShap - nShap_Phys) 

      INTEGER  Shap_funct, nShapT, nShapUV, nShapTrPhys, nShapUVPhys
      COMMON /SHAP_FILT_INT/ Shap_funct, nShapT , nShapTrPhys, 
     &                                   nShapUV, nShapUVPhys

C-    Shapiro Filter parameters
      _RL Shap_Trtau, Shap_TrLength
      _RL Shap_uvtau, Shap_uvLength
      COMMON /SHAP_FILT_PARAMS/ 
     &                  Shap_Trtau, Shap_TrLength,
     &                  Shap_uvtau, Shap_uvLength

#endif /* ALLOW_SHAP_FILT */
