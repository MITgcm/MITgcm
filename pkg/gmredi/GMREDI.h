C $Header: /u/gcmpack/MITgcm/pkg/gmredi/GMREDI.h,v 1.6 2001/12/16 18:54:49 jmc Exp $
C $Name:  $

#ifdef ALLOW_GMREDI

C     Package flag
      logical GMREDIisON
      logical GM_AdvForm
      logical GM_ExtraDiag 
      COMMON /GM_PACKAGE/ GMREDIisON, GM_AdvForm, GM_ExtraDiag

C     GM/Redi parameters
      _RL GM_isopycK
      _RL GM_background_K
      _RL GM_maxSlope
      _RL GM_Kmin_horiz
      _RL GM_Visbeck_alpha
      _RL GM_Visbeck_length
      _RL GM_Visbeck_depth
      _RL GM_Visbeck_maxval_K
      CHARACTER*(MAX_LEN_FNAM) GM_taper_scheme
      _RL GM_Scrit
      _RL GM_Sd
      COMMON /GM_PARAMS/ GM_isopycK, GM_background_K,
     &                   GM_maxSlope,
     &                   GM_Kmin_horiz,
     &                   GM_Visbeck_alpha,
     &                   GM_Visbeck_length,
     &                   GM_Visbeck_depth,
     &                   GM_Visbeck_maxval_K,
     &                   GM_taper_scheme,
     &                   GM_Scrit,
     &                   GM_Sd


C     More GM/Redi parameters diagnosed from previous block
C     (not directly user configured)
      _RL GM_rMaxSlope
      _RL GM_skewflx
      _RL GM_advect
      COMMON /GM_PARAMS2/ GM_rMaxSlope, 
     &                    GM_skewflx, GM_advect

C     GM/Redi tensor elements

C     Bottom row of tensor corresponds to W points
C     Kwx is K_31 element, X direction at W point
C     Kwy is K_32 element, Y direction at W point
C     Kwz is K_33 element, Z direction at W point
      _RL Kwx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kwy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kwz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_Wtensor/ Kwx,Kwy,Kwz

#ifdef GM_NON_UNITY_DIAGONAL
C     Horizontal part of the tensor 
C     Kux is K_11 element, X direction at U point
C     Kvy is K_22 element, Y direction at V point
      _RL Kux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kvy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_HorTensor/ Kux,Kvy
#else
      _RL Kux,Kvy
      PARAMETER(Kux=1.,Kvy=1.)
#endif

#ifdef GM_EXTRA_DIAGONAL
C     First/second rows of tensor corresponds to U/V points
C     Kuz is K_13 element, Z direction at U point
C     Kvz is K_23 element, Z direction at V point
      _RL Kuz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kvz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_UVtensor/ Kuz,Kvz
#else
      _RL Kuz,Kvz
      PARAMETER(Kuz=1.,Kvz=1.)
#endif

#ifdef GM_BOLUS_ADVEC
C     GM advection formulation: bolus velocities are derived from 2 
C        streamfunctions PsiX and PsiY : 
      _RL GM_PsiX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GM_PsiY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_BOLUS/ GM_PsiX,GM_PsiY
#endif

#ifdef GM_VISBECK_VARIABLE_K
C     GM mixing/stirring coefficient (spatially variable in horizontal
C     for Visbeck et al. parameterization)
      _RL VisbeckK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /GM_Visbeck/ VisbeckK
#endif

#endif /* ALLOW_GMREDI */
