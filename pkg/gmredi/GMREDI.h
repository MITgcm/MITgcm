C $Header: /u/gcmpack/MITgcm/pkg/gmredi/GMREDI.h,v 1.2 2000/11/13 16:35:28 heimbach Exp $

#ifdef ALLOW_GMREDI

C     Package flag
      logical gmrediIsOn

C     GM/Redi parameters
      _RL GM_background_K
      _RL GM_maxSlope
      _RL GM_Visbeck_alpha
      _RL GM_Visbeck_length
      _RL GM_Visbeck_depth
      _RL GM_Visbeck_maxval_K
      CHARACTER*(MAX_LEN_FNAM) GM_taper_scheme
      _RL GM_Scrit
      _RL GM_Sd
      COMMON /GM_PARAMS/ GM_background_K,
     &                   GM_maxSlope,
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
      COMMON /GM_PARAMS2/ GM_rMaxSlope


C     GM/Redi tensor elements

C     Bottom row of tensor corresponds to W points
C     Kwx is K_31 element, X direction at W point
C     Kwy is K_32 element, Y direction at W point
C     Kwz is K_33 element, Z direction at W point
      _RL Kwx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,MAX_NO_THREADS)
      _RL Kwy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,MAX_NO_THREADS)
      _RL Kwz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,MAX_NO_THREADS)
      COMMON /GM_Wtensor/ Kwx,Kwy,Kwz

#ifdef GM_NON_UNITY_DIAGONAL
C     First/second rows of tensor corresponds to U/V points
C     Kux is K_11 element, X direction at U point
C     Kvy is K_22 element, Y direction at V point
      _RL Kux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,MAX_NO_THREADS)
      _RL Kvy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,MAX_NO_THREADS)
      COMMON /GM_UVtensor/ Kux,Kvy
#else
      _RL Kux,Kvy
      PARAMETER(Kux=1.,Kvy=1.)
#endif

#ifdef GM_VISBECK_VARIABLE_K
C     GM mixing/stirring coefficient (spatially variable in horizontal
C     for Visbeck et al. parameterization)
      _RL VisbeckK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,MAX_NO_THREADS)
      COMMON /GM_Visbeck/ VisbeckK
#endif

#endif /* ALLOW_GMREDI */
