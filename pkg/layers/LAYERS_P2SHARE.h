CBOP
C    !ROUTINE: LAYERS_P2SHARE.h
C    !INTERFACE:
C    include LAYERS_P2SHARE.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | LAYERS_P2SHARE.h
C     | o Header file for pkg "layers" parameters that are used
C     |   outside pkg/layers (and need to be shared).
C     *==========================================================*
C     \ev
CEOP
C     layers_useThermo :: true if using LAYERS_THERMODYNAMICS code
      LOGICAL layers_useThermo
      COMMON / LAYERS_PARAMS_TO_SHARE_L / layers_useThermo

