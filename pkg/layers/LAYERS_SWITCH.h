CBOP
C    !ROUTINE: LAYERS_SWITCH.h
C    !INTERFACE:
C    include LAYERS_SWITCH.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | LAYERS_SWITCH.h
C     | o variables for switching on/off some calculations
C     |   Note: intended to be included also outside pkg/layers
C     *==========================================================*
C     \ev
CEOP
C     layers_useThermo :: true if using LAYERS_THERMODYNAMICS code
      LOGICAL layers_useThermo
      COMMON / LAYERS_SWITCH_L / layers_useThermo

