C
C CPP flags controlling which code is included in the files that
C will be compiled.

C ********************************************************************
C ***                  Adjoint Support Package                     ***
C ********************************************************************

C o Include/exclude code in order to be able to automatically
C   differentiate the MITgcmUV by using the Tangent Linear and
C   Adjoint Model Compiler (TAMC).
C
#define ALLOW_AUTODIFF_TAMC
C
C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING
C
C       >>> Extract adjoint state
#undef ALLOW_AUTODIFF_MONITOR
C
C       >>> DO 2-level checkpointing instead of 3-level
#undef AUTODIFF_2_LEVEL_CHECKPOINT
C
C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT

C ********************************************************************
C ***                     Calender Package                         ***
C ********************************************************************
C
C CPP flags controlling which code is included in the files that
C will be compiled.

CPH >>>>>> THERE ARE NO MORE CAL OPTIONS TO BE SET <<<<<<

C ********************************************************************
C ***                Cost function Package                         ***
C ********************************************************************
C 
C       >>> Cost function contributions
#define ALLOW_COST
#define ALLOW_COST_TEST
#undef ALLOW_COST_TRACER
#undef ALLOW_COST_ATLANTIC_HEAT

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************
C 
#undef  ALLOW_NONDIMENSIONAL_CONTROL_IO
C       >>> Initial values.
#define ALLOW_THETA0_CONTROL
#define ALLOW_SALT0_CONTROL
#define ALLOW_UVEL0_CONTROL
#define ALLOW_VVEL0_CONTROL
#define ALLOW_ETAN0_CONTROL
#undef ALLOW_TR10_CONTROL
#undef ALLOW_TAUU0_CONTROL
#undef ALLOW_TAUV0_CONTROL
#undef ALLOW_SFLUX0_CONTROL
#undef ALLOW_HFLUX0_CONTROL
#undef ALLOW_SSS0_CONTROL
#undef ALLOW_SST0_CONTROL
#undef ALLOW_DIFFKR_CONTROL
#undef ALLOW_KAPGM_CONTROL

