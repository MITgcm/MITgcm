C $Header: /u/gcmpack/MITgcm/verification/halfpipe_streamice/code_ad/ECCO_CPPOPTIONS.h,v 1.2 2013/07/24 20:53:07 jmc Exp $
C $Name:  $

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
C ***                Cost function Package                         ***
C ********************************************************************
C
C       >>> Cost function contributions
#define ALLOW_COST_TEST

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************
C
#undef EXCLUDE_CTRL_PACK
#undef  ALLOW_NONDIMENSIONAL_CONTROL_IO
C
C       >>> Initial values.
#define ALLOW_GENARR2D_CONTROL

