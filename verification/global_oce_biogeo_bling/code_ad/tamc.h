CBOP
C     !ROUTINE: tamc.h
C     !INTERFACE:
C     #include "tamc.h"

C     !DESCRIPTION:
C     *================================================================*
C     | tamc.h
C     | o Header file defining parameters and variables for the use of
C     |   the Tangent Linear and Adjoint Model Compiler (TAMC)
C     |   or the Transformations in Fortran tool (TAF).
C     |
C     | started: Christian Eckert eckert@mit.edu  04-Feb-1999
C     | changed: Patrick Heimbach heimbach@mit.edu 06-Jun-2000
C     | cleanup: Martin Losch Martin.Losch@awi.de Nov-2022
C     *================================================================*
CEOP
#ifdef ALLOW_AUTODIFF_TAMC

C     TAMC checkpointing parameters:
C     ==============================
C
C     The checkpointing parameters have to be consistent with other model
C     parameters and variables. This has to be checked before the model is
C     run.
C

#ifdef ALLOW_TAMC_CHECKPOINTING

C     nchklev_1 :: length of inner loop (=size of storage in memory)
C     nchklev_2 :: length of second loop (stored on disk)
C     nchklev_3 :: length of outer loop of 3-level checkpointing
      INTEGER    nchklev_1
      PARAMETER( nchklev_1 =   1 )
      INTEGER    nchklev_2
      PARAMETER( nchklev_2 =   2 )
      INTEGER    nchklev_3
      PARAMETER( nchklev_3 =   2 )
#ifdef AUTODIFF_4_LEVEL_CHECKPOINT
C     nchklev_4 :: length of outer loop of 4-level checkpointing
      INTEGER    nchklev_4
      PARAMETER( nchklev_4 =   1 )
#endif

C--   Note always check for the correct sizes of the common blocks!
C     The product of the nchklev_X needs to be at least equal to
C     nTimeSteps.

#else /* ALLOW_TAMC_CHECKPOINTING undefined */

C     Without ALLOW_TAMC_CHECKPOINTING, nchklev_1 needs to be at least
C     equal to nTimeSteps. This (arbitrary) setting would accommodate a
C     short run (e.g., 10.d with deltaT=10.mn)
      INTEGER    nchklev_1
      PARAMETER( nchklev_1 = 1500 )

#endif /* ALLOW_TAMC_CHECKPOINTING */

C     TAMC keys:
C     ==========
C
C     The keys are used for storing and reading data of the reference
C     trajectory. Currently there is only one global key.
C     ikey_dynamics :: key for main time stepping loop

      COMMON /TAMC_KEYS_I/ ikey_dynamics
      INTEGER ikey_dynamics

C     isbyte :: precision of tapes (both memory and disk).
C               For smaller tapes replace 8 by 4.
      INTEGER    isbyte
      PARAMETER( isbyte    = 8 )

C     maxpass :: maximum number of (active + passive) tracers
C                Note: defined in PTRACERS_SIZE.h if compiling pkg/ptracers
#ifndef ALLOW_PTRACERS
      INTEGER    maxpass
      PARAMETER( maxpass   = 3 )
#endif
C     maxcube :: for Multi-Dim advection, max number of horizontal directions
      INTEGER    maxcube
      PARAMETER( maxcube   = 2 )

#ifdef ALLOW_CG2D_NSA
C     Parameter that is needed for the tape complev_cg2d_iter
C     cannot be smaller than the allowed number of iterations in cg2d
C     (numItersMax >= cg2dMaxIters in data-file)
      INTEGER numItersMax
      PARAMETER ( numItersMax = 100 )
#endif

#endif /* ALLOW_AUTODIFF_TAMC */
C     ================================================================
C     END OF HEADER TAMC
C     ================================================================
