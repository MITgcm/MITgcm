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

      INTEGER    nchklev_1
      PARAMETER( nchklev_1      =    30 )
      INTEGER    nchklev_2
      PARAMETER( nchklev_2      =    5 )
c     PARAMETER( nchklev_2      =  150 )
      INTEGER    nchklev_3
      PARAMETER( nchklev_3      =    5 )
c     PARAMETER( nchklev_3      =  150 )

C--   Note always check for the correct sizes of the common blocks!

#else /* ALLOW_TAMC_CHECKPOINTING undefined */

C     Without ALLOW_TAMC_CHECKPOINTING, nchklev_1 needs to be at least
C     equal to nTimeSteps.
      INTEGER    nchklev_1
      PARAMETER( nchklev_1      =  30*5*5 )

#endif /* ALLOW_TAMC_CHECKPOINTING */

C     TAMC keys:
C     ==========
C
C     The keys are used for storing and reading data of the reference
C     trajectory.
C
C     The convention used here is:
C                                    ikey_<name>
C
C     which means that this key is used in routine <name> for reading
C     and writing data.

      COMMON /TAMC_KEYS_I/
     &                     ikey_dynamics
      INTEGER ikey_dynamics

      INTEGER    isbyte
C     For smaller tapes replace 8 by 4.
      PARAMETER( isbyte      = 8 )

C     maxpass :: maximum number of (active + passive) tracers
#ifndef ALLOW_PTRACERS
      INTEGER    maxpass
      PARAMETER( maxpass     = 2 )
#endif
C     maxcube :: for Multi-Dim advection, max number of horizontal directions
      INTEGER    maxcube
      PARAMETER( maxcube     = 1 )

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
