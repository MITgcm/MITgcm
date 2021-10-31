#ifdef ALLOW_OPPS

CBOP
C !ROUTINE: OPPS.h

C !DESCRIPTION: \bv
C     *==========================================================*
C     | OPPS.h                                                   |
C     | o Basic header for Paluszkiewicz and Romea (1997)        |
C     |   Ocean Penetrative Plume Scheme (OPPS)                  |
C     |   Contains all OPPS field declarations.                  |
C     *==========================================================*

C-----------------------------------------------------------------------
C Parameters that can be set in data.opps
C
C     MAX_ABE_ITERATIONS  :: maximum for iteration on fractional size
C                            (default=1)
C                            In the present implementation, there is no
C                            iteration and max_abe_iterations should
C                            always be 1
C     OPPSdebugLevel      :: sets internal debug level (default = 0) to
C                            produce some output for debugging
C     PlumeRadius         :: default = 100 m
C     STABILITY_THRESHOLD :: threshold of vertical density difference,
C                            beyond which convection starts
C                            (default = -1.e-4 kg/m^3)
C     FRACTIONAL_AREA     :: (initial) fractional area that plume(s)
C                            occupies (default = 0.1)
C     MAX_FRACTIONAL_AREA :: maximum of above (default = 0.8), not used
C     VERTICAL_VELOCITY   :: initial (positive=downward) vertical
C                            velocity of plume (default=0.03m/s)
C     ENTRAINMENT_RATE    :: default = - 0.05
C     e2                  :: 2*ENTRAINMENT_RATE (cannot be set)
C     useGCMwVel          :: flag to replace VERTICAL_VELOCITY with
C                            actual vertical velocity of GCM, probably
C                            useless (default = .false.)
C     OPPSdumpFreq        :: default = dumpFreq (currently there is no
C                            snap-shot output)
C-----------------------------------------------------------------------
C \ev
CEOP

      INTEGER MAX_ABE_ITERATIONS
CML     default values from original code
CML      PARAMETER(MAX_ABE_ITERATIONS=1)
      INTEGER OPPSdebugLevel
      COMMON /OPPS_PARMS_I/
     &     MAX_ABE_ITERATIONS, OPPSdebugLevel

      _RL PlumeRadius
      _RL STABILITY_THRESHOLD
      _RL FRACTIONAL_AREA
      _RL MAX_FRACTIONAL_AREA
      _RL VERTICAL_VELOCITY
      _RL ENTRAINMENT_RATE
      _RL e2
C      _RL OPPSdumpFreq
CML     default values from original code
CML      PARAMETER ( PlumeRadius          =  100.D0   )
CML      PARAMETER ( STABILITY_THRESHOLD  =  -1.E-4   )
CML      PARAMETER ( FRACTIONAL_AREA      =  .1E0    )
CML      PARAMETER ( MAX_FRACTIONAL_AREA  =  .8E0     )
CML      PARAMETER ( VERTICAL_VELOCITY    =  .02E0   )
CML      PARAMETER ( ENTRAINMENT_RATE     =  -.05E0     )
CML      PARAMETER ( e2    =   2.E0*ENTRAINMENT_RATE  )
      COMMON /OPPS_PARMS_R/
     &     PlumeRadius,
     &     STABILITY_THRESHOLD,
     &     FRACTIONAL_AREA,
     &     MAX_FRACTIONAL_AREA,
     &     VERTICAL_VELOCITY,
     &     ENTRAINMENT_RATE,
     &     e2
C     &     , OPPSdumpFreq

      LOGICAL OPPSisOn, useGCMwVel
C     LOGICAL OPPSwriteState
      COMMON /OPPS_PARMS_L/
     &     OPPSisOn, useGCMwVel
C    &     , OPPSwriteState

#endif /* ALLOW_OPPS */
