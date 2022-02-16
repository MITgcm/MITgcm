#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DO_THE_MODEL_IO

C     !INTERFACE:
      SUBROUTINE DO_THE_MODEL_IO(
     I                            modelEnd,
     I                            myTime, myIter, myThid )

C     !DESCRIPTION:
C     This is the controlling routine for IO in the model main
C     time--stepping loop.  Many systems do not have thread safe IO so it
C     is easier to lump everything together and do dumping of fields and
C     updating of forcing terms in a single place.  The approach to IO
C     used here is that writes are only performed by thread 1 and that a
C     process only writes out its data (it does not know about anyone
C     elses data!)  Reading on the other hand is assumed to be from a
C     file containing all the data for all the processes. Only the
C     portion of data of interest to this process is actually loaded. To
C     work well this assumes the existence of some reliable tool to join
C     datasets together at the end of a run -- see joinds.

C     !CALLING SEQUENCE:
C     DO_THE_MODEL_IO
C       |
C       |-- WRITE_STATE
C       |
C       |-- TIMEAVE_STATV_WRITE
C       |
C       |-- FIZHI_WRITE_STATE
C       |
C       |-- AIM_WRITE_TAVE
C       |
C       |-- LAND_OUTPUT
C       |
C       |-- OBCS_OUTPUT
C       |
C       |-- GMREDI_OUTPUT
C       |
C       |-- KPP_OUTPUT
C       |
C       |-- PP81_OUTPUT
C       |
C       |-- KL10_OUTPUT
C       |
C       |-- MY82_OUTPUT
C       |
C       |-- OPPS_OUTPUT
C       |
C       |-- GGL90_OUTPUT
C       |
C       |-- SBO_CALC
C       |-- SBO_OUTPUT
C       |
C       |-- STERGLOH_OUTPUT
C       |
C       |-- SEAICE_OUTPUT
C       |
C       |-- SHELFICE_OUTPUT
C       |
C       |-- BULKF_OUTPUT
C       |
C       |-- THSICE_OUTPUT
C       |
C       |-- PTRACERS_OUTPUT
C       |
C       |-- MATRIX_OUTPUT
C       |
C       |-- GCHEM_OUTPUT
C       |
C       |-- CPL_OUTPUT
C       |
C       |-- LAYERS_CALC
C       |-- LAYERS_OUTPUT
C       |
C       |-- DIAGNOSTICS_WRITE

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     modelEnd :: true if call at end of model run.
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Iteration number
C     myThid   :: Thread number for this instance of the routine.
      LOGICAL modelEnd
      _RL     myTime
      INTEGER myThid
      INTEGER myIter

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
CEOP

C     Generaly only thread 1 does IO here. It can not start until
C     all threads fields are ready.
      IF ( plotLevel.GE.debLevD ) THEN
        IF ( DIFFERENT_MULTIPLE(dumpFreq,myTime,deltaTClock)
     &     ) THEN

          _BARRIER

C         Write "text-plots" of certain fields
          CALL PLOT_FIELD_XYZRL( uVel , 'Current uVel  ',
     &         Nr, myIter, myThid )
          CALL PLOT_FIELD_XYZRL( vVel , 'Current vVel  ',
     &         Nr, myIter, myThid )
          CALL PLOT_FIELD_XYZRL( theta, 'Current theta ',
     &         Nr, myIter, myThid )
          CALL PLOT_FIELD_XYRL( etaN  , 'Current etaN  ',
     &         myIter, myThid )

        ENDIF
      ENDIF

C     Write model state to binary file
      IF ( .NOT.useOffLine ) THEN
        CALL WRITE_STATE( myTime, myIter, myThid )
      ENDIF

#ifdef ALLOW_TIMEAVE
C     Do time averages
      IF (taveFreq.GT.0. .AND. myIter.NE.nIter0 ) THEN
        CALL TIMEAVE_STATV_WRITE( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_FIZHI
      IF ( useFIZHI )
     &     CALL FIZHI_WRITE_STATE( myTime, myIter, myThid )
#endif

#ifdef ALLOW_AIM
C     Do AIM time averages
      IF ( useAIM )
     &     CALL AIM_WRITE_TAVE( myTime, myIter, myThid )
#endif
#ifdef ALLOW_LAND
C     Do LAND output
      IF ( useLAND )
     &     CALL LAND_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_OBCS
      IF (useOBCS )
     &     CALL OBCS_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_GMREDI
C     Do GMRedi output.
      IF (useGMRedi .AND. myIter.NE.nIter0 .AND. .NOT.useOffLine )
     &     CALL GMREDI_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_KPP
C     Do KPP diagnostics.
      IF ( useKPP )
     &     CALL KPP_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_PP81
C     Do PP81 diagnostics.
      IF ( usePP81 )
     &     CALL PP81_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_KL10
C     Do KL10 diagnostics.
      IF ( useKL10 )
     &     CALL KL10_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_MY82
C     Do MY82 diagnostics.
      IF ( useMY82 )
     &     CALL MY82_OUTPUT( myTime, myIter, myThid )
#endif

c#ifdef ALLOW_OPPS
C--   Do OPPS diagnostics.
c     IF ( useOPPS )
c    & CALL OPPS_OUTPUT( myTime, myIter, myThid )
c#endif

#ifdef ALLOW_GGL90
C--   Do GGL90 diagnostics.
      IF ( useGGL90 )
     & CALL GGL90_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_SBO
C     Do SBO diagnostics.
      IF ( useSBO ) THEN
        CALL SBO_CALC( myTime, myIter, myThid )
        CALL SBO_OUTPUT( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_ECCO
      IF ( useECCO ) THEN
        CALL STERGLOH_OUTPUT( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef  ALLOW_SEAICE
      IF ( useSEAICE ) THEN
        CALL SEAICE_OUTPUT( myTime, myIter, myThid )
      ENDIF
#endif  /* ALLOW_SEAICE */

#ifdef  ALLOW_SHELFICE
      IF ( useShelfIce ) THEN
        CALL SHELFICE_OUTPUT( myTime, myIter, myThid )
      ENDIF
#endif  /* ALLOW_SHELFICE */

#ifdef ALLOW_BULK_FORCE
C     Do bulkf output.
      IF ( useBulkForce .AND. myIter.NE.nIter0 )
     &     CALL BULKF_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_THSICE
C     Do seaice output.
      IF (useThSIce)
     &     CALL THSICE_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_PTRACERS
C--   Do ptracer output.
      IF ( usePTRACERS )
     & CALL PTRACERS_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_MATRIX
C--   Do matrix output
      IF (useMATRIX)
     & CALL MATRIX_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_GCHEM
C--   Do GCHEM diagnostics.
      IF (useGCHEM)
     & CALL GCHEM_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_OFFLINE
C--   Do Off-Line variables output
c     IF (useOffLine)
c    & CALL OFFLINE_STATE( myTime, myIter, myThid )
#endif

#ifdef COMPONENT_MODULE
      IF ( useCoupler )
     &     CALL CPL_OUTPUT( myTime, myIter, myThid )
#endif

C-- added by RPA
#ifdef ALLOW_LAYERS
      IF ( useLayers ) THEN
        CALL LAYERS_CALC( myTime, myIter, myThid )
        CALL LAYERS_OUTPUT( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_LAYERS */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics )
     &     CALL DIAGNOSTICS_WRITE( modelEnd, myTime, myIter, myThid )
#endif

      RETURN
      END
