C $Header: /u/gcmpack/MITgcm/model/src/do_the_model_io.F,v 1.54 2005/08/15 23:05:28 molod Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DO_THE_MODEL_IO

C     !INTERFACE:
      SUBROUTINE DO_THE_MODEL_IO(
     I     myTime,
     I     myIter,
     I     myThid )

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

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !INPUT/OUTPUT PARAMETERS:
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      _RL     myTime
CEOP


C     Generaly only thread 1 does IO here. It can not start until 
C     all threads fields are ready.
      IF (debugMode) THEN 
        IF ( DIFFERENT_MULTIPLE(dumpFreq,myTime,deltaTClock)
     &     .AND. (.not. nodumps) ) THEN

          _BARRIER

C         Write "text-plots" of certain fields
          CALL PLOT_FIELD_XYZRL( uVel , 'Current uVel  ' , 
     &         Nr, myIter, myThid )
          CALL PLOT_FIELD_XYZRL( vVel , 'Current vVel  ' , 
     &         Nr, myIter, myThid )
          CALL PLOT_FIELD_XYZRL( theta, 'Current theta ' , 
     &         Nr, myIter, myThid )
          CALL PLOT_FIELD_XYRL( etaN  , 'Current etaN  ' , 
     &         myIter, myThid )
          
        ENDIF
      ENDIF

C     Write model state to binary file
      if( .not. nodumps ) then
       CALL WRITE_STATE( myTime, myIter, myThid )
      endif

#ifdef ALLOW_TIMEAVE
C     Do time averages
      IF (taveFreq.GT.0. .AND. myIter.NE.nIter0 ) THEN
        CALL TIMEAVE_STATV_WRITE( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_FIZHI
      if( .not. nodumps ) then
      if(useFIZHI) call fizhi_write_state( myTime, myIter, myThid )
      endif
#endif

#ifdef ALLOW_AIM
C     Do AIM time averages
      IF ( useAIM )
     &     CALL AIM_WRITE_TAVE( myTime, myIter, myThid )
#endif                                                             
#ifdef ALLOW_LAND
C     Do LAND time averages
      IF ( useLAND )
     &     CALL LAND_WRITE_DIAGS( myTime, myIter, myThid )
#endif                                                             

#ifdef ALLOW_OBCS
      IF (useOBCS .AND. myIter.NE.nIter0 )
     &     CALL OBCS_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_GMREDI
C     Do GMRedi output.
      IF (useGMRedi .AND. myIter.NE.nIter0 )
     &     CALL GMREDI_OUTPUT( myTime, myIter, myThid )
#endif

#ifdef ALLOW_KPP
C     Do KPP diagnostics.
      IF ( useKPP )
     &     CALL KPP_DO_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_PP81
C     Do PP81 diagnostics.
      IF (usePP81 .AND. myIter.NE.nIter0 )
     &     CALL PP81_DO_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_MY82
C     Do MY82 diagnostics.
      IF (useMY82 .AND. myIter.NE.nIter0 )
     &     CALL MY82_DO_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_OPPS
C--   Do OPPS diagnostics.
      IF (useOPPS .AND. myIter.NE.nIter0 )
     & CALL OPPS_DO_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_GGL90
C--   Do GGL90 diagnostics.
      IF (useGGL90 .AND. myIter.NE.nIter0 )
     & CALL GGL90_DO_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_SBO
C     Do SBO diagnostics.
      IF (useSBO .AND. myIter.NE.nIter0 ) THEN
        CALL SBO_CALC ( myTime, myIter, myThid )
        CALL SBO_DIAGS( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef  ALLOW_SEAICE
      IF ( useSEAICE ) THEN
        CALL SEAICE_DO_DIAGS( myTime, myIter, myThid )
      ENDIF
#endif  /* ALLOW_SEAICE */

#ifdef ALLOW_BULK_FORCE
C     Do bulkf diagnostics.
      IF (useBulkForce .AND. myIter.NE.nIter0 )
     &     CALL BULKF_DIAGS( myTime, myIter, myThid )
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
     & CALL GCHEM_DIAGS( myTime, myIter, myThid )
#endif

#ifdef COMPONENT_MODULE
      IF ( useCoupler )
     &     CALL CPL_WRITE_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( usediagnostics ) 
     &     CALL DIAGNOSTICS_WRITE( myTime, myIter, myThid )
#endif

      RETURN
      END
