C $Header: /u/gcmpack/MITgcm/model/src/do_the_model_io.F,v 1.39 2004/02/26 02:25:01 molod Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DO_THE_MODEL_IO
C     !INTERFACE:
      SUBROUTINE DO_THE_MODEL_IO( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DO_THE_MODEL_IO                                
C     | o Controlling routine for IO in model main time-stepping  
C     |   loop.                                                   
C     *==========================================================*
C     | Many systems do not have thread safe IO so it is easier   
C     | to lump everything together and do dumping of fields      
C     | and updating of forcing terms in a single place.          
C     | The approach to IO used here is that writes are only      
C     | performed by thread 1 and that a process only writes out  
C     | its data ( it does not know about anyone elses data!)     
C     | Reading on the other hand is assumed to be from a file    
C     | containing all the data for all the processes. Only the   
C     | portion of data of interest to this process is actually   
C     | loaded. To work well this assumes the existence of some   
C     | reliable tool to join datasets together at the end of a   
C     | run - see joinds.p                                        
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"


      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      _RL     myTime

CEOP

C--   Generaly only thread 1 does IO here. It can not start until 
C--   all threads fields are ready.
      IF (debugMode) THEN 
       IF ( DIFFERENT_MULTIPLE(dumpFreq,myTime, myTime-deltaTClock)
     &    ) THEN

       _BARRIER

C--    Write "text-plots" of certain fields
       CALL PLOT_FIELD_XYZRL( uVel , 'Current uVel  ' , 
     &                        Nr, myIter, myThid )
       CALL PLOT_FIELD_XYZRL( vVel , 'Current vVel  ' , 
     &                        Nr, myIter, myThid )
       CALL PLOT_FIELD_XYZRL( theta, 'Current theta ' , 
     &                        Nr, myIter, myThid )
       CALL PLOT_FIELD_XYRL( etaN  , 'Current etaN  ' , 
     &                        myIter, myThid )

       ENDIF
      ENDIF

C--   Write model state to binary file
      CALL WRITE_STATE( myTime, myIter, myThid )

#ifdef ALLOW_TIMEAVE
C--   Do time averages
      IF (taveFreq.GT.0. .AND. myIter.NE.nIter0 ) THEN
       CALL TIMEAVE_STATV_WRITE( myTime, myIter, myThid )
#ifdef ALLOW_PTRACERS
       IF ( usePTRACERS )
     & CALL PTRACERS_STATV_WRITE( myTime, myIter, myThid )
#endif
      ENDIF
#endif

#ifdef ALLOW_FIZHI
      if(useFIZHI) call fizhi_write_state( myTime, myIter, myThid )
#endif

#ifdef ALLOW_AIM
C--   Do AIM time averages
      IF ( useAIM )
     &  CALL AIM_WRITE_TAVE( myTime, myIter, myThid )
#endif                                                             
#ifdef ALLOW_LAND
C--   Do LAND time averages
      IF ( useLAND )
     &  CALL LAND_WRITE_TAVE( myTime, myIter, myThid )
#endif                                                             

#ifdef ALLOW_OBCS
      IF (useOBCS .AND. myIter.NE.nIter0 )
     & CALL OBCS_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_GMREDI
C--   Do KPP diagnostics.
      IF (useGMRedi .AND. myIter.NE.nIter0 )
     & CALL GMREDI_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_KPP
C--   Do KPP diagnostics.
      IF (useKPP .AND. myIter.NE.nIter0 )
     & CALL KPP_DO_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_SBO
C--   Do SBO diagnostics.
      IF (useSBO .AND. myIter.NE.nIter0 ) THEN
         CALL SBO_CALC ( myTime, myIter, myThid )
         CALL SBO_DIAGS( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_BULK_FORCE
C--   Do bulkf diagnostics.
      IF (useBulkForce .AND. myIter.NE.nIter0 )
     & CALL BULKF_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_THSICE
C--   Do seaice diagnostics.
      IF (useThSIce)
     & CALL THSICE_DIAGS( myTime, myIter, myThid )
#endif

cswdptr ---  add ---
#ifdef ALLOW_GCHEM
ceh3 ???  why is one package depending upon another here?
C--   Do ptracer diagnostics.
      IF (usePTRACERS)
     & CALL GCHEM_DIAGS( myTime, myIter, myThid )
#endif
cswdptr -- end add ---

#ifdef COMPONENT_MODULE
      IF ( useCoupler )
     &  CALL CPL_WRITE_DIAGS( myTime, myIter, myThid )
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( usediagnostics ) call diagnostics_write (myThid, myIter)
#endif

      RETURN
      END
