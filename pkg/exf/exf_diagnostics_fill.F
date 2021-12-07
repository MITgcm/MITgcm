#include "EXF_OPTIONS.h"

CBOP 0
C     !ROUTINE: EXF_DIAGNOSTICS_FILL
C     !INTERFACE:
      SUBROUTINE EXF_DIAGNOSTICS_FILL(
     I                                 myTime, myIter, myThid )

C     !DESCRIPTION:
C     Fill-in External Forcing (EXF) pkg diagnostics
C     directly from pkg global variables (in common)

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"

C     !INPUT PARAMETERS:
C     myTime    :: current time in simulation [s]
C     myIter    :: current iteration number in simulation
C     myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIAGNOSTICS
C     !FUNCTIONS:
c     LOGICAL  DIAGNOSTICS_IS_ON
c     EXTERNAL DIAGNOSTICS_IS_ON

C     !LOCAL VARIABLES:
CEOP

      IF ( useDiagnostics ) THEN

       IF ( .NOT.diags_opOceWeighted ) THEN
        CALL DIAGNOSTICS_FILL( ustress,   'EXFtaux ',0,1,0,1,1,myThid )
        CALL DIAGNOSTICS_FILL( vstress,   'EXFtauy ',0,1,0,1,1,myThid )
        CALL DIAGNOSTICS_FILL( sflux,     'EXFempmr',0,1,0,1,1,myThid )
        CALL DIAGNOSTICS_FILL( hflux,     'EXFqnet ',0,1,0,1,1,myThid )
#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
        CALL DIAGNOSTICS_FILL( swflux,    'EXFswnet',0,1,0,1,1,myThid )
#endif
       ENDIF
        CALL DIAGNOSTICS_FILL( uwind,     'EXFuwind',0,1,0,1,1,myThid )
        CALL DIAGNOSTICS_FILL( vwind,     'EXFvwind',0,1,0,1,1,myThid )
        CALL DIAGNOSTICS_FILL( wspeed,    'EXFwspee',0,1,0,1,1,myThid )
#ifdef ALLOW_ATM_TEMP
        CALL DIAGNOSTICS_FILL( atemp,     'EXFatemp',0,1,0,1,1,myThid )
        CALL DIAGNOSTICS_FILL( aqh,       'EXFaqh  ',0,1,0,1,1,myThid )
       IF ( .NOT.diags_opOceWeighted ) THEN
        CALL DIAGNOSTICS_FILL( hs,        'EXFhs   ',0,1,0,1,1,myThid )
        CALL DIAGNOSTICS_FILL( hl,        'EXFhl   ',0,1,0,1,1,myThid )
        CALL DIAGNOSTICS_FILL( lwflux,    'EXFlwnet',0,1,0,1,1,myThid )
        CALL DIAGNOSTICS_FILL( evap,      'EXFevap ',0,1,0,1,1,myThid )
       ENDIF
        CALL DIAGNOSTICS_FILL( precip,    'EXFpreci',0,1,0,1,1,myThid )
        CALL DIAGNOSTICS_FILL( snowprecip,'EXFsnow ',0,1,0,1,1,myThid )
#endif
#ifdef ALLOW_DOWNWARD_RADIATION
        CALL DIAGNOSTICS_FILL( lwdown,    'EXFlwdn ',0,1,0,1,1,myThid )
        CALL DIAGNOSTICS_FILL( swdown,    'EXFswdn ',0,1,0,1,1,myThid )
#endif
#ifdef ATMOSPHERIC_LOADING
        CALL DIAGNOSTICS_FILL( apressure, 'EXFpress',0,1,0,1,1,myThid )
#endif
#ifdef ALLOW_RUNOFF
        CALL DIAGNOSTICS_FILL( runoff,    'EXFroff ',0,1,0,1,1,myThid )
#endif
#ifdef ALLOW_RUNOFTEMP
        CALL DIAGNOSTICS_FILL( runoftemp, 'EXFroft ',0,1,0,1,1,myThid )
#endif
#ifdef ALLOW_SALTFLX
        CALL DIAGNOSTICS_FILL( saltflx,   'EXFsalfx',0,1,0,1,1,myThid )
#endif

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
