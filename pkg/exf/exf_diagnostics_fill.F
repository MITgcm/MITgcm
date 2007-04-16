C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_diagnostics_fill.F,v 1.5 2007/04/16 23:27:21 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_diagnostics_fill(
     &           mycurrenttime, mycurrentiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_diagnostics_fill
c     ==================================================================
c
      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"

c     == routine arguments ==

      integer mythid
      integer mycurrentiter
      _RL     mycurrenttime

c     == local variables ==

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
c
         CALL DIAGNOSTICS_FILL(ustress,    'EXFtaux ',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(vstress,    'EXFtauy ',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(wspeed,     'EXFwspee',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(sflux,      'EXFempmr',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(hflux,      'EXFqnet ',0,1,0,1,1,myThid)
#ifdef SHORTWAVE_HEATING
         CALL DIAGNOSTICS_FILL(swflux,     'EXFswnet',0,1,0,1,1,myThid)
#endif
#ifdef ALLOW_ATM_WIND
         CALL DIAGNOSTICS_FILL(uwind,      'EXFuwind',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(vwind,      'EXFvwind',0,1,0,1,1,myThid)
#endif
#ifdef ALLOW_ATM_TEMP
         CALL DIAGNOSTICS_FILL(atemp,      'EXFatemp',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(aqh,        'EXFaqh  ',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(hs    ,     'EXFhs   ',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(hl    ,     'EXFhl   ',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(lwflux,     'EXFlwnet',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(precip,     'EXFpreci',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(snowprecip, 'EXFsnow ',0,1,0,1,1,myThid)
#endif
#if (defined (ALLOW_ATM_TEMP) || defined (EXF_READ_EVAP))
         CALL DIAGNOSTICS_FILL(evap,       'EXFevap ',0,1,0,1,1,myThid)
#endif
#ifdef ALLOW_DOWNWARD_RADIATION
         CALL DIAGNOSTICS_FILL(lwdown,     'EXFlwdn ',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(swdown,     'EXFswdn ',0,1,0,1,1,myThid)
#endif
#ifdef ATMOSPHERIC_LOADING
         CALL DIAGNOSTICS_FILL(apressure,  'EXFpress',0,1,0,1,1,myThid)
#endif
c
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      end
