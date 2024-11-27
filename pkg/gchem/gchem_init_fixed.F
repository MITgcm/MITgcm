#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_INIT_FIXED(myThid )

C !DESCRIPTION:
C calls subroutines that initialized fixed variables for any tracer
C experiment

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GCHEM.h"
#ifdef ALLOW_EXF
# include "GCHEM_EXF.h"
# include "EXF_PARAM.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     errCount  :: error counter
      INTEGER errCount

C jmc: simpler to put MASTER thread directive within individual S/R :
C      that leave the choice, for each S/R, to do either single (master)
C      thread tasks or Multi-threaded tasks.
c        _BEGIN_MASTER( myThid )
ccccccccccccccccccccccccccc
c initialize parameters   c
ccccccccccccccccccccccccccc
#ifdef ALLOW_CFC
      IF ( useCFC ) THEN
         print*,'QQ start cfc params'
         CALL CFC_PARAM( myThid )
         print*,'QQ read cfc atmos data'
         CALL CFC_ATMOS( myThid )
      ENDIF
#endif /* ALLOW_CFC */
#ifdef ALLOW_DIC
      IF ( useDIC ) THEN
         print*,'QQ load dic parameters, initial fixed'
         CALL DIC_INIT_FIXED( myThid )
      ENDIF
#endif /* ALLOW_DIC */
#ifdef ALLOW_BLING
      IF ( useBLING ) THEN
         CALL BLING_INIT_FIXED( myThid )
      ENDIF
#endif /* ALLOW_BLING */
c         _END_MASTER( myThid )

#ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
         CALL DARWIN_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_SPOIL
      IF ( useSPOIL ) THEN
         CALL SPOIL_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
C     Define diagnostics Names :
      IF ( useDiagnostics ) THEN
        CALL GCHEM_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_EXF

      IF ( useEXF ) THEN
C-- from bling:
c      apco2mask = 'c'
c      IF ( useSEAICE ) THEN
c       apco2mask = ' '
c      ENDIF

       _BEGIN_MASTER( myThid )
       errCount = 0

       IF ( gchem_silicafile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START silica',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                          'gchem', 'SurfSi', silicaperiod,
     I                          silicastartdate1, silicastartdate2,
     U                          silicaStartTime, errCount,
     I                          myThid )
       ENDIF

       IF ( gchem_PARfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START PAR',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                         'gchem', 'PAR', PARperiod,
     I                         PARstartdate1, PARstartdate2,
     U                         PARStartTime, errCount,
     I                         myThid )
       ENDIF

       IF ( gchem_ironfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START iron',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                         'gchem', 'iron', ironperiod,
     I                         ironstartdate1, ironstartdate2,
     U                         ironStartTime, errCount,
     I                         myThid )
       ENDIF

       IF ( gchem_apCO2file .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START pCO2',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                         'gchem', 'apCO2', apCO2period,
     I                         apCO2startdate1, apCO2startdate2,
     U                         apCO2StartTime, errCount,
     I                         myThid )
       ENDIF

       _END_MASTER( myThid )

C     endif useEXF
      ENDIF

      _BARRIER
#endif /* ALLOW_EXF */

      RETURN
      END
