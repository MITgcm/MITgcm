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

      IF ( gchem_useEXF ) THEN
C-- from bling:
c      apco2mask = 'c'
c      IF ( useSEAICE ) THEN
c       apco2mask = ' '
c      ENDIF

       _BEGIN_MASTER( myThid )
       errCount = 0

       IF ( gchem_SiFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START silica',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'gchem', 'SurfSi', gchem_SiPeriod,
     I                    gchem_SiStartDate1, gchem_SiStartDate2,
     U                    gchem_SiStartTime, errCount,
     I                    myThid )
       ENDIF

#ifdef GCHEM_3D_SILICA
       IF ( gchem_3DSiFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START silica3D',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'gchem', '3DSi', gchem_3DSiPeriod,
     I                    gchem_3DSiStartDate1, gchem_3DSiStartDate2,
     U                    gchem_3DSiStartTime, errCount,
     I                    myThid )
       ENDIF
#endif

       IF ( gchem_parFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START PAR',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'gchem', 'PAR', gchem_parPeriod,
     I                    gchem_parStartDate1, gchem_parStartDate2,
     U                    gchem_parStartTime, errCount,
     I                    myThid )
       ENDIF

       IF ( gchem_apresFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START APRES',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'gchem', 'APRES', gchem_apresPeriod,
     I                    gchem_apresStartDate1, gchem_apresStartDate2,
     U                    gchem_apresStartTime, errCount,
     I                    myThid )
       ENDIF

       IF ( gchem_windFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START wind',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'gchem', 'wind', gchem_windPeriod,
     I                    gchem_windStartDate1, gchem_windStartDate2,
     U                    gchem_windStartTime, errCount,
     I                    myThid )
       ENDIF
       
       IF ( gchem_iceFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START ice',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'gchem', 'ice', gchem_icePeriod,
     I                    gchem_iceStartDate1, gchem_iceStartDate2,
     U                    gchem_iceStartTime, errCount,
     I                    myThid )
       ENDIF

       IF ( gchem_FeFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START iron',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'gchem', 'iron', gchem_FePeriod,
     I                    gchem_FeStartDate1, gchem_FeStartDate2,
     U                    gchem_FeStartTime, errCount,
     I                    myThid )
       ENDIF

       IF ( gchem_apco2File .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START pCO2',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'gchem', 'apCO2', gchem_apco2Period,
     I                    gchem_apco2StartDate1, 
     I                    gchem_apco2StartDate2,
     U                    gchem_apco2StartTime, errCount,
     I                    myThid )
       ENDIF

       IF ( gchem_chlfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START CHL',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                   'gchem', 'chl', gchem_chlperiod,
     I                   gchem_chlstartdate1, gchem_chlstartdate2,
     U                   gchem_chlStartTime, errCount,
     I                   myThid )
       ENDIF
       
       _END_MASTER( myThid )

C     endif gchem_useEXF
      ENDIF

      _BARRIER
#endif /* ALLOW_EXF */

      RETURN
      END
