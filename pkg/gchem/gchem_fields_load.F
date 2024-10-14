#include "GCHEM_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: GCHEM_FIELDS_LOAD

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_FIELDS_LOAD (
     I           myTime, myIter, myThid )

C !DESCRIPTION:
C  calls routines which read in fields needed for any tracer experiment
C !USES: ===============================================================
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
#ifdef ALLOW_EXF
ctmp# include "EXF_PARAM.h"
# include "EXF_INTERP_SIZE.h"
ctmp# include "EXF_FIELDS.h"
# include "GCHEM_EXF.h"
C ?? Is this enough? from Darwin
c#endif
c#ifdef ALLOW_EXF
c#include "EXF_INTERP_SIZE.h"
#endif
#include "GCHEM.h"

C !INPUT PARAMETERS: ===================================================
C  myTime               :: current time
C  myIter               :: current iteration
C  myThid               :: thread number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
CEOP

#ifdef ALLOW_GCHEM

cccccccccccccccccccccccccc
c load external data     c
cccccccccccccccccccccccccc
#ifdef ALLOW_DIC
      IF ( useDIC ) THEN
       CALL DIC_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_BLING
      IF ( useBLING ) THEN
       CALL BLING_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_CFC
      IF ( useCFC ) THEN
       CALL CFC_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
       CALL DARWIN_FIELDS_LOAD( myIter,myTime,myThid )
      ENDIF
#endif

#ifdef ALLOW_EXF
      IF ( useEXF ) THEN
       IF ( exf_silicafile .NE. ' '  ) THEN
       CALL EXF_SET_FLD(
     &     'silica', exf_silicafile, silicamask,
     &     silicaStartTime, silicaperiod, silicaRepCycle,
     &     gchem_inscal_silica,
     &     silica_exfremo_intercept, silica_exfremo_slope,
     &     gchemSi, gSi0, gSi1,
#ifdef USE_EXF_INTERPOLATION
     &     silica_lon0, silica_lon_inc,
     &     silica_lat0, silica_lat_inc,
     &     silica_nlon, silica_nlat, xC, yC,
     &     silica_interpMethod,
#endif
     &     mytime, myiter, mythid )
       ENDIF
       
       IF ( exf_PARfile .NE. ' '  ) THEN
       CALL EXF_SET_FLD(
     &     'PAR', exf_PARfile, PARmask,
     &     PARStartTime, PARperiod, PARRepCycle,
     &     gchem_inscal_PAR,
     &     PAR_exfremo_intercept, PAR_exfremo_slope,
     &     gchemPAR, gPAR0, gPAR1,
#ifdef USE_EXF_INTERPOLATION
     &     PAR_lon0, PAR_lon_inc,
     &     PAR_lat0, PAR_lat_inc,
     &     PAR_nlon, PAR_nlat, xC, yC,
     &     PAR_interpMethod,
#endif
     &     mytime, myiter, mythid )
       ENDIF

       IF ( exf_ironfile .NE. ' '  ) THEN
       CALL EXF_SET_FLD(
     &     'iron', exf_ironfile, ironmask,
     &     ironStartTime, ironperiod, ironRepCycle,
     &     gchem_inscal_iron,
     &     iron_exfremo_intercept, iron_exfremo_slope,
     &     gchemFe, giron0, giron1,
#ifdef USE_EXF_INTERPOLATION
     &     iron_lon0, iron_lon_inc,
     &     iron_lat0, iron_lat_inc,
     &     iron_nlon, iron_nlat, xC, yC,
     &     iron_interpMethod,
#endif
     &     mytime, myiter, mythid )
       ENDIF

       IF ( exf_apco2file .NE. ' '  ) THEN
       CALL EXF_SET_FLD(
     &     'apCO2', exf_apCO2file, apCO2mask,
     &     apCO2StartTime, apCO2period, apCO2RepCycle,
     &     gchem_inscal_apCO2,
     &     apCO2_exfremo_intercept, apCO2_exfremo_slope,
     &     gchemapCO2, apCO20, apCO21,
#ifdef USE_EXF_INTERPOLATION
     &     apCO2_lon0, apCO2_lon_inc,
     &     apCO2_lat0, apCO2_lat_inc,
     &     apCO2_nlon, apCO2_nlat, xC, yC,
     &     apCO2_interpMethod,
#endif
     &     mytime, myiter, mythid )
       ENDIF
      ENDIF
      
#endif /* ALLOW_EXF */
#endif /* ALLOW_GCHEM */

      RETURN
      END

