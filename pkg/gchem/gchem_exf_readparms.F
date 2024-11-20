#include "GCHEM_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: GCHEM_EXF_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_EXF_READPARMS( iUnit, myThid )

C !DESCRIPTION:
C     Initialize GCHEM parameters, read in data.gchem

C !USES: ===============================================================
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif
#ifdef ALLOW_GCHEM
#include "GCHEM.h"
#endif
#ifdef ALLOW_EXF
#include "EXF_PARAM.h"
#include "EXF_INTERP_SIZE.h"
#include "EXF_INTERP_PARAM.h"
#endif
#include "GCHEM_SIZE.h"
#include "GCHEM_EXF.h"

C !INPUT PARAMETERS: ===================================================
C  iUnit   :: unit number for reading
C  myThid  :: thread number
      INTEGER iUnit, myThid

C !OUTPUT PARAMETERS: ==================================================
C  none
CEOP

#if ( defined ALLOW_GCHEM && defined ALLOW_EXF )

C !LOCAL VARIABLES: ====================================================
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER j, errCount

      NAMELIST /GCHEM_FORCING_PARAMS/
C
     &    silicaperiod,
     &    silicaRepCycle,
     &    silicaStartTime,
     &    silicastartdate1,
     &    silicastartdate2,
     &    silicaconst,
     &    silica_exfremo_intercept,
     &    silica_exfremo_slope,
     &    silicamask,
     &    gchem_inscal_silica,
C
     &    PARperiod,
     &    PARRepCycle,
     &    PARStartTime,
     &    PARstartdate1,
     &    PARstartdate2,
     &    PARconst,
     &    PAR_exfremo_intercept,
     &    PAR_exfremo_slope,
     &    PARmask,
     &    gchem_inscal_PAR,
C
     &    ironperiod,
     &    ironRepCycle,
     &    ironStartTime,
     &    ironstartdate1,
     &    ironstartdate2,
     &    ironconst,
     &    iron_exfremo_intercept,
     &    iron_exfremo_slope,
     &    ironmask,
     &    gchem_inscal_iron,
C
     &    apCO2period,
     &    apCO2RepCycle,
     &    apCO2StartTime,
     &    apCO2startdate1,
     &    apCO2startdate2,
     &    apCO2const,
     &    apCO2_exfremo_intercept,
     &    apCO2_exfremo_slope,
     &    apCO2mask,
     &    gchem_inscal_apCO2,
C
     &    windperiod,
     &    windRepCycle,
     &    windStartTime,
     &    windstartdate1,
     &    windstartdate2,
     &    windconst,
     &    wind_exfremo_intercept,
     &    wind_exfremo_slope,
     &    windmask,
     &    gchem_inscal_wind,
C
     &    apresperiod,
     &    apresRepCycle,
     &    apresStartTime,
     &    apresstartdate1,
     &    apresstartdate2,
     &    apresconst,
     &    apres_exfremo_intercept,
     &    apres_exfremo_slope,
     &    apresmask,
     &    gchem_inscal_apres

#ifdef USE_EXF_INTERPOLATION
      NAMELIST /GCHEM_INTERP_PARAMS/
C
     &    silica_lon0,
     &    silica_lat0,
     &    silica_nlon,
     &    silica_nlat,
     &    silica_lon_inc,
     &    silica_interpMethod,
     &    silica_lat_inc,
C
     &    PAR_lon0,
     &    PAR_lat0,
     &    PAR_nlon,
     &    PAR_nlat,
     &    PAR_lon_inc,
     &    PAR_interpMethod,
     &    PAR_lat_inc,
C
     &    iron_lon0,
     &    iron_lat0,
     &    iron_nlon,
     &    iron_nlat,
     &    iron_lon_inc,
     &    iron_interpMethod,
     &    iron_lat_inc,
C
     &    apCO2_lon0,
     &    apCO2_lat0,
     &    apCO2_nlon,
     &    apCO2_nlat,
     &    apCO2_lon_inc,
     &    apCO2_interpMethod,
     &    apCO2_lat_inc,
C
     &    wind_lon0,
     &    wind_lat0,
     &    wind_nlon,
     &    wind_nlat,
     &    wind_lon_inc,
     &    wind_interpMethod,
     &    wind_lat_inc,
C
     &    apres_lon0,
     &    apres_lat0,
     &    apres_nlon,
     &    apres_nlat,
     &    apres_lon_inc,
     &    apres_interpMethod,
     &    apres_lat_inc
#endif

C Open and read the data.gchem file

      silicaperiod             = 0.0 _d 0
      silicaRepCycle           = repeatPeriod
      silicaStartTime          = UNSET_RL
      silicastartdate1         = 0
      silicastartdate2         = 0
      silicaconst              = UNSET_RL
      silica_exfremo_intercept = 0.0 _d 0
      silica_exfremo_slope     = 0.0 _d 0
      silicamask               = 'c'
      gchem_inscal_silica      = 1. _d 0

      PARperiod             = 0.0 _d 0
      PARRepCycle           = repeatPeriod
      PARStartTime          = UNSET_RL
      PARstartdate1         = 0
      PARstartdate2         = 0
      PARconst              = UNSET_RL
      PAR_exfremo_intercept = 0.0 _d 0
      PAR_exfremo_slope     = 0.0 _d 0
      PARmask               = 'c'
      gchem_inscal_PAR      = 1. _d 0

      ironperiod             = 0.0 _d 0
      ironRepCycle           = repeatPeriod
      ironStartTime          = UNSET_RL
      ironstartdate1         = 0
      ironstartdate2         = 0
      ironconst              = 0 _d 0
      iron_exfremo_intercept = 0.0 _d 0
      iron_exfremo_slope     = 0.0 _d 0
      ironmask               = 'c'
      gchem_inscal_iron      = 1. _d 0

      apCO2period             = 0.0 _d 0
      apCO2RepCycle           = repeatPeriod
      apCO2StartTime          = UNSET_RL
      apCO2startdate1         = 0
      apCO2startdate2         = 0
      apCO2const              = UNSET_RL
      apCO2_exfremo_intercept = 0.0 _d 0
      apCO2_exfremo_slope     = 0.0 _d 0
      apCO2mask               = 'c'
      gchem_inscal_apCO2      = 1. _d 0

      windperiod             = 0.0 _d 0
      windRepCycle           = repeatPeriod
      windStartTime          = UNSET_RL
      windstartdate1         = 0
      windstartdate2         = 0
      windconst              = 0 _d 0
      wind_exfremo_intercept = 0.0 _d 0
      wind_exfremo_slope     = 0.0 _d 0
      windmask               = 'c'
      gchem_inscal_wind      = 1. _d 0

      apresperiod             = 0.0 _d 0
      apresRepCycle           = repeatPeriod
      apresStartTime          = UNSET_RL
      apresstartdate1         = 0
      apresstartdate2         = 0
      apresconst              = UNSET_RL
      apres_exfremo_intercept = 0.0 _d 0
      apres_exfremo_slope     = 0.0 _d 0
      apresmask               = 'c'
      gchem_inscal_apres      = 1. _d 0

#ifdef USE_EXF_INTERPOLATION

      silica_lon0 = inp_lon0
      silica_lat0 = inp_lat0
      silica_nlon = inp_gNx
      silica_nlat = inp_gNy
      silica_lon_inc = inp_dLon
      silica_interpMethod  = 1

      PAR_lon0 = inp_lon0
      PAR_lat0 = inp_lat0
      PAR_nlon = inp_gNx
      PAR_nlat = inp_gNy
      PAR_lon_inc = inp_dLon
      PAR_interpMethod  = 1

      iron_lon0 = inp_lon0
      iron_lat0 = inp_lat0
      iron_nlon = inp_gNx
      iron_nlat = inp_gNy
      iron_lon_inc = inp_dLon
      iron_interpMethod  = 1

      apCO2_lat0 = inp_lat0
      apCO2_nlon = inp_gNx
      apCO2_nlat = inp_gNy
      apCO2_lon_inc = inp_dLon
      apCO2_interpMethod  = 1

      wind_lon0 = inp_lon0
      wind_lat0 = inp_lat0
      wind_nlon = inp_gNx
      wind_nlat = inp_gNy
      wind_lon_inc = inp_dLon
      wind_interpMethod  = 1

      apres_lat0 = inp_lat0
      apres_nlon = inp_gNx
      apres_nlat = inp_gNy
      apres_lon_inc = inp_dLon
      apres_interpMethod  = 1

      DO j=1,MAX_LAT_INC
        silica_lat_inc(j) = inp_dLat(j)
        PAR_lat_inc(j) = inp_dLat(j)
        iron_lat_inc(j) = inp_dLat(j)
        apCO2_lat_inc(j) = inp_dLat(j)
        wind_lat_inc(j) = inp_dLat(j)
        apres_lat_inc(j) = inp_dLat(j)
      ENDDO
#endif /* USE_EXF_INTERPOLATION */

C ======================================================================

      READ(UNIT=iUnit, NML=GCHEM_FORCING_PARAMS)
#ifdef USE_EXF_INTERPOLATION
      READ(UNIT=iUnit, NML=GCHEM_INTERP_PARAMS)
#endif

      IF (gchem_apCO2file.EQ.' ') THEN
        WRITE(msgBuf,'(2A,E13.6,A)')
     &    '** WARNING ** GCHEM_EXF_READPARMS: ',
     &    'default atmos pCO2 of ',GCHEM_pCO2,' atm is used'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

      errCount = 0
      IF ( useExfYearlyFields ) THEN
       IF ( PARRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'GCHEM_EXF_READPARMS: The use of ',
     &   'useExfYearlyFields AND ',
     &   'PARRepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( ironRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'GCHEM_EXF_READPARMS: The use of ',
     &   'useExfYearlyFields AND ',
     &   'ironRepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( silicaRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'GCHEM_EXF_READPARMS: The use of ',
     &   'useExfYearlyFields AND ',
     &   'silicaRepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( apCO2RepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'GCHEM_EXF_READPARMS: The use of ',
     &   'useExfYearlyFields AND ',
     &   'apCO2RepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( windRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'GCHEM_EXF_READPARMS: The use of ',
     &   'useExfYearlyFields AND ',
     &   'windRepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( apresRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'GCHEM_EXF_READPARMS: The use of ',
     &   'useExfYearlyFields AND ',
     &   'apresRepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &     'GCHEM_EXF_READPARMS: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R GCHEM_EXF_READPARMS'
      ENDIF

#endif /* ALLOW_GCHEM and ALLOW_EXF */

      RETURN
      END
