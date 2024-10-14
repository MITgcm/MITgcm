#include "GCHEM_OPTIONS.h"
#include "EXF_OPTIONS.h"

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

#ifdef ALLOW_GCHEM

C !LOCAL VARIABLES: ====================================================
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL gchem_haveapCO2
      INTEGER j, errCount

      NAMELIST /GCHEM_FORCING_PARAMS/
     &    exf_silicafile,
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
     &    exf_PARfile,
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
     &    exf_ironfile,
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
     &    exf_apCO2file,
     &    apCO2period,
     &    apCO2RepCycle,
     &    apCO2StartTime,
     &    apCO2startdate1,
     &    apCO2startdate2,
     &    apCO2const,
     &    apCO2_exfremo_intercept,
     &    apCO2_exfremo_slope,
     &    apCO2mask,
     &    gchem_inscal_apCO2

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
     &    apCO2_lat_inc
#endif

C Open and read the data.gchem file

c      gchem_useEXFwind = .FALSE.
c      gchem_useQsw = .FALSE.

      exf_silicafile = ' '
      silicaperiod             = 0.0 _d 0
      silicaRepCycle           = repeatPeriod
      silicaStartTime          = UNSET_RL
      silicastartdate1         = 0
      silicastartdate2         = 0
      silicaconst              = UNSET_RL
      silica_exfremo_intercept = 0.0 _d 0
      silica_exfremo_slope     = 0.0 _d 0
      silicamask = 'c'

      gchem_inscal_silica    =  1. _d 0

      exf_PARfile = ' '
      PARperiod             = 0.0 _d 0
      PARRepCycle           = repeatPeriod
      PARStartTime          = UNSET_RL
      PARstartdate1         = 0
      PARstartdate2         = 0
      PARconst              = UNSET_RL
      PAR_exfremo_intercept = 0.0 _d 0
      PAR_exfremo_slope     = 0.0 _d 0
      PARmask = 'c'

      gchem_inscal_PAR    =  1. _d 0

      exf_ironfile = ' '
      ironperiod             = 0.0 _d 0
      ironRepCycle           = repeatPeriod
      ironStartTime          = UNSET_RL
      ironstartdate1         = 0
      ironstartdate2         = 0
      ironconst              = 0 _d 0
      iron_exfremo_intercept = 0.0 _d 0
      iron_exfremo_slope     = 0.0 _d 0
      ironmask = 'c'

      gchem_inscal_iron    =  1. _d 0

      exf_apCO2file = ' '
      apCO2period             = 0.0 _d 0
      apCO2RepCycle           = repeatPeriod
      apCO2StartTime          = UNSET_RL
      apCO2startdate1         = 0
      apCO2startdate2         = 0
      apCO2const              = UNSET_RL
      apCO2_exfremo_intercept = 0.0 _d 0
      apCO2_exfremo_slope     = 0.0 _d 0
      apCO2mask = 'c'

      gchem_inscal_apCO2    =  1. _d 0


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

      DO j=1,MAX_LAT_INC
        silica_lat_inc(j) = inp_dLat(j)
        PAR_lat_inc(j) = inp_dLat(j)
        iron_lat_inc(j) = inp_dLat(j)
        apCO2_lat_inc(j) = inp_dLat(j)
      ENDDO
#endif /* USE_EXF_INTERPOLATION */

C ======================================================================

      silicaconst = UNSET_RL
      PARconst = UNSET_RL

      READ(UNIT=iUnit, NML=GCHEM_FORCING_PARAMS)
#ifdef USE_EXF_INTERPOLATION
      READ(UNIT=iUnit, NML=GCHEM_INTERP_PARAMS)
#endif

c      IF (.NOT.GCHEM_haveapCO2) THEN
c        WRITE(msgBuf,'(2A,E13.6,A)')
c     &    '** WARNING ** GCHEM_EXF_READPARMS: ',
c     &    'default atmos pCO2 of ',apCO2const,' atm is used'
c        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
c     &                      SQUEEZE_RIGHT, myThid )
c      ENDIF

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
       IF ( apCO2RepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(3A)') 'GCHEM_EXF_READPARMS: The use of ',
     &   'useExfYearlyFields AND ',
     &   'apCO2RepCycle is not implemented'
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

#endif /* ALLOW_GCHEM */

      RETURN
      END

