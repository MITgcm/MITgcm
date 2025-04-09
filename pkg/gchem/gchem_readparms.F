#include "GCHEM_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: GCHEM_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize GCHEM parameters, read in data.gchem

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GCHEM.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
# include "GCHEM_EXF.h"
# ifdef USE_EXF_INTERPOLATION
#  include "EXF_INTERP_SIZE.h"
#  include "EXF_INTERP_PARAM.h"
#  include "GCHEM_INTERP_PARAM.h"
# endif
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid         :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

C !LOCAL VARIABLES: ====================================================
C  tIter0         :: retired parameter
C  iUnit          :: unit number for I/O
C  msgBuf         :: message buffer
      INTEGER tIter0
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
# ifdef USE_EXF_INTERPOLATION
      INTEGER j
# endif
CEOP

C- Sub-package on/off flags: not fully implemented, requires
C  to test the flag before any corresponding pkg S/R call

      NAMELIST /GCHEM_PARM01/
     &                   useCFC,
     &                   useDIC,
     &                   useBLING,
     &                   useSPOIL,
     &                   useDARWIN,
     &                   gchem_useEXF,
     &           fileName1, fileName2, fileName3,
     &           fileName4, fileName5,
     &           gchem_int1, gchem_int2, gchem_int3,
     &           gchem_int4, gchem_int5, nsubtime,
     &           gchem_rl1, gchem_rl2, gchem_rl3,
     &           gchem_rl4, gchem_rl5,
     &           gchem_ForcingPeriod, gchem_ForcingCycle,
     &           gchem_silicafile, gchem_PARfile, gchem_ironfile,
     &           gchem_apCO2file, gchem_windFile, gchem_atmospFile,
     &           gchem_iceFile,
     &           gchem_silica_const, gchem_par_const, gchem_iron_const,
     &           gchem_apCO2_const, gchem_wind_const, gchem_apres_const,
     &           tIter0

# ifdef ALLOW_EXF
      NAMELIST /GCHEM_FORCING_PARAMS/
     &     gchem_SiPeriod, gchem_SiRepCycle, gchem_SiStartTime,
     &     gchem_SiStartDate1, gchem_SiStartDate2,
     &     gchem_Si_exfremo_intercept, gchem_Si_exfremo_slope,
     &     gchem_SiMask, gchem_inscal_Si,
C
     &     gchem_parPeriod, gchem_parRepCycle, gchem_parStartTime,
     &     gchem_parStartDate1, gchem_parStartDate2,
     &     gchem_par_exfremo_intercept, gchem_par_exfremo_slope,
     &     gchem_parMask, gchem_inscal_par,
C
     &     gchem_FePeriod, gchem_FeRepCycle, gchem_FeStartTime,
     &     gchem_FeStartDate1, gchem_FeStartDate2,
     &     gchem_Fe_exfremo_intercept, gchem_Fe_exfremo_slope,
     &     gchem_FeMask, gchem_inscal_Fe,
C
     &     gchem_apco2Period, gchem_apco2RepCycle, gchem_apco2StartTime,
     &     gchem_apco2StartDate1, gchem_apco2StartDate2,
     &     gchem_apco2_exfremo_intercept, gchem_apco2_exfremo_slope,
     &     gchem_apco2Mask, gchem_inscal_apco2,
C
     &     gchem_wind2Period, gchem_windRepCycle, gchem_windStartTime,
     &     gchem_windStartDate1, gchem_windStartDate2,
     &     gchem_wind_exfremo_intercept, gchem_wind_exfremo_slope,
     &     gchem_windMask, gchem_inscal_wind,
C
     &     gchem_apresPeriod, gchem_apresRepCycle, gchem_apresStartTime,
     &     gchem_apresStartDate1, gchem_apresStartDate2,
     &     gchem_apres_exfremo_intercept, gchem_apres_exfremo_slope,
     &     gchem_apresMask, gchem_inscal_apres

#  ifdef USE_EXF_INTERPOLATION
      NAMELIST /GCHEM_INTERP_PARAMS/
     &     silica_lon0, silica_lat0, silica_nlon,silica_nlat,
     &     silica_lon_inc, silica_lat_inc, silica_interpMethod,
C
     &     PAR_lon0, PAR_lat0, PAR_nlon, PAR_nlat,
     &     PAR_lon_inc, PAR_lat_inc, PAR_interpMethod,
C
     &     iron_lon0, iron_lat0, iron_nlon, iron_nlat,
     &     iron_lon_inc, iron_lat_inc, iron_interpMethod,
C
     &     apCO2_lon0, apCO2_lat0, apCO2_nlon, apCO2_nlat,
     &     apCO2_lon_inc, apCO2_lat_inc, apCO2_interpMethod,
C
     &     wind_lon0, wind_lat0, wind_nlon, wind_nlat,
     &     wind_lon_inc, wind_lat_inc, wind_interpMethod,
C
     &     apres_lon0, apres_lat0, apres_nlon, apres_nlat,
     &     apres_lon_inc, apres_lat_inc, apres_interpMethod
#  endif
# endif /* ALLOW_EXF */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useGCHEM ) THEN
C-    pkg GCHEM is not used
        _BEGIN_MASTER( myThid )
C-    Track pkg activation status:
C     print a (weak) warning if data.gchem is found
         CALL PACKAGES_UNUSED_MSG( 'useGCHEM', ' ', ' ' )
        _END_MASTER( myThid )
        RETURN
      ENDIF

      _BEGIN_MASTER( myThid )

C- Set defaults values for parameters in GCHEM.h
      useCFC = .FALSE.
      useDIC = .FALSE.
      useBLING  = .FALSE.
      useSPOIL  = .FALSE.
      useDARWIN = .FALSE.
      gchem_useEXF = .FALSE.
      tIter0 = UNSET_I
      fileName1=' '
      fileName2=' '
      fileName3=' '
      fileName4=' '
      fileName5=' '
      nsubtime = 1
      gchem_int1=0
      gchem_int2=0
      gchem_int3=0
      gchem_int4=0
      gchem_int5=0
      gchem_rl1=0. _d 0
      gchem_rl2=0. _d 0
      gchem_rl3=0. _d 0
      gchem_rl4=0. _d 0
      gchem_rl5=0. _d 0

C     default periodic forcing to same as for physics
      gchem_ForcingPeriod=externForcingPeriod
      gchem_ForcingCycle=externForcingCycle

C     some predefined forcing fields and constants
      gchem_iceFile    = ' '
      gchem_windFile   = ' '
      gchem_atmospFile = ' '
      gchem_silicafile = ' '
      gchem_PARfile    = ' '
      gchem_ironfile   = ' '
      gchem_apCO2file  = ' '
C     some sensible defaults
      gchem_silica_const =    7.6 _d -3
      gchem_par_const    =    0.  _d 0
      gchem_iron_const   =    0.  _d 0
      gchem_apCO2_const  =  280.  _d -6
      gchem_wind_const   =    0.  _d 0
      gchem_apres_const  = 1013.  _d 2

C- Open and read the data.gchem file
      WRITE(msgBuf,'(A)') ' GCHEM_READPARMS: opening data.gchem'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.gchem', 'GCHEM_PARM01',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=GCHEM_PARM01)

# ifdef ALLOW_EXF

      IF ( gchem_useEXF ) THEN

       gchem_SiPeriod             = 0.0 _d 0
       gchem_SiRepCycle           = repeatPeriod
       gchem_SiStartTime          = UNSET_RL
       gchem_SiStartDate1         = 0
       gchem_SiStartDate2         = 0
       gchem_Si_exfremo_intercept = 0.0 _d 0
       gchem_Si_exfremo_slope     = 0.0 _d 0
       gchem_SiMask               = 'c'
       gchem_inscal_Si      = 1. _d 0

       gchem_parPeriod             = 0.0 _d 0
       gchem_parRepCycle           = repeatPeriod
       gchem_parStartTime          = UNSET_RL
       gchem_parStartDate1         = 0
       gchem_parStartDate2         = 0
       gchem_par_exfremo_intercept = 0.0 _d 0
       gchem_par_exfremo_slope     = 0.0 _d 0
       gchem_parMask               = 'c'
       gchem_inscal_par      = 1. _d 0

       gchem_FePeriod             = 0.0 _d 0
       gchem_FeRepCycle           = repeatPeriod
       gchem_FeStartTime          = UNSET_RL
       gchem_FeStartDate1         = 0
       gchem_FeStartDate2         = 0
       gchem_Fe_exfremo_intercept = 0.0 _d 0
       gchem_Fe_exfremo_slope     = 0.0 _d 0
       gchem_FeMask               = 'c'
       gchem_inscal_Fe      = 1. _d 0

       gchem_apco2Period             = 0.0 _d 0
       gchem_apco2RepCycle           = repeatPeriod
       gchem_apco2StartTime          = UNSET_RL
       gchem_apco2StartDate1         = 0
       gchem_apco2StartDate2         = 0
       gchem_apco2_exfremo_intercept = 0.0 _d 0
       gchem_apco2_exfremo_slope     = 0.0 _d 0
       gchem_apco2Mask               = 'c'
       gchem_inscal_apco2      = 1. _d 0

       gchem_wind2Period             = 0.0 _d 0
       gchem_windRepCycle           = repeatPeriod
       gchem_windStartTime          = UNSET_RL
       gchem_windStartDate1         = 0
       gchem_windStartDate2         = 0
       gchem_wind_exfremo_intercept = 0.0 _d 0
       gchem_wind_exfremo_slope     = 0.0 _d 0
       gchem_windMask               = 'c'
       gchem_inscal_wind      = 1. _d 0

       gchem_apresPeriod             = 0.0 _d 0
       gchem_apresRepCycle           = repeatPeriod
       gchem_apresStartTime          = UNSET_RL
       gchem_apresStartDate1         = 0
       gchem_apresStartDate2         = 0
       gchem_apres_exfremo_intercept = 0.0 _d 0
       gchem_apres_exfremo_slope     = 0.0 _d 0
       gchem_apresMask               = 'c'
       gchem_inscal_apres      = 1. _d 0

       READ(UNIT=iUnit, NML=GCHEM_FORCING_PARAMS)

#  ifdef USE_EXF_INTERPOLATION

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
        PAR_lat_inc(j)    = inp_dLat(j)
        iron_lat_inc(j)   = inp_dLat(j)
        apCO2_lat_inc(j)  = inp_dLat(j)
        wind_lat_inc(j)   = inp_dLat(j)
        apres_lat_inc(j)  = inp_dLat(j)
       ENDDO

       READ(UNIT=iUnit, NML=GCHEM_INTERP_PARAMS)

#  endif /* USE_EXF_INTERPOLATION */
C     gchem_useEXF
      ENDIF
# endif /* ALLOW_EXF */


      WRITE(msgBuf,'(A)')
     &  ' GCHEM_READPARMS: finished reading data.gchem'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C- Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      IF (gchem_apCO2file.EQ.' ') THEN
        WRITE(msgBuf,'(2A,E13.6,A)')
     &    '** WARNING ** GCHEM_READPARMS: ',
     &    'default atmos pCO2 of ', gchem_apCO2_const,' atm is used'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

      errCount = 0

#ifdef ALLOW_EXF
      IF ( gchem_useEXF .AND. useExfYearlyFields ) THEN
       IF ( gchem_parRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'GCHEM_READPARMS: The use of ',
     &   'useExfYearlyFields AND gchem_parRepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( gchem_FeRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'GCHEM_READPARMS: The use of ',
     &   'useExfYearlyFields AND gchem_FeRepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( gchem_SiRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'GCHEM_READPARMS: The use of ',
     &   'useExfYearlyFields AND gchem_SiRepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( gchem_apco2RepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'GCHEM_READPARMS: The use of ',
     &   'useExfYearlyFields AND gchem_apco2RepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( gchem_windRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'GCHEM_READPARMS: The use of ',
     &   'useExfYearlyFields AND gchem_windRepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( gchem_apresRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'GCHEM_READPARMS: The use of ',
     &   'useExfYearlyFields AND gchem_apresRepCycle is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
C     gchem_useEXF and useExfYearlyFields
      ENDIF
#endif /* ALLOW_EXF */

C- Check for retired parameters:
      IF ( tIter0 .NE. UNSET_I ) THEN
       WRITE(msgBuf,'(A,A)')
     &  'S/R GCHEM_READPARMS: Paramater "tIter0" is',
     &  ' no longer allowed in file "data.gchem"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R GCHEM_READPARMS: "tIter0" has been moved to',
     &  ' PTRACERS_Iter0 in file "data.ptracers".'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

C--   Stop if there was at least one error
      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &     'GCHEM_READPARMS: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R GCHEM_READPARMS'
      ENDIF

      _END_MASTER( myThid )

C  Everyone else must wait for the parameters to be loaded
      _BARRIER

#ifdef ALLOW_CFC
      IF ( useCFC ) THEN
        CALL CFC_READPARMS( myThid )
      ENDIF
#endif

#ifdef ALLOW_DIC
      IF ( useDIC ) THEN
        CALL DIC_READPARMS( myThid )
      ENDIF
#endif

#ifdef ALLOW_BLING
      IF ( useBLING ) THEN
        CALL BLING_READPARMS( myThid )
      ENDIF
#endif

#ifdef ALLOW_SPOIL
      IF ( useSPOIL ) THEN
        CALL SPOIL_READPARMS( myThid )
      ENDIF
#endif

#ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
        CALL DARWIN_READPARMS( myThid )
      ENDIF
#endif

C- Register GCHEM tracer indices
      CALL GCHEM_TR_REGISTER( myThid )

      RETURN
      END
