#include "EXF_OPTIONS.h"

C--  File exf_summary.F: Routines to print out EXF settings
C--   Contents
C--   o EXF_SUMMARY
C--   o EXF_FLD_SUMMARY
C--   o EXF_PRINT_INTERP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXF_SUMMARY( myThid )
C     !INTERFACE:

      SUBROUTINE EXF_SUMMARY( myThid )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE exf_summary
C     ==================================================================
C
C     o List all the settings of the external forcing.
C
C     started: Christian Eckert eckert@mit.edu 11-Jan-1999
C
C     changed: Christian Eckert eckert@mit.edu 12-Feb-2000
C              - changed routine names (package prefix: exf_)
C
C     changed: Patrick Heimbach heimbach@mit.edu 04-May-2000
C              - changed the handling of precip and sflux with respect
C                to CPP options ALLOW_BULKFORMULAE and ALLOW_ATM_TEMP
C
C     changed: Dimitris Menemenlis menemenlis@jpl.nasa.gov 20-Dec-2002
C              - modifications for using pkg/exf with pkg/seaice
C
C     ==================================================================
C     SUBROUTINE exf_summary
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_INTERP_SIZE.h"
#include "EXF_INTERP_PARAM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid  :: My thread Id number
      INTEGER myThid

C     !FUNCTIONS:
#ifdef ALLOW_RUNOFTEMP
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER iL
#endif

C     !LOCAL VARIABLES:
      LOGICAL prtBlkLn, addBlkLn
      CHARACTER*1 blkLin
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BEGIN_MASTER( myThid )

      blkLin = ' '
      CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// External forcing (EXF) configuration >>> START <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Print general parameters:
      WRITE(msgBuf,'(A)') ' EXF general parameters:'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      CALL WRITE_0D_I( exf_iprec, INDEX_NONE, 'exf_iprec =',
     &  ' /* exf file precision */')
      CALL WRITE_0D_L( useExfYearlyFields, INDEX_NONE,
     &  'useExfYearlyFields =',
     &  ' /* add extension _YEAR to input file names */')
      CALL WRITE_0D_L( twoDigitYear, INDEX_NONE, 'twoDigitYear =',
     &  ' /* use 2-digit year extension */')
      CALL WRITE_0D_L( useExfCheckRange, INDEX_NONE,
     &  'useExfCheckRange =', ' /* check for fields range */')
#ifdef USE_EXF_INTERPOLATION
      CALL WRITE_0D_L( exf_output_interp, INDEX_NONE,
     &  'exf_output_interp =',
     &  ' /* output directly interpolation result */')
#endif
      CALL WRITE_0D_L( diags_opOceWeighted, INDEX_NONE,
     &  'diags_opOceWeighted =',
     &  ' /* weight flux diags by open-ocean fraction */')
      CALL WRITE_0D_I( exf_debugLev, INDEX_NONE, 'exf_debugLev =',
     &  ' /* select EXF-debug printing level */')
      CALL WRITE_0D_RL( exf_monFreq, INDEX_NONE, 'exf_monFreq  =',
     &  ' /* EXF monitor frequency [ s ] */')
      CALL WRITE_0D_I( exf_adjMonSelect, INDEX_NONE,
     &  'exf_adjMonSelect =',
     &  ' /* select group of exf AD-variables to monitor */')
      CALL WRITE_0D_RL( repeatPeriod, INDEX_NONE, 'repeatPeriod =',
     &  ' /* period for cycling forcing dataset [ s ] */')
      CALL WRITE_0D_RL( climtempfreeze, INDEX_NONE,'climTempFreeze=',
     &  ' /* Minimum climatological temperature [deg.C] */')
      CALL WRITE_0D_RL( windStressMax, INDEX_NONE,'windStressMax =',
     &  ' /* Maximum absolute windstress [ Pa ] */')
      CALL WRITE_0D_L( stressIsOnCgrid,INDEX_NONE,'stressIsOnCgrid =',
     &  ' /* set u,v_stress on Arakawa C-grid */')
      CALL WRITE_0D_L( rotateStressOnAgrid,INDEX_NONE,
     &  'rotateStressOnAgrid =',
     &  ' /* rotate u,v_stress on Arakawa A-grid */')
      CALL WRITE_0D_RL( cen2kel, INDEX_NONE, 'cen2kel =',
     &  ' /* conversion of deg. Centigrade to Kelvin [K] */')
      CALL WRITE_0D_RL( gravity_mks, INDEX_NONE, 'gravity_mks=',
     &  ' /* gravitational acceleration [m/s^2] */')
      CALL WRITE_0D_RL( atmrho, INDEX_NONE, 'atmrho =',
     & '  /* mean atmospheric density [kg/m^3] */')
      CALL WRITE_0D_RL( atmcp, INDEX_NONE, 'atmcp =',
     & '  /* mean atmospheric specific heat [J/kg/K] */')
      CALL WRITE_0D_RL( flamb, INDEX_NONE, 'flamb =',
     & '  /* latent heat of evaporation [J/kg] */')
      CALL WRITE_0D_RL( flami, INDEX_NONE, 'flami =',
     & '  /* latent heat of pure-ice melting [J/kg] */')
      CALL WRITE_0D_RL( cvapor_fac, INDEX_NONE, 'cvapor_fac =',
     &  ' /* const. for Saturation calculation [?] */')
      CALL WRITE_0D_RL( cvapor_exp, INDEX_NONE, 'cvapor_exp =',
     &  ' /* const. for Saturation calculation [?] */')
      CALL WRITE_0D_RL( cvapor_fac_ice, INDEX_NONE, 'cvapor_fac_ice=',
     &  ' /* const. for Saturation calculation [?] */')
      CALL WRITE_0D_RL( cvapor_exp_ice, INDEX_NONE, 'cvapor_exp_ice=',
     &  ' /* const. for Saturation calculation [?] */')
      CALL WRITE_0D_RL( humid_fac, INDEX_NONE, 'humid_fac =',
     &  ' /* humidity coef. in virtual temp. [(kg/kg)^-1] */')
      CALL WRITE_0D_RL( gamma_blk, INDEX_NONE, 'gamma_blk =',
     &  ' /* adiabatic lapse rate [?] */')
      CALL WRITE_0D_RL( saltsat, INDEX_NONE, 'saltsat =',
     &  ' /* reduction of Qsat over salty water [-] */')
      CALL WRITE_0D_L( noNegativeEvap,INDEX_NONE,'noNegativeEvap =',
     &  ' /* prevent negative Evaporation */')
      CALL WRITE_0D_RL( sstExtrapol, INDEX_NONE, 'sstExtrapol =',
     &  ' /* extrapolation coeff from lev. 1 & 2 to surf [-] */')
      CALL WRITE_0D_RL( cdrag_1, INDEX_NONE, 'cDrag_1 =',
     &  ' /* coef used in drag calculation [m/s] */')
      CALL WRITE_0D_RL( cdrag_2, INDEX_NONE, 'cDrag_2 =',
     &  ' /* coef used in drag calculation [-] */')
      CALL WRITE_0D_RL( cdrag_3, INDEX_NONE, 'cDrag_3 =',
     &  ' /* coef used in drag calculation [s/m] */')
      CALL WRITE_0D_RL( cdrag_8, INDEX_NONE, 'cDrag_8 =',
     &  ' /* coef used in drag calculation [(s/m)^6] */')
      CALL WRITE_0D_RL( cdragMax, INDEX_NONE, 'cDragMax =',
     &  ' /* maximum drag (Large and Yeager, 2009) [-] */')
      CALL WRITE_0D_RL( umax, INDEX_NONE, 'umax =',
     &  ' /* at maximum wind (Large and Yeager, 2009) [m/s] */')
      CALL WRITE_0D_RL( cstanton_1, INDEX_NONE, 'cStanton_1 =',
     &  ' /* coef used in Stanton number calculation [-] */')
      CALL WRITE_0D_RL( cstanton_2, INDEX_NONE, 'cStanton_2 =',
     &  ' /* coef used in Stanton number calculation [-] */')
      CALL WRITE_0D_RL( cdalton, INDEX_NONE, 'cDalton =',
     &  ' /* Dalton number [-] */')
      CALL WRITE_0D_RL( exf_scal_BulkCdn, INDEX_NONE,
     &  'exf_scal_BulkCdn=',
     &  ' /* Drag coefficient scaling factor [-] */')
      CALL WRITE_0D_RL( zolmin, INDEX_NONE, 'zolmin =',
     &  ' /* minimum stability parameter [?] */')
      CALL WRITE_0D_RL( psim_fac, INDEX_NONE, 'psim_fac =',
     &  ' /* coef used in turbulent fluxes calculation [-] */')
      CALL WRITE_0D_RL( zref, INDEX_NONE, 'zref =',
     & '  /* reference height [ m ] */')
      CALL WRITE_0D_RL( hu, INDEX_NONE, 'hu =',
     & '  /* height of mean wind [ m ] */')
      CALL WRITE_0D_RL( ht, INDEX_NONE, 'ht =',
     & '  /* height of mean temperature [ m ] */')
      CALL WRITE_0D_RL( hq, INDEX_NONE, 'hq =',
     & '  /* height of mean spec.humidity [ m ] */')
      CALL WRITE_0D_RL( umin, INDEX_NONE, 'uMin =',
     &  ' /* minimum wind speed [m/s] */')
      CALL WRITE_0D_L( useStabilityFct_overIce, INDEX_NONE,
     &  'useStabilityFct_overIce=',
     &  ' /* transfert Coeffs over sea-ice depend on stability */')
      CALL WRITE_0D_RL( exf_iceCd, INDEX_NONE, 'exf_iceCd =',
     &  ' /* drag coefficient over sea-ice (fixed) [-] */')
      CALL WRITE_0D_RL( exf_iceCe, INDEX_NONE, 'exf_iceCe =',
     &  ' /* transfert coeff. over sea-ice, for Evap (fixed) [-] */')
      CALL WRITE_0D_RL( exf_iceCh, INDEX_NONE, 'exf_iceCh =',
     &  ' /* transfert coeff. over sea-ice, Sens.Heat.(fixed)[-] */')
      CALL WRITE_0D_RL( exf_albedo, INDEX_NONE, 'exf_albedo =',
     & '  /* Sea-water albedo [-] */')
      CALL WRITE_0D_L( useExfZenAlbedo, INDEX_NONE, 'useExfZenAlbedo =',
     &  ' /* Sea-water albedo varies with zenith angle */')
      CALL WRITE_0D_I( select_ZenAlbedo,INDEX_NONE,'select_ZenAlbedo =',
     &  ' /* Sea-water albedo computation method */')
      CALL WRITE_0D_L( useExfZenIncoming, INDEX_NONE,
     &  'useExfZenIncoming =',' /* compute incoming solar radiation */')
      CALL WRITE_0D_RL( ocean_emissivity, INDEX_NONE,
     &  'ocean_emissivity =',
     &  ' /* longwave ocean-surface emissivity [-] */')
      CALL WRITE_0D_RL( ice_emissivity, INDEX_NONE,'ice_emissivity =',
     &  ' /* longwave seaice emissivity [-] */')
      CALL WRITE_0D_RL(snow_emissivity, INDEX_NONE,'snow_emissivity =',
     &  ' /* longwave snow  emissivity [-] */')
      CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Print settings of some CPP flags.
      WRITE(msgBuf,'(A)') ' EXF main CPP flags:'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

#ifdef USE_EXF_INTERPOLATION
      WRITE(msgBuf,'(A)')
     &'// USE_EXF_INTERPOLATION:              defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
#else
      WRITE(msgBuf,'(A)')
     &'// USE_EXF_INTERPOLATION:          NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
#endif

#ifdef ALLOW_ATM_TEMP
      WRITE(msgBuf,'(A)')
     &'// ALLOW_ATM_TEMP:                     defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#else
      WRITE(msgBuf,'(A)')
     &'// ALLOW_ATM_TEMP:                 NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

      IF ( useAtmWind ) THEN
      WRITE(msgBuf,'(A)')
     &'// ALLOW_ATM_WIND (useAtmWind):        defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      ELSE
      WRITE(msgBuf,'(A)')
     &'// ALLOW_ATM_WIND (useAtmWind):    NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      ENDIF

#ifdef ALLOW_DOWNWARD_RADIATION
      WRITE(msgBuf,'(A)')
     &'// ALLOW_DOWNWARD_RADIATION:           defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#else
      WRITE(msgBuf,'(A)')
     &'// ALLOW_DOWNWARD_RADIATION:       NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

#ifdef ALLOW_BULKFORMULAE
      WRITE(msgBuf,'(A)')
     &'// ALLOW_BULKFORMULAE:                 defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#else
      WRITE(msgBuf,'(A)')
     &'// ALLOW_BULKFORMULAE:             NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   For each data set used the summary prints the calendar data
C     and the corresponding file from which the data will be read.
      prtBlkLn = .FALSE.
      addBlkLn = .TRUE.

C--   Zonal wind stress.
      IF ( .NOT.useAtmWind .AND. ustressfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Zonal wind stress forcing',
     I      ustressfile, ustressRepCycle, ustressperiod,
     I      ustressStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'ustress',
     &  ustress_lon0, ustress_lon_inc, ustress_lat0, ustress_lat_inc,
     &  ustress_nlon, ustress_nlat, ustress_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

C--   Meridional wind stress.
      IF ( .NOT.useAtmWind .AND. vstressfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Meridional wind stress forcing',
     I      vstressfile, vstressRepCycle, vstressperiod,
     I      vstressStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'vstress',
     &  vstress_lon0, vstress_lon_inc, vstress_lat0, vstress_lat_inc,
     &  vstress_nlon, vstress_nlat, vstress_interpMethod, myThid )
       WRITE(msgBuf,'(2A,L5)') '    Interp. U & V comp. together:',
     &                   ' uvInterp_stress =', uvInterp_stress
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

C--   Heat flux.
      IF ( hfluxfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Heat flux forcing',
     I      hfluxfile, hfluxRepCycle, hfluxperiod,
     I      hfluxStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'hflux',
     &  hflux_lon0, hflux_lon_inc, hflux_lat0, hflux_lat_inc,
     &  hflux_nlon, hflux_nlat, hflux_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

C--   Fresh-Water flux.
      IF ( sfluxfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Fresh-Water flux forcing',
     I      sfluxfile, sfluxRepCycle, sfluxperiod,
     I      sfluxStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'sflux',
     &  sflux_lon0, sflux_lon_inc, sflux_lat0, sflux_lat_inc,
     &  sflux_nlon, sflux_nlat, sflux_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
C--   Net shortwave.
      IF ( swfluxfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Net shortwave flux forcing',
     I      swfluxfile, swfluxRepCycle, swfluxperiod,
     I      swfluxStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'swflux',
     &  swflux_lon0, swflux_lon_inc, swflux_lat0, swflux_lat_inc,
     &  swflux_nlon, swflux_nlat, swflux_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF
#endif /* ALLOW_ATM_TEMP or SHORTWAVE_HEATING */

C--   Zonal wind.
      IF ( useAtmWind .AND. uwindfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Zonal wind forcing',
     I      uwindfile, uwindRepCycle, uwindperiod,
     I      uwindStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'uwind',
     &  uwind_lon0, uwind_lon_inc, uwind_lat0, uwind_lat_inc,
     &  uwind_nlon, uwind_nlat, uwind_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

C--   Meridional wind.
      IF ( useAtmWind .AND. vwindfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Meridional wind forcing',
     I      vwindfile, vwindRepCycle, vwindperiod,
     I      vwindStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'vwind',
     &  vwind_lon0, vwind_lon_inc, vwind_lat0, vwind_lat_inc,
     &  vwind_nlon, vwind_nlat, vwind_interpMethod, myThid )
       WRITE(msgBuf,'(2A,L5)') '    Interp. U & V comp. together:',
     &                      ' uvInterp_wind =', uvInterp_wind
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

C--   Surface wind speed
      IF ( wspeedfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Surface wind speed',
     I      wspeedfile, wspeedRepCycle, wspeedperiod,
     I      wspeedStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'wspeed',
     &  wspeed_lon0, wspeed_lon_inc, wspeed_lat0, wspeed_lat_inc,
     &  wspeed_nlon, wspeed_nlat, wspeed_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

#ifdef ALLOW_ATM_TEMP
C--   Atmospheric temperature.
      IF ( atempfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Atmospheric temperature',
     I      atempfile, atempRepCycle, atempperiod,
     I      atempStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'atemp',
     &  atemp_lon0, atemp_lon_inc, atemp_lat0, atemp_lat_inc,
     &  atemp_nlon, atemp_nlat, atemp_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

C--   Atmospheric specific humidity.
      IF ( aqhfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Atmospheric specific humidity',
     I      aqhfile, aqhRepCycle, aqhperiod,
     I      aqhStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'aqh',
     &  aqh_lon0, aqh_lon_inc, aqh_lat0, aqh_lat_inc,
     &  aqh_nlon, aqh_nlat, aqh_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

C--   Turbulent heat flues
      IF ( prtBlkLn ) THEN
       CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       prtBlkLn = .FALSE.
      ENDIF
#ifdef ALLOW_READ_TURBFLUXES
      WRITE(msgBuf,'(A)')
     &'// ALLOW_READ_TURBFLUXES:              defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Sensible heat flux
      IF ( hs_file.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Sensible heat flux (+=down)',
     I      hs_file, hs_RepCycle, hs_period,
     I      hs_StartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'hs',
     &  hs_lon0, hs_lon_inc, hs_lat0, hs_lat_inc,
     &  hs_nlon, hs_nlat, hs_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

C--   Latent heat flux
      IF ( hl_file.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Latent heat flux (+=down)',
     I      hl_file, hl_RepCycle, hl_period,
     I      hl_StartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'hl',
     &  hl_lon0, hl_lon_inc, hl_lat0, hl_lat_inc,
     &  hl_nlon, hl_nlat, hl_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

#else /* ALLOW_READ_TURBFLUXES */
      WRITE(msgBuf,'(A)')
     &'// ALLOW_READ_TURBFLUXES:          NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif /* ALLOW_READ_TURBFLUXES */

C--   Net longwave.
      IF ( lwfluxfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Net longwave flux forcing',
     I      lwfluxfile, lwfluxRepCycle, lwfluxperiod,
     I      lwfluxStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'lwflux',
     &  lwflux_lon0, lwflux_lon_inc, lwflux_lat0, lwflux_lat_inc,
     &  lwflux_nlon, lwflux_nlat, lwflux_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF
#endif /* ALLOW_ATM_TEMP */

C--   Evaporation.
      IF ( prtBlkLn ) THEN
       CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       prtBlkLn = .FALSE.
      ENDIF
#ifdef EXF_READ_EVAP
      WRITE(msgBuf,'(A)')
     &'// EXF_READ_EVAP:                      defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
# ifdef ALLOW_ATM_TEMP
      IF ( evapfile.NE.' ' ) THEN
       addBlkLn = .FALSE.
       CALL EXF_FLD_SUMMARY( 'Evaporation',
     I      evapfile, evapRepCycle, evapperiod,
     I      evapStartTime, useExfYearlyFields, addBlkLn, myThid )
       addBlkLn = .TRUE.
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'evap',
     &  evap_lon0, evap_lon_inc, evap_lat0, evap_lat_inc,
     &  evap_nlon, evap_nlat, evap_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF
# endif /* ALLOW_ATM_TEMP */
#else /* EXF_READ_EVAP */
      WRITE(msgBuf,'(A)')
     &'// EXF_READ_EVAP:                  NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif /* EXF_READ_EVAP */

#ifdef ALLOW_ATM_TEMP
C--   Precipitation.
      IF ( precipfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Precipitation data',
     I      precipfile, precipRepCycle, precipperiod,
     I      precipStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'precip',
     &  precip_lon0, precip_lon_inc, precip_lat0, precip_lat_inc,
     &  precip_nlon, precip_nlat, precip_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

C--   Snow Precipitation
      IF ( snowprecipfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Snow Precipitation data',
     I      snowprecipfile, snowprecipRepCycle, snowprecipperiod,
     I      snowprecipStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'snowprecip',
     &  snowprecip_lon0, snowprecip_lon_inc, snowprecip_lat0,
     &  snowprecip_lat_inc, snowprecip_nlon, snowprecip_nlat,
     &  snowprecip_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF
#endif /* ALLOW_ATM_TEMP */

C--   Runoff.
      IF ( prtBlkLn ) THEN
       CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       prtBlkLn = .FALSE.
      ENDIF
#ifdef ALLOW_RUNOFF
      WRITE(msgBuf,'(A)')
     &'// ALLOW_RUNOFF:                       defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      IF ( runofffile.NE.' ' ) THEN
       addBlkLn = .FALSE.
       CALL EXF_FLD_SUMMARY( 'Runoff data',
     I      runofffile, runoffRepCycle, runoffperiod,
     I      runoffStartTime, useExfYearlyFields, addBlkLn, myThid )
       addBlkLn = .TRUE.
# ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'runoff',
     &  runoff_lon0, runoff_lon_inc, runoff_lat0, runoff_lat_inc,
     &  runoff_nlon, runoff_nlat, runoff_interpMethod, myThid )
# endif /* USE_EXF_INTERPOLATION */
       prtBlkLn = .TRUE.
      ENDIF
      IF ( prtBlkLn ) THEN
       CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       prtBlkLn = .FALSE.
      ENDIF
# ifdef ALLOW_RUNOFTEMP
      WRITE(msgBuf,'(A)')
     &'// ALLOW_RUNOFTEMP:                    defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      IF ( runoftempfile.NE.' ' ) THEN
c      addBlkLn = .FALSE.
c      CALL EXF_FLD_SUMMARY( 'Runoff temp.',
c    I      runoftempfile, runoffRepCycle, runoffperiod,
c    I      runoffStartTime, useExfYearlyFields, addBlkLn, myThid )
c      addBlkLn = .TRUE.
       iL = ILNBLNK(runoftempfile)
       WRITE(msgBuf,'(A)')
     & '   Runoff temp. is read from file:'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(3A)') '   >> ', runoftempfile(1:iL), ' <<'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       prtBlkLn = .TRUE.
      ENDIF
# else /* ALLOW_RUNOFTEMP */
      WRITE(msgBuf,'(A)')
     &'// ALLOW_RUNOFTEMP:                NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
# endif /* ALLOW_RUNOFTEMP */
#else /* ALLOW_RUNOFF */
      WRITE(msgBuf,'(A)')
     &'// ALLOW_RUNOFF:                   NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif /* ALLOW_RUNOFF */

C--   Salt-Flux
      IF ( prtBlkLn ) THEN
       CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       prtBlkLn = .FALSE.
      ENDIF
#ifdef ALLOW_SALTFLX
      WRITE(msgBuf,'(A)')
     &'// ALLOW_SALTFLX:                      defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      IF ( saltflxfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Net upward Salt-Flux forcing',
     I      saltflxfile, saltflxRepCycle, saltflxperiod,
     I      saltflxStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'saltflx',
     &  saltflx_lon0, saltflx_lon_inc, saltflx_lat0, saltflx_lat_inc,
     &  saltflx_nlon, saltflx_nlat, saltflx_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF
#else /* ALLOW_SALTFLX */
      WRITE(msgBuf,'(A)')
     &'// ALLOW_SALTFLX:                  NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif /* ALLOW_SALTFLX */

#ifdef ALLOW_DOWNWARD_RADIATION
C--   Downward shortwave.
      IF ( swdownfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Downward shortwave flux',
     I      swdownfile, swdownRepCycle, swdownperiod,
     I      swdownStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'swdown',
     &  swdown_lon0, swdown_lon_inc, swdown_lat0, swdown_lat_inc,
     &  swdown_nlon, swdown_nlat, swdown_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF

C--   Downward longwave.
      IF ( lwdownfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Downward longwave flux',
     I      lwdownfile, lwdownRepCycle, lwdownperiod,
     I      lwdownStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'lwdown',
     &  lwdown_lon0, lwdown_lon_inc, lwdown_lat0, lwdown_lat_inc,
     &  lwdown_nlon, lwdown_nlat, lwdown_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF
#endif /* ALLOW_DOWNWARD_RADIATION */

#ifdef ATMOSPHERIC_LOADING
C--   Atmospheric pressure.
      IF ( apressurefile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Atmospheric pressure forcing',
     I      apressurefile, apressureRepCycle, apressureperiod,
     I      apressureStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'apressure', apressure_lon0,
     &  apressure_lon_inc, apressure_lat0, apressure_lat_inc,
     &  apressure_nlon, apressure_nlat, apressure_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF
#endif /* ATMOSPHERIC_LOADING */

#ifdef EXF_ALLOW_TIDES
C--   Tidal geopotential
      IF ( tidePotFile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Tidal geopotential',
     I      tidePotFile, tidePotRepCycle, tidePotPeriod,
     I      tidePotStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'tidePot', tidePot_lon0,
     &  tidePot_lon_inc, tidePot_lat0, tidePot_lat_inc,
     &  tidePot_nlon, tidePot_nlat, tidePot_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF
#endif /* EXF_ALLOW_TIDES */

#ifdef EXF_SEAICE_FRACTION
C--   Fractional ice-covered area
      IF ( areamaskfile.NE.' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Fractional ice-covered area',
     I      areamaskfile, areamaskRepCycle, areamaskperiod,
     I      areamaskStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'areamask', areamask_lon0,
     &  areamask_lon_inc, areamask_lat0, areamask_lat_inc,
     &  areamask_nlon, areamask_nlat, areamask_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ENDIF
#endif /* EXF_SEAICE_FRACTION */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// External forcing (EXF) climatology configuration :'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      prtBlkLn = .FALSE.
      addBlkLn = .FALSE.

C     For each data set used the summary prints the calendar data
C     and the corresponding file from which the data will be read.

C     The climatological data sets are assumed to contain monthly
C     data. This can be changed in a later version to an arbitrary
C     number of intervals during a given year.

#ifdef ALLOW_CLIMSST_RELAXATION
      WRITE(msgBuf,'(A)')
     &'// ALLOW_CLIMSST_RELAXATION:           defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C     Relaxation to SST climatology.
      IF ( climsstfile .NE. ' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Climatological SST',
     I      climsstfile, climsstRepCycle, climsstperiod,
     I      climsstStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'climsst',
     &  climsst_lon0, climsst_lon_inc, climsst_lat0, climsst_lat_inc,
     &  climsst_nlon, climsst_nlat, climsst_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ELSE
       WRITE(msgBuf,'(A)') '   climsst relaxation is NOT used'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
#else /* ALLOW_CLIMSST_RELAXATION */
      WRITE(msgBuf,'(A)')
     &'// ALLOW_CLIMSST_RELAXATION:       NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif /* ALLOW_CLIMSST_RELAXATION */

c     IF ( prtBlkLn ) THEN
       CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
c      prtBlkLn = .FALSE.
c     ENDIF

#ifdef ALLOW_CLIMSSS_RELAXATION
      WRITE(msgBuf,'(A)')
     &'// ALLOW_CLIMSSS_RELAXATION:           defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C     Relaxation to SSS climatology.
      IF ( climsssfile .NE. ' ' ) THEN
       CALL EXF_FLD_SUMMARY( 'Climatological SSS',
     I      climsssfile, climsssRepCycle, climsssperiod,
     I      climsssStartTime, useExfYearlyFields, addBlkLn, myThid )
#ifdef USE_EXF_INTERPOLATION
       CALL EXF_PRINT_INTERP( 'climsss',
     &  climsss_lon0, climsss_lon_inc, climsss_lat0, climsss_lat_inc,
     &  climsss_nlon, climsss_nlat, climsss_interpMethod, myThid )
#endif
       prtBlkLn = .TRUE.
      ELSE
       WRITE(msgBuf,'(A)') '   climsss relaxation is NOT used'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
#else /* ALLOW_CLIMSSS_RELAXATION */
      WRITE(msgBuf,'(A)')
     &'// ALLOW_CLIMSSS_RELAXATION:       NOT defined'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif /* ALLOW_CLIMSSS_RELAXATION */

      CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// External forcing (EXF) configuration  >>> END <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _END_MASTER( myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXF_FLD_SUMMARY
C     !INTERFACE:
      SUBROUTINE EXF_FLD_SUMMARY(
     I               fld_fullName, fld_file,
     I               fld_repeatCycle, fld_period, fld_start_time,
     I               useYearlyFields, addBlkLn, myThid )

C     !DESCRIPTION:
C     Print EXF timing parameters for one EXF input field

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     fld_fullName    :: field full name description (shorter than 32c)
C     fld_file        :: file-name for this field
C     fld_repeatCycle :: time duration of a repeating cycle
C     fld_period      :: time period (in sec) between 2 reccords
C     fld_start_time  :: corresponding starting time (in sec) for this field
C     useYearlyFields :: select if using EXF Yearly-fields or not
C     addBlkLn        :: print blank line before parameter summary
C     myThid          :: My Thread Id number
      CHARACTER*(*) fld_fullName
      CHARACTER*(*) fld_file
      _RL fld_repeatCycle
      _RL fld_period
      _RL fld_start_time
      LOGICAL useYearlyFields
      LOGICAL addBlkLn
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER iL, jL
      CHARACTER*(MAX_LEN_MBUF) tmpBuf, msgBuf
      CHARACTER*1 blkLin
CEOP

      blkLin = ' '
      jL = 47

      IF ( addBlkLn ) THEN
       CALL PRINT_MESSAGE( blkLin, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( fld_period.GT.0. .OR.
     &     (fld_period.EQ.-1. .AND. .NOT.useYearlyFields) ) THEN
       WRITE(tmpBuf,'(3A)') '   ', fld_fullName, ' starts at'
       WRITE(msgBuf,'(A,F12.0)') tmpBuf(1:jL), fld_start_time
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
       WRITE(tmpBuf,'(3A)') '   ', fld_fullName, ' period is'
       WRITE(msgBuf,'(A,F12.0)') tmpBuf(1:jL), fld_period
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
c     IF ( fld_period.GT.0. .AND. .NOT.useYearlyFields ) THEN
      IF ( fld_period.GT.0. ) THEN
       iL = ILNBLNK(fld_fullName)
       IF ( iL.LT.28 ) THEN
         WRITE(tmpBuf,'(3A)') '   ', fld_fullName, ' repeat-cycle is'
       ELSE
         WRITE(tmpBuf,'(3A)') '   ', fld_fullName, ' rep-cycle is'
       ENDIF
       WRITE(msgBuf,'(A,F12.0)') tmpBuf(1:jL), fld_repeatCycle
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
       WRITE(msgBuf,'(3A)') '   ', fld_fullName, ' is read from file:'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       iL = ILNBLNK(fld_file)
       WRITE(msgBuf,'(3A)') '   >> ', fld_file(1:iL), ' <<'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

      RETURN
      END

#ifdef USE_EXF_INTERPOLATION
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXF_PRINT_INTERP
C     !INTERFACE:
      SUBROUTINE EXF_PRINT_INTERP( var_name,
     I               var_lon0, var_lon_inc, var_lat0, var_lat_inc,
     I               var_nlon, var_nlat, var_interpMethod, myThid )

C     !DESCRIPTION:
C     Print EXF interpolation parameters for one EXF input field

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     myThid :: My Thread Id number
      CHARACTER*(*) var_name
      _RL     var_lon0, var_lon_inc, var_lat0
      _RL     var_lat_inc(*)
      INTEGER var_nlon, var_nlat, var_interpMethod
      INTEGER myThid

C     !FUNCTIONS:
c     INTEGER  ILNBLNK
c     EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER i
      _RL var_min, var_max
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF ( var_interpMethod.EQ.0 ) THEN
        WRITE(msgBuf,'(3X,A,A,A)')
     &    'assume "',var_name,'" on model-grid (no interpolation)'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ELSE
        WRITE(msgBuf,'(3X,A,A,A,I3,A)')
     &    'interpolate "',var_name,'" (method=',var_interpMethod,' ):'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
C-      print input-field longitude position:
        IF ( ABS(var_lon0).LT.1000. .AND. var_lon_inc.GT.-10.
     &                              .AND. var_lon_inc.LT.100. ) THEN
          WRITE(msgBuf,'(3X,A,F10.5,A,I6,A,F10.7)')
     &    'lon0=', var_lon0, ', nlon=', var_nlon,
     &    ', lon_inc=',var_lon_inc
        ELSE
          WRITE(msgBuf,'(3X,A,1PE10.3,A,I6,A,1PE10.3)')
     &    'lon0=', var_lon0, ', nlon=', var_nlon,
     &    ', lon_inc=',var_lon_inc
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
C-      print input-field latitude position:
        var_min = var_lat_inc(1)
        var_max = var_lat_inc(1)
        DO i=1,var_nlat-1
          var_min = MIN( var_lat_inc(i), var_min )
          var_max = MAX( var_lat_inc(i), var_max )
        ENDDO
        IF ( ABS(var_lat0).LT.1000. .AND. var_min.GT.-10.
     &                              .AND. var_max.LT.100. ) THEN
         IF ( var_min.EQ.var_max ) THEN
          WRITE(msgBuf,'(3X,A,F10.5,A,I6,A,F10.7)')
     &    'lat0=', var_lat0, ', nlat=', var_nlat,
     &    ', lat_inc=', var_min
         ELSE
          WRITE(msgBuf,'(3X,A,F10.5,A,I6,A,2F8.5)')
     &    'lat0=', var_lat0, ', nlat=', var_nlat,
     &    ', inc(min,max)=', var_min, var_max
         ENDIF
        ELSE
         IF ( var_min.EQ.var_max ) THEN
          WRITE(msgBuf,'(3X,A,1PE10.3,A,I6,A,1PE10.3)')
     &    'lat0=', var_lat0, ', nlat=', var_nlat,
     &    ', lat_inc=', var_min
         ELSE
          WRITE(msgBuf,'(3X,A,1PE10.3,A,I6,A,1P2E10.3)')
     &    'lat0=', var_lat0, ', nlat=', var_nlat,
     &    ', inc(min,max)=', var_min, var_max
         ENDIF
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF

      RETURN
      END
#endif /* USE_EXF_INTERPOLATION */
