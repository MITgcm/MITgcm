C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_summary.F,v 1.34 2014/06/05 15:37:46 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

C--  File exf_summary.F: Routines to print out EXF settings
C--   Contents
C--   o EXF_SUMMARY
C--   o EXF_PRINT_INTERP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXF_SUMMARY( myThid )

c     ==================================================================
c     SUBROUTINE exf_summary
c     ==================================================================
c
c     o List all the settings of the external forcing.
c
c     started: Christian Eckert eckert@mit.edu 11-Jan-1999
c
c     changed: Christian Eckert eckert@mit.edu 12-Feb-2000
c              - changed routine names (package prefix: exf_)
c
c     changed: Patrick Heimbach heimbach@mit.edu 04-May-2000
c              - changed the handling of precip and sflux with respect
c                to CPP options ALLOW_BULKFORMULAE and ALLOW_ATM_TEMP
c
c     changed: Dimitris Menemenlis menemenlis@jpl.nasa.gov 20-Dec-2002
c              - modifications for using pkg/exf with pkg/seaice
c
c     ==================================================================
c     SUBROUTINE exf_summary
c     ==================================================================

      implicit none

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "EXF_CONSTANTS.h"
#include "EXF_PARAM.h"

C     == routine arguments ==

C     myThid  - thread number for this instance of the routine.

      integer myThid

C     == local variables ==

      integer il

      character*(max_len_mbuf) msgbuf

C     == external ==

      integer  ilnblnk
      external ilnblnk

C     == end of interface ==

      _BEGIN_MASTER( myThid )

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// External forcing (EXF) configuration >>> START <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
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
      CALL WRITE_0D_I( exf_debugLev, INDEX_NONE, 'exf_debugLev =',
     &  ' /* select EXF-debug printing level */')
      CALL WRITE_0D_RL( exf_monFreq, INDEX_NONE, 'exf_monFreq  =',
     &  ' /* EXF monitor frequency [ s ] */')
      CALL WRITE_0D_RL( repeatPeriod, INDEX_NONE, 'repeatPeriod =',
     &  ' /* period for cycling forcing dataset [ s ] */')
      CALL WRITE_0D_RL( climtempfreeze, INDEX_NONE,'climTempFreeze=',
     &  ' /* Minimum climatological temperature [deg.C] */')
      CALL WRITE_0D_RL( windStressMax, INDEX_NONE,'windStressMax =',
     &  ' /* Maximum absolute windstress [ Pa ] */')
      CALL WRITE_0D_L( stressIsOnCgrid,INDEX_NONE,'stressIsOnCgrid =',
     &  ' /* set u,v_stress on Arakawa C-grid */')
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
     &  ' /* coef used in drag calculation [?] */')
      CALL WRITE_0D_RL( cdrag_2, INDEX_NONE, 'cDrag_2 =',
     &  ' /* coef used in drag calculation [?] */')
      CALL WRITE_0D_RL( cdrag_3, INDEX_NONE, 'cDrag_3 =',
     &  ' /* coef used in drag calculation [?] */')
      CALL WRITE_0D_RL( cstanton_1, INDEX_NONE, 'cStanton_1 =',
     &  ' /* coef used in Stanton number calculation [?] */')
      CALL WRITE_0D_RL( cstanton_2, INDEX_NONE, 'cStanton_2 =',
     &  ' /* coef used in Stanton number calculation [?] */')
      CALL WRITE_0D_RL( cdalton, INDEX_NONE, 'cDalton =',
     &  ' /* coef used in Dalton number calculation [?] */')
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
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Print settings of some CPP flags.
      WRITE(msgBuf,'(A)') ' EXF main CPP flags:'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
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
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_TEMP:                     defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#else
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_TEMP:                 NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

      IF ( useAtmWind ) THEN
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_WIND (useAtmWind):        defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      ELSE
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_WIND (useAtmWind):    NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      ENDIF

#ifdef ALLOW_DOWNWARD_RADIATION
      write(msgbuf,'(a)')
     &'// ALLOW_DOWNWARD_RADIATION:           defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#else
      write(msgbuf,'(a)')
     &'// ALLOW_DOWNWARD_RADIATION:       NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

#ifdef ALLOW_BULKFORMULAE
      write(msgbuf,'(a)')
     &'// ALLOW_BULKFORMULAE:                 defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#else
      write(msgbuf,'(a)')
     &'// ALLOW_BULKFORMULAE:             NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   For each data set used the summary prints the calendar data
C     and the corresponding file from which the data will be read.

      IF ( .NOT.useAtmWind ) THEN
C--   Zonal wind stress.
      il = ilnblnk(ustressfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind stress forcing starts at         ',
     &    ustressstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind stress forcing period is         ',
     &    ustressperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Zonal wind stress forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',ustressfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'ustress',
     & ustress_lon0, ustress_lon_inc, ustress_lat0, ustress_lat_inc,
     & ustress_nlon, ustress_nlat, ustress_interpMethod, myThid )
#endif

C--   Meridional wind stress.
      il = ilnblnk(vstressfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind stress forcing starts at    ',
     &    vstressstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind stress forcing period is    ',
     &    vstressperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Meridional wind stress forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',vstressfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'vstress',
     & vstress_lon0, vstress_lon_inc, vstress_lat0, vstress_lat_inc,
     & vstress_nlon, vstress_nlat, vstress_interpMethod, myThid )
      WRITE(msgBuf,'(2A,L5)') '    Interp. U & V comp. together:',
     &                  ' uvInterp_stress =', uvInterp_stress
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif
      ENDIF

#ifndef ALLOW_ATM_TEMP
C--   Heat flux.
      il = ilnblnk(hfluxfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Heat flux forcing starts at                ',
     &    hfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Heat flux forcing period is                 ',
     &    hfluxperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Heat flux forcing is read from file:        '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',hfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'hflux',
     & hflux_lon0, hflux_lon_inc, hflux_lat0, hflux_lat_inc,
     & hflux_nlon, hflux_nlat, hflux_interpMethod, myThid )
#endif

C--   Salt flux.
      il = ilnblnk(sfluxfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Salt flux forcing starts at                 ',
     &    sfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Salt flux forcing period is                 ',
     &    sfluxperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Salt flux forcing is read from file:        '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',sfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'sflux',
     & sflux_lon0, sflux_lon_inc, sflux_lat0, sflux_lat_inc,
     & sflux_nlon, sflux_nlat, sflux_interpMethod, myThid )
#endif
#endif /* ndef ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
C--   Net shortwave.
      il = ilnblnk(swfluxfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Net shortwave flux forcing starts at      ',
     &    swfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Net shortwave flux forcing period is      ',
     &    swfluxperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Net shortwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',swfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'swflux',
     & swflux_lon0, swflux_lon_inc, swflux_lat0, swflux_lat_inc,
     & swflux_nlon, swflux_nlat, swflux_interpMethod, myThid )
#endif
#endif /* ALLOW_ATM_TEMP or SHORTWAVE_HEATING */

      IF ( useAtmWind ) THEN
C--   Zonal wind.
      il = ilnblnk(uwindfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind forcing starts at                ',
     &    uwindstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind forcing period is                ',
     &    uwindperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Zonal wind forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',uwindfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'uwind',
     & uwind_lon0, uwind_lon_inc, uwind_lat0, uwind_lat_inc,
     & uwind_nlon, uwind_nlat, uwind_interpMethod, myThid )
#endif

C--   Meridional wind.
      il = ilnblnk(vwindfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind forcing starts at           ',
     &    vwindstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind forcing period is           ',
     &    vwindperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Meridional wind forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',vwindfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'vwind',
     & vwind_lon0, vwind_lon_inc, vwind_lat0, vwind_lat_inc,
     & vwind_nlon, vwind_nlat, vwind_interpMethod, myThid )
      WRITE(msgBuf,'(2A,L5)') '    Interp. U & V comp. together:',
     &                     ' uvInterp_wind =', uvInterp_wind
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif
      ENDIF

#ifdef ALLOW_ATM_TEMP
C--   Atmospheric temperature.
      il = ilnblnk(atempfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric temperature starts at           ',
     &    atempstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric temperature period is           ',
     &    atempperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Atmospheric temperature is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',atempfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'atemp',
     & atemp_lon0, atemp_lon_inc, atemp_lat0, atemp_lat_inc,
     & atemp_nlon, atemp_nlat, atemp_interpMethod, myThid )
#endif

C--   Atmospheric specific humidity.
      il = ilnblnk(aqhfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric specific humidity starts at     ',
     &    aqhstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric specific humidity period is     ',
     &    aqhperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Atmospheric specific humidity is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',aqhfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'aqh',
     & aqh_lon0, aqh_lon_inc, aqh_lat0, aqh_lat_inc,
     & aqh_nlon, aqh_nlat, aqh_interpMethod, myThid )
#endif

C--   Net longwave.
      il = ilnblnk(lwfluxfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Net longwave flux forcing starts at       ',
     &    lwfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Net longwave flux forcing period is       ',
     &    lwfluxperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Net longwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',lwfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'lwflux',
     & lwflux_lon0, lwflux_lon_inc, lwflux_lat0, lwflux_lat_inc,
     & lwflux_nlon, lwflux_nlat, lwflux_interpMethod, myThid )
#endif

C--   Precipitation.
      il = ilnblnk(precipfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Precipitation data set starts at            ',
     &    precipstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Precipitation data period is                ',
     &    precipperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Precipitation data is read from file:       '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',precipfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'precip',
     & precip_lon0, precip_lon_inc, precip_lat0, precip_lat_inc,
     & precip_nlon, precip_nlat, precip_interpMethod, myThid )
#endif
#endif /* ALLOW_ATM_TEMP */

C--   Evaporation.
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef EXF_READ_EVAP
      write(msgbuf,'(a)')
     &'// EXF_READ_EVAP:                      defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      il = ilnblnk(evapfile)
      write(msgbuf,'(a,f12.0)')
     &'   Evaporation starts at     ',
     &    evapstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Evaporation period is     ',
     &    evapperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Evaporation is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',evapfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'evap',
     & evap_lon0, evap_lon_inc, evap_lat0, evap_lat_inc,
     & evap_nlon, evap_nlat, evap_interpMethod, myThid )
#endif
#else /* EXF_READ_EVAP */
      write(msgbuf,'(a)')
     &'// EXF_READ_EVAP:                  NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#endif /* EXF_READ_EVAP */

C--   Runoff.
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef ALLOW_RUNOFF
      write(msgbuf,'(a)')
     &'// ALLOW_RUNOFF:                       defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      il = ilnblnk(runofffile)
      write(msgbuf,'(a,f12.0)')
     &'   Runoff starts at     ',
     &    runoffstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Runoff period is     ',
     &    runoffperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Runoff is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',runofffile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
# ifdef ALLOW_RUNOFTEMP
      write(msgbuf,'(a)')
     &'// ALLOW_RUNOFTEMP:                   defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      il = ilnblnk(runoftempfile)
      write(msgbuf,'(a)')
     &'   Runoff temp. is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',runoftempfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
# else /* ALLOW_RUNOFTEMP */
      write(msgbuf,'(a)')
     &'// ALLOW_RUNOFTEMP:                NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
# endif /* ALLOW_RUNOFTEMP */
# ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'runoff',
     & runoff_lon0, runoff_lon_inc, runoff_lat0, runoff_lat_inc,
     & runoff_nlon, runoff_nlat, runoff_interpMethod, myThid )
# endif /* USE_EXF_INTERPOLATION */
#else /* ALLOW_RUNOFF */
      write(msgbuf,'(a)')
     &'// ALLOW_RUNOFF:                   NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#endif /* ALLOW_RUNOFF */

#ifdef ALLOW_DOWNWARD_RADIATION
C--   Downward shortwave.
      il = ilnblnk(swdownfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Downward shortwave flux forcing starts at      ',
     &    swdownstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Downward shortwave flux forcing period is      ',
     &    swdownperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Downward shortwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',swdownfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'swdown',
     & swdown_lon0, swdown_lon_inc, swdown_lat0, swdown_lat_inc,
     & swdown_nlon, swdown_nlat, swdown_interpMethod, myThid )
#endif

C--   Downward longwave.
      il = ilnblnk(lwdownfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Downward longwave flux forcing starts at       ',
     &    lwdownstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Downward longwave flux forcing period is       ',
     &    lwdownperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Downward longwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',lwdownfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'lwdown',
     & lwdown_lon0, lwdown_lon_inc, lwdown_lat0, lwdown_lat_inc,
     & lwdown_nlon, lwdown_nlat, lwdown_interpMethod, myThid )
#endif
#endif /* ALLOW_DOWNWARD_RADIATION */

#ifdef ATMOSPHERIC_LOADING
C--   Atmospheric pressure.
      il = ilnblnk(apressurefile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric pressure forcing starts at      ',
     &    apressurestartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric pressure forcing period is      ',
     &    apressureperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Atmospheric pressureforcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',apressurefile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'apressure', apressure_lon0,
     & apressure_lon_inc, apressure_lat0, apressure_lat_inc,
     & apressure_nlon, apressure_nlat, apressure_interpMethod, myThid )
#endif
#endif /* ATMOSPHERIC_LOADING */

#ifdef EXF_SEAICE_FRACTION
C--   fractional ice-covered area MASK.
      il = ilnblnk(areamaskfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   fractional ice-covered area MASK starts at      ',
     &    areamaskstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   fractional ice-covered area MASK period is      ',
     &    areamaskperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   fractional ice-covered area MASK is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',areamaskfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'areamask', areamask_lon0,
     & areamask_lon_inc, areamask_lat0, areamask_lat_inc,
     & areamask_nlon, areamask_nlat, areamask_interpMethod, myThid )
#endif
#endif /* EXF_SEAICE_FRACTION */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// External forcing (EXF) climatology configuration :'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )

C     For each data set used the summary prints the calendar data
C     and the corresponding file from which the data will be read.

#ifdef ALLOW_CLIMSST_RELAXATION
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSST_RELAXATION:           defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#else
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSST_RELAXATION:       NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSSS_RELAXATION:           defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#else
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSSS_RELAXATION:       NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

C     The climatological data sets are assumed to contain monthly
C     data. This can be changed in a later version to an arbitrary
C     number of intervals during a given year.

#ifdef ALLOW_CLIMSST_RELAXATION
C     Relaxation to SST climatology.
      il = ilnblnk(climsstfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SST starts at         ',
     &    climsststartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SST period is         ',
     &    climsstperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Climatological SST is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',climsstfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'climsst',
     & climsst_lon0, climsst_lon_inc, climsst_lat0, climsst_lat_inc,
     & climsst_nlon, climsst_nlat, climsst_interpMethod, myThid )
#endif
#endif /* ALLOW_CLIMSST_RELAXATION */

#ifdef ALLOW_CLIMSSS_RELAXATION
C     Relaxation to SSS climatology.
      il = ilnblnk(climsssfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SSS starts at         ',
     &    climsssstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SSS period is         ',
     &    climsssperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'   Climatological SSS is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,a)')
     &'   >>  ',climsssfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef USE_EXF_INTERPOLATION
      CALL EXF_PRINT_INTERP( 'climsss',
     & climsss_lon0, climsss_lon_inc, climsss_lat0, climsss_lat_inc,
     & climsss_nlon, climsss_nlat, climsss_interpMethod, myThid )
#endif
#endif /* ALLOW_CLIMSSS_RELAXATION */

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// External forcing (EXF) configuration  >>> END <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )

      _END_MASTER( myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef USE_EXF_INTERPOLATION

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
