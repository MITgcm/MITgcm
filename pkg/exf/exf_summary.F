C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_summary.F,v 1.15 2007/05/14 19:34:57 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


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
#include "cal.h"
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
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// External forcing configuration >>> START <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

C--   Print general parameters:
      WRITE(msgBuf,'(A)') ' EXF general parameters:'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      CALL WRITE_0D_C( exf_yftype, 0, INDEX_NONE, 'exf_yftype = ',
     &  ' /* ? */')
      CALL WRITE_0D_I( exf_iprec, INDEX_NONE, 'exf_iprec =',
     &  ' /* exf file precision */')
      CALL WRITE_0D_L( useExfYearlyFields, INDEX_NONE,
     &  'useExfYearlyFields =',
     &  ' /* add extension _YEAR to input file names */')
      CALL WRITE_0D_L( twoDigitYear, INDEX_NONE, 'twoDigitYear =',
     &  ' /* use 2-digit year extension */')
      CALL WRITE_0D_L( useExfCheckRange, INDEX_NONE,
     &  'useExfCheckRange =',
     &  ' /* check for fields range */')
      CALL WRITE_0D_R8( exf_monFreq, INDEX_NONE, 'exf_monFreq =',
     &  ' /* EXF monitor frequency [ s ] */')
      CALL WRITE_0D_R8( repeatPeriod, INDEX_NONE, 'repeatPeriod =',
     &  ' /* period for cycling forcing dataset [ s ] */')
      CALL WRITE_0D_R8( climtempfreeze, INDEX_NONE,'climTempFreeze=',
     &  ' /* Minimum climatological temperature [deg.C] */')
      CALL WRITE_0D_R8( windStressMax, INDEX_NONE,'windStressMax =',
     &  ' /* Maximum absolute windstress [ Pa ] */')
      CALL WRITE_0D_L( stressIsOnCgrid,INDEX_NONE,'stressIsOnCgrid =',
     &  ' /* set u,v_stress on Arakawa C-grid */')
      CALL WRITE_0D_R8( cen2kel, INDEX_NONE, 'cen2kel =',
     &  ' /* conversion of deg. Centigrade to Kelvin [K] */')
      CALL WRITE_0D_R8( gravity_mks, INDEX_NONE, 'gravity_mks=',
     &  ' /* gravitational acceleration [m/s^2] */')
      CALL WRITE_0D_R8( atmrho, INDEX_NONE, 'atmrho =',
     & '  /* mean atmospheric density [kg/m^3] */')
      CALL WRITE_0D_R8( atmcp, INDEX_NONE, 'atmcp =',
     & '  /* mean atmospheric specific heat [J/kg/K] */')
      CALL WRITE_0D_R8( flamb, INDEX_NONE, 'flamb =',
     & '  /* latent heat of evaporation [J/kg] */')
      CALL WRITE_0D_R8( flami, INDEX_NONE, 'flami =',
     & '  /* latent heat of pure-ice melting [J/kg] */')
      CALL WRITE_0D_R8( cvapor_fac, INDEX_NONE, 'cvapor_fac =',
     &  ' /* const. for Saturation calculation [?] */')
      CALL WRITE_0D_R8( cvapor_exp, INDEX_NONE, 'cvapor_exp =',
     &  ' /* const. for Saturation calculation [?] */')
      CALL WRITE_0D_R8( cvapor_fac_ice, INDEX_NONE, 'cvapor_fac_ice=',
     &  ' /* const. for Saturation calculation [?] */')
      CALL WRITE_0D_R8( cvapor_exp_ice, INDEX_NONE, 'cvapor_exp_ice=',
     &  ' /* const. for Saturation calculation [?] */')
      CALL WRITE_0D_R8( humid_fac, INDEX_NONE, 'humid_fac =',
     &  ' /* humidity coef. in virtual temp. [(kg/kg)^-1] */')
      CALL WRITE_0D_R8( gamma_blk, INDEX_NONE, 'gamma_blk =',
     &  ' /* adiabatic lapse rate [?] */')
      CALL WRITE_0D_R8( saltsat, INDEX_NONE, 'saltsat =',
     &  ' /* reduction of Qsat over salty water [-] */')
      CALL WRITE_0D_R8( cdrag_1, INDEX_NONE, 'cDrag_1 =',
     &  ' /* coef used in drag calculation [?] */')
      CALL WRITE_0D_R8( cdrag_2, INDEX_NONE, 'cDrag_2 =',
     &  ' /* coef used in drag calculation [?] */')
      CALL WRITE_0D_R8( cdrag_3, INDEX_NONE, 'cDrag_3 =',
     &  ' /* coef used in drag calculation [?] */')
      CALL WRITE_0D_R8( cstanton_1, INDEX_NONE, 'cStanton_1 =',
     &  ' /* coef used in Stanton number calculation [?] */')
      CALL WRITE_0D_R8( cstanton_2, INDEX_NONE, 'cStanton_2 =',
     &  ' /* coef used in Stanton number calculation [?] */')
      CALL WRITE_0D_R8( cdalton, INDEX_NONE, 'cDalton =',
     &  ' /* coef used in Dalton number calculation [?] */')
      CALL WRITE_0D_R8( exf_scal_BulkCdn, INDEX_NONE,
     &  'exf_scal_BulkCdn=',
     &  ' /* Drag coefficient scaling factor [-] */')
      CALL WRITE_0D_R8( zolmin, INDEX_NONE, 'zolmin =',
     &  ' /* minimum stability parameter [?] */')
      CALL WRITE_0D_R8( psim_fac, INDEX_NONE, 'psim_fac =',
     &  ' /* coef used in turbulent fluxes calculation [-] */')
      CALL WRITE_0D_R8( zref, INDEX_NONE, 'zref =',
     & '  /* reference height [ m ] */')
      CALL WRITE_0D_R8( hu, INDEX_NONE, 'hu =',
     & '  /* height of mean wind [ m ] */')
      CALL WRITE_0D_R8( ht, INDEX_NONE, 'ht =',
     & '  /* height of mean temperature [ m ] */')
      CALL WRITE_0D_R8( hq, INDEX_NONE, 'hq =',
     & '  /* height of mean spec.humidity [ m ] */')
      CALL WRITE_0D_R8( umin, INDEX_NONE, 'uMin =',
     &  ' /* minimum wind speed [m/s] */')
      CALL WRITE_0D_L( useStabilityFct_overIce, INDEX_NONE,
     &  'useStabilityFct_overIce=',
     &  ' /* transfert Coeffs over sea-ice depend on stability */')
      CALL WRITE_0D_R8( exf_iceCd, INDEX_NONE, 'exf_iceCd =',
     &  ' /* drag coefficient over sea-ice (fixed) [-] */')
      CALL WRITE_0D_R8( exf_iceCe, INDEX_NONE, 'exf_iceCe =',
     &  ' /* transfert coeff. over sea-ice, for Evap (fixed) [-] */')
      CALL WRITE_0D_R8( exf_iceCh, INDEX_NONE, 'exf_iceCh =',
     &  ' /* transfert coeff. over sea-ice, Sens.Heat.(fixed)[-] */')
      CALL WRITE_0D_R8( exf_albedo, INDEX_NONE, 'exf_albedo =',
     & '  /* Sea-water albedo [-] */')
      CALL WRITE_0D_R8( ocean_emissivity, INDEX_NONE,
     &  'ocean_emissivity =',
     &  ' /* longwave ocean-surface emissivity [-] */')
      CALL WRITE_0D_R8( ice_emissivity, INDEX_NONE,'ice_emissivity =',
     &  ' /* longwave seaice emissivity [-] */')
      CALL WRITE_0D_R8(snow_emissivity, INDEX_NONE,'snow_emissivity =',
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

#ifdef ALLOW_ATM_TEMP
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_TEMP:                     defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_TEMP:                 NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_ATM_WIND
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_WIND:                     defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_WIND:                 NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_DOWNWARD_RADIATION
      write(msgbuf,'(a)')
     &'// ALLOW_DOWNWARD_RADIATION:           defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_DOWNWARD_RADIATION:       NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_BULKFORMULAE
      write(msgbuf,'(a)')
     &'// ALLOW_BULKFORMULAE:                 defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_BULKFORMULAE:             NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   For each data set used the summary prints the calendar data
C     and the corresponding file from which the data will be read.

#ifndef ALLOW_ATM_WIND
C--   Zonal wind stress.
      il = ilnblnk(ustressfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind stress forcing starts at         ',
     &    ustressstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind stress forcing period is         ',
     &    ustressperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Zonal wind stress forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',ustressfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

C--   Meridional wind stress.
      il = ilnblnk(vstressfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind stress forcing starts at    ',
     &    vstressstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind stress forcing period is    ',
     &    vstressperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Meridional wind stress forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',vstressfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifndef ALLOW_ATM_TEMP
C--   Heat flux.
      il = ilnblnk(hfluxfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Heat flux forcing starts at                ',
     &    hfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Heat flux forcing period is                 ',
     &    hfluxperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Heat flux forcing is read from file:        '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',hfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

C--   Salt flux.
      il = ilnblnk(sfluxfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Salt flux forcing starts at                 ',
     &    sfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Salt flux forcing period is                 ',
     &    sfluxperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Salt flux forcing is read from file:        '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',sfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
C--   Net shortwave.
      il = ilnblnk(swfluxfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Net shortwave flux forcing starts at      ',
     &    swfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Net shortwave flux forcing period is      ',
     &    swfluxperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Net shortwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',swfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_ATM_WIND
C--   Zonal wind.
      il = ilnblnk(uwindfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind forcing starts at                ',
     &    uwindstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind forcing period is                ',
     &    uwindperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Zonal wind forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',uwindfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

C--   Meridional wind.
      il = ilnblnk(vwindfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind forcing starts at           ',
     &    vwindstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind forcing period is           ',
     &    vwindperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Meridional wind forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',vwindfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_ATM_TEMP
C--   Atmospheric temperature.
      il = ilnblnk(atempfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric temperature starts at           ',
     &    atempstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric temperature period is           ',
     &    atempperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Atmospheric temperature is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',atempfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

C--   Atmospheric specific humidity.
      il = ilnblnk(aqhfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric specific humidity starts at     ',
     &    aqhstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric specific humidity period is     ',
     &    aqhperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Atmospheric specific humidity is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',aqhfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

C--   Net longwave.
      il = ilnblnk(lwfluxfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Net longwave flux forcing starts at       ',
     &    lwfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Net longwave flux forcing period is       ',
     &    lwfluxperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Net longwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',lwfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

C--   Precipitation.
      il = ilnblnk(precipfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Precipitation data set starts at            ',
     &    precipstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Precipitation data period is                ',
     &    precipperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Precipitation data is read from file:       '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',precipfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

C--   Evaporation.
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#ifdef EXF_READ_EVAP
      write(msgbuf,'(a)')
     &'// EXF_READ_EVAP:                      defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(evapfile)
      write(msgbuf,'(a,f12.0)')
     &'   Evaporation starts at     ',
     &    evapstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Evaporation period is     ',
     &    evapperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Evaporation is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',evapfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// EXF_READ_EVAP:                  NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

C--   Runoff.
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#ifdef ALLOW_RUNOFF
      write(msgbuf,'(a)')
     &'// ALLOW_RUNOFF:                       defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(runofffile)
      write(msgbuf,'(a,f12.0)')
     &'   Runnoff starts at     ',
     &    runoffstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Runoff period is     ',
     &    runoffperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Runoff is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',runofffile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else /* ALLOW_RUNOFF */
      write(msgbuf,'(a)')
     &'// ALLOW_RUNOFF:                   NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif /* ALLOW_RUNOFF */

#ifdef DOWNWARD_RADIATION
C--   Downward shortwave.
      il = ilnblnk(swdownfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Downward shortwave flux forcing starts at      ',
     &    swdownstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Downward shortwave flux forcing period is      ',
     &    swdownperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Downward shortwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',swdownfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

C--   Downward longwave.
      il = ilnblnk(lwdownfile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Downward longwave flux forcing starts at       ',
     &    lwdownstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Downward longwave flux forcing period is       ',
     &    lwdownperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Downward longwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',lwdownfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ATMOSPHERIC_LOADING
C--   Atmospheric pressure.
      il = ilnblnk(apressurefile)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric pressure forcing starts at      ',
     &    apressurestartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric pressure forcing period is      ',
     &    apressureperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Atmospheric pressureforcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',apressurefile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// External forcing configuration  >>> END <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// External forcing climatology configuration >>> START <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

C     For each data set used the summary prints the calendar data
C     and the corresponding file from which the data will be read.

#ifdef ALLOW_CLIMSST_RELAXATION
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSST_RELAXATION:           defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSST_RELAXATION:       NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSSS_RELAXATION:           defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSSS_RELAXATION:       NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
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
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SST starts at         ',
     &    climsststartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SST period is         ',
     &    climsstperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Climatological SST is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',climsstfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
C     Relaxation to SSS climatology.
      il = ilnblnk(climsssfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SSS starts at         ',
     &    climsssstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SSS period is         ',
     &    climsssperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Climatological SSS is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',climsssfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// External forcing climatology configuration  >>> END <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      _END_MASTER( myThid )

      RETURN
      END
