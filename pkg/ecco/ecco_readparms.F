C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_readparms.F,v 1.9 2005/05/23 19:28:45 heimbach Exp $

#include "COST_CPPOPTIONS.h"

      subroutine ecco_readparms( mythid )

c     ==================================================================
c     SUBROUTINE ecco_readparms
c     ==================================================================
c
c     o This routine initialises the package cost.
c     started: Ralf Giering 18-Jan-2001
c
c     ==================================================================
c     SUBROUTINE ecco_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"

#include "ecco_cost.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

c     == end of interface ==

c--   Read the namelist input.
      namelist /ecco_cost_nml/ 
     &                   data_errfile,
     &                   tbarfile, sbarfile, psbarfile,
     &                   ubarfile, vbarfile, wbarfile,
     &                   tauxbarfile, tauybarfile,
     &                   hfluxbarfile, sfluxbarfile,
     &                   using_topex,
     &                   topexstartdate1, topexstartdate2,
     &                   topexperiod, topexfile, topexmeanfile,
     &                   using_ers,
     &                   ersstartdate1, ersstartdate2,
     &                   ersperiod, ersfile,
     &                   scatstartdate1, scatstartdate2, 
     &                   scatperiod, scatxdatfile, scatydatfile,
     &                   scatx_errfile, scaty_errfile,
     &                   ssh_errfile,
     &                   tmistartdate1, tmistartdate2, tmidatfile,
     &                   sststartdate1, sststartdate2, sstdatfile, 
     &                   sssstartdate1, sssstartdate2, sssdatfile, 
     &                   tdatfile, sdatfile,
     &                   ctdtfile, ctdsfile,
     &                   ctdtclimfile, ctdsclimfile,
     &                   xbtfile,
     &                   argotstartdate1, argotstartdate2, argotfile,
     &                   argosstartdate1, argosstartdate2, argosfile,
     &                   udriftfile, vdriftfile,
     &                   udrifterrfile, vdrifterrfile,
     &                   curmtrufile,curmtrvfile,
     &                   salterrfile, temperrfile, velerrfile,
     &                   hflux_errfile, sflux_errfile,
     &                   tauu_errfile, tauv_errfile,
     &                   atemp_errfile, aqh_errfile,
     &                   uwind_errfile, vwind_errfile,
     &                   geoid_errfile, geoid_covariancefile,
     &                   mult_hflux, mult_sflux, mult_tauu, mult_tauv,
     &                   mult_atemp, mult_aqh, mult_uwind, mult_vwind,
     &                   mult_hmean, mult_h, mult_temp, mult_salt,
     &                   mult_tmi, mult_sst, mult_sss,
     &                   mult_ctdt, mult_ctds, 
     &                   mult_ctdtclim, mult_ctdsclim, 
     &                   mult_xbt, mult_drift,
     &                   mult_argot, mult_argos,
     &                   mult_scatx, mult_scaty,
     &                   mult_sdrift, mult_tdrift, mult_wdrift,
     &                   mult_temp0, mult_salt0,
     &                   mult_obcsn, mult_obcss,
     &                   mult_obcsw, mult_obcse, mult_obcsvol,
     &                   mult_curmtr, mult_ageos,
     &                   mult_ice,
     &                   mult_kapgm, mult_diffkr,
     &                   costIceStart1, costIceStart2,
     &                   costIceEnd1, costIceEnd2,
     &                   cost_ice_flag,
     &                   cost_iprec, cost_yftype

      _BEGIN_MASTER( myThid )

c--     Set default values.
        data_errfile          =    ' '
        tbarfile              =    'tbar'
        sbarfile              =    'sbar'
        psbarfile             =    'psbar'
        ubarfile              =    'ubar'
        vbarfile              =    'vbar'
        wbarfile              =    'wbar'
        tauxbarfile           =    'tauxbar'
        tauybarfile           =    'tauybar'
        hfluxbarfile          =    'hfluxbar'
        sfluxbarfile          =    'sfluxbar'
        using_topex           = .false.
        topexstartdate1       =      0
        topexstartdate2       =      0
        topexperiod           =      0. _d 0
        topexfile             =    ' '
        topexmeanfile         =    ' '
        using_ers             = .false.
        ersstartdate1         =      0
        ersstartdate2         =      0
        ersperiod             =      0. _d 0
        ersfile               =    ' '
        scatstartdate1        =      0
        scatstartdate2        =      0
        scatperiod            =      0. _d 0
        scatxdatfile          =    ' '
        scatydatfile          =    ' '
        ssh_errfile           =    ' '
        scatx_errfile         =    ' '
        scaty_errfile         =    ' '
        tmistartdate1         =      0
        tmistartdate2         =      0
        tmidatfile            =    ' '
        sststartdate1         =      0
        sststartdate2         =      0
        sstdatfile            =    ' '
        sssstartdate1         =      0
        sssstartdate2         =      0
        sssdatfile            =    ' '
        tdatfile              =    ' '
        sdatfile              =    ' '
        ctdtfile              =    ' '
        ctdsfile              =    ' '
        ctdtclimfile          =    ' '
        ctdsclimfile          =    ' '
        curmtrufile           =    ' '
        curmtrvfile           =    ' '
        xbtfile               =    ' '
        argotstartdate1       =      0
        argotstartdate2       =      0
        argotfile             =    ' '
        argosstartdate1       =      0
        argosstartdate2       =      0
        argosfile             =    ' '
        udriftfile            =    ' '
        vdriftfile            =    ' '
        udrifterrfile         =    ' '
        vdrifterrfile         =    ' '
        salterrfile           =    ' '
        temperrfile           =    ' '
        velerrfile            =    ' '
        hflux_errfile         =    ' '
        sflux_errfile         =    ' '
        tauu_errfile          =    ' '
        tauv_errfile          =    ' '
        atemp_errfile         =    ' '
        aqh_errfile           =    ' '
        uwind_errfile         =    ' '
        vwind_errfile         =    ' '
        geoid_errfile         =    ' '
        geoid_covariancefile  =    ' '
        mult_hflux            =      0. _d 0
        mult_sflux            =      0. _d 0
        mult_tauu             =      0. _d 0
        mult_tauv             =      0. _d 0
        mult_atemp            =      0. _d 0
        mult_aqh              =      0. _d 0
        mult_uwind            =      0. _d 0
        mult_vwind            =      0. _d 0
        mult_hmean            =      0. _d 0
        mult_h                =      0. _d 0
        mult_temp             =      0. _d 0
        mult_salt             =      0. _d 0
        mult_temp0            =      0. _d 0
        mult_salt0            =      0. _d 0
        mult_tmi              =      0. _d 0
        mult_sst              =      0. _d 0
        mult_sss              =      0. _d 0
        mult_ctdt             =      0. _d 0
        mult_ctds             =      0. _d 0
        mult_ctdtclim         =      0. _d 0
        mult_ctdsclim         =      0. _d 0
        mult_xbt              =      0. _d 0
        mult_argot            =      0. _d 0
        mult_argos            =      0. _d 0
        mult_drift            =      0. _d 0
        mult_tdrift           =      0. _d 0
        mult_sdrift           =      0. _d 0
        mult_wdrift           =      0. _d 0
        mult_scatx            =      0. _d 0
        mult_scaty            =      0. _d 0
        mult_obcsn            =      0. _d 0
        mult_obcss            =      0. _d 0
        mult_obcsw            =      0. _d 0
        mult_obcse            =      0. _d 0
        mult_obcsvol          =      0. _d 0
        mult_ageos            =      0. _d 0
        mult_curmtr           =      0. _d 0
        mult_ice              =      0. _d 0
        mult_kapgm            =      0. _d 0
        mult_diffkr           =      0. _d 0
        costIceStart1         =      0
        costIceStart2         =      0
        costIceEnd1           =      0
        costIceEnd2           =      0
        cost_ice_flag         =      1

c--     Next, read the cost data file.
        WRITE(msgBuf,'(A)') 'ECCO_READPARMS: opening data.ecco'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.ecco', 'ECCO_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(  iUnit, nml = ecco_cost_nml )

        WRITE(msgBuf,'(A)') 'ECCO_READPARMS: finished reading data.ecco'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , 1)

        CLOSE( iUnit )

#ifdef ALLOW_CAL
c--     Get the complete dates of the ...
c--     ... SST data.
        call cal_FullDate( sststartdate1,   sststartdate2,
     &                     sststartdate,    mythid )
c--     ... TMI data.
        call cal_FullDate( tmistartdate1,   tmistartdate2,
     &                     tmistartdate,    mythid )
c--     ... SSS data.
        call cal_FullDate( sssstartdate1,   sssstartdate2,
     &                     sssstartdate,    mythid )     
c--     ... T/P data.
        call cal_FullDate( topexstartdate1, topexstartdate2,
     &                     topexstartdate,  mythid )
c--     ... ERS data.
        call cal_FullDate( ersstartdate1,   ersstartdate2,
     &                     ersstartdate,    mythid )
c--     ... SCAT data.
        call cal_FullDate( scatstartdate1,   scatstartdate2,
     &                     scatxstartdate,    mythid )
        call cal_FullDate( scatstartdate1,   scatstartdate2,   
     &                     scatystartdate,    mythid )
#endif

      _END_MASTER( mythid )

      _BARRIER

      end
