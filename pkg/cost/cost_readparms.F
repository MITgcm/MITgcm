C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_readparms.F,v 1.2 2001/07/13 13:37:45 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_readparms(
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE cost_readparms
c     ==================================================================
c
c     o This routine initialises the package cost.
c
c     started: Ralf Giering 18-Jan-2001
c
c     ==================================================================
c     SUBROUTINE cost_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "cost.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer sststartdate1
      integer sststartdate2
      integer topexstartdate1
      integer topexstartdate2
      integer ersstartdate1
      integer ersstartdate2

c     == external functions ==

      integer  cal_IntMonths
      external cal_IntMonths
      integer  cal_IntDays
      external cal_IntDays

c     == end of interface ==

c--   Read the namelist input.
      namelist /cost_nml/ 
     &                   data_errfile,
     &                   tbarfile, sbarfile, psbarfile,
     &                   using_topex,
     &                   topexstartdate1, topexstartdate2,
     &                   topexperiod, topexfile, topexmeanfile,
     &                   using_ers,
     &                   ersstartdate1, ersstartdate2,
     &                   ersperiod, ersfile,
     &                   ssh_errfile,
     &                   sststartdate1, sststartdate2, sstdatfile,
     &                   tdatfile, sdatfile,
     &                   ctdtfile, ctdsfile,
     &                   hflux_errfile, sflux_errfile,
     &                   tauu_errfile, tauv_errfile,
     &                   geoid_errfile, geoid_covariancefile,
     &                   mult_hq,    mult_hs, mult_tauu, mult_tauv,
     &                   mult_hmean, mult_h,  mult_temp, mult_salt,
     &                   mult_sst, mult_atl, mult_ctdt, mult_ctds, 
     &                   mult_test, mult_tracer,
     &                   iprec_cost, yftype_cost

      _BEGIN_MASTER( myThid )

c--     Set default values.
        data_errfile          =    ' '
        tbarfile              =    ' '
        sbarfile              =    ' '
        psbarfile             =    ' '
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
        ssh_errfile           =    ' '
        sststartdate1         =      0
        sststartdate2         =      0
        sstdatfile            =    ' '
        tdatfile              =    ' '
        sdatfile              =    ' '
        ctdtfile              =    ' '
        ctdsfile              =    ' '
        hflux_errfile         =    ' '
        sflux_errfile         =    ' '
        tauu_errfile          =    ' '
        tauv_errfile          =    ' '
        geoid_errfile         =    ' '
        geoid_covariancefile  =    ' '
        mult_hq               =      0. _d 0
        mult_hs               =      0. _d 0
        mult_tauu             =      0. _d 0
        mult_tauv             =      0. _d 0
        mult_hmean            =      0. _d 0
        mult_h                =      0. _d 0
        mult_temp             =      0. _d 0
        mult_salt             =      0. _d 0
        mult_sst              =      0. _d 0
        mult_atl              =      0. _d 0
        mult_ctdt             =      0. _d 0
        mult_ctds             =      0. _d 0
        mult_test             =      0. _d 0
        mult_tracer           =      0. _d 0

c--     Check versions.

c--     Next, read the cost data file.
        call nml_filter( 'data.cost', scrunit1, myThid )
        if (scrunit1 .eq. 0) then
           stop 'cost_readparms: reading namelist failed'
        end if
        read(  scrunit1, nml = cost_nml )
        close( scrunit1 )

        call cost_Summary( mythid )

      _END_MASTER( mythid )

      _BARRIER

      end
