
#include "CTRL_OPTIONS.h"

cph #ifdef TARGET_CRAY_VECTOR
cph #  define NML_OPTIM ECCO_OPTIM
cph #  define NML_ECCO  ECCO_PARMS
cph #else
cph #  define NML_OPTIM NML=ECCO_OPTIM
cph #  define NML_ECCO  NML=ECCO_PARMS
cph #endif

c     ==================================================================
c
c     optim.F: Routines for doing an off-line optimization after the
c              ECCO forward and adjoint model have been run.
c
c     numbmod       - Returns number of variables.
c     simul         - Mid-level routine that calls the model and its
c                     adjoint
c     model         - Forward model.
c     admodel       - Modified forward model and adjoint model.
c     initmod       - Initialisation routine.
c     postmod       - Final routine that prints results.
c
c
c     Documentation:
c
c     The collection of these routines originated mainly from Ralf
c     Giering. Patrick Heimbach improved and corrected some parts of
c     the original code. Christian Eckert contributed the interface
c     to the ECCO release of the MITgcmUV in order to get the off-
c     line version going. The on-line optimisation uses a simple
c     example, whereas the off-line version deals with the ECCO
c     release output. The off-line version can, of course, only
c     do one optimization step at a time.
c
c     started:  Christian Eckert  eckert@mit.edu  15-Feb-2000
c
c               - On-line and off-line capability and some cosmetic
c                 changes.
c
c     changed:  Patrick Heimbach heimbach@mit.edu 19-Jun-2000
c               - finished, revised and debugged
c
c     ==================================================================


      subroutine optim_numbmod(
     O                    nn
     &                  )

c     ==================================================================
c     SUBROUTINE optim_numbmod
c     ==================================================================
c
c     o Set the number of control variables.
c
c     started: Christian Eckert eckert@mit.edu 15-Feb-2000
c
c     changed: Christian Eckert eckert@mit.edu 09-Mar-2000
c
c              - Added ECCO layout.
c
c     changed:  Patrick Heimbach heimbach@mit.edu 19-Jun-2000
c               - finished, revised and debugged
c
c     ==================================================================
c     SUBROUTINE optim_numbmod
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "ctrl.h"
#include "optim.h"
#include "minimization.h"
#include "CTRL_OBCS.h"
#ifndef ECCO_CTRL_DEPRECATED
# include "ctrl_local_params.h"
#endif

c     == routine arguments ==

      integer nn

c     == local variables ==

      integer il
      integer errio

      _RL            ff

#if defined (DYNAMIC)
      _RL            vv(nn)
#elif defined (USE_POINTER) || (MAX_INDEPEND == 0)
      _RL            vv
      pointer (pvv,vv(1))
#else
      integer nmax
      parameter( nmax = MAX_INDEPEND )
      _RL   vv(nmax)
#endif

      character*(max_len_prec) record

      logical lheaderonly

c     == external ==

      integer  ilnblnk

c     == end of interface ==

      namelist /CTRL_NML/
     &  xx_theta_file, xx_salt_file,
     &  xx_hflux_file, xx_hflux_remo_intercept, xx_hflux_remo_slope,
     &  xx_hfluxstartdate1, xx_hfluxstartdate2, xx_hfluxperiod,
     &  xx_sflux_file, xx_sflux_remo_intercept, xx_sflux_remo_slope,
     &  xx_sfluxstartdate1, xx_sfluxstartdate2, xx_sfluxperiod,
     &  xx_tauu_file, xx_tauu_remo_intercept, xx_tauu_remo_slope,
     &  xx_tauustartdate1,  xx_tauustartdate2,  xx_tauuperiod,
     &  xx_tauv_file, xx_tauv_remo_intercept, xx_tauv_remo_slope,
     &  xx_tauvstartdate1,  xx_tauvstartdate2,  xx_tauvperiod,
     &  xx_atemp_file, xx_atemp_remo_intercept, xx_atemp_remo_slope,
     &  xx_atempstartdate1, xx_atempstartdate2, xx_atempperiod,
     &  xx_aqh_file, xx_aqh_remo_intercept, xx_aqh_remo_slope,
     &  xx_aqhstartdate1, xx_aqhstartdate2, xx_aqhperiod,
     &  xx_precip_file, xx_precip_remo_intercept, xx_precip_remo_slope,
     &  xx_precipstartdate1, xx_precipstartdate2, xx_precipperiod,
     &  xx_swflux_file, xx_swflux_remo_intercept, xx_swflux_remo_slope,
     &  xx_swfluxstartdate1, xx_swfluxstartdate2, xx_swfluxperiod,
     &  xx_swdown_file, xx_swdown_remo_intercept, xx_swdown_remo_slope,
     &  xx_swdownstartdate1, xx_swdownstartdate2, xx_swdownperiod,
     &  xx_lwflux_file, xx_lwflux_remo_intercept, xx_lwflux_remo_slope,
     &  xx_lwfluxstartdate1, xx_lwfluxstartdate2, xx_lwfluxperiod,
     &  xx_lwdown_file, xx_lwdown_remo_intercept, xx_lwdown_remo_slope,
     &  xx_lwdownstartdate1, xx_lwdownstartdate2, xx_lwdownperiod,
     &  xx_evap_file, xx_evap_remo_intercept, xx_evap_remo_slope,
     &  xx_evapstartdate1, xx_evapstartdate2, xx_evapperiod,
     &  xx_snowprecip_file, xx_snowprecip_remo_intercept, 
     &  xx_snowprecip_remo_slope,  xx_snowprecipperiod,
     &  xx_snowprecipstartdate1, xx_snowprecipstartdate2,
     &  xx_apressure_file, xx_apressure_remo_intercept, 
     &  xx_apressure_remo_slope, xx_apressureperiod,
     &  xx_apressurestartdate1, xx_apressurestartdate2,
     &  xx_runoff_file, xx_runoff_remo_intercept, xx_runoff_remo_slope,
     &  xx_runoffstartdate1, xx_runoffstartdate2, xx_runoffperiod,
     &  xx_uwind_file, xx_uwind_remo_intercept, xx_uwind_remo_slope,
     &  xx_uwindstartdate1, xx_uwindstartdate2, xx_uwindperiod,
     &  xx_vwind_file, xx_vwind_remo_intercept, xx_vwind_remo_slope,
     &  xx_vwindstartdate1, xx_vwindstartdate2, xx_vwindperiod,
     &  xx_obcsn_file, xx_obcss_file, xx_obcsw_file, xx_obcse_file,
     &  xx_obcsnstartdate1,  xx_obcsnstartdate2,  xx_obcsnperiod,
     &  xx_obcssstartdate1,  xx_obcssstartdate2,  xx_obcssperiod,
     &  xx_obcswstartdate1,  xx_obcswstartdate2,  xx_obcswperiod,
     &  xx_obcsestartdate1,  xx_obcsestartdate2,  xx_obcseperiod,
     &  xx_diffkr_file, xx_kapgm_file, xx_tr1_file,
     &  xx_sst_file, xx_sss_file,
     &  xx_sststartdate1, xx_sststartdate2, xx_sstperiod,
     &  xx_sssstartdate1, xx_sssstartdate2, xx_sssperiod,
     &  xx_depth_file, xx_efluxy_file, xx_efluxp_file,
     &  xx_bottomdrag_file, xx_edtaux_file, xx_edtauy_file,
     &  xx_uvel_file, xx_vvel_file, xx_etan_file,
     &  xx_shifwflx_file, 
     &  xx_shifwflx_remo_intercept, xx_shifwflx_remo_slope, 
     &  xx_shifwflxstartdate1, xx_shifwflxstartdate2, xx_shifwflxperiod,
     &  doInitXX, doPackDiag, doZscaleUnpack, doZscalePack,
     &  doMainUnpack, doMainPack, doAdmtlmBypassAD, delZexp,
     &  xx_hfluxm_file, doSinglePrecTapelev

      namelist /CTRL_PACKNAMES/
     &  yadmark, yctrlid, yctrlposunpack, yctrlpospack,
     &  ctrlname, costname, scalname, maskname, metaname

      namelist /OPTIM/
     &                      optimcycle,
     &                      numiter, nfunc, fmin, iprint,
     &                      epsf, epsx, epsg,
     &                      nupdate, eps

c--   Preset the optimization parameters.
      optimcycle =  0
      nvars      =  0
      numiter    =  1
      nfunc      =  1
      fmin       =  0.0
      iprint     =  10
      epsx       =  1.e-6
      epsg       =  1.e-6
      eps        = -1.e-6
      nupdate    =  1
      ff         = 0.
cdfer      expId      = 'MIT_CE_000'
      yctrlid    = 'MIT_CE_000'

      modeldataunit = 14
      scrunit1      = 11

c--   Read control parameters from file.
      open(unit=scrunit1,status='scratch')

      open(unit = modeldataunit,file = 'data.ctrl',
     &     status = 'old', iostat = errio)
      if ( errio .lt. 0 ) then
         stop ' stopped in optim_numbmod'
      endif

      do while ( .true. )
         read(modeldataunit, fmt='(a)', end=21) record
         il = max(ilnblnk(record),1)
         if ( record(1:1) .ne. commentcharacter )
     &        write(unit=scrunit1, fmt='(a)') record(:il)
      enddo
   21 continue
      close( modeldataunit )

      rewind( scrunit1 )
      read(unit = scrunit1, nml = ctrl_nml)
      read(unit = scrunit1, nml = ctrl_packnames)
      close( scrunit1 )
      print*, ' OPTIM_NUMBMOD: Control options have been read.'

cph(
cdfer      expId = yctrlid
cph)

c--   Read optimization parameters from file.
      open(unit=scrunit1,status='scratch')

      open(unit = modeldataunit,file = 'data.optim',
     &     status = 'old', iostat = errio)
      if ( errio .lt. 0 ) then
         stop ' stopped in optim_numbmod'
      endif

      do while ( .true. )
         read(modeldataunit, fmt='(a)', end=22) record
         il = max(ilnblnk(record),1)
         if ( record(1:1) .ne. commentcharacter )
     &        write(unit=scrunit1, fmt='(a)') record(:il)
      enddo
   22 continue
      close( modeldataunit )

      rewind( scrunit1 )
      read(unit = scrunit1, nml = optim)
      close( scrunit1 )
      print*, ' OPTIM_NUMBMOD: Minimization options have been read.'

      if (eps .gt. 0.0) then
        epsf = eps
        epsx = eps
        epsg = eps
      endif

      lheaderonly = .true.
      call optim_readdata ( nn, ctrlname, lheaderonly, ff, vv)

c--   Do some final printout.
      print*
      print*, ' OPTIM_NUMBMOD: Iteration number = ', optimcycle
      print*, ' number of control variables     = ', nn
      print*, ' Data will be read from the following files:'
      print*

ce    --> data.err file in case dimensional i/o is used.
ce    --> scaling  file in case dimensional i/o is used.

      return
      end
