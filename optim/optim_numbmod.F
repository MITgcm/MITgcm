
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

#include "ecco.h"
#include "ctrl.h"
#include "optim.h"
#include "minimization.h"

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
      external ilnblnk

c     == end of interface ==

      namelist /CTRL_NML/
     &  xx_theta_file,
     &  xx_salt_file,
     &  xx_hflux_file,
     &  xx_hfluxstartdate1, xx_hfluxstartdate2, xx_hfluxperiod,
     &  xx_sflux_file,
     &  xx_sfluxstartdate1, xx_sfluxstartdate2, xx_sfluxperiod,
     &  xx_tauu_file,
     &  xx_tauustartdate1,  xx_tauustartdate2,  xx_tauuperiod,
     &  xx_tauv_file,
     &  xx_tauvstartdate1,  xx_tauvstartdate2,  xx_tauvperiod,
     &  xx_atemp_file,
     &  xx_atempstartdate1, xx_atempstartdate2, xx_atempperiod,
     &  xx_aqh_file,
     &  xx_aqhstartdate1, xx_aqhstartdate2, xx_aqhperiod,
     &  xx_uwind_file,
     &  xx_uwindstartdate1, xx_uwindstartdate2, xx_uwindperiod,
     &  xx_vwind_file,
     &  xx_vwindstartdate1, xx_vwindstartdate2, xx_vwindperiod,
     &  xx_obcsn_file,
     &  xx_obcsnstartdate1,  xx_obcsnstartdate2,  xx_obcsnperiod,
     &  xx_obcss_file,
     &  xx_obcssstartdate1,  xx_obcssstartdate2,  xx_obcssperiod,
     &  xx_obcsw_file,
     &  xx_obcswstartdate1,  xx_obcswstartdate2,  xx_obcswperiod,
     &  xx_obcse_file,
     &  xx_obcsestartdate1,  xx_obcsestartdate2,  xx_obcseperiod,
     &  xx_diffkr_file,
     &  xx_kapgm_file,
     &  xx_tr1_file,
     &  xx_sst_file,
     &  xx_sss_file,
     &  xx_hfacc_file,
     &  xx_efluxy_file,
     &  xx_efluxp_file,
     &  xx_bottomdrag_file,
     &  doInitXX

      namelist /CTRL_PACKNAMES/
     &  yadmark,
     &  ctrlname, costname, scalname, maskname, metaname

      namelist /ECCO_PARMS/
     &                      expId
    
      namelist /ECCO_OPTIM/
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
      expId      = 'MIT_CE_000'

      modeldataunit = 14
      scrunit1      = 11

c--   Read ecco parameters from file.
      open(unit=scrunit1,status='scratch')

      open(unit = modeldataunit,file = 'data.ecco',
     &     status = 'old', iostat = errio)
      if ( errio .lt. 0 ) then
         stop ' stopped in optim_numbmod'
      endif

      do while ( .true. )
         read(modeldataunit, fmt='(a)', end=20) record
         il = max(ilnblnk(record),1)
         if ( record(1:1) .ne. commentcharacter )
     &        write(unit=scrunit1, fmt='(a)') record(:il)
      enddo
   20 continue
      close( modeldataunit )

      rewind( scrunit1 )
      read(unit = scrunit1, nml = ecco_parms)
      close( scrunit1 )
      print*, ' OPTIM_NUMBMOD: ECCO options have been read.'

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
      read(unit = scrunit1, nml = ecco_optim)
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
