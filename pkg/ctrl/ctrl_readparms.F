C
C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_readparms.F,v 1.10 2005/01/04 22:02:31 heimbach Exp $
C $Name:  $

#include "AD_CONFIG.h"
#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_readparms( mythid )

c     ==================================================================
c     SUBROUTINE ctrl_readparms
c     ==================================================================
c
c     o read ctrl parameters
c       split from ctrl_init
c
c     started: heimbach@mit.edu 12-Jun-2003
c
c     ==================================================================
c     SUBROUTINE ctrl_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ctrl.h"

#ifdef ALLOW_OBCS_CONTROL
# include "OBCS.h"
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer ntmp
      integer ivarindex

      integer iobcs
      integer il
      integer errio
      integer startrec
      integer endrec
      integer difftime(4)
      _RL     diffsecs
      _RL     dummy

      character*(80)   ymaskobcs
      character*(max_len_prec) record
      character*(max_len_mbuf) msgbuf

      integer nwetc3d

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Read the namelist input.
      namelist /ctrl_nml/
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
     &  doInitXX,
     &  doPackDiag

      namelist /ctrl_packnames/
     &  yadmark, yctrlid, yctrlposunpack, yctrlpospack,
     &  ctrlname, costname, scalname, maskname, metaname

      _BEGIN_MASTER( myThid )

c--     Set default values.
        doInitXX            = .TRUE.
#ifdef ALLOW_ADMTLM
        doAdmTlm            = .TRUE.
#else
        doAdmTlm            = .FALSE.
#endif
        doPackDiag          = .FALSE.

        xx_theta_file      = ' '
        xx_salt_file       = ' '
        xx_hfluxstartdate1 =   0
        xx_hfluxstartdate2 =   0
        xx_hfluxperiod     =   0. _d 0
        xx_hflux_file      = ' '
        xx_sfluxstartdate1 =   0
        xx_sfluxstartdate2 =   0
        xx_sfluxperiod     =   0. _d 0
        xx_sflux_file      = ' '
        xx_tauustartdate1  =   0
        xx_tauustartdate2  =   0
        xx_tauuperiod      =   0. _d 0
        xx_tauu_file       = ' '
        xx_tauvstartdate1  =   0
        xx_tauvstartdate2  =   0
        xx_tauvperiod      =   0. _d 0
        xx_tauv_file       = ' '
        xx_atempstartdate1  =   0
        xx_atempstartdate2  =   0
        xx_atempperiod      =   0. _d 0
        xx_atemp_file       = ' '
        xx_aqhstartdate1    =   0
        xx_aqhstartdate2    =   0
        xx_aqhperiod        =   0. _d 0
        xx_aqh_file         = ' '
        xx_uwindstartdate1  =   0
        xx_uwindstartdate2  =   0
        xx_uwindperiod      =   0. _d 0
        xx_uwind_file       = ' '
        xx_vwindstartdate1  =   0
        xx_vwindstartdate2  =   0
        xx_vwindperiod      =   0. _d 0
        xx_vwind_file       = ' '
        xx_obcsnstartdate1  =   0
        xx_obcsnstartdate2  =   0
        xx_obcsnperiod      =   0. _d 0
        xx_obcsn_file       = ' '
        xx_obcssstartdate1  =   0
        xx_obcssstartdate2  =   0
        xx_obcssperiod      =   0. _d 0
        xx_obcss_file       = ' '
        xx_obcswstartdate1  =   0
        xx_obcswstartdate2  =   0
        xx_obcswperiod      =   0. _d 0
        xx_obcsw_file       = ' '
        xx_obcsestartdate1  =   0
        xx_obcsestartdate2  =   0
        xx_obcseperiod      =   0. _d 0
        xx_obcse_file       = ' '
        xx_diffkr_file      = ' '
        xx_kapgm_file       = ' '
        xx_tr1_file         = ' '
        xx_sst_file         = ' '
        xx_sss_file         = ' '
        xx_hfacc_file       = ' '
        xx_efluxy_file      = ' '
        xx_efluxp_file      = ' '
        xx_bottomdrag_file  = ' '

#ifdef ALLOW_TANGENTLINEAR_RUN
        yadprefix           = 'g_'
        yadmark             = 'g_'
#else
        yadprefix           = 'ad'
        yadmark             = 'ad'
#endif
        yctrlid             = 'MIT_CE_000'
        yctrlposunpack      = '.opt'
        yctrlpospack        = '.opt'
        ctrlname            = ' '
        costname            = ' '
        scalname            = ' '
        maskname            = ' '
        metaname            = ' '

c--     Check versions.

        open(unit=scrunit1,status='scratch')

c--     Next, read the ecco data file.
        open(unit = modeldataunit,file = 'data.ctrl',
     &       status = 'old', iostat = errio)
        if ( errio .lt. 0 ) then
          stop ' stopped in ctrl_readparms'
        endif

        do while ( .true. )
          read(modeldataunit, fmt='(a)', end=1001) record
          il = max(ilnblnk(record),1)
          if ( record(1:1) .ne. commentcharacter )
     &        write(unit=scrunit1, fmt='(a)') record(:il)
        enddo
 1001   continue
        close( modeldataunit )

        rewind( scrunit1 )
        read(unit = scrunit1, nml = ctrl_nml)
        read(unit = scrunit1, nml = ctrl_packnames)
        close( scrunit1 )

      _END_MASTER( myThid )

      _BARRIER

      return
      end

