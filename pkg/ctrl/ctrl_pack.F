
#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_pack(
     I                      myiter,
     I                      mytime,
     I                      mythid
     &                    )

c     ==================================================================
c     SUBROUTINE ctrl_pack
c     ==================================================================
c
c     o Compress the control vector such that only ocean points are
c       written to file.
c
c     started: Christian Eckert eckert@mit.edu  10-Mar=2000
c
c     changed: Patrick Heimbach heimbach@mit.edu 06-Jun-2000
c              - Transferred some filename declarations
c                from here to namelist in ctrl_init
c  
c              Patrick Heimbach heimbach@mit.edu 16-Jun-2000
c              - single file name convention with or without
c                ALLOW_ECCO_OPTIMIZATION
c
c              G. Gebbie, added open boundary control packing,
c                  gebbie@mit.edu  18 -Mar- 2003
c
c     ==================================================================
c     SUBROUTINE ctrl_pack
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ecco.h"
#include "ctrl.h"
#include "cost.h"

#ifdef ALLOW_ECCO_OPTIMIZATION
#include "optim.h"
#endif

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

c     == local variables ==

#ifndef ALLOW_ECCO_OPTIMIZATION
      integer optimcycle
#endif

      integer i, j, k
      integer ii
      integer il
      integer irec
      integer ig,jg
      integer ivartype
      integer iobcs

      logical doglobalread
      logical ladinit
      integer cbuffindex

      integer cunit
      _RL     tmpvar

      character*(128) cfile
      character*( 80) weighttype

      character*( 80) fname_theta
      character*( 80) fname_salt
      character*( 80) fname_hflux
      character*( 80) fname_sflux
      character*( 80) fname_tauu
      character*( 80) fname_tauv
      character*( 80) adfname_theta
      character*( 80) adfname_salt
      character*( 80) adfname_hflux
      character*( 80) adfname_sflux
      character*( 80) adfname_tauu
      character*( 80) adfname_tauv
      character*( 80)   fname_atemp
      character*( 80) adfname_atemp
      character*( 80)   fname_aqh
      character*( 80) adfname_aqh
      character*( 80)   fname_uwind
      character*( 80) adfname_uwind
      character*( 80)   fname_vwind
      character*( 80) adfname_vwind
      character*( 80)   fname_obcsn
      character*( 80) adfname_obcsn
      character*( 80)   fname_obcss
      character*( 80) adfname_obcss
      character*( 80)   fname_obcsw
      character*( 80) adfname_obcsw
      character*( 80)   fname_obcse
      character*( 80) adfname_obcse
      character*( 80)   fname_diffkr
      character*( 80) adfname_diffkr
      character*( 80)   fname_kapgm
      character*( 80) adfname_kapgm
      character*( 80)   fname_tr1
      character*( 80) adfname_tr1
      character*( 80)   fname_sst
      character*( 80) adfname_sst
      character*( 80)   fname_sss
      character*( 80) adfname_sss
      character*( 80)   fname_hfacc
      character*( 80) adfname_hfacc
      character*( 80)   fname_efluxy
      character*( 80) adfname_efluxy
      character*( 80)   fname_efluxp
      character*( 80) adfname_efluxp
      character*( 80)   fname_bottomdrag
      character*( 80) adfname_bottomdrag

      logical lxxadxx

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

#ifndef ALLOW_ECCO_OPTIMIZATION
      optimcycle = 0
#endif

      tmpvar = -9999. _d 0

c--   Tiled files are used.
      doglobalread = .false.

c--   Initialise adjoint variables on active files.
      ladinit = .false.

c--   Assign file names.

      call ctrl_set_fname(
     I     xx_theta_file, fname_theta, adfname_theta, mythid )
      call ctrl_set_fname(
     I     xx_salt_file, fname_salt, adfname_salt, mythid )
      call ctrl_set_fname(
     I     xx_hflux_file, fname_hflux, adfname_hflux, mythid )
      call ctrl_set_fname(
     I     xx_sflux_file, fname_sflux, adfname_sflux, mythid )
      call ctrl_set_fname(
     I     xx_tauu_file, fname_tauu, adfname_tauu, mythid )
      call ctrl_set_fname(
     I     xx_tauv_file, fname_tauv, adfname_tauv, mythid )
      call ctrl_set_fname(
     I     xx_atemp_file, fname_atemp, adfname_atemp, mythid )
      call ctrl_set_fname(
     I     xx_aqh_file, fname_aqh, adfname_aqh, mythid )
      call ctrl_set_fname(
     I     xx_uwind_file, fname_uwind, adfname_uwind, mythid )
      call ctrl_set_fname(
     I     xx_vwind_file, fname_vwind, adfname_vwind, mythid )
      call ctrl_set_fname(
     I     xx_obcsn_file, fname_obcsn, adfname_obcsn, mythid )
      call ctrl_set_fname(
     I     xx_obcss_file, fname_obcss, adfname_obcss, mythid )
      call ctrl_set_fname(
     I     xx_obcsw_file, fname_obcsw, adfname_obcsw, mythid )
      call ctrl_set_fname(
     I     xx_obcse_file, fname_obcse, adfname_obcse, mythid )
      call ctrl_set_fname(
     I     xx_diffkr_file, fname_diffkr, adfname_diffkr, mythid )
      call ctrl_set_fname(
     I     xx_kapgm_file, fname_kapgm, adfname_kapgm, mythid )
      call ctrl_set_fname(
     I     xx_tr1_file, fname_tr1, adfname_tr1, mythid )
      call ctrl_set_fname(
     I     xx_sst_file, fname_sst, adfname_sst, mythid )
      call ctrl_set_fname(
     I     xx_sss_file, fname_sss, adfname_sss, mythid )
      call ctrl_set_fname(
     I     xx_hfacc_file, fname_hfacc, adfname_hfacc, mythid )
      call ctrl_set_fname(
     I     xx_efluxy_file, fname_efluxy, adfname_efluxy, mythid )
      call ctrl_set_fname(
     I     xx_efluxp_file, fname_efluxp, adfname_efluxp, mythid )
      call ctrl_set_fname(
     I     xx_bottomdrag_file, fname_bottomdrag, adfname_bottomdrag
     I   , mythid )

c
c--     Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

c     >>> Write control vector <<<

cph   this part was removed since it's not necessary
cph   and causes huge amounts of wall clock time on parallel machines



c     >>> Write gradient vector <<<
      lxxadxx = .FALSE.

          call mdsfindunit( cunit, mythid )
          write(cfile(1:128),'(4a,i4.4)')
     &    costname(1:9),'_',yctrlid(1:10),'.opt',
     &    optimcycle

          open( cunit, file   = cfile,
     &               status = 'unknown',
     &               form   = 'unformatted',
     &               access  = 'sequential'   )

c--       Header information.
          write(cunit) nvartype
          write(cunit) nvarlength
          write(cunit) yctrlid
          write(cunit) optimCycle
          write(cunit) fc
          write(cunit) 1
          write(cunit) 1
          write(cunit) 1
          write(cunit) 1
          write(cunit) (nWetcGlobal(k), k=1,nr)
          write(cunit) (nWetsGlobal(k), k=1,nr)
          write(cunit) (nWetwGlobal(k), k=1,nr)
#ifdef ALLOW_CTRL_WETV
          write(cunit) (nWetvGlobal(k), k=1,nr)
#endif
#ifdef ALLOW_OBCSN_CONTROL
          write(cunit) ((nWetobcsnGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
          write(cunit) ((nWetobcssGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
          write(cunit) ((nWetobcswGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
          write(cunit) ((nWetobcseGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
          write(cunit) (ncvarindex(i), i=1,maxcvars)
          write(cunit) (ncvarrecs(i),  i=1,maxcvars)
          write(cunit) (nx,  i=1,maxcvars)
          write(cunit) (ny,  i=1,maxcvars)
          write(cunit) (ncvarnrmax(i), i=1,maxcvars)
          write(cunit) (ncvargrd(i),   i=1,maxcvars)
          write(cunit)

#ifdef ALLOW_THETA0_CONTROL
          ivartype = 1
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtheta"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, adfname_theta, "hFacC",
     &         weighttype, wtheta, lxxadxx, mythid)
#endif

#ifdef ALLOW_SALT0_CONTROL
          ivartype = 2
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsalt"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, adfname_salt, "hFacC",
     &         weighttype, wsalt, lxxadxx, mythid)
#endif

#if (defined (ALLOW_HFLUX_CONTROL) || \
     defined (ALLOW_HFLUX0_CONTROL))
          ivartype = 3
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "whflux"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_hflux, "hFacC", weighttype,
     &         lxxadxx, mythid)
#endif

#if (defined (ALLOW_SFLUX_CONTROL) || \
     defined (ALLOW_SFLUX0_CONTROL))
          ivartype = 4
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsflux"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_sflux, "hFacC", weighttype,
     &         lxxadxx, mythid)
#endif

#if (defined (ALLOW_USTRESS_CONTROL) || \
     defined (ALLOW_TAUU0_CONTROL))
          ivartype = 5
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauu"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_tauu, "maskW", weighttype,
     &         lxxadxx, mythid)
#endif

#if (defined (ALLOW_VSTRESS_CONTROL) || \
     defined (ALLOW_TAUV0_CONTROL))
          ivartype = 6
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauv"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_tauv, "maskS", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_ATEMP_CONTROL
          ivartype = 7
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "watemp"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_atemp, "hFacC", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_AQH_CONTROL
          ivartype = 8
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "waqh"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_aqh, "hFacC", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_UWIND_CONTROL
          ivartype = 9
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wuwind"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_uwind, "maskW", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_VWIND_CONTROL
          ivartype = 10
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wvwind"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_vwind, "maskS", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSN_CONTROL
          ivartype = 11
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcsn"
          call ctrl_set_pack_xz(
     &         cunit, ivartype, adfname_obcsn, "maskobcsn",
     &         weighttype, wobcsn, lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSS_CONTROL
          ivartype = 12
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcss"
          call ctrl_set_pack_xz(
     &         cunit, ivartype, adfname_obcss, "maskobcss",
     &         weighttype, wobcss, lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSW_CONTROL
          ivartype = 13
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcsw"
          call ctrl_set_pack_yz(
     &         cunit, ivartype, adfname_obcsw, "maskobcsw",
     &         weighttype, wobcsw, lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSE_CONTROL
          ivartype = 14
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcse"
          call ctrl_set_pack_yz(
     &         cunit, ivartype, adfname_obcse, "maskobcse",
     &         weighttype, wobcse, lxxadxx, mythid)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
          ivartype = 15
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wdiffkr"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, adfname_diffkr, "hFacC",
     &         weighttype, wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_KAPGM_CONTROL
          ivartype = 16
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wkapgm"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, adfname_kapgm, "hFacC",
     &         weighttype, wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_TR10_CONTROL
          ivartype = 17
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtr1"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, adfname_tr1, "hFacC",
     &         weighttype, wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_SST0_CONTROL
          ivartype = 18
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsst0"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_sst0, "hFacC", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_SSS0_CONTROL
          ivartype = 19
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsss0"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_sss0, "hFacC", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_HFACC_CONTROL
          ivartype = 20
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "whfacc"
# ifdef ALLOW_HFACC3D_CONTROL
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, adfname_hfacc, "hFacC",
     &         weighttype, wunit, lxxadxx, mythid)
# else
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_hfacc, "hFacC", weighttype,
     &         lxxadxx, mythid)
# endif
#endif

#ifdef ALLOW_EFLUXY0_CONTROL
          ivartype = 21
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wefluxy0"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, adfname_efluxy, "hFacS",
     &         weighttype, wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_EFLUXP0_CONTROL
          ivartype = 22
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wefluxp0"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, adfname_efluxp, "hFacV",
     &         weighttype, wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_BOTTOMDRAG_CONTROL
          ivartype = 23
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wbottomdrag"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, adfname_bottomdrag, "hFacC", weighttype,
     &         lxxadxx, mythid)
#endif

          close ( cunit )

        _END_MASTER( mythid )

      return
      end

