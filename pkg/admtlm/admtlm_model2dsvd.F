#include "ADMTLM_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine admtlm_model2dsvd(
     &     first, preprocev, mythid )

c     ==================================================================
c     SUBROUTINE admtlm_model2dsvd
c     ==================================================================
c
c     o Compress the control vector such that only ocean points are
c       written to file.
c
c     ==================================================================
c     SUBROUTINE admtlm_model2dsvd
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ctrl.h"
#include "optim.h"

#ifdef ALLOW_COST
# include "cost.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco_cost.h"
#else
# include "ctrl_weights.h"
#endif

c     == routine arguments ==

      logical first
      logical preprocev
      integer mythid

#ifndef EXCLUDE_CTRL_PACK
c     == local variables ==

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
      logical lxxadxx

      integer cunit
      integer ictrlgrad

      character*(128) cfile
      character*( 80) weighttype

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Tiled files are used.
      doglobalread = .false.

c--   Initialise adjoint variables on active files.
      ladinit = .false.

c--   Initialise global buffer index
      nbuffglobal = 0

cph-new(
      if ( preprocev ) then
         yadprefix = 'ev'
      else
         yadprefix = 'ad'
      endif
      nveccount = 0
cph-new)

c--   Assign file names.

      call ctrl_set_fname(xx_theta_file, fname_theta, mythid)
      call ctrl_set_fname(xx_salt_file, fname_salt, mythid)
      call ctrl_set_fname(xx_hflux_file, fname_hflux, mythid)
      call ctrl_set_fname(xx_sflux_file, fname_sflux, mythid)
      call ctrl_set_fname(xx_tauu_file, fname_tauu, mythid)
      call ctrl_set_fname(xx_tauv_file, fname_tauv, mythid)
      call ctrl_set_fname(xx_atemp_file, fname_atemp, mythid)
      call ctrl_set_fname(xx_aqh_file, fname_aqh, mythid)
      call ctrl_set_fname(xx_precip_file, fname_precip, mythid)
      call ctrl_set_fname(xx_swflux_file, fname_swflux, mythid)
      call ctrl_set_fname(xx_swdown_file, fname_swdown, mythid)
      call ctrl_set_fname(xx_uwind_file, fname_uwind, mythid)
      call ctrl_set_fname(xx_vwind_file, fname_vwind, mythid)
      call ctrl_set_fname(xx_obcsn_file, fname_obcsn, mythid)
      call ctrl_set_fname(xx_obcss_file, fname_obcss, mythid)
      call ctrl_set_fname(xx_obcsw_file, fname_obcsw, mythid)
      call ctrl_set_fname(xx_obcse_file, fname_obcse, mythid)
      call ctrl_set_fname(xx_diffkr_file, fname_diffkr, mythid)
      call ctrl_set_fname(xx_kapgm_file, fname_kapgm, mythid)
      call ctrl_set_fname(xx_tr1_file, fname_tr1, mythid)
      call ctrl_set_fname(xx_sst_file, fname_sst, mythid)
      call ctrl_set_fname(xx_sss_file, fname_sss, mythid)
      call ctrl_set_fname(xx_depth_file, fname_depth, mythid)
      call ctrl_set_fname(xx_efluxy_file, fname_efluxy, mythid)
      call ctrl_set_fname(xx_efluxp_file, fname_efluxp, mythid)
      call ctrl_set_fname(xx_bottomdrag_file, fname_bottomdrag, mythid)
      call ctrl_set_fname(xx_edtaux_file, fname_edtaux, mythid)
      call ctrl_set_fname(xx_edtauy_file, fname_edtauy, mythid)
      call ctrl_set_fname(xx_uvel_file, fname_uvel, mythid)
      call ctrl_set_fname(xx_vvel_file, fname_vvel, mythid)
      call ctrl_set_fname(xx_etan_file, fname_etan, mythid)
      call ctrl_set_fname(xx_relaxsst_file, fname_relaxsst, mythid)
      call ctrl_set_fname(xx_relaxsss_file, fname_relaxsss, mythid)

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

      if ( first ) then
c     >>> Initialise control vector for optimcycle=0 <<<
          lxxadxx   = .TRUE.
          ictrlgrad = 1
          write(cfile(1:128),'(4a,i4.4)')
     &         ctrlname(1:9),'_',yctrlid(1:10),
     &         yctrlpospack, optimcycle
          print *, 'ph-pack: packing ', ctrlname(1:9)
      else
c     >>> Write gradient vector <<<
          lxxadxx   = .FALSE.
          ictrlgrad = 2
          write(cfile(1:128),'(4a,i4.4)')
     &         costname(1:9),'_',yctrlid(1:10),
     &         yctrlpospack, optimcycle
          print *, 'ph-pack: packing ', costname(1:9)
       endif

       call mdsfindunit( cunit, mythid )

#ifdef ALLOW_ADMTLM

       if ( preprocev ) then
cph do a dummy write of initial EV fields
          write(cfile(1:128),'(a)') ' '
          write(cfile,'(a,i4.4)')
     &         'admtlm_eigen', optimcycle
       else
          write(cfile(1:128),'(a)') ' '
          write(cfile,'(a,i4.4)')
     &         'admtlm_vector.it', optimcycle + 1
       endif
       print *, 'ph-pack: unpacking ', cfile
cph       open( cunit, file   = cfile,
cph     &      status = 'unknown',
cph     &      form   = 'unformatted',
cph     &      access  = 'sequential'   )

#else /* ndef ALLOW_ADMTLM */

       open( cunit, file   = cfile,
     &      status = 'unknown',
     &      form   = 'unformatted',
     &      access  = 'sequential'   )

c--       Header information.
          write(cunit) nvartype
          write(cunit) nvarlength
          write(cunit) yctrlid
          write(cunit) optimCycle
          write(cunit) fc
C     place holder of obsolete variable iG
          write(cunit) 1
C     place holder of obsolete variable jG
          write(cunit) 1
          write(cunit) nsx
          write(cunit) nsy
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
          write(cunit) (ncvarxmax(i),  i=1,maxcvars)
          write(cunit) (ncvarymax(i),  i=1,maxcvars)
          write(cunit) (ncvarnrmax(i), i=1,maxcvars)
          write(cunit) (ncvargrd(i),   i=1,maxcvars)
          write(cunit)

#endif /* ALLOW_ADMTLM */

#ifdef ALLOW_THETA0_CONTROL
          ivartype = 1
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtheta"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_theta(ictrlgrad), "maskCtrlC",
     &         weighttype, wtheta, lxxadxx, mythid)
#endif

#ifdef ALLOW_SALT0_CONTROL
          ivartype = 2
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsalt"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_salt(ictrlgrad), "maskCtrlC",
     &         weighttype, wsalt, lxxadxx, mythid)
#endif

#if (defined (ALLOW_HFLUX_CONTROL) || defined (ALLOW_HFLUX0_CONTROL))
          ivartype = 3
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "whflux"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_hflux(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#if (defined (ALLOW_SFLUX_CONTROL) || defined (ALLOW_SFLUX0_CONTROL))
          ivartype = 4
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsflux"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_sflux(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#if (defined (ALLOW_USTRESS_CONTROL) || defined (ALLOW_TAUU0_CONTROL))
          ivartype = 5
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauu"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_tauu(ictrlgrad), "maskCtrlW",
     &         weighttype, lxxadxx, mythid)
#endif

#if (defined (ALLOW_VSTRESS_CONTROL) || defined (ALLOW_TAUV0_CONTROL))
          ivartype = 6
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauv"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_tauv(ictrlgrad), "maskCtrlS",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_ATEMP_CONTROL
          ivartype = 7
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "watemp"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_atemp(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_AQH_CONTROL
          ivartype = 8
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "waqh"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_aqh(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_UWIND_CONTROL
          ivartype = 9
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wuwind"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_uwind(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_VWIND_CONTROL
          ivartype = 10
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wvwind"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_vwind(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSN_CONTROL
          ivartype = 11
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcsn"
          call ctrl_set_pack_xz(
     &         cunit, ivartype, fname_obcsn(ictrlgrad), "maskobcsn",
     &         weighttype, wobcsn, lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSS_CONTROL
          ivartype = 12
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcss"
          call ctrl_set_pack_xz(
     &         cunit, ivartype, fname_obcss(ictrlgrad), "maskobcss",
     &         weighttype, wobcss, lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSW_CONTROL
          ivartype = 13
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcsw"
          call ctrl_set_pack_yz(
     &         cunit, ivartype, fname_obcsw(ictrlgrad), "maskobcsw",
     &         weighttype, wobcsw, lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSE_CONTROL
          ivartype = 14
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcse"
          call ctrl_set_pack_yz(
     &         cunit, ivartype, fname_obcse(ictrlgrad), "maskobcse",
     &         weighttype, wobcse, lxxadxx, mythid)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
          ivartype = 15
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wdiffkr"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_diffkr(ictrlgrad), "maskCtrlC",
     &         weighttype, wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_KAPGM_CONTROL
          ivartype = 16
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wkapgm"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_kapgm(ictrlgrad), "maskCtrlC",
     &         weighttype, wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_TR10_CONTROL
          ivartype = 17
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtr1"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_tr1(ictrlgrad), "maskCtrlC",
     &         weighttype, wunit, lxxadxx, mythid)
#endif

#if (defined (ALLOW_SST_CONTROL) || defined (ALLOW_SST0_CONTROL))
          ivartype = 18
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsst"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_sst(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#if (defined (ALLOW_SSS_CONTROL) || defined (ALLOW_SSS0_CONTROL))
          ivartype = 19
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsss"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_sss(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_DEPTH_CONTROL
          ivartype = 20
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wdepth"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_depth(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_EFLUXY0_CONTROL
          ivartype = 21
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wefluxy0"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_efluxy(ictrlgrad), "maskCtrlS",
     &         weighttype, wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_EFLUXP0_CONTROL
          ivartype = 22
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wefluxp0"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_efluxp(ictrlgrad), "maskhFacV",
     &         weighttype, wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_BOTTOMDRAG_CONTROL
          ivartype = 23
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wbottomdrag"
          call ctrl_set_pack_xy(
     &      cunit, ivartype, fname_bottomdrag(ictrlgrad), "maskCtrlC",
     &      weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_EDDYPSI_CONTROL
          ivartype = 25
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wedtaux"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_edtaux(ictrlgrad), "maskCtrlW",
     &         weighttype, wunit, lxxadxx, mythid)

          ivartype = 26
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wedtauy"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_edtauy(ictrlgrad), "maskCtrlS",
     &         weighttype, wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_UVEL0_CONTROL
          ivartype = 27
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wuvvel"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_uvel(ictrlgrad), "maskCtrlW",
     &         weighttype, wuvvel, lxxadxx, mythid)
#endif

#ifdef ALLOW_VVEL0_CONTROL
          ivartype = 28
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wuvvel"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_vvel(ictrlgrad), "maskCtrlS",
     &         weighttype, wuvvel, lxxadxx, mythid)
#endif

#ifdef ALLOW_ETAN0_CONTROL
          ivartype = 29
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wetan"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_etan(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_RELAXSST_CONTROL
          ivartype = 30
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wrelaxsst"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_relaxsst(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_RELAXSSS_CONTROL
          ivartype = 31
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wrelaxsss"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_relaxsss(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_PRECIP_CONTROL
          ivartype = 32
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wprecip"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_precip(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_SWFLUX_CONTROL
          ivartype = 33
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wswflux"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_swflux(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_SWDOWN_CONTROL
          ivartype = 34
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wswdown"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_swdown(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

          close ( cunit )

        _END_MASTER( mythid )

#endif /* EXCLUDE_CTRL_PACK */

      return
      end
