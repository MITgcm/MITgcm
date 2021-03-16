#include "CTRL_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

      subroutine ctrl_pack( first, mythid )

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
c              heimbach@mit.edu totally restructured 28-Oct-2003
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

#ifdef ALLOW_CTRL
# include "ctrl.h"
# include "CTRL_SIZE.h"
# include "CTRL_GENARR.h"
# include "optim.h"
# include "CTRL_OBCS.h"
#endif
#ifdef ALLOW_COST
# include "cost.h"
#endif
#if (defined (ALLOW_ECCO) && defined (ECCO_CTRL_DEPRECATED))
# include "ecco_cost.h"
#else
# include "ctrl_weights.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif

c     == routine arguments ==
      logical first
      integer mythid

#ifndef EXCLUDE_CTRL_PACK
c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==

      _RL    fcloc

      integer i, j, k
      integer ii
      integer il
      integer irec
      integer ig,jg
      integer ivartype
#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
      integer iobcs
      character*(80) fname_obcsn(3)
      character*(80) fname_obcss(3)
      character*(80) fname_obcsw(3)
      character*(80) fname_obcse(3)
#endif
#if (defined ALLOW_GENARR2D_CONTROL) || (defined ALLOW_GENARR3D_CONTROL) || (defined ALLOW_GENTIM2D_CONTROL)
C-    Provided we set the file name just before calling ctrl_set_pack,
C     the same local file name variable can be used for different variables.
C     This is how GENARR2/3D_CONTROL is implemented (+ provides an example)
      integer iarr
      character*(80) fname_local(3)
#endif

      logical doglobalread
      logical ladinit
      integer cbuffindex
      logical lxxadxx

      integer cunit
      integer ictrlgrad

      character*(128) cfile
      character*( 80) weighttype
C 9 character limit set by set_(un)pack
      character*( 9) mskNameForSetPack

c     == end of interface ==

#ifndef ALLOW_ECCO_OPTIMIZATION
      fmin       = 0. _d 0
#endif

c--   Tiled files are used.
      doglobalread = .false.

c--   Initialise adjoint variables on active files.
      ladinit = .false.

c--   Initialise global buffer index
      nbuffglobal = 0

c--   Assign file names.

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
      call ctrl_set_fname(xx_obcsn_file, fname_obcsn, mythid)
      call ctrl_set_fname(xx_obcss_file, fname_obcss, mythid)
      call ctrl_set_fname(xx_obcsw_file, fname_obcsw, mythid)
      call ctrl_set_fname(xx_obcse_file, fname_obcse, mythid)
#endif /* ALLOW_CTRL and ALLOW_OBCS */

#ifdef ECCO_CTRL_DEPRECATED
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
      call ctrl_set_fname(xx_lwflux_file, fname_lwflux, mythid)
      call ctrl_set_fname(xx_lwdown_file, fname_lwdown, mythid)
      call ctrl_set_fname(xx_evap_file, fname_evap, mythid)
      call ctrl_set_fname(xx_snowprecip_file, fname_snowprecip, mythid)
      call ctrl_set_fname(xx_apressure_file, fname_apressure, mythid)
      call ctrl_set_fname(xx_runoff_file, fname_runoff, mythid)
      call ctrl_set_fname(xx_uwind_file, fname_uwind, mythid)
      call ctrl_set_fname(xx_vwind_file, fname_vwind, mythid)
      call ctrl_set_fname(xx_diffkr_file, fname_diffkr, mythid)
      call ctrl_set_fname(xx_kapgm_file, fname_kapgm, mythid)
      call ctrl_set_fname(xx_kapredi_file, fname_kapredi, mythid)
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
      call ctrl_set_fname(xx_siarea_file, fname_siarea, mythid)
      call ctrl_set_fname(xx_siheff_file, fname_siheff, mythid)
      call ctrl_set_fname(xx_sihsnow_file, fname_sihsnow, mythid)
      call ctrl_set_fname(xx_shifwflx_file, fname_shifwflx, mythid)
cHFLUXM_CONTROL
      call ctrl_set_fname(xx_hfluxm_file, fname_hfluxm, mythid)
cHFLUXM_CONTROL
#endif /* ECCO_CTRL_DEPRECATED */

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

      if ( first ) then
c     >>> Initialise control vector for optimcycle=0 <<<
          lxxadxx   = .TRUE.
          ictrlgrad = 1
          fcloc     = fmin
          write(cfile(1:128),'(4a,i4.4)')
     &         ctrlname(1:9),'_',yctrlid(1:10),
     &         yctrlpospack, optimcycle
          write(standardMessageUnit,*) 'ph-pack: packing ',ctrlname(1:9)
      else
c     >>> Write gradient vector <<<
          lxxadxx   = .FALSE.
          ictrlgrad = 2
#ifdef ALLOW_COST
# ifdef ALLOW_OPENAD
          fcloc     = fc%v
# else
          fcloc     = fc
# endif
#else
          fcloc     = zeroRL
#endif
          write(cfile(1:128),'(4a,i4.4)')
     &         costname(1:9),'_',yctrlid(1:10),
     &         yctrlpospack, optimcycle
          write(standardMessageUnit,*) 'ph-pack: packing ',costname(1:9)
       endif

c--   Only Proc 0 will do I/O.
      IF ( myProcId .eq. 0 ) THEN

       call mdsfindunit( cunit, mythid )
       open( cunit, file   = cfile,
     &      status = 'unknown',
     &      form   = 'unformatted',
     &      access  = 'sequential'   )

c--       Header information.
          write(cunit) nvartype
          write(cunit) nvarlength
          write(cunit) yctrlid
          write(cunit) optimCycle
#ifdef ALLOW_COST
          write(cunit) fc
#else
          write(cunit) zeroRL
#endif
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
#ifdef ALLOW_SHELFICE
          write(cunit) (nWetiGlobal(k), k=1,nr)
c          write(cunit) nWetiGlobal(1)
#endif

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
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
#endif /* ALLOW_CTRL and ALLOW_OBCS */

          write(cunit) (ncvarindex(i), i=1,maxcvars)
          write(cunit) (ncvarrecs(i),  i=1,maxcvars)
          write(cunit) (ncvarxmax(i),  i=1,maxcvars)
          write(cunit) (ncvarymax(i),  i=1,maxcvars)
          write(cunit) (ncvarnrmax(i), i=1,maxcvars)
          write(cunit) (ncvargrd(i),   i=1,maxcvars)
          write(cunit)

#ifdef ALLOW_PACKUNPACK_METHOD2
      ENDIF
      _END_MASTER( mythid )
      _BARRIER
#endif

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
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
#endif /* ALLOW_CTRL and ALLOW_OBCS */

#ifdef ECCO_CTRL_DEPRECATED

#ifdef ALLOW_THETA0_CONTROL
          ivartype = 1
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wthetaLev"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_theta(ictrlgrad), "maskCtrlC",
     &         weighttype, wtheta, lxxadxx, mythid)
#endif

#ifdef ALLOW_SALT0_CONTROL
          ivartype = 2
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsaltLev"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_salt(ictrlgrad), "maskCtrlC",
     &         weighttype, wsalt, lxxadxx, mythid)
#endif

#if (defined (ALLOW_HFLUX_CONTROL) || defined (ALLOW_HFLUX0_CONTROL))
          ivartype = 3
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "whflux"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_hflux(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#if (defined (ALLOW_SFLUX_CONTROL) || defined (ALLOW_SFLUX0_CONTROL))
          ivartype = 4
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsflux"
          call ctrl_set_pack_xy(
     &         cunit, ivartype,  forcingPrecond,
     &         fname_sflux(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#if (defined (ALLOW_USTRESS_CONTROL) || defined (ALLOW_TAUU0_CONTROL))
#ifdef ALLOW_EXF
      IF ( .NOT.useAtmWind ) THEN
#endif
          ivartype = 5
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauu"
          call ctrl_set_pack_xy(
#ifndef ALLOW_ROTATE_UV_CONTROLS
     &         cunit, ivartype, forcingPrecond,
     &         fname_tauu(ictrlgrad), "maskCtrlW",
#else
     &         cunit, ivartype, forcingPrecond,
     &         fname_tauu(ictrlgrad), "maskCtrlC",
#endif
     &         weighttype, lxxadxx, mythid)
#ifdef ALLOW_EXF
      ENDIF
#endif
#endif

#if (defined (ALLOW_VSTRESS_CONTROL) || defined (ALLOW_TAUV0_CONTROL))
#ifdef ALLOW_EXF
      IF ( .NOT.useAtmWind ) THEN
#endif
          ivartype = 6
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauv"
          call ctrl_set_pack_xy(
#ifndef ALLOW_ROTATE_UV_CONTROLS
     &         cunit, ivartype, forcingPrecond,
     &         fname_tauv(ictrlgrad), "maskCtrlS",
#else
     &         cunit, ivartype, forcingPrecond,
     &         fname_tauv(ictrlgrad), "maskCtrlC",
#endif
     &         weighttype, lxxadxx, mythid)
#ifdef ALLOW_EXF
      ENDIF
#endif
#endif

#ifdef ALLOW_ATEMP_CONTROL
          ivartype = 7
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "watemp"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_atemp(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_AQH_CONTROL
          ivartype = 8
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "waqh"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_aqh(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_UWIND_CONTROL
#ifdef ALLOW_EXF
      IF ( useAtmWind ) THEN
#endif
          ivartype = 9
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wuwind"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_uwind(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#ifdef ALLOW_EXF
      ENDIF
#endif
#endif

#ifdef ALLOW_VWIND_CONTROL
#ifdef ALLOW_EXF
      IF ( useAtmWind ) THEN
#endif
          ivartype = 10
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wvwind"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_vwind(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#ifdef ALLOW_EXF
      ENDIF
#endif
#endif

#ifdef ALLOW_DIFFKR_CONTROL
          ivartype = 15
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wdiffkr"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_diffkr(ictrlgrad), "maskCtrlC",
     &         weighttype, wdiffkr, lxxadxx, mythid)
#endif

#ifdef ALLOW_KAPGM_CONTROL
          ivartype = 16
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wkapgm"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_kapgm(ictrlgrad), "maskCtrlC",
     &         weighttype, wkapgm, lxxadxx, mythid)
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
     &         cunit, ivartype, forcingPrecond,
     &         fname_sst(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#if (defined (ALLOW_SSS_CONTROL) || defined (ALLOW_SSS0_CONTROL))
          ivartype = 19
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsss"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_sss(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_DEPTH_CONTROL
          ivartype = 20
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wdepth"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_depth(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif /* ALLOW_DEPTH_CONTROL */

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
     &         cunit, ivartype, forcingPrecond,
     &         fname_bottomdrag(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_HFLUXM_CONTROL
          ivartype = 24
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "whfluxm"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_hfluxm(ictrlgrad), "maskCtrlC",
     &         weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_EDDYPSI_CONTROL
          ivartype = 25
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wedtaux"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_edtaux(ictrlgrad), "maskCtrlW",
     &         weighttype, wedtaux, lxxadxx, mythid)

          ivartype = 26
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wedtauy"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_edtauy(ictrlgrad), "maskCtrlS",
     &         weighttype, wedtauy, lxxadxx, mythid)
#endif

#ifdef ALLOW_UVEL0_CONTROL
          ivartype = 27
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wuvel"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_uvel(ictrlgrad), "maskCtrlW",
     &         weighttype, wuvel, lxxadxx, mythid)
#endif

#ifdef ALLOW_VVEL0_CONTROL
          ivartype = 28
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wvvel"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_vvel(ictrlgrad), "maskCtrlS",
     &         weighttype, wvvel, lxxadxx, mythid)
#endif

#ifdef ALLOW_ETAN0_CONTROL
          ivartype = 29
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wetan"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_etan(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_RELAXSST_CONTROL
          ivartype = 30
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wrelaxsst"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_relaxsst(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_RELAXSSS_CONTROL
          ivartype = 31
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wrelaxsss"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_relaxsss(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_PRECIP_CONTROL
          ivartype = 32
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wprecip"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_precip(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_SWFLUX_CONTROL
          ivartype = 33
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wswflux"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_swflux(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_SWDOWN_CONTROL
          ivartype = 34
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wswdown"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_swdown(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_LWFLUX_CONTROL
          ivartype = 35
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wlwflux"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_lwflux(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_LWDOWN_CONTROL
          ivartype = 36
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wlwdown"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_lwdown(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_EVAP_CONTROL
          ivartype = 37
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wevap"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_evap(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_SNOWPRECIP_CONTROL
          ivartype = 38
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsnowprecip"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_snowprecip(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_APRESSURE_CONTROL
          ivartype = 39
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wapressure"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_apressure(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_RUNOFF_CONTROL
          ivartype = 40
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wrunoff"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_runoff(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_SIAREA_CONTROL
          ivartype = 41
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_siarea(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_SIHEFF_CONTROL
          ivartype = 42
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_siheff(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_SIHSNOW_CONTROL
          ivartype = 43
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_sihsnow(ictrlgrad),
     &         "maskCtrlC", weighttype, lxxadxx, mythid)
#endif

#ifdef ALLOW_KAPREDI_CONTROL
          ivartype = 44
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wkapredi"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_kapredi(ictrlgrad), "maskCtrlC",
     &         weighttype, wkapredi, lxxadxx, mythid)
#endif

#ifdef ALLOW_SHIFWFLX_CONTROL
          ivartype = 45
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wshifwflx"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, forcingPrecond,
     &         fname_shifwflx(ictrlgrad),
     &         "maskCtrlI", weighttype, lxxadxx, mythid)
#endif

#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_GENARR2D_CONTROL
       do iarr = 1, maxCtrlArr2D
        if (xx_genarr2d_weight(iarr).NE.' ') then
         mskNameForSetPack='maskCtrlC'
# ifdef ALLOW_SHELFICE
         if ((xx_genarr2d_file(iarr)(1:11).eq.'xx_shicoeff').or.
     &       (xx_genarr2d_file(iarr)(1:11).eq.'xx_shicdrag')) then
          mskNameForSetPack='maskCtrlI'
         endif
# endif
          call ctrl_set_fname( xx_genarr2d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 100+iarr
cc          write(weighttype(1:80),'(80a)') ' '
cc          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, genarr2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetPack,
     &         xx_genarr2d_weight(iarr),
     &         lxxadxx, mythid)
        endif
       enddo
#endif

#ifdef ALLOW_GENARR3D_CONTROL
       do iarr = 1, maxCtrlArr3D
        if (xx_genarr3d_weight(iarr).NE.' ') then
          call ctrl_set_fname( xx_genarr3d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 200+iarr
cc          write(weighttype(1:80),'(80a)') ' '
cc          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_local(ictrlgrad), "maskCtrlC",
     &         xx_genarr3d_weight(iarr),
     &         wunit, lxxadxx, mythid)
        endif
       enddo
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
       do iarr = 1, maxCtrlTim2D
        if (xx_gentim2d_weight(iarr).NE.' ') then
         mskNameForSetPack='maskCtrlC'
# ifdef ALLOW_SHELFICE
         if (xx_gentim2d_file(iarr)(1:11).eq.'xx_shifwflx')
     &          mskNameForSetPack='maskCtrlI'
# endif
          call ctrl_set_fname( xx_gentim2d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 300+iarr
cc          write(weighttype(1:80),'(80a)') ' '
cc          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, gentim2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetPack,
     &         xx_gentim2d_weight(iarr),
     &         lxxadxx, mythid)
        endif
       enddo
#endif

#ifdef ALLOW_PACKUNPACK_METHOD2
      _BEGIN_MASTER( mythid )
      IF ( myProcId .eq. 0 ) THEN
#endif

       close ( cunit )
       ENDIF !IF ( myProcId .eq. 0 )
       _END_MASTER( mythid )
      _BARRIER
#endif /* EXCLUDE_CTRL_PACK */

      return
      end
