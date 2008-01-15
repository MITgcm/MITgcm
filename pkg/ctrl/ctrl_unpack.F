C
C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_unpack.F,v 1.30 2008/01/15 19:56:27 dfer Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CTRL_CPPOPTIONS.h"

      subroutine ctrl_unpack( first, mythid )

c     ==================================================================
c     SUBROUTINE ctrl_unpack
c     ==================================================================
c
c     o Unpack the control vector such that the land points are filled
c       in.
c
c     started: Christian Eckert eckert@mit.edu  10-Mar-2000
c
c     changed: Patrick Heimbach heimbach@mit.edu 06-Jun-2000
c              - Transferred some filename declarations
c                from here to namelist in ctrl_init
c
c              Patrick Heimbach heimbach@mit.edu 16-Jun-2000
c              - single file name convention with or without
c                ALLOW_ECCO_OPTIMIZATION
C
c              Armin Koehl akoehl@ucsd.edu 05-Dec-2000
c              - single processor reads global parameter file
c               and writes multiple xx* and adxx* files
c
c              G Gebbie gebbie@mit.edu 18-Mar-2003
c              - open boundary packing
c
c              heimbach@mit.edu totally restructured 28-Oct-2003
c
c     ==================================================================
c     SUBROUTINE ctrl_unpack
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
      integer mythid

#ifndef EXCLUDE_CTRL_PACK
c     == local variables ==

      integer i, j, k
      integer ii
      integer il
      integer irec
      integer ivartype
      integer ictrlgrad

      integer cbuffindex
      integer cunit

      character*(128) cfile
      character*( 80) weighttype

      logical lxxadxx
      
cgg(  Add OBCS mask names.
#ifdef ALLOW_OBCSN_CONTROL
      integer        filenWetobcsnGlo(nr,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
      integer        filenWetobcssGlo(nr,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
      integer        filenWetobcswGlo(nr,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
      integer        filenWetobcseGlo(nr,nobcs)
#endif
      integer iobcs
cgg)

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

#ifndef ALLOW_ECCO_OPTIMIZATION
      fmin       = 0. _d 0
#endif

c--   Initialise
      nbuffGlobal = 0

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
      call ctrl_set_fname(xx_lwflux_file, fname_lwflux, mythid)
      call ctrl_set_fname(xx_lwdown_file, fname_lwdown, mythid)
      call ctrl_set_fname(xx_evap_file, fname_evap, mythid)
      call ctrl_set_fname(xx_snowprecip_file, fname_snowprecip, mythid)
      call ctrl_set_fname(xx_apressure_file, fname_apressure, mythid)
      call ctrl_set_fname(xx_runoff_file, fname_runoff, mythid)
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
      call ctrl_set_fname(xx_siarea_file, fname_siarea, mythid)
      call ctrl_set_fname(xx_siheff_file, fname_siheff, mythid)
      call ctrl_set_fname(xx_sihsnow_file, fname_sihsnow, mythid)
cHFLUXM_CONTROL
      call ctrl_set_fname(xx_hfluxm_file, fname_hfluxm, mythid)
cHFLUXM_CONTROL

c--     Only the master thread will do I/O.
        _BEGIN_MASTER( mythid )

c *********************************************************************

      if ( first ) then
c     >>> Initialise control vector for optimcycle=0 <<<
          lxxadxx   = .TRUE.
          ictrlgrad = 1
          write(cfile(1:128),'(4a,i4.4)')
     &         ctrlname(1:9),'_',yctrlid(1:10), 
     &         yctrlposunpack(1:4), optimcycle
          print *, 'ph-pack: unpacking ', ctrlname(1:9)
      else
c     >>> Write gradient vector <<<
          lxxadxx   = .FALSE.
          ictrlgrad = 2
          write(cfile(1:128),'(4a,i4.4)')
     &         costname(1:9),'_',yctrlid(1:10),
     &         yctrlposunpack(1:4), optimcycle
          print *, 'ph-pack: unpacking ', costname(1:9)
       endif

          call mdsfindunit( cunit, mythid )

#ifndef ALLOW_ADMTLM

          open( cunit, file   = cfile,
     &         status = 'old',
     &         form   = 'unformatted',
     &         access  = 'sequential'   )

c--       Header information.
          read(cunit) filenvartype
          read(cunit) filenvarlength
          read(cunit) fileYctrlid
          read(cunit) fileOptimCycle
          read(cunit) filefc
          read(cunit) fileIg
          read(cunit) fileJg
          read(cunit) filensx
          read(cunit) filensy
          read(cunit) (filenWetcGlobal(k),   k=1,nr)
          read(cunit) (filenWetsGlobal(k),   k=1,nr)
          read(cunit) (filenWetwGlobal(k),   k=1,nr)
#ifdef ALLOW_CTRL_WETV
          read(cunit) (filenWetvGlobal(k),   k=1,nr)
#endif

cgg(     Add OBCS mask information to the header.
#ifdef ALLOW_OBCSN_CONTROL
          read(cunit) ((filenWetobcsnGlo(k,iobcs),
     &         k=1,nr), iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
          read(cunit) ((filenWetobcssGlo(k,iobcs),
     &         k=1,nr), iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
          read(cunit) ((filenWetobcswGlo(k,iobcs),
     &         k=1,nr), iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
          read(cunit) ((filenWetobcseGlo(k,iobcs),
     &         k=1,nr), iobcs= 1,nobcs)
#endif
cgg)
          read(cunit) (filencvarindex(i), i=1,maxcvars)
          read(cunit) (filencvarrecs(i),  i=1,maxcvars)
          read(cunit) (filencvarxmax(i),  i=1,maxcvars)
          read(cunit) (filencvarymax(i),  i=1,maxcvars)
          read(cunit) (filencvarnrmax(i), i=1,maxcvars)
          read(cunit) (filencvargrd(i),   i=1,maxcvars)
          read(cunit)

c         Check file header info.
c
          if ( filenvarlength .NE. nvarlength ) then
             print *, 'WARNING: wrong nvarlength ',
     &            filenvarlength, nvarlength
             STOP 'in S/R ctrl_unpack'
          else if ( filensx .NE. nsx .OR. filensy .NE. nsy ) then
             print *, 'WARNING: wrong nsx or nsy ',
     &            filensx, nsx, filensy, nsy
             STOP 'in S/R ctrl_unpack'
          endif
          do k = 1, nr
             if ( filenWetcGlobal(k) .NE. nWetcGlobal(k) .OR.
     &            filenWetsGlobal(k) .NE. nWetsGlobal(k) .OR.
     &            filenWetwGlobal(k) .NE. nWetwGlobal(k) .OR.
     &            filenWetvGlobal(k) .NE. nWetvGlobal(k)  ) then
                print *, 'WARNING: wrong nWet?Global for k = ', k
                STOP
             endif
          end do

cgg(   Lets also check the OBCS mask info in the header.

#ifdef ALLOW_OBCSN_CONTROL
       do iobcs = 1, nobcs
         do k = 1, nr
           if (filenWetobcsnGlo(k,iobcs) .NE. 
     &           nWetobcsnGlo(k,iobcs)) then
             print *, 'WARNING: OBCSN wrong nWet?Global for k = ', k
             STOP
           endif
         end do
       end do
#endif

#ifdef ALLOW_OBCSS_CONTROL
       do iobcs = 1, nobcs
         do k = 1, nr
           if (filenWetobcssGlo(k,iobcs) .NE. 
     &           nWetobcssGlo(k,iobcs)) then
             print *, 'WARNING: OBCSS wrong nWet?Global for k = ', k
             STOP
           endif
         end do
       end do
#endif

#ifdef ALLOW_OBCSW_CONTROL
       do iobcs = 1, nobcs
         do k = 1, nr
           if (filenWetobcswGlo(k,iobcs) .NE. 
     &           nWetobcswGlo(k,iobcs)) then
             print *, 'WARNING: OBCSW wrong nWet?Global for k = ', k
             STOP
           endif
         end do
       end do
#endif

#ifdef ALLOW_OBCSE_CONTROL
       do iobcs = 1, nobcs
         do k = 1, nr
           if (filenWetobcseGlo(k,iobcs) .NE. 
     &           nWetobcseGlo(k,iobcs)) then
             print *, 'WARNING: OBCSE wrong nWet?Global for k = ', k
             STOP
           endif
         end do
       end do
#endif
cgg)  End OBCS mask check.

#endif /* ndef ALLOW_ADMTLM */

c----------------------------------------------------------------------

#ifdef ALLOW_THETA0_CONTROL
          ivartype = 1
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wthetaLev"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_theta(ictrlgrad), "maskCtrlC",
     &         weighttype, wtheta, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SALT0_CONTROL
          ivartype = 2
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsaltLev"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_salt(ictrlgrad), "maskCtrlC",
     &         weighttype, wsalt, nwetcglobal, mythid)
#endif

#if (defined (ALLOW_HFLUX_CONTROL) || defined (ALLOW_HFLUX0_CONTROL))
          ivartype    = 3
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "whflux"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_hflux(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#if (defined (ALLOW_SFLUX_CONTROL) || defined (ALLOW_SFLUX0_CONTROL))
          ivartype = 4
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsflux"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_sflux(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#if (defined (ALLOW_USTRESS_CONTROL) || defined (ALLOW_TAUU0_CONTROL))
          ivartype = 5
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauu"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_tauu(ictrlgrad), "maskCtrlW",
     &         weighttype, nwetwglobal, mythid)
#endif

#if (defined (ALLOW_VSTRESS_CONTROL) || defined (ALLOW_TAUV0_CONTROL))
          ivartype = 6
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauv"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_tauv(ictrlgrad), "maskCtrlS",
     &         weighttype, nwetsglobal, mythid)
#endif

#ifdef ALLOW_ATEMP_CONTROL
          ivartype    = 7
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "watemp"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_atemp(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_AQH_CONTROL
          ivartype    = 8
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "waqh"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_aqh(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_UWIND_CONTROL
          ivartype    = 9
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wuwind"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_uwind(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_VWIND_CONTROL
          ivartype    = 10
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wvwind"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_vwind(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_OBCSN_CONTROL
          ivartype    = 11
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcsn"
          call ctrl_set_unpack_xz(
     &         cunit, ivartype, fname_obcsn(ictrlgrad), "maskobcsn",
     &         weighttype, wobcsn, nWetobcsnGlo, mythid)
#endif

#ifdef ALLOW_OBCSS_CONTROL
          ivartype    = 12
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcss"
          call ctrl_set_unpack_xz(
     &         cunit, ivartype, fname_obcss(ictrlgrad), "maskobcss",
     &         weighttype, wobcss, nWetobcssGlo, mythid)
#endif

#ifdef ALLOW_OBCSW_CONTROL
          ivartype    = 13
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcsw"
          call ctrl_set_unpack_yz(
     &         cunit, ivartype, fname_obcsw(ictrlgrad), "maskobcsw",
     &         weighttype, wobcsw, nWetobcswGlo, mythid)
#endif

#ifdef ALLOW_OBCSE_CONTROL
          ivartype    = 14
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcse"
          call ctrl_set_unpack_yz(
     &         cunit, ivartype, fname_obcse(ictrlgrad), "maskobcse",
     &         weighttype, wobcse, nWetobcseGlo, mythid)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
          ivartype    = 15
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wdiffkr"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_diffkr(ictrlgrad), "maskCtrlC",
     &         weighttype, wdiffkr, nwetcglobal, mythid)
#endif

#ifdef ALLOW_KAPGM_CONTROL
          ivartype    = 16
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wkapgm"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_kapgm(ictrlgrad), "maskCtrlC",
     &         weighttype, wkapgm, nwetcglobal, mythid)
#endif

#ifdef ALLOW_TR10_CONTROL
          ivartype    = 17
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtr1"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_tr1(ictrlgrad), "maskCtrlC",
     &         weighttype, wunit, nwetcglobal, mythid)
#endif

#if (defined (ALLOW_SST_CONTROL) || defined (ALLOW_SST0_CONTROL))
          ivartype    = 18
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsst"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_sst(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#if (defined (ALLOW_SSS_CONTROL) || defined (ALLOW_SSS0_CONTROL))
          ivartype    = 19
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsss"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_sss(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_DEPTH_CONTROL
          ivartype    = 20
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wdepth"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_depth(ictrlgrad), "maskCtrlC",
     &         weighttype, weighttype, nwetcglobal, mythid)
CML# endif
#endif /* ALLOW_DEPTH_CONTROL */

#ifdef ALLOW_EFLUXY0_CONTROL
          ivartype    = 21
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wefluxy0"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_efluxy(ictrlgrad), "maskCtrlS",
     &         weighttype, wefluxy, nwetsglobal, mythid)
#endif

#ifdef ALLOW_EFLUXP0_CONTROL
          ivartype    = 22
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wefluxp0"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_efluxp(ictrlgrad), "maskhFacV",
     &         weighttype, wefluxp, nwetvglobal, mythid)
#endif

#ifdef ALLOW_BOTTOMDRAG_CONTROL
          ivartype    = 23
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wbottomdrag"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_bottomdrag(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_HFLUXM_CONTROL
          ivartype    = 24
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "whfluxm"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_hfluxm(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_EDTAUX_CONTROL
          ivartype    = 25
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wedtaux"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_edtaux(ictrlgrad), "maskCtrlW",
     &         weighttype, wedtaux, nwetwglobal, mythid)
#endif

#ifdef ALLOW_EDTAUY_CONTROL
          ivartype    = 26
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wedtauy"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_edtauy(ictrlgrad), "maskCtrlS",
     &         weighttype, wedtauy, nwetsglobal, mythid)
#endif

#ifdef ALLOW_UVEL0_CONTROL
          ivartype = 27
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wuvel"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_uvel(ictrlgrad), "maskCtrlW",
     &         weighttype, wunit, nwetwglobal, mythid)
#endif

#ifdef ALLOW_VVEL0_CONTROL
          ivartype = 28
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wvvel"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_vvel(ictrlgrad), "maskCtrlS",
     &         weighttype, wunit, nwetsglobal, mythid)
#endif

#ifdef ALLOW_ETAN0_CONTROL
          ivartype = 29
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wetan"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_etan(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_RELAXSST_CONTROL
          ivartype = 30
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wrelaxsst"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_relaxsst(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_RELAXSSS_CONTROL
          ivartype = 31
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wrelaxsss"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_relaxsss(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_PRECIP_CONTROL
          ivartype    = 32
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wprecip"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_precip(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SWFLUX_CONTROL
          ivartype    = 33
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wswflux"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_swflux(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SWDOWN_CONTROL
          ivartype    = 34
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wswdown"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_swdown(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_LWFLUX_CONTROL
          ivartype    = 35
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wlwflux"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_lwflux(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_LWDOWN_CONTROL
          ivartype    = 36
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wlwdown"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_lwdown(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_EVAP_CONTROL
          ivartype    = 37
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wevap"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_evap(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SNOWPRECIP_CONTROL
          ivartype    = 38
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsnowprecip"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_snowprecip(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_APRESSURE_CONTROL
          ivartype    = 39
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wapressure"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_apressure(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_RUNOFF_CONTROL
          ivartype    = 40
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wrunoff"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_runoff(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SIAREA_CONTROL
          ivartype    = 41
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_siarea(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SIHEFF_CONTROL
          ivartype    = 42
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_siheff(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SIHSNOW_CONTROL
          ivartype    = 43
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_unpack_xy( lxxadxx, cunit, ivartype,
     &         fname_sihsnow(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

         close ( cunit )
 
      _END_MASTER( mythid )

#endif /* EXCLUDE_CTRL_PACK */

      return
      end

