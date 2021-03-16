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

      integer i, j, k
      integer ii
      integer il
      integer irec
      integer ivartype
      integer ictrlgrad
#if (defined ALLOW_GENARR2D_CONTROL) || (defined ALLOW_GENARR3D_CONTROL) || (defined ALLOW_GENTIM2D_CONTROL)
C-    Provided we set the file name just before calling ctrl_set_unpack,
C     the same local file name variable can be used for different variables.
C     This is how GENARR2/3D_CONTROL is implemented (+ provides an example)
      integer iarr
      character*(80) fname_local(3)
#endif

      integer cbuffindex
      integer cunit
      integer nwettmp(Nr)

      character*(128) cfile
      character*( 80) weighttype
      character*( 9) mskNameForSetUnpack

      logical lxxadxx

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
      character*(80) fname_obcsn(3)
      character*(80) fname_obcss(3)
      character*(80) fname_obcsw(3)
      character*(80) fname_obcse(3)
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
#endif /* ALLOW_CTRL and ALLOW_OBCS */

c     == end of interface ==

#ifndef ALLOW_ECCO_OPTIMIZATION
      fmin       = 0. _d 0
#endif

c--   Initialise
      nbuffGlobal = 0

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
          write(standardMessageUnit,*) 'ph-pack: unpacking ',
     &         ctrlname(1:9)
      else
c     >>> Write gradient vector <<<
          lxxadxx   = .FALSE.
          ictrlgrad = 2
          write(cfile(1:128),'(4a,i4.4)')
     &         costname(1:9),'_',yctrlid(1:10),
     &         yctrlposunpack(1:4), optimcycle
          write(standardMessageUnit,*) 'ph-pack: unpacking ',
     &         costname(1:9)
       endif

c--   Only Proc 0 will do I/O.
      IF ( myProcId .eq. 0 ) THEN

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
#ifdef ALLOW_SHELFICE
          read(cunit) (filenWetiGlobal(k),   k=1,nr)
c         read(cunit) filenWetiGlobal(1)
#endif

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
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
#endif /* ALLOW_CTRL and ALLOW_OBCS */

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
#ifdef ALLOW_SHELFICE
          do k=1,1
           if ( filenWetiGlobal(k) .NE. nWetiGlobal(k) ) then
            print *, 'WARNING: wrong nWetiGlobal for k = ', k
            STOP
           endif
          enddo
#endif /* ALLOW_SHELFICE */

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
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
#endif /* ALLOW_CTRL and ALLOW_OBCS */

#endif /* ndef ALLOW_ADMTLM */

#ifdef ALLOW_PACKUNPACK_METHOD2
      ENDIF
      _END_MASTER( mythid )
      _BARRIER
#endif

c----------------------------------------------------------------------

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
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
#endif /* ALLOW_CTRL and ALLOW_OBCS */

#ifdef ECCO_CTRL_DEPRECATED

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
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_hflux(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#if (defined (ALLOW_SFLUX_CONTROL) || defined (ALLOW_SFLUX0_CONTROL))
          ivartype = 4
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsflux"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_sflux(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#if (defined (ALLOW_USTRESS_CONTROL) || defined (ALLOW_TAUU0_CONTROL))
#ifdef ALLOW_EXF
      IF ( .NOT.useAtmWind ) THEN
#endif
          ivartype = 5
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauu"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
#ifndef ALLOW_ROTATE_UV_CONTROLS
     &         fname_tauu(ictrlgrad), "maskCtrlW",
     &         weighttype, nwetwglobal, mythid)
#else
     &         fname_tauu(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif
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
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
#ifndef ALLOW_ROTATE_UV_CONTROLS
     &         fname_tauv(ictrlgrad), "maskCtrlS",
     &         weighttype, nwetsglobal, mythid)
#else
     &         fname_tauv(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif
#ifdef ALLOW_EXF
      ENDIF
#endif
#endif

#ifdef ALLOW_ATEMP_CONTROL
          ivartype    = 7
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "watemp"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_atemp(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_AQH_CONTROL
          ivartype    = 8
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "waqh"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_aqh(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_UWIND_CONTROL
#ifdef ALLOW_EXF
      IF ( useAtmWind ) THEN
#endif
          ivartype    = 9
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wuwind"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_uwind(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#ifdef ALLOW_EXF
      ENDIF
#endif
#endif

#ifdef ALLOW_VWIND_CONTROL
#ifdef ALLOW_EXF
      IF ( useAtmWind ) THEN
#endif
          ivartype    = 10
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wvwind"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_vwind(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#ifdef ALLOW_EXF
      ENDIF
#endif
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
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_sst(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#if (defined (ALLOW_SSS_CONTROL) || defined (ALLOW_SSS0_CONTROL))
          ivartype    = 19
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsss"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_sss(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_DEPTH_CONTROL
          ivartype    = 20
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wdepth"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_depth(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
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
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_bottomdrag(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_HFLUXM_CONTROL
          ivartype    = 24
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "whfluxm"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_hfluxm(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_EDDYPSI_CONTROL
          ivartype    = 25
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wedtaux"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_edtaux(ictrlgrad), "maskCtrlW",
     &         weighttype, wedtaux, nwetwglobal, mythid)

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
     &         weighttype, wuvel, nwetwglobal, mythid)
#endif

#ifdef ALLOW_VVEL0_CONTROL
          ivartype = 28
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wvvel"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_vvel(ictrlgrad), "maskCtrlS",
     &         weighttype, wvvel, nwetsglobal, mythid)
#endif

#ifdef ALLOW_ETAN0_CONTROL
          ivartype = 29
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wetan"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_etan(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_RELAXSST_CONTROL
          ivartype = 30
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wrelaxsst"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_relaxsst(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_RELAXSSS_CONTROL
          ivartype = 31
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wrelaxsss"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_relaxsss(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_PRECIP_CONTROL
          ivartype    = 32
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wprecip"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_precip(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SWFLUX_CONTROL
          ivartype    = 33
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wswflux"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_swflux(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SWDOWN_CONTROL
          ivartype    = 34
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wswdown"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_swdown(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_LWFLUX_CONTROL
          ivartype    = 35
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wlwflux"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_lwflux(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_LWDOWN_CONTROL
          ivartype    = 36
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wlwdown"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_lwdown(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_EVAP_CONTROL
          ivartype    = 37
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wevap"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_evap(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SNOWPRECIP_CONTROL
          ivartype    = 38
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsnowprecip"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_snowprecip(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_APRESSURE_CONTROL
          ivartype    = 39
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wapressure"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_apressure(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_RUNOFF_CONTROL
          ivartype    = 40
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wrunoff"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_runoff(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SIAREA_CONTROL
          ivartype    = 41
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_siarea(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SIHEFF_CONTROL
          ivartype    = 42
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_siheff(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SIHSNOW_CONTROL
          ivartype    = 43
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_sihsnow(ictrlgrad), "maskCtrlC",
     &         weighttype, nwetcglobal, mythid)
#endif

#ifdef ALLOW_KAPREDI_CONTROL
          ivartype    = 44
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wkapredi"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_kapredi(ictrlgrad), "maskCtrlC",
     &         weighttype, wkapredi, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SHIFWFLX_CONTROL
          ivartype    = 45
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wshifwflx"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, forcingPrecond,
     &         fname_shifwflx(ictrlgrad), "maskCtrlI",
     &         weighttype, nwetiglobal, mythid)
#endif

#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_GENARR2D_CONTROL
       do iarr = 1, maxCtrlArr2D
        if (xx_genarr2d_weight(iarr).NE.' ') then
         mskNameForSetUnpack='maskCtrlC'
         nwettmp(1:Nr) = nwetcglobal(1:Nr)
# ifdef ALLOW_SHELFICE
         if ((xx_genarr2d_file(iarr)(1:11).eq.'xx_shicoeff').or.
     &       (xx_genarr2d_file(iarr)(1:11).eq.'xx_shicdrag')) then
          mskNameForSetUnpack='maskCtrlI'
          nwettmp(1:Nr) = nwetiglobal(1:Nr)
         endif
# endif
          call ctrl_set_fname( xx_genarr2d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 100+iarr
cc          write(weighttype(1:80),'(80a)') ' '
cc          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, genarr2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetUnpack,
     &         xx_genarr2d_weight(iarr),
     &         nwettmp, mythid)
        endif
       enddo
#endif

#ifdef ALLOW_GENARR3D_CONTROL
       do iarr = 1, maxCtrlArr3D
        if (xx_genarr3d_weight(iarr).NE.' ') then
          call ctrl_set_fname( xx_genarr3d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 200+iarr
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_unpack_xyz( lxxadxx, cunit, ivartype,
     &         fname_local(ictrlgrad), "maskCtrlC",
     &         xx_genarr3d_weight(iarr),
     &         wunit, nwetcglobal, mythid)
        endif
       enddo
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
       do iarr = 1, maxCtrlTim2D
        if (xx_gentim2d_weight(iarr).NE.' ') then
         mskNameForSetUnpack='maskCtrlC'
         nwettmp(1:Nr) = nwetcglobal(1:Nr)
# ifdef ALLOW_SHELFICE
         if (xx_gentim2d_file(iarr)(1:11).eq.'xx_shifwflx') then
            mskNameForSetUnpack='maskCtrlI'
            nwettmp(1:Nr) = nwetiglobal(1:Nr)
         endif
# endif
          call ctrl_set_fname( xx_gentim2d_file(iarr),
     O                         fname_local, mythid )
          ivartype    = 300+iarr
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wunit"
          call ctrl_set_unpack_xy(
     &         lxxadxx, cunit, ivartype, gentim2dPrecond(iarr),
     &         fname_local(ictrlgrad), mskNameForSetUnpack,
     &         xx_gentim2d_weight(iarr),
     &         nwettmp, mythid)
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
