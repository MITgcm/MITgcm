
#include "CTRL_CPPOPTIONS.h"

CBOI
C
C !TITLE: CONTROL VECTOR
C !AUTHORS: mitgcm developers ( support@mitgcm.org )
C !AFFILIATION: Massachussetts Institute of Technology
C !DATE:
C !INTRODUCTION: control vector handling
C \bv
c The control vector package is connected to the differntiability
c of the code. Differentiability refers to computing the derivative
c of a cost function with respect to a set of control variables
c (initial state, boundary values, model parameters).
c To reduce memory, the control variables are kept on file and
c are only read/written, when needed.
c To achieve this, adjoint support routines are available which
c handle I/O of active variables.
c In addition, a single control vector is created, which contains
c all wet points of all control variables.
c
C     !CALLING SEQUENCE:
c    ...
c     |-- the_model_main
c         |
c         |-- initialise_fixed
c         |   |
c         |   |-- packages_readparms
c         |       |
c         |       |-- ctrl_init                - initialise control
c         |                                      package
c         |-- ctrl_unpack                      - unpack control vector
c         |
c         |-- adthe_main_loop                  - forward/adjoint run
c         |   |
c         |   |-- initialise_variables
c         |   |   |
c         |   |   |-- packages_init_variables
c         |   |       |
c         |   |       |-- ctrl_map_ini         - link init. state to
c         |   |                                  control variables
c         |   |-- ctrl_map_forcing             - link forcing fields to
c         |  ...                                 control variables
c         |
c         |-- ctrl_pack                        - pack control vector
C \ev
CEOI

CBOP
C     !ROUTINE: ctrl_unpack
C     !INTERFACE:
      subroutine ctrl_unpack( myiter, mytime, mythid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE ctrl_unpack
c     | Unpack the control vector
c     | * If in sensitivity study mode,
c     |   control variable and adjoint variable arrays and files
c     |   are initialised.
c     | * If in optimization mode,
c     |   wet points of updated control variables (i.e. anomalies 
c     |   to corresponding fields) from previous optimization
c     |   iteration are extracted from single control vector,
c     |   mapped onto full (2D or 3D) fields and distributed 
c     |   to control variable files.
c     *=================================================================
C     \ev

C     !USES:
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

      integer cbuffindex
      integer cunit

      character*(128) cfile
      character*( 80) weighttype

      logical first
      
      integer        filenvartype
      integer        filenvarlength
      character*(10) fileYctrlid
      integer        fileOptimCycle
      integer        filencbuffindex
      _RL            fileDummy
      integer        fileIg
      integer        fileJg
      integer        fileI
      integer        fileJ
      integer        filensx
      integer        filensy
      integer        filek
      integer        filenWetcGlobal(nr)
      integer        filenWetsGlobal(nr)
      integer        filenWetwGlobal(nr)
      integer        filenWetvGlobal(nr)
      integer        filencvarindex(maxcvars)
      integer        filencvarrecs(maxcvars)
      integer        filencvarxmax(maxcvars)
      integer        filencvarymax(maxcvars)
      integer        filencvarnrmax(maxcvars)
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
      character*( 1) filencvargrd(maxcvars)
      character*( 80)   fname_theta
      character*( 80) adfname_theta
      character*( 80)   fname_salt
      character*( 80) adfname_salt
      character*( 80)   fname_hflux
      character*( 80) adfname_hflux
      character*( 80)   fname_sflux
      character*( 80) adfname_sflux
      character*( 80)   fname_tauu
      character*( 80) adfname_tauu
      character*( 80)   fname_tauv
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
      character*( 80)   fname_efluxy
      character*( 80) adfname_efluxy
      character*( 80)   fname_efluxp
      character*( 80) adfname_efluxp

cgg(  Initialization of ecco_ctrl requires two new local var.
      _RL     tmpvar
      logical lxxadxx
cgg)

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

#ifndef ALLOW_ECCO_OPTIMIZATION
      optimcycle = 0
#endif

      tmpvar = 0. _d 0

c--   Check if this is the first model time step.
      if ( mytime .lt. startTime + 0.5*deltaTClock ) then
        first = .true.
      else
        first = .false.
      endif

      if ( first ) then

c--     Only the master thread will do I/O.
        _BEGIN_MASTER( mythid )

        jG = 1 + (myygloballo - 1)/sny
        iG = 1 + (myxgloballo - 1)/snx

#ifdef ALLOW_THETA0_CONTROL
        ivartype = 1
        call ctrl_set_fname(
     &       xx_theta_file, fname_theta, adfname_theta, mythid )
        call ctrl_set_globfld_xyz(
     &       adfname_theta, ivartype, mythid)
#endif /* ALLOW_THETA0_CONTROL */

#ifdef ALLOW_SALT0_CONTROL
        ivartype = 2
        call ctrl_set_fname(
     I       xx_salt_file, fname_salt, adfname_salt, mythid )
        call ctrl_set_globfld_xyz(
     &       adfname_salt, ivartype, mythid)
#endif

#if (defined (ALLOW_HFLUX_CONTROL) || \
     defined (ALLOW_HFLUX0_CONTROL))
        ivartype = 3
        call ctrl_set_fname(
     I       xx_hflux_file, fname_hflux, adfname_hflux, mythid )
        call ctrl_set_globfld_xy(
     &       adfname_hflux, ivartype, mythid)
#endif

#if (defined (ALLOW_SFLUX_CONTROL) || \
     defined (ALLOW_SFLUX0_CONTROL))
        ivartype = 4
        call ctrl_set_fname(
     I       xx_sflux_file, fname_sflux, adfname_sflux, mythid )
        call ctrl_set_globfld_xy(
     &       adfname_sflux, ivartype, mythid)
#endif

#if (defined (ALLOW_USTRESS_CONTROL) || \
     defined (ALLOW_TAUU0_CONTROL))
        ivartype = 5
        call ctrl_set_fname(
     I       xx_tauu_file, fname_tauu, adfname_tauu, mythid )
        call ctrl_set_globfld_xy(
     &       adfname_tauu, ivartype, mythid)
#endif

#if (defined (ALLOW_VSTRESS_CONTROL) || \
     defined (ALLOW_TAUV0_CONTROL))
        ivartype = 6
        call ctrl_set_fname(
     I       xx_tauv_file, fname_tauv, adfname_tauv, mythid )
        call ctrl_set_globfld_xy(
     &       adfname_tauv, ivartype, mythid)
#endif

#ifdef ALLOW_ATEMP_CONTROL
        ivartype = 7
        call ctrl_set_fname(
     I       xx_atemp_file, fname_atemp, adfname_atemp, mythid )
        call ctrl_set_globfld_xy(
     &       adfname_atemp, ivartype, mythid)
#endif

#ifdef ALLOW_AQH_CONTROL
        ivartype = 8
        call ctrl_set_fname(
     I       xx_aqh_file, fname_aqh, adfname_aqh, mythid )
        call ctrl_set_globfld_xy(
     &       adfname_aqh, ivartype, mythid)
#endif

#ifdef ALLOW_UWIND_CONTROL
        ivartype = 9
        call ctrl_set_fname(
     I       xx_uwind_file, fname_uwind, adfname_uwind, mythid )
        call ctrl_set_globfld_xy(
     &       adfname_uwind, ivartype, mythid)
#endif

#ifdef ALLOW_VWIND_CONTROL
        ivartype = 10
        call ctrl_set_fname(
     I       xx_vwind_file, fname_vwind, adfname_vwind, mythid )
        call ctrl_set_globfld_xy(
     &       adfname_vwind, ivartype, mythid)
#endif

#ifdef ALLOW_OBCSN_CONTROL
        ivartype = 11
        call ctrl_set_fname(
     I       xx_obcsn_file, fname_obcsn, adfname_obcsn, mythid )
        call ctrl_set_globfld_xz(
     &       adfname_obcsn, ivartype, mythid)
#endif

#ifdef ALLOW_OBCSS_CONTROL
        ivartype = 12
        call ctrl_set_fname(
     I       xx_obcss_file, fname_obcss, adfname_obcss, mythid )
        call ctrl_set_globfld_xz(
     &       adfname_obcss, ivartype, mythid)
#endif

#ifdef ALLOW_OBCSW_CONTROL
        ivartype = 13
        call ctrl_set_fname(
     I       xx_obcsw_file, fname_obcsw, adfname_obcsw, mythid )
        call ctrl_set_globfld_yz(
     &       adfname_obcsw, ivartype, mythid)
#endif

#ifdef ALLOW_OBCSE_CONTROL
        ivartype = 14
        call ctrl_set_fname(
     I       xx_obcse_file, fname_obcse, adfname_obcse, mythid )
        call ctrl_set_globfld_yz(
     &       adfname_obcse, ivartype, mythid)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
        ivartype = 15
        call ctrl_set_fname(
     I       xx_diffkr_file, fname_diffkr, adfname_diffkr, mythid )
        call ctrl_set_globfld_xyz(
     &       adfname_diffkr, ivartype, mythid)
#endif

#ifdef ALLOW_KAPGM_CONTROL
        ivartype = 16
        call ctrl_set_fname(
     I       xx_kapgm_file, fname_kapgm, adfname_kapgm, mythid )
        call ctrl_set_globfld_xyz(
     &       adfname_kapgm, ivartype, mythid)
#endif

#ifdef ALLOW_TR10_CONTROL
        ivartype = 17
        call ctrl_set_fname(
     I       xx_tr1_file, fname_tr1, adfname_tr1, mythid )
        call ctrl_set_globfld_xyz(
     &       adfname_tr1, ivartype, mythid)
#endif

#ifdef ALLOW_EFLUXY0_CONTROL
        ivartype = 21
        call ctrl_set_fname(
     I       xx_efluxy_file, fname_efluxy, adfname_efluxy, mythid )
        call ctrl_set_globfld_xyz(
     &       adfname_efluxy, ivartype, mythid)
#endif

#ifdef ALLOW_EFLUXP0_CONTROL
        ivartype = 22
        call ctrl_set_fname(
     I       xx_efluxp_file, fname_efluxp, adfname_efluxp, mythid )
        call ctrl_set_globfld_xyz(
     &       adfname_efluxp, ivartype, mythid)
#endif

c *********************************************************************
c if NOT very first iteration of optimization
        if ( optimcycle .ne. 0 ) then
c *********************************************************************

          call mdsfindunit( cunit, mythid )

          write(cfile(1:128),'(4a,i4.4)')
     &      ctrlname(1:9),'_',yctrlid(1:10),'.opt',
     &      optimcycle

          open( cunit, file   = cfile,
     &                 status = 'old',
     &                 form   = 'unformatted',
     &                 access  = 'sequential'   )

c--       Header information.
          read(cunit) filenvartype
          read(cunit) filenvarlength
          read(cunit) fileYctrlid
          read(cunit) fileOptimCycle
          read(cunit) fileDummy
          read(cunit) fileIg
          read(cunit) fileJg
          read(cunit) filensx
          read(cunit) filensy
          read(cunit) (filenWetcGlobal(k),   k=1,nr)
          read(cunit) (filenWetsGlobal(k),   k=1,nr)
          read(cunit) (filenWetwGlobal(k),   k=1,nr)
          read(cunit) (filenWetvGlobal(k),   k=1,nr)

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

c----------------------------------------------------------------------

#ifdef ALLOW_THETA0_CONTROL
          ivartype = 1
          call ctrl_set_unpack_xyz(
     &         cunit, ivartype, fname_theta, "hFacC",
     &         wtheta, nwetcglobal, mythid)
#endif

#ifdef ALLOW_SALT0_CONTROL
          ivartype = 2
          call ctrl_set_unpack_xyz(
     &         cunit, ivartype, fname_salt, "hFacC",
     &         wsalt, nwetcglobal, mythid)
#endif

#if (defined (ALLOW_HFLUX_CONTROL) || \
     defined (ALLOW_HFLUX0_CONTROL))
          ivartype    = 3
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "whflux"
          call ctrl_set_unpack_xy(
     &         cunit, ivartype, fname_hflux, "hFacC", weighttype,
     &         nwetcglobal, mythid)
#endif

#if (defined (ALLOW_SFLUX_CONTROL) || \
     defined (ALLOW_SFLUX0_CONTROL))
          ivartype = 4
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsflux"
          call ctrl_set_unpack_xy(
     &         cunit, ivartype, fname_sflux, "hFacC", weighttype,
     &         nwetcglobal, mythid)
#endif

#if (defined (ALLOW_USTRESS_CONTROL) || \
     defined (ALLOW_TAUU0_CONTROL))
          ivartype = 5
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauu"
          call ctrl_set_unpack_xy(
     &         cunit, ivartype, fname_tauu, "maskW", weighttype,
     &         nwetwglobal, mythid)
#endif

#if (defined (ALLOW_VSTRESS_CONTROL) || \
     defined (ALLOW_TAUV0_CONTROL))
          ivartype = 6
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauv"
          call ctrl_set_unpack_xy(
     &         cunit, ivartype, fname_tauv, "maskS", weighttype,
     &         nwetsglobal, mythid)
#endif

#ifdef ALLOW_ATEMP_CONTROL
          ivartype    = 7
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "watemp"
          call ctrl_set_unpack_xy(
     &         cunit, ivartype, fname_atemp, "hFacC", weighttype,
     &         nwetcglobal, mythid)
#endif

#ifdef ALLOW_AQH_CONTROL
          ivartype    = 8
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "waqh"
          call ctrl_set_unpack_xy(
     &         cunit, ivartype, fname_aqh, "hFacC", weighttype,
     &         nwetcglobal, mythid)
#endif

#ifdef ALLOW_UWIND_CONTROL
          ivartype    = 9
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wuwind"
          call ctrl_set_unpack_xy(
     &         cunit, ivartype, fname_uwind, "maskW", weighttype,
     &         nwetcglobal, mythid)
#endif

#ifdef ALLOW_VWIND_CONTROL
          ivartype    = 10
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wvwind"
          call ctrl_set_unpack_xy(
     &         cunit, ivartype, fname_vwind, "maskS", weighttype,
     &         nwetcglobal, mythid)
#endif

#ifdef ALLOW_OBCSN_CONTROL
          ivartype    = 11
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcsn"
          call ctrl_set_unpack_xz(
     &         cunit, ivartype, fname_obcsn, "maskobcsn",
     &         wobcsn, nWetobcsnGlo, mythid)
#endif

#ifdef ALLOW_OBCSS_CONTROL
          ivartype    = 12
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcss"
          call ctrl_set_unpack_xz(
     &         cunit, ivartype, fname_obcss, "maskobcss",
     &         wobcss, nWetobcssGlo, mythid)
#endif

#ifdef ALLOW_OBCSW_CONTROL
          ivartype    = 13
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcsw"
          call ctrl_set_unpack_yz(
     &         cunit, ivartype, fname_obcsw, "maskobcsw",
     &         wobcsw, nWetobcswGlo, mythid)
#endif

#ifdef ALLOW_OBCSE_CONTROL
          ivartype    = 14
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wobcse"
          call ctrl_set_unpack_yz(
     &         cunit, ivartype, fname_obcse, "maskobcse",
     &         wobcse, nWetobcseGlo, mythid)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
          ivartype    = 15
          call ctrl_set_unpack_xyz(
     &         cunit, ivartype, fname_diffkr, "hFacC",
     &         wunit, nwetcglobal, mythid)
#endif

#ifdef ALLOW_KAPGM_CONTROL
          ivartype    = 16
          call ctrl_set_unpack_xyz(
     &         cunit, ivartype, fname_kapgm, "hFacC",
     &         wunit, nwetcglobal, mythid)
#endif

#ifdef ALLOW_TR10_CONTROL
          ivartype    = 17
          call ctrl_set_unpack_xyz(
     &         cunit, ivartype, fname_tr1, "hFacC",
     &         wunit, nwetcglobal, mythid)
#endif

#ifdef ALLOW_EFLUXY0_CONTROL
          ivartype    = 21
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wefluxy"
          call ctrl_set_unpack_xyz(
     &         cunit, ivartype, fname_efluxy, "hFacS",
     &         wefluxy, nwetsglobal, mythid)
#endif

#ifdef ALLOW_EFLUXP0_CONTROL
          ivartype    = 22
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wefluxp"
          call ctrl_set_unpack_xyz(
     &         cunit, ivartype, fname_efluxp, "hFacV",
     &         wefluxp, nwetvglobal, mythid)
#endif

         close ( cunit )

c *********************************************************************
c if very first iteration of optimization
        else
c *********************************************************************

c--       Write zeroes to file.

#ifdef ALLOW_THETA0_CONTROL
           ivartype = 1
           call ctrl_set_globfld_xyz( 
     I          fname_theta, ivartype, mythid )
#endif
           
#ifdef ALLOW_SALT0_CONTROL
           ivartype = 2
           call ctrl_set_globfld_xyz( 
     I          fname_salt, ivartype, mythid )
#endif

#if (defined (ALLOW_HFLUX_CONTROL) || \
     defined (ALLOW_HFLUX0_CONTROL))
           ivartype = 3
           call ctrl_set_globfld_xy( 
     I          fname_hflux, ivartype, mythid )
#endif

#if (defined (ALLOW_SFLUX_CONTROL) || \
     defined (ALLOW_SFLUX0_CONTROL))
           ivartype = 4
           call ctrl_set_globfld_xy( 
     I          fname_sflux, ivartype, mythid )
#endif

#if (defined (ALLOW_USTRESS_CONTROL) || \
     defined (ALLOW_TAUU0_CONTROL))
           ivartype = 5
           call ctrl_set_globfld_xy( 
     I          fname_tauu, ivartype, mythid )
#endif

#if (defined (ALLOW_VSTRESS_CONTROL) || \
     defined (ALLOW_TAUV0_CONTROL))
           ivartype = 6
           call ctrl_set_globfld_xy( 
     I          fname_tauv, ivartype, mythid )
#endif

#ifdef ALLOW_ATEMP_CONTROL
           ivartype = 7
           call ctrl_set_globfld_xy( 
     I          fname_atemp, ivartype, mythid )
#endif

#ifdef ALLOW_AQH_CONTROL
           ivartype = 8
           call ctrl_set_globfld_xy( 
     I          fname_aqh, ivartype, mythid )
#endif

#ifdef ALLOW_UWIND_CONTROL
           ivartype = 9
           call ctrl_set_globfld_xy( 
     I          fname_uwind, ivartype, mythid )
#endif

#ifdef ALLOW_VWIND_CONTROL
           ivartype = 10
           call ctrl_set_globfld_xy( 
     I          fname_vwind, ivartype, mythid )
#endif

#ifdef ALLOW_OBCSN_CONTROL
           ivartype = 11
           call ctrl_set_globfld_xz( 
     I          fname_obcsn, ivartype, mythid )
#endif

#ifdef ALLOW_OBCSS_CONTROL
           ivartype = 12
           call ctrl_set_globfld_xz( 
     I          fname_obcss, ivartype, mythid )
#endif

#ifdef ALLOW_OBCSW_CONTROL
           ivartype = 13
           call ctrl_set_globfld_yz( 
     I          fname_obcsw, ivartype, mythid )
#endif

#ifdef ALLOW_OBCSE_CONTROL
           ivartype = 14
           call ctrl_set_globfld_yz( 
     I          fname_obcse, ivartype, mythid )
#endif

#ifdef ALLOW_DIFFKR_CONTROL
           ivartype = 15
           call ctrl_set_globfld_xyz( 
     I          fname_diffkr, ivartype, mythid )
#endif

#ifdef ALLOW_KAPGM_CONTROL
           ivartype = 16
           call ctrl_set_globfld_xyz( 
     I          fname_kapgm, ivartype, mythid )
#endif

#ifdef ALLOW_TR10_CONTROL
           ivartype = 17
           call ctrl_set_globfld_xyz( 
     I          fname_tr1, ivartype, mythid )
#endif

#ifdef ALLOW_EFLUXY0_CONTROL
           ivartype = 21
           call ctrl_set_globfld_xyz( 
     I          fname_efluxy, ivartype, mythid )
#endif

#ifdef ALLOW_EFLUXP0_CONTROL
           ivartype = 22
           call ctrl_set_globfld_xyz( 
     I          fname_efluxp, ivartype, mythid )
#endif

cgg( For optimcycle = 0, we need to output ecco_ctrl file to disk with the
cgg  header information and everything else 0. 
c     >>> Write control vector <<<
          lxxadxx = .TRUE.

          call mdsfindunit( cunit, mythid )
          write(cfile(1:128),'(4a,i4.4)')
     &      ctrlname(1:9),'_',yctrlid(1:10),'.opt',
     &      optimcycle

          open( cunit, file   = cfile,
     &                 status = 'unknown',
     &                 form   = 'unformatted',
     &                 access = 'sequential'   )

c--       Header information.
          
          write(cunit) nvartype
          write(cunit) nvarlength
          write(cunit) yctrlid
          write(cunit) optimCycle
          write(cunit) tmpvar
          write(cunit) 1
          write(cunit) 1
          write(cunit) 1
          write(cunit) 1
          write(cunit) (nWetcGlobal(k), k=1,nr)
          write(cunit) (nWetsGlobal(k), k=1,nr)
          write(cunit) (nWetwGlobal(k), k=1,nr)
          write(cunit) (nWetvGlobal(k), k=1,nr)

cgg(    Add OBCS Mask information into the header section for optimization.
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
cgg)
          write(cunit) (ncvarindex(i), i=1,maxcvars)
          write(cunit) (ncvarrecs(i),  i=1,maxcvars)
          write(cunit) (nx,  i=1,maxcvars)
          write(cunit) (ny,  i=1,maxcvars)
          write(cunit) (ncvarnrmax(i), i=1,maxcvars)
          write(cunit) (ncvargrd(i),   i=1,maxcvars)
          write(cunit)

#ifdef ALLOW_THETA0_CONTROL
          ivartype = 1
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_theta, "hFacC",
     &         wtheta, lxxadxx, mythid)
#endif

#ifdef ALLOW_SALT0_CONTROL
          ivartype = 2
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_salt, "hFacC",
     &         wsalt, lxxadxx, mythid)
#endif

#if (defined (ALLOW_HFLUX_CONTROL) || \
     defined (ALLOW_HFLUX0_CONTROL))
          ivartype = 3
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "whflux"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_hflux, "hFacC", weighttype,
     &         lxxadxx, mythid)
#endif

#if (defined (ALLOW_SFLUX_CONTROL) || \
     defined (ALLOW_SFLUX0_CONTROL))
          ivartype = 4
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wsflux"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_sflux, "hFacC", weighttype,
     &         lxxadxx, mythid)
#endif

#if (defined (ALLOW_USTRESS_CONTROL) || \
     defined (ALLOW_TAUU0_CONTROL))
          ivartype = 5
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauu"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_tauu, "maskW", weighttype,
     &         lxxadxx, mythid)
#endif

#if (defined (ALLOW_VSTRESS_CONTROL) || \
     defined (ALLOW_TAUV0_CONTROL))
          ivartype = 6
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wtauv"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_tauv, "maskS", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_ATEMP_CONTROL
          ivartype = 7
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "watemp"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_atemp, "hFacC", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_AQH_CONTROL
          ivartype = 8
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "waqh"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_aqh, "hFacC", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_UWIND_CONTROL
          ivartype = 9
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wuwind"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_uwind, "maskW", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_VWIND_CONTROL
          ivartype = 10
          write(weighttype(1:80),'(80a)') ' '
          write(weighttype(1:80),'(a)') "wvwind"
          call ctrl_set_pack_xy(
     &         cunit, ivartype, fname_vwind, "maskS", weighttype,
     &         lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSN_CONTROL
          ivartype = 11
          call ctrl_set_pack_xz(
     &         cunit, ivartype, fname_obcsn, "maskobcsn",
     &         wobcsn, lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSS_CONTROL
          ivartype = 12
          call ctrl_set_pack_xz(
     &         cunit, ivartype, fname_obcsn, "maskobcss",
     &         wobcss, lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSW_CONTROL
          ivartype = 13
          call ctrl_set_pack_yz(
     &         cunit, ivartype, fname_obcsw, "maskobcsw",
     &         wobcsw, lxxadxx, mythid)
#endif

#ifdef ALLOW_OBCSE_CONTROL
          ivartype = 14
          call ctrl_set_pack_yz(
     &         cunit, ivartype, fname_obcse, "maskobcse",
     &         wobcse, lxxadxx, mythid)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
          ivartype = 15
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_diffkr, "hFacC",
     &         wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_KAPGM_CONTROL
          ivartype = 16
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_kapgm, "hFacC",
     &         wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_TR10_CONTROL
          ivartype = 17
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_tr1, "hFacC",
     &         wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_EFLUXY0_CONTROL
          ivartype = 21
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_efluxy, "hFacS",
     &         wunit, lxxadxx, mythid)
#endif

#ifdef ALLOW_EFLUXP0_CONTROL
          ivartype = 22
          call ctrl_set_pack_xyz(
     &         cunit, ivartype, fname_efluxp, "hFacV",
     &         wunit, lxxadxx, mythid)
#endif

          close ( cunit )
cgg)  End of the initialization of the ecco_ctrl file for optimcycle=0.

        endif
cgg   End of optimcycle if.
 
      _END_MASTER( mythid )

      endif
cgg   End of "first" if.

      return
      end

