C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_setxx.F,v 1.11 2005/07/28 13:54:36 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"


      subroutine grdchk_setxx(
     I                       icvrec,
     I                       theSimulationMode,
     I                       itile,
     I                       jtile,
     I                       layer,
     I                       itilepos,
     I                       jtilepos,
     I                       xx_comp_ref,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE grdchk_setxx
c     ==================================================================
c
c     o Set component a component of the control vector; xx(loc)
c
c     started: Christian Eckert eckert@mit.edu 08-Mar-2000
c     continued: heimbach@mit.edu: 13-Jun-2001
c
c     ==================================================================
c     SUBROUTINE grdchk_setxx
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "ctrl.h"
#include "optim.h"
#include "grdchk.h"

c     == routine arguments ==

      integer icvrec
      integer theSimulationMode
      integer jtile
      integer itile
      integer layer
      integer itilepos
      integer jtilepos
      _RL     xx_comp_ref
      integer mythid

#ifdef ALLOW_GRDCHK
c     == local variables ==

      integer i,j,k
      integer il
      integer dumiter
      _RL     dumtime
      _RL     dummy

      logical doglobalread
      logical ladinit

      character*(80) fname

c--   == external ==

      integer  ilnblnk
      external ilnblnk

c--   == end of interface ==

      doglobalread = .false.
      ladinit      = .false.
      dumiter      = 0
      dumtime      = 0. _d 0
      write(fname(1:80),'(80a)') ' '

      if ( grdchkvarindex .eq. 0 ) then
         STOP 'GRDCHK INDEX 0 NOT ALLOWED'

#ifdef ALLOW_THETA0_CONTROL
      else if ( grdchkvarindex .eq. 1 ) then
         il=ilnblnk( xx_theta_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_theta_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_theta_file(1:il),'.',optimcycle
         end if

         call active_read_xyz_loc( fname, tmpfld3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         tmpfld3d( itilepos,jtilepos,layer,itile,jtile ) = xx_comp_ref

         call active_write_xyz_loc( fname, tmpfld3d, 1,
     &                          optimcycle,
     &                          mythid, dummy)

#endif /* ALLOW_THETA0_CONTROL */

#ifdef ALLOW_SALT0_CONTROL
      else if ( grdchkvarindex .eq. 2 ) then
         il=ilnblnk( xx_salt_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_salt_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_salt_file(1:il),'.',optimcycle
         end if

         call active_read_xyz_loc( fname, tmpfld3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         tmpfld3d( itilepos,jtilepos,layer,itile,jtile ) = xx_comp_ref

         call active_write_xyz_loc( fname, tmpfld3d, 1,
     &                          optimcycle,
     &                          mythid, dummy)

#endif /* ALLOW_SALT0_CONTROL */

#ifdef ALLOW_HFLUX_CONTROL
      else if ( grdchkvarindex .eq. 3 ) then
         il=ilnblnk( xx_hflux_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_hflux_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_hflux_file(1:il),'.',optimcycle
         end if

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

        call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                        optimcycle,
     &                        mythid, dummy)

#endif /* ALLOW_HFLUX_CONTROL */

#ifdef ALLOW_SFLUX_CONTROL
      else if ( grdchkvarindex .eq. 4 ) then
         il=ilnblnk( xx_sflux_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_sflux_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_sflux_file(1:il),'.',optimcycle
         end if

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

         call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                         optimcycle,
     &                         mythid, dummy)

#endif /* ALLOW_SFLUX_CONTROL */

#ifdef ALLOW_USTRESS_CONTROL
      else if ( grdchkvarindex .eq. 5 ) then
         il=ilnblnk( xx_tauu_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_tauu_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_tauu_file(1:il),'.',optimcycle
         end if

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

         call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                         optimcycle,
     &                         mythid, dummy)

#endif /* ALLOW_USTRESS_CONTROL */

#ifdef ALLOW_VSTRESS_CONTROL
      else if ( grdchkvarindex .eq. 6 ) then
         il=ilnblnk( xx_tauv_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_tauv_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_tauv_file(1:il),'.',optimcycle
         end if

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

        call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                        optimcycle,
     &                        mythid, dummy)

#endif /* ALLOW_VSTRESS_CONTROL */

#ifdef ALLOW_ATEMP_CONTROL
      else if ( grdchkvarindex .eq. 7 ) then
         il=ilnblnk( xx_atemp_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_atemp_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_atemp_file(1:il),'.',optimcycle
         end if

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

        call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                        optimcycle,
     &                        mythid, dummy)

#endif /* ALLOW_ATEMP_CONTROL */

#ifdef ALLOW_AQH_CONTROL
      else if ( grdchkvarindex .eq. 8 ) then
         il=ilnblnk( xx_aqh_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_aqh_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_aqh_file(1:il),'.',optimcycle
         end if

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

        call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                        optimcycle,
     &                        mythid, dummy)

#endif /* ALLOW_AQH_CONTROL */

#ifdef ALLOW_UWIND_CONTROL
      else if ( grdchkvarindex .eq. 9 ) then
         il=ilnblnk( xx_uwind_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_uwind_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_uwind_file(1:il),'.',optimcycle
         end if

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

        call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                        optimcycle,
     &                        mythid, dummy)

#endif /* ALLOW_UWIND_CONTROL */

#ifdef ALLOW_VWIND_CONTROL
      else if ( grdchkvarindex .eq. 10 ) then
         il=ilnblnk( xx_vwind_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_vwind_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_vwind_file(1:il),'.',optimcycle
         end if

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

        call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                        optimcycle,
     &                        mythid, dummy)

#endif /* ALLOW_VWIND_CONTROL */

#ifdef ALLOW_OBCSN_CONTROL
      else if ( grdchkvarindex .eq. 11 ) then
         il=ilnblnk( xx_obcsn_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_obcsn_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_obcsn_file(1:il),'.',optimcycle
         end if

         call active_read_xz_loc( fname, tmpfldxz, icvrec,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         tmpfldxz( itilepos,layer,itile,jtile ) = xx_comp_ref

         call active_write_xz_loc( fname, tmpfldxz, icvrec,
     &                          optimcycle,
     &                          mythid, dummy)

#endif /* ALLOW_OBCSN_CONTROL */

#ifdef ALLOW_OBCSS_CONTROL
      else if ( grdchkvarindex .eq. 12 ) then
         il=ilnblnk( xx_obcss_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_obcss_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_obcss_file(1:il),'.',optimcycle
         end if

         call active_read_xz_loc( fname, tmpfldxz, icvrec,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         tmpfldxz( itilepos,layer,itile,jtile ) = xx_comp_ref

         call active_write_xz_loc( fname, tmpfldxz, icvrec,
     &                          optimcycle,
     &                          mythid, dummy)

#endif /* ALLOW_OBCSS_CONTROL */

#ifdef ALLOW_OBCSW_CONTROL
      else if ( grdchkvarindex .eq. 13 ) then
         il=ilnblnk( xx_obcsw_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_obcsw_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_obcsw_file(1:il),'.',optimcycle
         end if

         call active_read_yz_loc( fname, tmpfldyz, icvrec,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         tmpfldyz( jtilepos,layer,itile,jtile ) = xx_comp_ref

         call active_write_yz_loc( fname, tmpfldyz, icvrec,
     &                          optimcycle,
     &                          mythid, dummy)

#endif /* ALLOW_OBCSW_CONTROL */

#ifdef ALLOW_OBCSE_CONTROL
      else if ( grdchkvarindex .eq. 14 ) then
         il=ilnblnk( xx_obcse_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_obcse_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_obcse_file(1:il),'.',optimcycle
         end if

         call active_read_yz_loc( fname, tmpfldyz, icvrec,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         tmpfldyz( jtilepos,layer,itile,jtile ) = xx_comp_ref

         call active_write_yz_loc( fname, tmpfldyz, icvrec,
     &                          optimcycle,
     &                          mythid, dummy)

#endif /* ALLOW_OBCSE_CONTROL */

#ifdef ALLOW_PRECIP_CONTROL
      else if ( grdchkvarindex .eq. 17 ) then
         il=ilnblnk( xx_precip_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_precip_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_precip_file(1:il),'.',optimcycle
         end if

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

        call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                        optimcycle,
     &                        mythid, dummy)

#endif /* ALLOW_PRECIP_CONTROL */

#ifdef ALLOW_SST0_CONTROL
      else if ( grdchkvarindex .eq. 18 ) then
         il=ilnblnk( xx_sst_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_sst_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_sst_file(1:il),'.',optimcycle
         end if

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

        call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                        optimcycle,
     &                        mythid, dummy)

#endif /* ALLOW_SST0_CONTROL */

#ifdef ALLOW_SSS0_CONTROL
      else if ( grdchkvarindex .eq. 18 ) then
         il=ilnblnk( xx_sss_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_sss_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_sss_file(1:il),'.',optimcycle
         end if

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

        call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                        optimcycle,
     &                        mythid, dummy)

#endif /* ALLOW_SSS0_CONTROL */

#ifdef ALLOW_HFACC_CONTROL
      else if ( grdchkvarindex .eq. 20 ) then
         il=ilnblnk( xx_hfacc_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_hfacc_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_hfacc_file(1:il),'.',optimcycle
         end if

#ifdef ALLOW_HFACC3D_CONTROL

         call active_read_xyz_loc( fname, tmpfld3d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld3d( itilepos,jtilepos,layer,itile,jtile ) = xx_comp_ref

         call active_write_xyz_loc( fname, tmpfld3d, icvrec,
     &                         optimcycle,
     &                         mythid, dummy)

#else

         call active_read_xy_loc( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         tmpfld2d( itilepos,jtilepos,itile,jtile ) = xx_comp_ref

         call active_write_xy_loc( fname, tmpfld2d, icvrec,
     &                         optimcycle,
     &                         mythid, dummy)

#endif /* ALLOW_HFACC3D_CONTROL */
#endif /* ALLOW_HFACC_CONTROL */

#ifdef ALLOW_EFLUXY0_CONTROL
      else if ( grdchkvarindex .eq. 21 ) then
         il=ilnblnk( xx_efluxy_file )
         write(fname(1:80),'(80a)') ' '
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_efluxy_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_efluxy_file(1:il),'.',optimcycle
         end if

         call active_read_xyz_loc( fname, tmpfld3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         tmpfld3d( itilepos,jtilepos,layer,itile,jtile ) = xx_comp_ref

         call active_write_xyz_loc( fname, tmpfld3d, 1,
     &                          optimcycle,
     &                          mythid, dummy)

#endif /* ALLOW_EFLUXY0_CONTROL */

#ifdef ALLOW_EFLUXP0_CONTROL
      else if ( grdchkvarindex .eq. 22 ) then
         il=ilnblnk( xx_efluxp_file )
         write(fname(1:80),'(80a)') ' '
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_efluxp_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_efluxp_file(1:il),'.',optimcycle
         end if

         call active_read_xyz_loc( fname, tmpfld3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         tmpfld3d( itilepos,jtilepos,layer,itile,jtile ) = xx_comp_ref

         call active_write_xyz_loc( fname, tmpfld3d, 1,
     &                          optimcycle,
     &                          mythid, dummy)

#endif /* ALLOW_EFLUXP0_CONTROL */

#ifdef ALLOW_TR10_CONTROL
      else if ( grdchkvarindex .eq. 32 ) then
         il=ilnblnk( xx_tr1_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname(1:80),'(3a,i10.10)')
     &           yadmark, xx_tr1_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname(1:80),'(2a,i10.10)')
     &           xx_tr1_file(1:il),'.',optimcycle
         end if

         call active_read_xyz_loc( fname, tmpfld3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         tmpfld3d( itilepos,jtilepos,layer,itile,jtile ) = xx_comp_ref

         call active_write_xyz_loc( fname, tmpfld3d, 1,
     &                          optimcycle,
     &                          mythid, dummy)

#endif /* ALLOW_TR10_CONTROL */

      else
ce      --> this index does not exist yet.
      endif

#endif /* ALLOW_GRDCHK */

      end

