C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_getadxx.F,v 1.2 2001/07/13 14:50:46 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"


      subroutine grdchk_getadxx(
     I                       icvrec,
     I                       itile,
     I                       jtile,
     I                       layer,
     I                       itilepos,
     I                       jtilepos,
     I                       xx_comp,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE grdchk_getadxx
c     ==================================================================
c
c     o Set component a component of the control vector; xx(loc)
c
c     started: Christian Eckert eckert@mit.edu 08-Mar-2000
c     continued: heimbach@mit.edu: 13-Jun-2001
c
c     ==================================================================
c     SUBROUTINE grdchk_getadxx
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
      integer jtile
      integer itile
      integer layer
      integer itilepos
      integer jtilepos
      _RL     xx_comp
      integer mythid

#ifdef ALLOW_GRADIENT_CHECK
c     == local variables ==

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

      if      ( grdchkvarindex .eq. 1 ) then
#ifdef ALLOW_THETA0_CONTROL
         il=ilnblnk( xx_theta_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_theta_file(1:il),'.',optimcycle

         call active_read_xyz( fname, tmpfld3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         xx_comp = tmpfld3d( itilepos,jtilepos,layer,itile,jtile )

#endif /* ALLOW_THETA0_CONTROL */

#ifdef ALLOW_SALT0_CONTROL
      else if ( grdchkvarindex .eq. 2 ) then
         il=ilnblnk( xx_salt_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_salt_file(1:il),'.',optimcycle
         
         call active_read_xyz( fname, tmpfld3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         xx_comp = tmpfld3d( itilepos,jtilepos,layer,itile,jtile )

#endif /* ALLOW_SALT0_CONTROL */

#ifdef ALLOW_HFLUX_CONTROL
      else if ( grdchkvarindex .eq. 3 ) then
         il=ilnblnk( xx_hflux_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_hflux_file(1:il),'.',optimcycle

         call active_read_xy( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         xx_comp = tmpfld2d( itilepos,jtilepos,itile,jtile )

#endif /* ALLOW_HFLUX_CONTROL */

#ifdef ALLOW_SFLUX_CONTROL
      else if ( grdchkvarindex .eq. 4 ) then
         il=ilnblnk( xx_sflux_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_sflux_file(1:il),'.',optimcycle

         call active_read_xy( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         xx_comp = tmpfld2d( itilepos,jtilepos,itile,jtile )

#endif /* ALLOW_SFLUX_CONTROL */

#ifdef ALLOW_USTRESS_CONTROL
      else if ( grdchkvarindex .eq. 5 ) then
         il=ilnblnk( xx_tauu_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_tauu_file(1:il),'.',optimcycle

         call active_read_xy( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         xx_comp = tmpfld2d( itilepos,jtilepos,itile,jtile )

#endif /* ALLOW_USTRESS_CONTROL */

#ifdef ALLOW_VSTRESS_CONTROL
      else if ( grdchkvarindex .eq. 6 ) then
         il=ilnblnk( xx_tauv_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_tauv_file(1:il),'.',optimcycle

         call active_read_xy( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         xx_comp = tmpfld2d( itilepos,jtilepos,itile,jtile )

#endif /* ALLOW_VSTRESS_CONTROL */

#ifdef ALLOW_TR10_CONTROL
      else if ( grdchkvarindex .eq. 17 ) then
         il=ilnblnk( xx_tr1_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_tr1_file(1:il),'.',optimcycle
         
         call active_read_xyz( fname, tmpfld3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)

         xx_comp = tmpfld3d( itilepos,jtilepos,layer,itile,jtile )

#endif /* ALLOW_TR10_CONTROL */

#ifdef ALLOW_SST0_CONTROL
      else if ( grdchkvarindex .eq. 18 ) then
         il=ilnblnk( xx_sst_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_sst_file(1:il),'.',optimcycle

         call active_read_xy( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         xx_comp = tmpfld2d( itilepos,jtilepos,itile,jtile )

#endif /* ALLOW_SST0_CONTROL */

#ifdef ALLOW_SSS0_CONTROL
      else if ( grdchkvarindex .eq. 19 ) then
         il=ilnblnk( xx_sss_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_sss_file(1:il),'.',optimcycle

         call active_read_xy( fname, tmpfld2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)

         xx_comp = tmpfld2d( itilepos,jtilepos,itile,jtile )

#endif /* ALLOW_SSS0_CONTROL */

      else
ce      --> this index does not exist yet.
      endif

#endif /* ALLOW_GRADIENT_CHECK */

      end

