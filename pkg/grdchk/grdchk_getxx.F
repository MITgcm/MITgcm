#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine grdchk_getxx(
     I                       icvrec,
     I                       theSimulationMode,
     I                       itile,
     I                       jtile,
     I                       layer,
     I                       itilepos,
     I                       jtilepos,
     I                       xx_comp_ref,
     I                       xx_comp_pert,
     I                       localEps,
     I                       ierr,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE grdchk_getxx
c     ==================================================================
c
c     o Set component a component of the control vector; xx(loc)
c
c     started: Christian Eckert eckert@mit.edu 08-Mar-2000
c     continued: heimbach@mit.edu: 13-Jun-2001
c
c     ==================================================================
c     SUBROUTINE grdchk_getxx
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "CTRL_OBCS.h"
#include "grdchk.h"
#include "optim.h"

c     == routine arguments ==

      integer icvrec
      integer theSimulationMode
      integer jtile
      integer itile
      integer layer
      integer itilepos
      integer jtilepos
      _RL     xx_comp_ref
      _RL     xx_comp_pert
      _RL     localEps
      integer ierr
      integer mythid

#ifdef ALLOW_GRDCHK
c     == local variables ==

      integer il, ilDir
      integer dumiter
      _RL     dumtime
      _RL     dummy

      integer iarr
      logical doglobalread
      logical ladinit

#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      _RL tmpfldxz (1-OLx:sNx+OLx,Nr,nSx,nSy)
#endif
#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      _RL tmpfldyz (1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
      _RL loctmp2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL loctmp3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      character*(80) fname

c--   == external ==

      integer  ilnblnk
      external ilnblnk

c--   == end of interface ==

      doglobalread = .false.
      ladinit      = .false.
      dumiter      = 0
      dumtime      = 0. _d 0
c     Find ctrlDir (w/o trailing blanks) length
      ilDir = ilnblnk(ctrlDir)
      write(fname,'(80a)') ' '

      if ( grdchkvarindex .eq. 0 ) then
         STOP 'GRDCHK INDEX 0 NOT ALLOWED'

#ifdef ALLOW_OBCSN_CONTROL
      else if ( grdchkvarindex .eq. 11 ) then
         il=ilnblnk( xx_obcsn_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &           yadmark, xx_obcsn_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname,'(2a,i10.10)') ctrlDir(1:ilDir)//
     &           xx_obcsn_file(1:il),'.',optimcycle
         end if
#endif /* ALLOW_OBCSN_CONTROL */

#ifdef ALLOW_OBCSS_CONTROL
      else if ( grdchkvarindex .eq. 12 ) then
         il=ilnblnk( xx_obcss_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &           yadmark, xx_obcss_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname,'(2a,i10.10)') ctrlDir(1:ilDir)//
     &           xx_obcss_file(1:il),'.',optimcycle
         end if
#endif /* ALLOW_OBCSS_CONTROL */

#ifdef ALLOW_OBCSW_CONTROL
      else if ( grdchkvarindex .eq. 13 ) then
         il=ilnblnk( xx_obcsw_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &           yadmark, xx_obcsw_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname,'(2a,i10.10)') ctrlDir(1:ilDir)//
     &           xx_obcsw_file(1:il),'.',optimcycle
         end if
#endif /* ALLOW_OBCSW_CONTROL */

#ifdef ALLOW_OBCSE_CONTROL
      else if ( grdchkvarindex .eq. 14 ) then
         il=ilnblnk( xx_obcse_file )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &           yadmark, xx_obcse_file(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname,'(2a,i10.10)') ctrlDir(1:ilDir)//
     &           xx_obcse_file(1:il),'.',optimcycle
         end if
#endif /* ALLOW_OBCSE_CONTROL */

#ifdef ALLOW_GENARR2D_CONTROL
      else if ( grdchkvarindex .ge. 101 .and.
     &          grdchkvarindex .le. 100+maxCtrlArr2D ) then
       do iarr = 1, maxCtrlArr2D
        if ( grdchkvarindex .eq. 100+iarr ) then
         il=ilnblnk( xx_genarr2d_file(iarr) )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &           yadmark, xx_genarr2d_file(iarr)(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname,'(2a,i10.10)') ctrlDir(1:ilDir)//
     &           xx_genarr2d_file(iarr)(1:il),'.',optimcycle
         end if
        endif
       enddo
#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
      else if ( grdchkvarindex .ge. 201 .and.
     &          grdchkvarindex .le. 200+maxCtrlArr3D ) then
       do iarr = 1, maxCtrlArr3D
        if ( grdchkvarindex .eq. 200+iarr ) then
         il=ilnblnk( xx_genarr3d_file(iarr) )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &           yadmark, xx_genarr3d_file(iarr)(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname,'(2a,i10.10)') ctrlDir(1:ilDir)//
     &           xx_genarr3d_file(iarr)(1:il),'.',optimcycle
         end if
        endif
       enddo
#endif /* ALLOW_GENARR3D_CONTROL */

#ifdef ALLOW_GENTIM2D_CONTROL
      else if ( grdchkvarindex .ge. 301 .and.
     &          grdchkvarindex .le. 300+maxCtrlTim2D ) then
       do iarr = 1, maxCtrlTim2D
        if ( grdchkvarindex .eq. 300+iarr ) then
         il=ilnblnk( xx_gentim2d_file(iarr) )
         if ( theSimulationMode .EQ. TANGENT_SIMULATION ) then
            write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &           yadmark, xx_gentim2d_file(iarr)(1:il),'.',optimcycle
         else if ( theSimulationMode .EQ. FORWARD_SIMULATION ) then
            write(fname,'(2a,i10.10)') ctrlDir(1:ilDir)//
     &           xx_gentim2d_file(iarr)(1:il),'.',optimcycle
         end if
        endif
       enddo
#endif /* ALLOW_GENTIM2D_CONTROL */

      else
ce      --> this index does not exist yet.
      endif

      xx_comp_ref=0. _d 0
      xx_comp_pert=0. _d 0

      if (ierr .EQ. 0 ) then
      if ( grdchkvarindex.EQ.1 .OR. grdchkvarindex.EQ.2 .OR.
     &     grdchkvarindex.EQ.27 .OR. grdchkvarindex.EQ.28 .OR.
     &     grdchkvarindex.EQ.15 .OR. grdchkvarindex.EQ.16 .OR.
     &     grdchkvarindex.EQ.17 .OR. grdchkvarindex.EQ.21 .OR.
     &     grdchkvarindex.EQ.22 .OR. grdchkvarindex.EQ.31 .OR.
     &     grdchkvarindex.EQ.44
#ifdef ALLOW_GENARR3D_CONTROL
     &     .OR. ( grdchkvarindex .ge. 201 .and.
     &     grdchkvarindex .le. 200+maxCtrlArr3D )
#endif
     &                                            ) then

         call active_read_xyz( fname, loctmp3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp_ref = loctmp3d( itilepos,jtilepos,layer,itile,jtile )
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp_pert = xx_comp_ref + localEps
         if ( myProcId .EQ. grdchkwhichproc )
     &   loctmp3d( itilepos,jtilepos,layer,itile,jtile ) = xx_comp_pert
         call active_write_xyz( fname, loctmp3d, 1,
     &                          optimcycle,
     &                          mythid, dummy)

#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      elseif ( grdchkvarindex.EQ.11 .OR. grdchkvarindex.EQ.12) then
         call active_read_xz( fname, tmpfldxz, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp_ref = tmpfldxz( itilepos,layer,itile,jtile )
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp_pert = xx_comp_ref + localEps
         if ( myProcId .EQ. grdchkwhichproc )
     &   tmpfldxz( itilepos,layer,itile,jtile ) = xx_comp_pert
         call active_write_xz( fname, tmpfldxz, icvrec,
     &                         optimcycle,
     &                         mythid, dummy)
#endif

#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      elseif ( grdchkvarindex.EQ.13 .OR. grdchkvarindex.EQ.14) then
         call active_read_yz( fname, tmpfldyz, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp_ref = tmpfldyz( jtilepos,layer,itile,jtile )
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp_pert = xx_comp_ref + localEps
         if ( myProcId .EQ. grdchkwhichproc )
     &   tmpfldyz( jtilepos,layer,itile,jtile ) = xx_comp_pert
         call active_write_yz( fname, tmpfldyz, icvrec,
     &                         optimcycle,
     &                         mythid, dummy)
#endif

      else

         call active_read_xy( fname, loctmp2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp_ref = loctmp2d( itilepos,jtilepos,itile,jtile )
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp_pert = xx_comp_ref + localEps
         if ( myProcId .EQ. grdchkwhichproc )
     &   loctmp2d( itilepos,jtilepos,itile,jtile ) = xx_comp_pert
         call active_write_xy( fname, loctmp2d, icvrec,
     &                         optimcycle,
     &                         mythid, dummy)

      endif
      endif

#endif /* ALLOW_GRDCHK */

      return
      end
