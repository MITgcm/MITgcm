#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine grdchk_getadxx(
     I                       icvrec,
     I                       itile,
     I                       jtile,
     I                       layer,
     I                       itilepos,
     I                       jtilepos,
     I                       xx_comp,
     I                       ierr,
     I                       mythid )

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
C     CTRL_SIZE.h must be included before GRDCHK.h
#include "CTRL_SIZE.h"
#include "CTRL.h"
#include "CTRL_GENARR.h"
#ifdef ALLOW_OBCS_CONTROL
C     CTRL_OBCS.h must be included before GRDCHK.h
# include "CTRL_OBCS.h"
#endif
#include "optim.h"
#include "GRDCHK.h"

c     == routine arguments ==
      integer icvrec
      integer itile
      integer jtile
      integer layer
      integer itilepos
      integer jtilepos
      _RL     xx_comp
      integer ierr
      integer mythid

#ifdef ALLOW_GRDCHK
c--   == external ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
      integer iarr
      integer il, ilDir
      integer dumiter
      _RL     dumtime
      _RL     dummy
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
      character*(MAX_LEN_FNAM) fname

c--   == end of interface ==

      doglobalread = .false.
      ladinit      = .false.
      dumiter      = 0
      dumtime      = 0. _d 0
c     Find ctrlDir (w/o trailing blanks) length
      ilDir = ilnblnk(ctrlDir)

      if ( grdchkvarindex .eq. 0 ) then
         STOP 'GRDCHK INDEX 0 NOT ALLOWED'

#ifdef ALLOW_OBCSN_CONTROL
      else if ( grdchkvarindex .eq. 11 ) then
         il=ilnblnk( xx_obcsn_file )
         write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &        yadmark, xx_obcsn_file(1:il),'.',optimcycle
#endif /* ALLOW_OBCSN_CONTROL */

#ifdef ALLOW_OBCSS_CONTROL
      else if ( grdchkvarindex .eq. 12 ) then
         il=ilnblnk( xx_obcss_file )
         write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &        yadmark, xx_obcss_file(1:il),'.',optimcycle
#endif /* ALLOW_OBCSS_CONTROL */

#ifdef ALLOW_OBCSW_CONTROL
      else if ( grdchkvarindex .eq. 13 ) then
         il=ilnblnk( xx_obcsw_file )
         write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &        yadmark, xx_obcsw_file(1:il),'.',optimcycle
#endif /* ALLOW_OBCSW_CONTROL */

#ifdef ALLOW_OBCSE_CONTROL
      else if ( grdchkvarindex .eq. 14 ) then
         il=ilnblnk( xx_obcse_file )
         write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &        yadmark, xx_obcse_file(1:il),'.',optimcycle
#endif /* ALLOW_OBCSE_CONTROL */

#ifdef ALLOW_GENARR2D_CONTROL
      else if ( grdchkvarindex .ge. 101 .and.
     &          grdchkvarindex .le. 100+maxCtrlArr2D ) then
       do iarr = 1, maxCtrlArr2D
        if ( grdchkvarindex .eq. 100+iarr ) then
         il=ilnblnk( xx_genarr2d_file(iarr) )
         write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &        yadmark, xx_genarr2d_file(iarr)(1:il),'.',optimcycle
        endif
       enddo
#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
      else if ( grdchkvarindex .ge. 201 .and.
     &          grdchkvarindex .le. 200+maxCtrlArr3D ) then
       do iarr = 1, maxCtrlArr3D
        if ( grdchkvarindex .eq. 200+iarr ) then
         il=ilnblnk( xx_genarr3d_file(iarr) )
         write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &        yadmark, xx_genarr3d_file(iarr)(1:il),'.',optimcycle
        endif
       enddo
#endif /* ALLOW_GENARR3D_CONTROL */

#ifdef ALLOW_GENTIM2D_CONTROL
      else if ( grdchkvarindex .ge. 301 .and.
     &          grdchkvarindex .le. 300+maxCtrlTim2D ) then
       do iarr = 1, maxCtrlTim2D
        if ( grdchkvarindex .eq. 300+iarr ) then
         il=ilnblnk( xx_gentim2d_file(iarr) )
         write(fname,'(3a,i10.10)') ctrlDir(1:ilDir)//
     &        yadmark, xx_gentim2d_file(iarr)(1:il),'.',optimcycle
        endif
       enddo
#endif /* ALLOW_GENTIM2D_CONTROL */

      else
         STOP 'grdchk_getadxx: grdchkvarindex not implemented'
      endif

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
     &   xx_comp = loctmp3d( itilepos,jtilepos,layer,itile,jtile )

#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      elseif ( grdchkvarindex.EQ.11 .OR. grdchkvarindex.EQ.12) then
         call active_read_xz( fname, tmpfldxz, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp = tmpfldxz( itilepos,layer,itile,jtile )
#endif

#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      elseif ( grdchkvarindex.EQ.13 .OR. grdchkvarindex.EQ.14) then
         call active_read_yz( fname, tmpfldyz, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp = tmpfldyz( jtilepos,layer,itile,jtile )
#endif

      else

         call active_read_xy( fname, loctmp2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp = loctmp2d( itilepos,jtilepos,itile,jtile )

      endif
      endif

#endif /* ALLOW_GRDCHK */

      return
      end
