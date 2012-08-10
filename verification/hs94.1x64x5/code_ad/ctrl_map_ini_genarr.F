C $Header: /u/gcmpack/MITgcm/verification/hs94.1x64x5/code_ad/ctrl_map_ini_genarr.F,v 1.3 2012/08/10 19:30:43 jmc Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: ctrl_map_ini_genarr
C     !INTERFACE:
      subroutine ctrl_map_ini_genarr( mythid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE ctrl_map_ini_genarr
c     | Add the generic arrays of the
c     | control vector to the model state and update the tile halos.
c     | The control vector is defined in the header file "ctrl.h".
c     *=================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
c#include "PTRACERS_PARAMS.h"
# include "PTRACERS_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer mythid

C     !LOCAL VARIABLES:
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer il
      integer iarr

      logical equal
      logical doglobalread
      logical ladinit

      character*( 80)   fnamegeneric

      _RL     fac
      _RL tmptest

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      doglobalread = .false.
      ladinit      = .false.

      equal = .true.

      if ( equal ) then
        fac = 1. _d 0
      else
        fac = 0. _d 0
      endif

#ifdef ALLOW_GENARR2D_CONTROL
c--   An example of connecting specific fields
c--   to 3 generic 2D control arrays
cc--->>>
cc--->>> COMPILE FAILURE IS DELIBERATE
cc--->>> BE SURE WHAT YOU ARE DOING AND CUSTOMIZE <<<---
cc--->>>
c--   generic - user-defined control vars
      do iarr = 1, maxCtrlArr2D
       il=ilnblnk( xx_genarr2d_file(iarr) )
       write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_genarr2d_file(iarr)(1:il),'.',optimcycle
       call active_read_xy ( fnamegeneric, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_genarr2d_dummy(iarr) )
       do bj = jtlo,jthi
         do bi = itlo,ithi
           do j = jmin,jmax
             do i = imin,imax
              if ( iarr .eq. 1 ) then
                bottomdragfld(i,j,bi,bj) = bottomdragfld(i,j,bi,bj)
     &                                    + tmpfld2d(i,j,bi,bj)
              elseif ( iarr. eq. 2 ) then
                theta(i,j,1,bi,bj) = theta(i,j,1,bi,bj)
     &                               + tmpfld2d(i,j,bi,bj)
              elseif ( iarr .eq. 3 ) then
                salt(i,j,1,bi,bj) = salt(i,j,1,bi,bj)
     &                              + tmpfld2d(i,j,bi,bj)
              endif
             enddo
           enddo
         enddo
       enddo
c--
       _EXCH_XY_RL( bottomdragfld, mythid )
       _EXCH_XYZ_RL( theta, mythid )
       _EXCH_XYZ_RL( salt, mythid )
c--
      enddo
#endif

#ifdef ALLOW_GENARR3D_CONTROL
c--   An example of connecting specific fields
c--   to 3 generic 3D control arrays
cc--->>>
cc--->>> COMPILE FAILURE IS DELIBERATE
cc--->>> BE SURE WHAT YOU ARE DOING AND CUSTOMIZE <<<---
cc--->>>
c--   generic - user-defined control vars
      do iarr = 1, maxCtrlArr3D
       il=ilnblnk( xx_genarr3d_file(iarr) )
       write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_genarr3d_file(iarr)(1:il),'.',optimcycle
       call active_read_xyz( fnamegeneric, tmpfld3d, 1,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_genarr3d_dummy(iarr) )
       do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
               if ( iarr .eq. 1 ) then
                 theta(i,j,k,bi,bj) = theta(i,j,k,bi,bj) +
     &                                fac*tmpfld3d(i,j,k,bi,bj)
               elseif ( iarr .eq. 2 ) then
                 salt(i,j,k,bi,bj) = salt(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
               endif
              enddo
            enddo
          enddo
        enddo
       enddo
       _EXCH_XYZ_RL( theta, mythid )
       _EXCH_XYZ_RL( salt, mythid )
       _EXCH_XYZ_RL( diffkr, mythid )
c--
      enddo
#endif

      return
      end
