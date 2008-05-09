C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_set_obcs.F,v 1.13 2008/05/09 10:00:40 dimitri Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_set_obcs_xz ( 
     &       obcs_fld_xz, obcs_xz_0, obcs_xz_1
     I     , obcs_file, obcsmask
     I     , fac, first, changed, useYearlyFields, obcs_period
     I     , count0, count1, year0, year1
     I     , mycurrenttime, mycurrentiter, mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_set_obcs_xz
c     ==================================================================
c
c     o set open boundary conditions
c
c     started: heimbach@mit.edu 01-May-2001
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002

c     ==================================================================
c     SUBROUTINE exf_set_obcs_xz
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

c     == routine arguments ==

      _RL obcs_fld_xz(1-olx:snx+olx,Nr,nsx,nsy)
      _RL obcs_xz_0(1-olx:snx+olx,Nr,nsx,nsy)
      _RL obcs_xz_1(1-olx:snx+olx,Nr,nsx,nsy)

      character*(128) obcs_file
      character*1 obcsmask
      logical first, changed
      logical useYearlyFields
      _RL     obcs_period
      integer count0, count1, year0, year1
      _RL     fac
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_OBCS

c     == local variables ==

      integer bi, bj
      integer i, k

      integer il
      character*(128) obcs_file0, obcs_file1

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      if ( obcs_file .NE. ' ' ) then

         if ( first ) then

            call exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year0,
     I         obcs_file,
     O         obcs_file0,
     I         mycurrenttime, mycurrentiter, mythid )

            call mdsreadfieldxz( obcs_file0, exf_iprec, exf_yftype, Nr
     &           , obcs_xz_1, count0, mythid
     &           )
         endif

         if (( first ) .or. ( changed )) then
            call exf_swapffields_xz( obcs_xz_0, obcs_xz_1, mythid )

            call exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year1, 
     I         obcs_file,
     O         obcs_file1,
     I         mycurrenttime, mycurrentiter, mythid )

            call mdsreadfieldxz( obcs_file1, exf_iprec, exf_yftype, Nr
     &           , obcs_xz_1, count1, mythid
     &           )
         endif

         do bj = mybylo(mythid),mybyhi(mythid)
            do bi = mybxlo(mythid),mybxhi(mythid)
               do k = 1,Nr
                  do i = 1,snx
                     obcs_fld_xz(i,k,bi,bj) = 
     &                    fac * obcs_xz_0(i,k,bi,bj) +
     &                    (exf_one - fac) * obcs_xz_1(i,k,bi,bj)
                  enddo
               enddo
            enddo
         enddo

      endif

#endif /* ALLOW_OBCS */

      end

      subroutine exf_set_obcs_yz ( 
     &       obcs_fld_yz, obcs_yz_0, obcs_yz_1
     I     , obcs_file, obcsmask
     I     , fac, first, changed, useYearlyFields, obcs_period
     I     , count0, count1, year0, year1
     I     , mycurrenttime, mycurrentiter, mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_set_obcs_yz
c     ==================================================================
c
c     o set open boundary conditions
c
c     started: heimbach@mit.edu 01-May-2001

c     ==================================================================
c     SUBROUTINE exf_set_obcs_yz
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

c     == routine arguments ==

      _RL obcs_fld_yz(1-oly:sny+oly,Nr,nsx,nsy)
      _RL obcs_yz_0(1-oly:sny+oly,Nr,nsx,nsy)
      _RL obcs_yz_1(1-oly:sny+oly,Nr,nsx,nsy)
      character*(MAX_LEN_FNAM) obcs_file
      character*1 obcsmask
      logical first, changed
      logical useYearlyFields
      _RL     obcs_period
      integer count0, count1, year0, year1
      _RL     fac
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_OBCS

c     == local variables ==

      integer bi, bj
      integer j, k
      integer il
      character*(128) obcs_file0, obcs_file1

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      if ( obcs_file .NE. ' ' ) then

         if ( first ) then

            call exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year0, 
     I         obcs_file,
     O         obcs_file0,
     I         mycurrenttime, mycurrentiter, mythid )

            call mdsreadfieldyz( obcs_file0, exf_iprec, exf_yftype, Nr
     &           , obcs_yz_1, count0, mythid
     &           )
         endif

         if (( first ) .or. ( changed )) then
            call exf_swapffields_yz( obcs_yz_0, obcs_yz_1, mythid )

            call exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year1, 
     I         obcs_file,
     O         obcs_file1,
     I         mycurrenttime, mycurrentiter, mythid )

            call mdsreadfieldyz( obcs_file1, exf_iprec, exf_yftype, Nr
     &           , obcs_yz_1, count1, mythid
     &           )
         endif

         do bj = mybylo(mythid),mybyhi(mythid)
            do bi = mybxlo(mythid),mybxhi(mythid)
               do k = 1,Nr
                  do j = 1,sny
                     obcs_fld_yz(j,k,bi,bj) = 
     &                    fac             *obcs_yz_0(j,k,bi,bj) +
     &                    (exf_one - fac) *obcs_yz_1(j,k,bi,bj)
                  enddo
               enddo
            enddo
         enddo

      endif

#endif /* ALLOW_OBCS */

      end

      subroutine exf_set_obcs_x ( 
     &       obcs_fld_x, obcs_x_0, obcs_x_1
     I     , obcs_file, obcsmask
     I     , fac, first, changed, useYearlyFields, obcs_period
     I     , count0, count1, year0, year1
     I     , mycurrenttime, mycurrentiter, mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_set_obcs_x
c     ==================================================================
c
c     o set open boundary conditions
c       same as exf_set_obcs_xz but for NR=1
c
c     ==================================================================
c     SUBROUTINE exf_set_obcs_x
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

c     == routine arguments ==

      _RL obcs_fld_x(1-olx:snx+olx,nsx,nsy)
      _RL obcs_x_0(1-olx:snx+olx,nsx,nsy)
      _RL obcs_x_1(1-olx:snx+olx,nsx,nsy)

      character*(128) obcs_file
      character*1 obcsmask
      logical first, changed
      logical useYearlyFields
      _RL     obcs_period
      integer count0, count1, year0, year1
      _RL     fac
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_OBCS

c     == local variables ==

      integer bi, bj, i

      integer il
      character*(128) obcs_file0, obcs_file1

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      if ( obcs_file .NE. ' ' ) then

         if ( first ) then

            call exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year0, 
     I         obcs_file,
     O         obcs_file0,
     I         mycurrenttime, mycurrentiter, mythid )

            call mdsreadfieldxz( obcs_file0, exf_iprec, exf_yftype, 1
     &           , obcs_x_1, count0, mythid
     &           )
         endif

         if (( first ) .or. ( changed )) then
            call exf_swapffields_x( obcs_x_0, obcs_x_1, mythid )

            call exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year1, 
     I         obcs_file,
     O         obcs_file1,
     I         mycurrenttime, mycurrentiter, mythid )

            call mdsreadfieldxz( obcs_file1, exf_iprec, exf_yftype, 1
     &           , obcs_x_1, count1, mythid
     &           )
         endif

         do bj = mybylo(mythid),mybyhi(mythid)
            do bi = mybxlo(mythid),mybxhi(mythid)
               do i = 1,snx
                  obcs_fld_x(i,bi,bj) = 
     &                 fac * obcs_x_0(i,bi,bj) +
     &                 (exf_one - fac) * obcs_x_1(i,bi,bj)
               enddo
            enddo
         enddo

      endif

#endif /* ALLOW_OBCS */

      end

      subroutine exf_set_obcs_y ( 
     &       obcs_fld_y, obcs_y_0, obcs_y_1
     I     , obcs_file, obcsmask
     I     , fac, first, changed, useYearlyFields, obcs_period
     I     , count0, count1, year0, year1
     I     , mycurrenttime, mycurrentiter, mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_set_obcs_y
c     ==================================================================
c
c     o set open boundary conditions
c       same as exf_set_obcs_yz but for NR=1
c
c     ==================================================================
c     SUBROUTINE exf_set_obcs_y
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

c     == routine arguments ==

      _RL obcs_fld_y(1-oly:sny+oly,nsx,nsy)
      _RL obcs_y_0(1-oly:sny+oly,nsx,nsy)
      _RL obcs_y_1(1-oly:sny+oly,nsx,nsy)
      character*(MAX_LEN_FNAM) obcs_file
      character*1 obcsmask
      logical first, changed
      logical useYearlyFields
      _RL     obcs_period
      integer count0, count1, year0, year1
      _RL     fac
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_OBCS

c     == local variables ==

      integer bi, bj, j

      integer il
      character*(128) obcs_file0, obcs_file1

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      if ( obcs_file .NE. ' ' ) then

         if ( first ) then

            call exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year0, 
     I         obcs_file,
     O         obcs_file0,
     I         mycurrenttime, mycurrentiter, mythid )

            call mdsreadfieldyz( obcs_file0, exf_iprec, exf_yftype, 1
     &           , obcs_y_1, count0, mythid
     &           )
         endif

         if (( first ) .or. ( changed )) then
            call exf_swapffields_y( obcs_y_0, obcs_y_1, mythid )

            call exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year1, 
     I         obcs_file,
     O         obcs_file1,
     I         mycurrenttime, mycurrentiter, mythid )

            call mdsreadfieldyz( obcs_file1, exf_iprec, exf_yftype, 1
     &           , obcs_y_1, count1, mythid
     &           )
         endif

         do bj = mybylo(mythid),mybyhi(mythid)
            do bi = mybxlo(mythid),mybxhi(mythid)
               do j = 1,sny
                  obcs_fld_y(j,bi,bj) = 
     &                 fac             *obcs_y_0(j,bi,bj) +
     &                 (exf_one - fac) *obcs_y_1(j,bi,bj)
               enddo
            enddo
         enddo

      endif

#endif /* ALLOW_OBCS */

      end
