#include "EXF_OPTIONS.h"

      subroutine exf_set_obcs_xz ( 
     &       obcs_fld_xz, obcs_xz_0, obcs_xz_1
     I     , obcs_file, obcsmask
     I     , fac, first, changed, count0, count1
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
#include "exf_param.h"
#include "exf_constants.h"

c     == routine arguments ==

      _RL obcs_fld_xz(1-olx:snx+olx,Nr,nsx,nsy)
      _RL obcs_xz_0(1-olx:snx+olx,Nr,nsx,nsy)
      _RL obcs_xz_1(1-olx:snx+olx,Nr,nsx,nsy)

      character*(128) obcs_file
      character*1 obcsmask
      logical first, changed
      integer count0, count1
      _RL     fac
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_OBCS

c     == local variables ==

      integer bi, bj
      integer i, k

c     == end of interface ==

      if ( obcs_file .NE. ' ' ) then

         if ( first ) then
            call mdsreadfieldxz( obcs_file, exf_iprec, exf_yftype, Nr
     &           , obcs_xz_1, count0, mythid
     &           )
         endif

         if (( first ) .or. ( changed )) then
            call exf_swapffields_xz( obcs_xz_0, obcs_xz_1, mythid )

            call mdsreadfieldxz( obcs_file, exf_iprec, exf_yftype, Nr
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

#endif

      end

      subroutine exf_set_obcs_yz ( 
     &       obcs_fld_yz, obcs_yz_0, obcs_yz_1
     I     , obcs_file, obcsmask
     I     , fac, first, changed, count0, count1
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
#include "exf_param.h"
#include "exf_constants.h"

c     == routine arguments ==

      _RL obcs_fld_yz(1-oly:sny+oly,Nr,nsx,nsy)
      _RL obcs_yz_0(1-oly:sny+oly,Nr,nsx,nsy)
      _RL obcs_yz_1(1-oly:sny+oly,Nr,nsx,nsy)
      character*(MAX_LEN_FNAM) obcs_file
      character*1 obcsmask
      logical first, changed
      integer count0, count1
      _RL     fac
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_OBCS

c     == local variables ==

      integer bi, bj
      integer j, k

c     == end of interface ==

      if ( obcs_file .NE. ' ' ) then

         if ( first ) then
            call mdsreadfieldyz( obcs_file, exf_iprec, exf_yftype, Nr
     &           , obcs_yz_1, count0, mythid
     &           )
         endif

         if (( first ) .or. ( changed )) then
            call exf_swapffields_yz( obcs_yz_0, obcs_yz_1, mythid )

            call mdsreadfieldyz( obcs_file, exf_iprec, exf_yftype, Nr
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

#endif

      end
