#include "EXF_CPPOPTIONS.h"

      subroutine exf_set_obcs_xz ( 
     &       obcs_fld_xz, obcs_file, obcsmask
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

      _RL obcs_xz_0(1-olx:snx+olx,Nr,nsx,nsy)
      _RL obcs_xz_1(1-olx:snx+olx,Nr,nsx,nsy)

c     == end of interface ==

#ifndef ALLOW_AUTODIFF_TAMC
      if ( first ) then
#endif
        call mdsreadfieldxz( obcs_file, exf_iprec, exf_yftype, Nr
     &                     , obcs_xz_1, count0, mythid
     &                     )
#ifndef ALLOW_AUTODIFF_TAMC
      endif
#endif

#ifndef ALLOW_AUTODIFF_TAMC
      if (( first ) .or. ( changed )) then
#endif
        call exf_swapffields_xz( obcs_xz_0, obcs_xz_1, mythid )

        call mdsreadfieldxz( obcs_file, exf_iprec, exf_yftype, Nr
     &                     , obcs_xz_1, count1, mythid
     &                     )
#ifndef ALLOW_AUTODIFF_TAMC
      endif
#endif

      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          do k = 1,Nr
            do i = 1,snx
              obcs_fld_xz(i,k,bi,bj) = 
     &              fac             *obcs_xz_0(i,k,bi,bj) +
     &              (exf_one - fac) *obcs_xz_1(i,k,bi,bj)
            enddo
          enddo
        enddo
      enddo

#endif

      end

      subroutine exf_set_obcs_yz ( 
     &       obcs_fld_yz, obcs_file, obcsmask
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

      _RL obcs_yz_0(1-oly:sny+oly,Nr,nsx,nsy)
      _RL obcs_yz_1(1-oly:sny+oly,Nr,nsx,nsy)

c     == end of interface ==

#ifndef ALLOW_AUTODIFF_TAMC
      if ( first ) then
#endif
        call mdsreadfieldyz( obcs_file, exf_iprec, exf_yftype, Nr
     &                     , obcs_yz_1, count0, mythid
     &                     )
#ifndef ALLOW_AUTODIFF_TAMC
      endif
#endif

#ifndef ALLOW_AUTODIFF_TAMC
      if (( first ) .or. ( changed )) then
#endif
        call exf_swapffields_yz( obcs_yz_0, obcs_yz_1, mythid )

        call mdsreadfieldyz( obcs_file, exf_iprec, exf_yftype, Nr
     &                     , obcs_yz_1, count1, mythid
     &                     )
#ifndef ALLOW_AUTODIFF_TAMC
      endif
#endif

      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          do k = 1,Nr
            do j = 1,sny
              obcs_fld_yz(j,k,bi,bj) = 
     &              fac             *obcs_yz_0(j,k,bi,bj) +
     &              (exf_one - fac) *obcs_yz_1(j,k,bi,bj)
            enddo
          enddo
        enddo
      enddo

#endif

      end

      subroutine exf_init_obcs_xz(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_init_obcs_xz
c     ==================================================================
c
c     o 
c
c     started: heimbach@mit.edu 01-May-2001
c
c     ==================================================================
c     SUBROUTINE exf_init_obcs_xz
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_OBCS

c     == local variables ==

      integer bi, bj
      integer i, k

      _RL obcs_xz_0(1-olx:snx+olx,Nr,nsx,nsy)
      _RL obcs_xz_1(1-olx:snx+olx,Nr,nsx,nsy)

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do k = 1, Nr
            do i = 1, snx
              obcs_xz_0(i,k,bi,bj) = 0. _d 0
              obcs_xz_1(i,k,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

#endif
      end


      subroutine exf_init_obcs_yz(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_init_obcs_yz
c     ==================================================================
c
c     o 
c
c     started: heimbach@mit.edu 01-May-2001
c
c     ==================================================================
c     SUBROUTINE exf_init_obcs_yz
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      integer mythid

      _RL obcs_yz_0(1-oly:sny+oly,Nr,nsx,nsy)
      _RL obcs_yz_1(1-oly:sny+oly,Nr,nsx,nsy)

#ifdef ALLOW_OBCS

c     == local variables ==

      integer bi, bj
      integer i, j, k

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do k = 1, Nr
            do j = 1, sny
              obcs_yz_0(j,k,bi,bj) = 0. _d 0
              obcs_yz_1(j,k,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

#endif
      end
