c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_swapffields.F,v 1.2 2002/02/07 20:00:09 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exf_SwapFFields(
     U                            ffld0,
     U                            ffld1,
     I                            mythid
     &                          )

c     ==================================================================
c     SUBROUTINE exf_SwapFFields
c     ==================================================================
c
c     o Copy a forcing field ffld1 to ffld0 and set ffld0 to zero.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c
c              - Changed Routine names (package prefix: exf_)
c
c     ==================================================================
c     SUBROUTINE exf_SwapFFields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      integer mythid
      _RL ffld0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL ffld1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

c     == local variables ==

      integer bi
      integer bj
      integer  i
      integer  j
      integer  itlo,ithi
      integer  jtlo,jthi

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1,sny
            do i = 1,snx
              ffld0(i,j,bi,bj) = ffld1(i,j,bi,bj)
              ffld1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      end

      subroutine exf_SwapFFields_3d(
     U                            ffld0,
     U                            ffld1,
     I                            mythid
     &                          )

c     ==================================================================
c     SUBROUTINE exf_SwapFFields_3d
c     ==================================================================
c
c     o Copy a forcing field ffld1 to ffld0 and set ffld0 to zero.
c
c     ==================================================================
c     SUBROUTINE exf_SwapFFields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      integer mythid
      _RL ffld0(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL ffld1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

c     == local variables ==

      integer bi
      integer bj
      integer  i
      integer  j
      integer  k
      integer  itlo,ithi
      integer  jtlo,jthi

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = 1,sny
              do i = 1,snx
                ffld0(i,j,k,bi,bj) = ffld1(i,j,k,bi,bj)
                ffld1(i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

      end

      subroutine exf_swapffields_xz(
     U                               ffld0,
     U                               ffld1,
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE exf_swapffields_xz
c     ==================================================================
c
c     o Copy a forcing field ffld1 to ffld0 and set ffld0 to zero.
c
c     started: heimbach@mit.edu 01-May-2001
c
c     ==================================================================
c     SUBROUTINE exf_swapffields_xz
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      integer mythid
      _RL ffld0(1-olx:snx+olx,Nr,nsx,nsy)
      _RL ffld1(1-olx:snx+olx,Nr,nsx,nsy)

c     == local variables ==

      integer bi, bj
      integer  i, k
      integer  itlo,ithi
      integer  jtlo,jthi

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,Nr
            do i = 1,snx
              ffld0(i,k,bi,bj) = ffld1(i,k,bi,bj)
              ffld1(i,k,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      end

      subroutine exf_swapffields_yz(
     U                               ffld0,
     U                               ffld1,
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE exf_swapffields_yz
c     ==================================================================
c
c     o Copy a forcing field ffld1 to ffld0 and set ffld0 to zero.
c
c     started: heimbach@mit.edu 01-May-2001
c
c     ==================================================================
c     SUBROUTINE exf_swapffields_yz
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      integer mythid
      _RL ffld0(1-oly:sny+oly,Nr,nsx,nsy)
      _RL ffld1(1-oly:sny+oly,Nr,nsx,nsy)

c     == local variables ==

      integer bi, bj
      integer  j, k
      integer  itlo,ithi
      integer  jtlo,jthi

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,Nr
            do j = 1,sny
              ffld0(j,k,bi,bj) = ffld1(j,k,bi,bj)
              ffld1(j,k,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      end
