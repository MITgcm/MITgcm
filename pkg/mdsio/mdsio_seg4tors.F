C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_seg4tors.F,v 1.1 2001/03/06 15:28:54 adcroft Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      subroutine MDS_SEG4toRS( j,bi,bj,k,nNz, seg, copyTo, arr )
C IN:
C     j,bi,bj,k	integer - indices to array "arr"
C     nNz	integer - K dimension of array "arr"
C     seg	Real*4  - 1-D vector of length sNx
C OUT:
C     arr	_RS     - model tiled array
C
C Created: 03/20/99 adcroft@mit.edu

      implicit none
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      integer j,bi,bj,k,nNz
      _RS arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz,nSx,nSy)
      logical copyTo
      Real*4 seg(sNx)
C Local
      integer ii
C     ------------------------------------------------------------------
      if (copyTo) then
       do ii=1,sNx
        arr(ii,j,k,bi,bj)=seg(ii)
       enddo
      else
       do ii=1,sNx
        seg(ii)=arr(ii,j,k,bi,bj)
       enddo
      endif
C     ------------------------------------------------------------------
      return
      end
