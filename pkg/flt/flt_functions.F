C $Header: /u/gcmpack/MITgcm/pkg/flt/Attic/flt_functions.F,v 1.1 2001/09/13 17:43:55 adcroft Exp $
C $Name:  $

#include "FLT_CPPOPTIONS.h"

      _RL     function global2local_i(
     I                                rG,
     I                                bi,
     I                                bj,
     I                                mythid
     &                               )

c     ==================================================================
c     FUNCTION global2local_i
c     ==================================================================
c
c     o Converts global x-coordinates (grid) to local index for specific 
c       tile
c
c     ==================================================================
c     FUNCTION global2local_i
c     ==================================================================

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"

c     == routine arguments ==

      integer mythid, bi, bj
      _RL rG, fp, fm, dist, xlo, xhi
      INTEGER i, iG 

c     == local variables ==


c     == end of interface ==
      do i=0,sNx

         iG = myXGlobalLo + (bi-1)*sNx
         xlo = xc(i,  1,bi,bj)
         if (i.eq.0)   xlo = xc(i,1,bi,bj)-delX(iG)
         xhi = xc(i+1,1,bi,bj)
         if (i.eq.sNx) xhi = xc(i,1,bi,bj)+delX(iG+i-1)

         if (xlo .le. rG .and. xhi .gt. rG) then
            dist = xhi - xlo
            fp = (xhi - rG) / dist
            fm = (rG - xlo) / dist
            global2local_i = fp*i + fm*(i+1)
            goto 100
         endif
      enddo

 100  continue

      return
      end


      _RL    function global2local_j(
     I                                rG,
     I                                bi,
     I                                bj,
     I                                mythid
     &                               )

c     ==================================================================
c     FUNCTION global2local_j
c     ==================================================================
c
c     o Converts global y-coordinates (grid) to local index for specific 
c       tile
c
c              
c     started: Arne Biastoch abiastoch@ucsd.edu  07-AUG-2000
c
c
c     ==================================================================
c     FUNCTION global2local_j
c     ==================================================================

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"

c     == routine arguments ==

      integer mythid, bi, bj
      _RL rG, fp, fm, dist, ylo, yhi
      INTEGER j, jG

c     == local variables ==


c     == end of interface ==

      do j=0,sNy

         jG = myYGlobalLo + (bj-1)*sNy
         ylo = yc(1,j,  bi,bj)
         if (j.eq.0)   ylo = yc(1,j,bi,bj)-delY(jG)
         yhi = yc(1,j+1,bi,bj)
         if (j.eq.sNy) yhi = yc(1,j,bi,bj)+delY(jG+j-1)

         if (ylo .le. rG .and. yhi .gt. rG) then
            dist = yhi - ylo
            fp = (yhi - rG) / dist
            fm = (rG - ylo) / dist
            global2local_j = fp*j + fm*(j+1)
            goto 100
         endif
      enddo

 100  continue

      return
      end

      _RL     function global2local_k(
     I                                rG,
     I                                bi,
     I                                bj,
     I                                mythid
     &                               )

c     ==================================================================
c     FUNCTION global2local_k
c     ==================================================================
c
c     o Converts global z-coordinates (grid) to local index for specific 
c       tile
c
c              
c     started: Arne Biastoch abiastoch@ucsd.edu  07-AUG-2000
c
c
c     ==================================================================
c     FUNCTION global2local_k
c     ==================================================================

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"

c     == routine arguments ==

      integer mythid, bi, bj
      _RL rG, fp, fm, dist, zlo, zhi
      INTEGER k  

c     == local variables ==


c     == end of interface ==
      do k=0,Nr

         zlo = rC(k)
         if (k.eq.0)  zlo = rC(k)-delZ(1)*0.5
         zhi = rC(k+1)
         if (k.eq.Nr) zhi = rC(k)+delZ(Nr)*0.5

         if (zlo .le. rG .and. zhi .gt. rG) then
            dist = zhi - zlo
            fp = (zhi - rG) / dist
            fm = (rG - zlo) / dist
            global2local_k = fp*k + fm*(k+1)
            goto 100
         endif
      enddo

 100  continue

      return
      end

