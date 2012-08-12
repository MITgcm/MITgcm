C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_test.F,v 1.11 2012/08/12 01:38:02 jmc Exp $
C $Name:  $

#include "COST_OPTIONS.h"

      subroutine cost_test( myThid )
C     *==========================================================*
C     | subroutine cost_test
C     | o this routine computes the cost function for the tiles
C     |   of this processor
C     *==========================================================*
C     | Notes
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "cost.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_COST_TEST
C     == Local variables
      integer bi, bj
      _RL thetaRef
      integer i, j, k
      integer ig, jg
      integer itlo,ithi
      integer jtlo,jthi

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      if ( useCubedSphereExchange) then
         iLocOut =  7
         jLocOut =  28
         kLocOut =  1
      else
         iLocOut =  80
         jLocOut =  30
         kLocOut =  1
      endif

ce    some reference temperature
      thetaRef = 24.0 _d 0

C--   Calculate mask for tracer cells  (0 => land, 1 => water)
      k=1

C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j=1,sNy
          jg = myYGlobalLo-1+(bj-1)*sNy+j
            do i=1,sNx
            ig = myXGlobalLo-1+(bi-1)*sNx+i

#ifndef ALLOW_COST_TSQUARED
            if ((ig .eq. iLocOut) .and. (jg .eq. jLocOut)) then
               write(*,'(a,3(x,i4),a,4(x,i4))')
     &              'COST    ',ig,jg,kLocOut,' TILE ',i,j,bi,bj
               objf_test(bi,bj) = theta(i,j,kLocOut,bi,bj)
cph(
               print *, 'ph-cost ', ig, jg, kLocOut,
     &              theta(i,j,kLocOut,bi,bj)
cph)
            endif
#else
            objf_test(bi,bj) = objf_test(bi,bj) +
     &                         theta(i,j,kLocOut,bi,bj)**2
#endif

            enddo
          enddo
        enddo
      enddo

#endif  /* ALLOW_COST_TEST */

      RETURN
      END
