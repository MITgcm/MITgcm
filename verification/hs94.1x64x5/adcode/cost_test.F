C $Header: /u/gcmpack/MITgcm/verification/hs94.1x64x5/adcode/Attic/cost_test.F,v 1.1 2001/11/20 20:44:24 heimbach Exp $

#include "CPP_OPTIONS.h"

      subroutine cost_test( myThid )
C     /==========================================================\
C     | subroutine cost_test                                     |
C     | o this routine computes the cost function for the tiles  |
C     |   of this processor                                      |
C     |==========================================================|
C     |                                                          |
C     | Notes                                                    |
C     | =====                                                    |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_PASSIVE_TRACER
# include "TR1.h"
#endif

#include "cost.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer bi, bj
      integer myThid

#ifdef ALLOW_COST_TEST
C     == Local variables
      _RL thetaRef

      integer i, j, k
      integer ig, jg
      integer itlo,ithi
      integer jtlo,jthi

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      iLocOut =  1
      jLocOut =  32
      kLocOut =  1

      do bj = jtlo,jthi
        do bi = itlo,ithi
          objf_test(bi,bj)= 0. _d 0
        enddo
      enddo

ce    some reference temperature
      thetaRef = 24.0D0
C--   Calculate mask for tracer cells  (0 => land, 1 => water)
      k=1

C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
        do bi = itlo,ithi
c
          do j=1,sNy
          jg = myYGlobalLo-1+(bj-1)*sNy+j
            do i=1,sNx
            ig = myXGlobalLo-1+(bi-1)*sNx+i

            if ((jg .gt.30) .and. (jg .lt.40)) then
               objf_test(bi,bj) = objf_test(bi,bj) +
     &              theta(i,j,kLocOut,bi,bj)
               write(*,'(a,F10.1,3(x,i4),a,4(x,i4))')
     &              'objf_test  ', objf_test(bi,bj),
     &              ig,jg,kLocOut,' TILE ',i,j,bi,bj
            endif

            end do
          end do
c
          objf_test(bi,bj) = objf_test(bi,bj) / 9. _d 0
c
        end do
      end do

#endif

      END
