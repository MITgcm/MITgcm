C $Header: /u/gcmpack/MITgcm/verification/OpenAD/code_ad_singlelayer/cost_test.F,v 1.2 2007/08/14 21:02:18 heimbach Exp $

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
      thetaRef = 20.0D0

C--   Calculate mask for tracer cells  (0 => land, 1 => water)
      k=1

C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j=1,sNy
          jg = myYGlobalLo-1+(bj-1)*sNy+j
            do i=1,sNx
            ig = myXGlobalLo-1+(bi-1)*sNx+i
c
            objf_test(bi,bj) = objf_test(bi,bj) + maskW(i,j,k,bi,bj)
     &           *( uVel(i,j,k,bi,bj) )
     &           *( uVel(i,j,k,bi,bj) )
cph(
               print *, 'ph-cost: objf_test ', ig, jg,
     &              objf_test(bi,bj)
cph)
c
            end do
          end do
        end do
      end do

#endif

      END
