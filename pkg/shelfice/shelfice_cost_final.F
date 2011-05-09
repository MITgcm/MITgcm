C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_cost_final.F,v 1.1 2011/05/09 15:17:12 heimbach Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

      subroutine shelfice_cost_final( myThid )
C     *==========================================================*
C     | subroutine shelfice_cost_final                           |
C     | o final cost evalualtion                                 |
C     *==========================================================*
C     |                                                          |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SHELFICE.h"
#include "SHELFICE_COST.h"

#ifdef ALLOW_COST
#include "cost.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer bi, bj
      integer myThid

#ifdef ALLOW_COST
C     == Local variables
      _RL thetaRef
      _RL drLoc

      integer i, j, k
      integer ig, jg
      integer itlo,ithi
      integer jtlo,jthi
      integer kp1, klev

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
       do bi = itlo,ithi
ccc        do klev = 1, Nr
         do j=1,sNy
          do i=1,sNx
c--
             objf_shelfice(bi,bj) = objf_shelfice(bi,bj)
     &            + cMeanSHIforS(i,j,bi,bj)/lastinterval/rhoConstFresh
     &            *_rA(i,j,bi,bj)
c--
              end do
            end do
ccc          end do
        end do
      end do

#endif /* ALLOW_COST */

      END
