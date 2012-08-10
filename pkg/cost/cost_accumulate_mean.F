C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_accumulate_mean.F,v 1.8 2012/08/10 19:36:02 jmc Exp $
C $Name:  $

#include "COST_OPTIONS.h"

      subroutine cost_accumulate_mean( myThid )
C     *==========================================================*
C     | subroutine cost_accumulate_mean                          |
C     | o accumulate mean state for cost evalualtion             |
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

#include "cost.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer bi, bj
      integer myThid

#ifdef ALLOW_COST
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

C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1, Nr
            do j=1,sNy
              do i=1,sNx
                cMeanTheta(i,j,k,bi,bj) = cMeanTheta(i,j,k,bi,bj)
     &                + theta(i,j,k,bi,bj)
     &                /lastinterval*deltaTClock
                cMeanUVel(i,j,k,bi,bj) = cMeanUVel(i,j,k,bi,bj)
     &               + uVel(i,j,k,bi,bj)
     &               /lastinterval*deltaTClock
                cMeanVVel(i,j,k,bi,bj) = cMeanVVel(i,j,k,bi,bj)
     &               + vVel(i,j,k,bi,bj)
     &               /lastinterval*deltaTClock

                cMeanThetaUVel(i,j,k,bi,bj) =
     &               cMeanThetaUVel(i,j,k,bi,bj)
     &               + (theta(i,j,k,bi,bj)+theta(i-1,j,k,bi,bj))
     &                 /2.*uvel(i,j,k,bi,bj)
     &                 *maskW(i,j,k,bi,bj)*maskC(i,j,k,bi,bj)
     &                 /lastinterval*deltaTClock
                cMeanThetaVVel(i,j,k,bi,bj) =
     &               cMeanThetaVVel(i,j,k,bi,bj)
     &               + (theta(i,j,k,bi,bj)+theta(i,j-1,k,bi,bj))
     &                 /2.*vvel(i,j,k,bi,bj)
     &                 *maskS(i,j,k,bi,bj)*maskC(i,j,k,bi,bj)
     &                 /lastinterval*deltaTClock
              end do
            end do
          end do
        end do
      end do

#endif

      RETURN
      END
