C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_accumulate_mean.F,v 1.2 2007/09/28 12:45:53 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

      subroutine seaice_cost_accumulate_mean( myThid )
C     *==========================================================*
C     | subroutine seaice_cost_accumulate_mean                   |
C     | o accumulate time-mean state for cost evalualtion        |
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
#include "SEAICE.h"
#ifdef ALLOW_COST
# include "cost.h"
#endif

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

#ifdef ALLOW_SEAICE_COST_EXPORT

C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j=1,sNy
            do i=1,sNx
                uHeffExportCell(i,j,bi,bj) = 
     &               uHeffExportCell(i,j,bi,bj)
     &               + HEFF(i,j,1,bi,bj)*UICE(i,j,1,bi,bj)
     &               *_dyG(i,j,bi,bj)/lastinterval*deltaTClock
                vHeffExportCell(i,j,bi,bj) = 
     &               vHeffExportCell(i,j,bi,bj)
     &               + HEFF(i,j,1,bi,bj)*VICE(i,j,1,bi,bj)
     &               *_dxG(i,j,bi,bj)/lastinterval*deltaTClock
            end do
          end do
        end do
      end do

#endif

#endif

      END
