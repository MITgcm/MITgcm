C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_accumulate_mean.F,v 1.5 2010/03/15 16:08:29 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

      subroutine seaice_cost_accumulate_mean( 
     &     myTime, myIter, myThid )
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
      integer myIter
      _RL myTime

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

      IF ( myTime .GT. (EndTime - lastinterval) ) THEN

C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j=1,sNy
            do i=1,sNx
              uHeffExportCell(i,j,bi,bj) = 
     &           uHeffExportCell(i,j,bi,bj)
     &           + ( 0.92*0.5*(HEFF(i,j,bi,bj)+HEFF(i-1,j,bi,bj))
     &             + 0.33*0.5*(HSNOW(i,j,bi,bj)+HSNOW(i-1,j,bi,bj)) )
     &           *UICE(i,j,bi,bj)
     &           *_dyG(i,j,bi,bj)/lastinterval*deltaTClock
              vHeffExportCell(i,j,bi,bj) = 
     &           vHeffExportCell(i,j,bi,bj)
     &           + ( 0.92*0.5*(HEFF(i,j,bi,bj)+HEFF(i,j-1,bi,bj))
     &             + 0.33*0.5*(HSNOW(i,j,bi,bj)+HSNOW(i,j-1,bi,bj)) )
     &           *VICE(i,j,bi,bj)
     &           *_dxG(i,j,bi,bj)/lastinterval*deltaTClock
            end do
          end do
        end do
      end do

      ENDIF

#endif

#endif

      END
