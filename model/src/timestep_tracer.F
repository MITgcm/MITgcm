C $Header: /u/gcmpack/MITgcm/model/src/timestep_tracer.F,v 1.10 2001/09/28 02:26:57 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TIMESTEP_TRACER
C     !INTERFACE:
      SUBROUTINE TIMESTEP_TRACER(
     I                     bi, bj, iMin, iMax, jMin, jMax, K,
     I                     advection_scheme,
     I                     tracer, gTracer,
     I                     myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R TIMESTEP_TRACER                                       
C     | o Step model tracer field forward in time                 
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      INTEGER advection_scheme
      _RL  tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Extrapolate tendancies to mid-time level (n+1/2)
      DO j=jMin,jMax
       DO i=iMin,iMax
         gTracer(i,j,k,bi,bj)=tracer(i,j,k,bi,bj)
     &            +deltaTtracer*gTracer(i,j,k,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
