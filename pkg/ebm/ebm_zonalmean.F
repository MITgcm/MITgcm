C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_zonalmean.F,v 1.1 2004/05/14 21:10:34 heimbach Exp $
C $Name:  $

#include "EBM_OPTIONS.h"

      SUBROUTINE EBM_ZONALMEAN( myTime, myIter, myThid )
C     |==========================================================|
C     | S/R CALCULATE ZONAL MEAN TEMPERATURE                     |
C     |==========================================================|

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_EBM
# include "EBM.h"
#endif

C     === Routine arguments ===
C     myThid - Instance number for this innvocation 
      INTEGER myThid
      INTEGER myIter
      _RL myTime

CEndOfInterface
C     == Local variables ==
C     I, J, K - Loop counters
C     CountX_tile - number of ocean points in each latitude band on each tile
C     maskC - Land/Ocean mask
C     ZonalMean_tile - zonal temperature average on each tile

#ifdef ALLOW_EBM

      INTEGER i, j, k, bi, bj
      _RL CountX_tile(1-OLy:sNy+OLy,nSx,nSy)
      _RL ZonalMean_tile(1-OLy:sNy+OLy, nSx, nSy)

C--   Top layer only
      k = 1

c--   Initialise
      DO j=1-OLy,sNy+OLy
         ZonalMeanSST(j) = 0.0
         CountX(j) = 0.0
      ENDDO

C--   Calculate the zonal mean
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         CountX_tile(j,bi,bj) = 0.0
         ZonalMean_tile(j,bi,bj) = 0.0
         DO i=1,sNx
          ZonalMean_tile(j,bi,bj) = ZonalMean_tile(j,bi,bj) +
     &           theta(i,j,k,bi,bj)
          CountX_tile(j,bi,bj) = CountX_tile(j,bi,bj) +  
     &         maskC(i,j,k,bi,bj)
         ENDDO
         ZonalMeanSST(j) = ZonalMeanSST(j) + ZonalMean_tile(j,bi,bj)
         CountX(j) = CountX(j) + CountX_tile(j,bi,bj)
        ENDDO
       ENDDO
      ENDDO

      DO j=1-OLy,sNy+OLy
       IF ( CountX(j) .GT. 0.0) THEN
          ZonalMeanSST(j) = ZonalMeanSST(j)/CountX(j)
       ENDIF
      ENDDO

#endif /* ALLOW_EBM */

      RETURN
      END



