C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_driver.F,v 1.1 2004/05/14 21:10:34 heimbach Exp $
C $Name:  $

#include "EBM_OPTIONS.h"

      SUBROUTINE EBM_DRIVER( myTime, myIter, myThid )
C     |==========================================================|
C     | S/R top level for EBM                                    |
C     |==========================================================|
C      References:
C      * X. Wang, P. Stone and J. Marotzke, 1999:
C        Global thermohaline circulation. Part I:
C        Sensitivity to atmospheric moisture transport.
C        J. Climate 12(1), 71-82
C      * X. Wang, P. Stone and J. Marotzke, 1999:
C        Global thermohaline circulation. Part II:
C        Sensitivity with interactive transport.
C        J. Climate 12(1), 83-91
C      * M. Nakamura, P. Stone and J. Marotzke, 1994:
C        Destabilization of the thermohaline circulation
C        by atmospheric eddy transports.
C        J. Climate 7(12), 1870-1882

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
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

#ifdef ALLOW_EBM

c--   Compute zonal mean SST
      CALL EBM_ZONALMEAN  ( myTime, myIter, myThid )

c--   Compute area mean SST in NH/SH tropica and mid-lat
      CALL EBM_AREA_T    ( myTime, myIter, myThid )

c--   Comopute qnet, empmr via EBM
      CALL EBM_ATMOSPHERE ( myTime, myIter, myThid )

c--   Compute random wind perturbation
      CALL EBM_WIND_PERTURB( myTime, myIter, myThid )

#endif /* ALLOW_EBM */

      RETURN
      END



