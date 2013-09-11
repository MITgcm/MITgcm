C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_store_dynvars.F,v 1.1 2013/09/11 20:17:27 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_STORE_DYNVARS
C     !INTERFACE:
      SUBROUTINE ATM_STORE_DYNVARS(
     I                     bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_STORE_DYNVARS
C     | o Routine for saving dynamical fields (e.g.,  atmos.
C     |   sea-level pressure ) for export to coupling layer.
C     *==========================================================*
C     | This version interfaces to the main model
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
C     == Global variables (main model)
#include "DYNVARS.h"
C     == Global variables for coupling interface ==
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj    :: Tile indices
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id. number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i, j      :: Loop counters
      INTEGER i,j
      _RL cplTimeFraction
CEOP

C     o Accumulate Sea-level pressure anomaly from dynamical core
C       to the coupling layer. Eta is in Pa
       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       atmSLPrTime(bi,bj) = atmSLPrTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
        DO i=1,sNx
          atmSLPr(i,j,bi,bj) = atmSLPr(i,j,bi,bj)
     &                 + etaN(i,j,bi,bj)*cplTimeFraction
        ENDDO
       ENDDO

      RETURN
      END
