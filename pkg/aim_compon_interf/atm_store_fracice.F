C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_store_fracice.F,v 1.1 2007/06/18 21:28:52 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_STORE_FRACICE
C     !INTERFACE:
      SUBROUTINE ATM_STORE_FRACICE( bi,bj,
     I                              myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_STORE_FRACICE
C     | o Routine for saving atmos. Sea-Ice Fraction from thSIce
C     |   pkg for export to coupling layer.
C     *==========================================================*
C     | This version interfaces to the THSICE package.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#ifdef ALLOW_THSICE
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#endif
C     == Global variables for coupling interface ==
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj  :: Tile indices
C     myTime :: Current model time
C     myIter :: Current timestep number
C     myThid :: my Thread Id number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j
      _RL cplTimeFraction

C     o Accumulate Sea-Ice Mass from thSIce pkg that will be exported
C       to the coupling layer. seaIceMass is per surface unit, in kg/m2.
       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       fracIceTime(bi,bj) = fracIceTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
         DO i=1,sNx
          fracIce(i,j,bi,bj) = fracIce(i,j,bi,bj)
     &                       + iceMask(i,j,bi,bj)*cplTimeFraction
         ENDDO
       ENDDO

#endif /* ALLOW_THSICE */

      RETURN
      END
