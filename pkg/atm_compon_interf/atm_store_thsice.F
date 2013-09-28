C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_store_thsice.F,v 1.2 2013/09/28 00:19:08 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_STORE_THSICE
C     !INTERFACE:
      SUBROUTINE ATM_STORE_THSICE(
     I                     bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_STORE_THSICE
C     | o Routine for saving Sea-Ice fields from thSIce pkg
C     |   for export to coupling layer.
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
#include "ATMIDS.h"
#include "CPL_PARAMS.h"
#ifdef ALLOW_THSICE
# include "THSICE_PARAMS.h"
# include "THSICE_VARS.h"
#endif
C     == Global variables for coupling interface ==
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj  :: Tile indices
C     myTime :: Current model time
C     myIter :: Current timestep number
C     myThid :: my Thread Id number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     i, j   :: Loop counters
      INTEGER i,j
      _RL cplTimeFraction

C     o Accumulate Sea-Ice Mass from thSIce pkg that will be exported
C       to the coupling layer. seaIceMass is per surface unit, in kg/m2.
c      cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
c      seaIceTime(bi,bj) = seaIceTime(bi,bj) + cplTimeFraction
C-     Needs really to store the last time-step value and not the average
       cplTimeFraction = 1. _d 0
       seaIceTime(bi,bj) = cplTimeFraction
       DO j=1,sNy
        DO i=1,sNx
c         seaIceMass(i,j,bi,bj) = seaIceMass(i,j,bi,bj) +
          seaIceMass(i,j,bi,bj) =
     &                   ( snowHeight(i,j,bi,bj)*rhos
     &                    + iceHeight(i,j,bi,bj)*rhoi
     &                   )*iceMask(i,j,bi,bj)*cplTimeFraction
        ENDDO
       ENDDO

      IF ( atmCpl_exchange_DIC ) THEN
C     o Accumulate Sea-Ice concentration (no-units) from thSIce pkg
C       that will be exported to the coupling layer.
       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       fracIceTime(bi,bj) = fracIceTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
        DO i=1,sNx
          fracIce(i,j,bi,bj) = fracIce(i,j,bi,bj)
     &                       + iceMask(i,j,bi,bj)*cplTimeFraction
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_THSICE */

      RETURN
      END
