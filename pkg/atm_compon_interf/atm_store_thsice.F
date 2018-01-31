#include "ATM_CPL_OPTIONS.h"

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

      IF ( atm_cplExch1W_sIce ) THEN
C     o Accumulate Sea-Ice Mass from thSIce pkg that will be exported
C       to the coupling layer. seaIceMass is per surface unit, in kg/m2.
c      cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
c      sIceMassTime(bi,bj) = sIceMassTime(bi,bj) + cplTimeFraction
C-     Needs really to store the last time-step value and not the average
        sIceMassTime(bi,bj) = 1. _d 0
        DO j=1,sNy
         DO i=1,sNx
c          seaIceMass(i,j,bi,bj) = seaIceMass(i,j,bi,bj) + cplTimeFraction*
           seaIceMass(i,j,bi,bj) =
     &                   ( snowHeight(i,j,bi,bj)*rhos
     &                    + iceHeight(i,j,bi,bj)*rhoi
     &                   )*iceMask(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF

      IF ( ( atm_cplExch1W_sIce.AND.atm_cplExch_DIC )
     &  .OR. atm_cplExch2W_sIce ) THEN
C     o Store Sea-Ice concentration (no-units) from thSIce pkg
C       that will be exported to the coupling layer.
        sIceFracTime(bi,bj) = 1. _d 0
        DO j=1,sNy
         DO i=1,sNx
           sIceFrac_cpl(i,j,bi,bj) = iceMask(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF

      IF ( atm_cplExch2W_sIce ) THEN
C     o Store other thSIce state-vars
C       that will be exported to the coupling layer.
        sIceThickTime(bi,bj) = 1. _d 0
        sIceSnowHTime(bi,bj) = 1. _d 0
        sIceQ1Time   (bi,bj) = 1. _d 0
        sIceQ2Time   (bi,bj) = 1. _d 0
        DO j=1,sNy
         DO i=1,sNx
           sIceThick_cpl(i,j,bi,bj) = iceHeight(i,j,bi,bj)
           sIceSnowH_cpl(i,j,bi,bj) = snowHeight(i,j,bi,bj)
           sIceQ1_cpl   (i,j,bi,bj) = Qice1(i,j,bi,bj)
           sIceQ2_cpl   (i,j,bi,bj) = Qice2(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF

#endif /* ALLOW_THSICE */

      RETURN
      END
