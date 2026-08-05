#include "ATM_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_INI_VARS
C     !INTERFACE:
      SUBROUTINE CPL_INI_VARS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_INI_VARS
C     | o Initialisation routine for arrays that are used to
C     |   buffer data that interfaces to the coupling layer.
C     | - Atmospheric version -
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CPL_PARAMS.h"
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid

#ifdef COMPONENT_MODULE
C     !LOCAL VARIABLES:
C     i,j,bi,bj :: Loop counters
      INTEGER i,j,bi,bj
CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C-        Export field
          atmSLPr       (i,j,bi,bj) = 0.
          HeatFlux      (i,j,bi,bj) = 0.
          qShortWave    (i,j,bi,bj) = 0.
          tauX          (i,j,bi,bj) = 0.
          tauY          (i,j,bi,bj) = 0.
          EvMPrFlux     (i,j,bi,bj) = 0.
#ifdef ALLOW_LAND
          RunOffFlux    (i,j,bi,bj) = 0.
          RunOffEnFx    (i,j,bi,bj) = 0.
#endif /* ALLOW_LAND */
#ifdef ALLOW_THSICE
          iceSaltFlx    (i,j,bi,bj) = 0.
          seaIceMass    (i,j,bi,bj) = 0.
          saltPlmFlx_cpl(i,j,bi,bj) = 0.
#endif /* ALLOW_THSICE */
#ifdef ALLOW_AIM
          airCO2        (i,j,bi,bj) = 0.
          sWSpeed       (i,j,bi,bj) = 0.
#endif /* ALLOW_AIM */
#ifdef ALLOW_THSICE
          sIceFrac_cpl  (i,j,bi,bj) = 0.
          sIceThick_cpl (i,j,bi,bj) = 0.
          sIceSnowH_cpl (i,j,bi,bj) = 0.
          sIceQ1_cpl    (i,j,bi,bj) = 0.
          sIceQ2_cpl    (i,j,bi,bj) = 0.
#endif /* ALLOW_THSICE */
C-        Import field
c         ocMxlD        (i,j,bi,bj) = 0.
          SSTocn        (i,j,bi,bj) = 0.
          SSSocn        (i,j,bi,bj) = 0.
          vSqocn        (i,j,bi,bj) = 0.
#ifdef ALLOW_AIM
          flxCO2ocn     (i,j,bi,bj) = 0.
#endif /* ALLOW_AIM */
         ENDDO
        ENDDO
        atmSLPrTime   (bi,bj) = 0.
        HeatFluxTime  (bi,bj) = 0.
        qShortwaveTime(bi,bj) = 0.
        tauXTime      (bi,bj) = 0.
        tauYTime      (bi,bj) = 0.
        EvMPrTime     (bi,bj) = 0.
#ifdef ALLOW_LAND
        RunOffTime    (bi,bj) = 0.
        ROEnFxTime    (bi,bj) = 0.
#endif
#ifdef ALLOW_THSICE
        saltFxTime    (bi,bj) = 0.
        sIceMassTime  (bi,bj) = 0.
        saltPlmFlxTime(bi,bj) = 0.
#endif
#ifdef ALLOW_AIM
        airCO2Time    (bi,bj) = 0.
        sWSpeedTime   (bi,bj) = 0.
#endif /* ALLOW_AIM */
#ifdef ALLOW_THSICE
        sIceFracTime  (bi,bj) = 0.
        sIceThickTime (bi,bj) = 0.
        sIceSnowHTime (bi,bj) = 0.
        sIceQ1Time    (bi,bj) = 0.
        sIceQ2Time    (bi,bj) = 0.
#endif
       ENDDO
      ENDDO

      IF ( startTime .NE. baseTime .OR. nIter0 .NE. 0 ) THEN
       CALL ATM_CPL_READ_PICKUP( nIter0, myThid )
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
