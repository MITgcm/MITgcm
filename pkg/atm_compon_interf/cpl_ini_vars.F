C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/cpl_ini_vars.F,v 1.4 2007/06/18 21:28:53 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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
C     | For now this routine sets these arrays to the right
C     | state for an initial start-up. Eventually it should
C     | include some way to load this data from non-volatile
C     | external storage if they are needed during a restart.
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
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     == Local variables ==
C     I,J,bi,bj :: Loop counters
      INTEGER I,J,bi,bj

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myByLo(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C-        Export field
          atmSLPr(   I,J,bi,bj) = 0.
          HeatFlux(  I,J,bi,bj) = 0.
          qShortWave(I,J,bi,bj) = 0.
c         qSensible( I,J,bi,bj) = 0.
c         qLatent(   I,J,bi,bj) = 0.
c         qLongWave( I,J,bi,bj) = 0.
c         uVelGround(I,J,bi,bj) = 0.
c         vVelGround(I,J,bi,bj) = 0.
          tauX(      I,J,bi,bj) = 0.
          tauY(      I,J,bi,bj) = 0.
          EvMPrFlux( I,J,bi,bj) = 0.
          RunOffFlux(I,J,bi,bj) = 0.
          RunOffEnFx(I,J,bi,bj) = 0.
          iceSaltFlx(I,J,bi,bj) = 0.
          seaIceMass(I,J,bi,bj) = 0.
          airCO2    (I,J,bi,bj) = 0.
          sWSpeed   (I,J,bi,bj) = 0.
          fracIce   (I,J,bi,bj) = 0.
C-        Import field
c         ocMxlD   (I,J,bi,bj)  = 0.
          SSTocn   (I,J,bi,bj)  = 0.
          SSSocn   (I,J,bi,bj)  = 0.
          vSqocn   (I,J,bi,bj)  = 0.
          flxCO2ocn(I,J,bi,bj)  = 0.
         ENDDO
        ENDDO
        atmSLPrTime(bi,bj)    = 0.
        HeatFluxTime(bi,bj)   = 0.
        qShortwaveTime(bi,bj) = 0.
c       qSensibleTime(bi,bj)  = 0.
c       qLatentTime(bi,bj)    = 0.
c       qLongwaveTime(bi,bj)  = 0.
c       uVelGroundTime(bi,bj) = 0.
c       vVelGroundTime(bi,bj) = 0.
        tauXTime(bi,bj)       = 0.
        tauYTime(bi,bj)       = 0.
        EvMPrTime(bi,bj)      = 0.
        RunOffTime(bi,bj)     = 0.
        ROEnFxTime(bi,bj)     = 0.
        saltFxTime(bi,bj)     = 0.
        seaIceTime(bi,bj)     = 0.
        airCO2Time(bi,bj)     = 0.
        sWSpeedTime(bi,bj)    = 0.
        fracIceTime(bi,bj)    = 0.
       ENDDO
      ENDDO

      IF ( startTime .NE. baseTime .OR. nIter0 .NE. 0 ) THEN
       CALL ATM_CPL_READ_PICKUP( nIter0, myThid )
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
