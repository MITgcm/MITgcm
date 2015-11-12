C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/cpl_ini_vars.F,v 1.5 2015/11/12 01:01:58 jmc Exp $
C $Name:  $

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
C     i,j,bi,bj :: Loop counters
      INTEGER i,j,bi,bj

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myByLo(myThid)
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
C-        Export field
          atmSLPr(   i,j,bi,bj) = 0.
          HeatFlux(  i,j,bi,bj) = 0.
          qShortWave(i,j,bi,bj) = 0.
          tauX(      i,j,bi,bj) = 0.
          tauY(      i,j,bi,bj) = 0.
          EvMPrFlux( i,j,bi,bj) = 0.
          RunOffFlux(i,j,bi,bj) = 0.
          RunOffEnFx(i,j,bi,bj) = 0.
          iceSaltFlx(i,j,bi,bj) = 0.
          seaIceMass(i,j,bi,bj) = 0.
          airCO2    (i,j,bi,bj) = 0.
          sWSpeed   (i,j,bi,bj) = 0.
          fracIce   (i,j,bi,bj) = 0.
C-        Import field
c         ocMxlD   (i,j,bi,bj)  = 0.
          SSTocn   (i,j,bi,bj)  = 0.
          SSSocn   (i,j,bi,bj)  = 0.
          vSqocn   (i,j,bi,bj)  = 0.
          flxCO2ocn(i,j,bi,bj)  = 0.
         ENDDO
        ENDDO
        atmSLPrTime(bi,bj)    = 0.
        HeatFluxTime(bi,bj)   = 0.
        qShortwaveTime(bi,bj) = 0.
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
