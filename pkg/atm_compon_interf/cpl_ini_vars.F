C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/cpl_ini_vars.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_INI_VARS( myThid )
C     /==========================================================\
C     | SUBROUTINE CPL_INI_VARS                                  |
C     | o Initialisation routine for arrays that are used to     |
C     |   buffer data that interfaces to the coupling layer.     |
C     | - Atmospheric version -                                  |
C     |==========================================================|
C     | For now this routine sets these arrays to the right      |
C     | state for an initial start-up. Eventually it should      |
C     | include some way to load this data from non-volatile     |
C     | external storage if they are needed during a restart.    |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ATMCPL.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I,J,K,bi,bj - Loop counters
      INTEGER prec
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER I,J,K,bi,bj

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myByLo(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C         Export field
          HeatFlux(I,J,bi,bj)   = 0.
          EvMPrFlux( I,J,bi,bj) = 0.
          RunOffFlux(I,J,bi,bj) = 0.
          tauX(    I,J,bi,bj)   = 0.
          tauY(    I,J,bi,bj)   = 0.
          uVelGround(I,J,bi,bj) = 0.
          vVelGround(I,J,bi,bj) = 0.
          Qsensible(I,J,bi,bj)  = 0.
          Qshortwave(I,J,bi,bj) = 0.
          Qlongwave(I,J,bi,bj)  = 0.
          Qlatent(I,J,bi,bj)    = 0.
C         Import field
          SSTocn (I,J,bi,bj)    = 0.
         ENDDO
        ENDDO
        HeatFluxTime(bi,bj)   = 0.
        EvMPrTime(bi,bj)      = 0.
        RunOffTime(bi,bj)     = 0.
        tauXTime(bi,bj)       = 0.
        tauYTime(bi,bj)       = 0.
        uVelGroundTime(bi,bj) = 0.
        vVelGroundTime(bi,bj) = 0.
        qSensibleTime(bi,bj)  = 0.
        qLatentTime(bi,bj)    = 0.
        qLongwaveTime(bi,bj)  = 0.
        qShortwaveTime(bi,bj) = 0.
       ENDDO
      ENDDO

      IF ( startTime .NE. 0. .OR.  nIter0 .NE. 0 ) THEN
       prec=precFloat64
       WRITE(fn,'(A,I10.10)') 'pickup_cpl.',nIter0
       CALL MDSREADFIELD(fn,prec,'RL',1,SSTocn    , 1,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,HeatFlux  , 2,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,tauX      , 3,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,tauY      , 4,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,EvMPrFlux , 5,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,RunOffFlux, 6,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,uVelGround, 7,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,vVelGround, 8,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,qLatent   , 9,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,qSensible ,10,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,qLongwave ,11,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,qShortwave,12,myThid)
      ENDIF

      RETURN
      END
