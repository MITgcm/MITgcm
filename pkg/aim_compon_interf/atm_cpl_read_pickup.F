C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_cpl_read_pickup.F,v 1.1 2004/05/21 19:59:38 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_CPL_READ_PICKUP( myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE ATM_CPL_READ_PICKUP
C     | o Reads fields from a pickup file for a restart
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "ATMCPL.h"

C     == Routine arguments ==
C     myIter  :: time-step number
C     myThid  :: Thread number for this instance of the routine
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE
C     == Local variables ==
      INTEGER prec
      CHARACTER*(MAX_LEN_FNAM) fn

      WRITE(fn,'(A,I10.10)') 'pickup_cpl.',myIter
      prec = precFloat64

      IF ( cpl_oldPickup ) THEN
        CALL MDSREADFIELD(fn,prec,'RL',1,SSTocn    , 1,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,HeatFlux  , 2,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,tauX      , 3,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,tauY      , 4,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,EvMPrFlux , 5,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,RunOffFlux, 6,myThid)
c       CALL MDSREADFIELD(fn,prec,'RL',1,uVelGround, 7,myThid)
c       CALL MDSREADFIELD(fn,prec,'RL',1,vVelGround, 8,myThid)
c       CALL MDSREADFIELD(fn,prec,'RL',1,qLatent   , 9,myThid)
c       CALL MDSREADFIELD(fn,prec,'RL',1,qSensible ,10,myThid)
c       CALL MDSREADFIELD(fn,prec,'RL',1,qLongwave ,11,myThid)
c       CALL MDSREADFIELD(fn,prec,'RL',1,qShortwave,12,myThid)
      ELSE
        CALL MDSREADFIELD(fn,prec,'RL',1, ocMxlD   , 1,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1, SSTocn   , 2,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1, SSSocn   , 3,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1, vSqocn   , 4,myThid)
       IF ( cpl_earlyExpImpCall ) THEN
        CALL MDSREADFIELD(fn,prec,'RL',1,atmSLPr   , 5,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,HeatFlux  , 6,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,qShortWave, 7,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,tauX      , 8,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,tauY      , 9,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,EvMPrFlux ,10,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,RunOffFlux,11,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,RunOffEnFx,12,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,iceSaltFlx,13,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,seaIceMass,14,myThid) 
       ENDIF
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
