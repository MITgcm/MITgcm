C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/cpl_write_pickup.F,v 1.2 2004/05/21 20:00:48 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_WRITE_PICKUP(
     &               prec, lgf, permCheckPoint, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE  CPL_WRITE_PICKUP                             |
C     | o Store coupling state for restart.                      |
C     | - Atmospheric version -                                  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ATMCPL.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Timestep number.
C     permCheckpoint - Controls whether timestamped or rolling checkpoint.
C     lgf            - Controls whether global files are used.
      INTEGER myIter
      INTEGER myThid
      LOGICAL permCheckPoint
      INTEGER prec
      LOGICAL lgf
CEndOfInterface

#ifdef COMPONENT_MODULE
C     == Local variables ==
C     fn          - Workspace for building file name
      CHARACTER*(MAX_LEN_FNAM) fn

      IF ( permCheckPoint ) THEN
       WRITE(fn,'(A,I10.10)') 'pickup_cpl.',myIter
      ELSE
       WRITE(fn,'(A,A)') 'pickup_cpl.',checkPtSuff(nCheckLev)
      ENDIF
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,SSTocn    , 1,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,HeatFlux  , 2,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,tauX      , 3,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,tauY      , 4,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,EvMPrFlux , 5,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,RunOffFlux, 6,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,uVelGround, 7,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,vVelGround, 8,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qLatent   , 9,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qSensible ,10,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qLongwave ,11,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qShortwave,12,myIter,myThid)

      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,ocMxlD    , 1,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,SSTocn    , 2,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,SSSocn    , 3,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,vSqocn    , 4,myIter,myThid)

      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,atmSLPr   , 5,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,HeatFlux  , 6,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qShortWave, 7,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qLatent   , 7,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qSensible , 7,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qLongWave , 7,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,uVelGround, 7,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,vVelGround, 7,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,tauX      , 8,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,tauY      , 9,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,EvMPrFlux ,10,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,RunOffFlux,11,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,RunOffEnFx,12,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,iceSaltFlx,13,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,seaIceMass,14,myIter,myThid)

#endif /* COMPONENT_MODULE */

      RETURN
      END
