C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_write_pickup.F,v 1.3 2007/05/10 21:15:52 jscott Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_WRITE_PICKUP(
     &               prec, lgf, permCheckPoint, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE  CPL_WRITE_PICKUP                             |
C     | o Store coupling state for restart.                      |
C     | - Oceanic version -                                      |
C     |==========================================================|
C     | Presently, the atmospheric compon. is in charge of the   |
C     |  writing of coupling fields ; nothing done here for now. |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OCNIDS.h"
#include "OCNCPL.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Timestep number.
C     permCheckpoint - Controls whether timestamped or rolling checkpoint.
C     lgf            - Controls whether global files are used.
C     caroffset      - offset in pickup file if carbon variables included
      INTEGER myIter
      INTEGER myThid
      LOGICAL permCheckPoint
      INTEGER prec
      LOGICAL lgf
      INTEGER caroffset
CEndOfInterface

C     == Local variables ==
C     fn          - Workspace for building file name
      CHARACTER*(MAX_LEN_FNAM) fn

      caroffset=0
      IF ( permCheckPoint ) THEN
       WRITE(fn,'(A,I10.10)') 'pickup_cpl.',myIter
      ELSE
       WRITE(fn,'(A,A)') 'pickup_cpl.',checkPtSuff(nCheckLev)
      ENDIF
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,atmSLPr   , 1,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,HeatFlux  , 2,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qShortWave, 3,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qLatent   , 3,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qSensible , 3,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qLongWave , 3,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,uVelGround, 3,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,vVelGround, 3,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,tauX      , 4,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,tauY      , 5,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,FWFlux    , 6,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,iceSaltFlx, 7,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,seaIceMass, 8,myIter,myThid)

      IF ( ocnCpl_exchange_DIC ) THEN 
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,
     &                     airCO2    ,  9,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,
     &                     surfWSpeed, 10,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,
     &                     fracIce   , 11,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,
     &                     fluxCO2cpl, 12,myIter,myThid)
        caroffset=4
      ENDIF

      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,ocMxlD2cpl, 
     &                   9+caroffset, myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,SSTocn2cpl,
     &                   10+caroffset,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,SSSocn2cpl,
     &                   11+caroffset,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,vSqocn2cpl,
     &                   12+caroffset,myIter,myThid)


      RETURN
      END
