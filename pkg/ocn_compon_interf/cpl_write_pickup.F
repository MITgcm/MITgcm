C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_write_pickup.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
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
#include "OCNCPL.h"

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

C     == Local variables ==
C     fn          - Workspace for building file name
      CHARACTER*(MAX_LEN_FNAM) fn

      IF ( permCheckPoint ) THEN
       WRITE(fn,'(A,I10.10)') 'pickup_cpl.',myIter
      ELSE
       WRITE(fn,'(A,A)') 'pickup_cpl.',checkPtSuff(nCheckLev)
      ENDIF
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,SSTocn2cpl, 1,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,HeatFlux  , 2,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,tauX      , 3,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,tauY      , 4,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,FWFlux    , 5,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,uVelGround, 6,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,vVelGround, 7,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qLatent   , 8,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qSensible , 9,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qLongwave ,10,myIter,myThid)
c     CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,qShortwave,11,myIter,myThid)

      RETURN
      END
