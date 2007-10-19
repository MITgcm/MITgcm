C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/cpl_write_pickup.F,v 1.4 2007/10/19 03:21:39 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE CPL_WRITE_PICKUP(
     I                             suff, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_WRITE_PICKUP
C     | o Store coupling state for restart.
C     | - Atmospheric version -
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ATMIDS.h"
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id number
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     == Local variables ==
C     fn          - Workspace for building file name
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec

      prec = precFloat64
      WRITE(fn,'(A,A)') 'pickup_cpl.',suff

c     CALL WRITE_REC_3D_RL(fn,prec,1,SSTocn    , 1,myIter,myThid)
c     CALL WRITE_REC_3D_RL(fn,prec,1,HeatFlux  , 2,myIter,myThid)
c     CALL WRITE_REC_3D_RL(fn,prec,1,tauX      , 3,myIter,myThid)
c     CALL WRITE_REC_3D_RL(fn,prec,1,tauY      , 4,myIter,myThid)
c     CALL WRITE_REC_3D_RL(fn,prec,1,EvMPrFlux , 5,myIter,myThid)
c     CALL WRITE_REC_3D_RL(fn,prec,1,RunOffFlux, 6,myIter,myThid)
c     CALL WRITE_REC_3D_RL(fn,prec,1,uVelGround, 7,myIter,myThid)
c     CALL WRITE_REC_3D_RL(fn,prec,1,vVelGround, 8,myIter,myThid)
c     CALL WRITE_REC_3D_RL(fn,prec,1,qLatent   , 9,myIter,myThid)
c     CALL WRITE_REC_3D_RL(fn,prec,1,qSensible ,10,myIter,myThid)
c     CALL WRITE_REC_3D_RL(fn,prec,1,qLongwave ,11,myIter,myThid)
c     CALL WRITE_REC_3D_RL(fn,prec,1,qShortwave,12,myIter,myThid)

      CALL WRITE_REC_3D_RL( fn,prec,1, ocMxlD    ,  1, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, SSTocn    ,  2, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, SSSocn    ,  3, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, vSqocn    ,  4, myIter,myThid )

      CALL WRITE_REC_3D_RL( fn,prec,1, atmSLPr   ,  5, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, HeatFlux  ,  6, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, qShortWave,  7, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, tauX      ,  8, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, tauY      ,  9, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, EvMPrFlux , 10, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, RunOffFlux, 11, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, RunOffEnFx, 12, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, iceSaltFlx, 13, myIter,myThid )
      CALL WRITE_REC_3D_RL( fn,prec,1, seaIceMass, 14, myIter,myThid )

      IF ( atmCpl_exchange_DIC ) THEN
       CALL WRITE_REC_3D_RL(fn,prec,1, flxCO2ocn , 15, myIter,myThid )
       CALL WRITE_REC_3D_RL(fn,prec,1, airCO2    , 16, myIter,myThid )
       CALL WRITE_REC_3D_RL(fn,prec,1, sWSpeed   , 17, myIter,myThid )
       CALL WRITE_REC_3D_RL(fn,prec,1, fracIce   , 18, myIter,myThid )
      ENDIF
#endif /* COMPONENT_MODULE */

      RETURN
      END
