C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_cpl_read_pickup.F,v 1.2 2007/05/10 21:15:52 jscott Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE OCN_CPL_READ_PICKUP( myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE OCN_CPL_READ_PICKUP
C     | o Reads fields from a pickup file for a restart
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "OCNIDS.h"
#include "OCNCPL.h"

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

       CALL MDSREADFIELD(fn,prec,'RL',1,atmSLPr   , 1,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,HeatFlux  , 2,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,qShortWave, 3,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,tauX      , 4,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,tauY      , 5,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,FWFlux    , 6,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,iceSaltFlx, 7,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,seaIceMass, 8,myThid)

       IF ( ocnCpl_exchange_DIC ) THEN
         CALL MDSREADFIELD(fn,prec,'RL',1,airCO2    ,  9,myThid)
         CALL MDSREADFIELD(fn,prec,'RL',1,surfWSpeed, 10,myThid)
         CALL MDSREADFIELD(fn,prec,'RL',1,fracIce   , 11,myThid)
         CALL MDSREADFIELD(fn,prec,'RL',1,fluxCO2cpl, 12,myThid)
       ENDIF

c      CALL MDSREADFIELD(fn,prec,'RL',1,ocMxlD2cpl, 9,myThid)
c      CALL MDSREADFIELD(fn,prec,'RL',1,SSTocn2cpl,10,myThid)
c      CALL MDSREADFIELD(fn,prec,'RL',1,SSSocn2cpl,11,myThid)
c      CALL MDSREADFIELD(fn,prec,'RL',1,vSqocn2cpl,12,myThid)

C--   Fill in the halo region:
       _EXCH_XY_R8( atmSLPr   , myThid )
       _EXCH_XY_R8( HeatFlux  , myThid )
       _EXCH_XY_R8( qShortWave, myThid )
       CALL EXCH_UV_XY_RL(tauX, tauY, .TRUE., myThid ) 
       _EXCH_XY_R8( FWFlux    , myThid )
       _EXCH_XY_R8( iceSaltFlx, myThid )
       _EXCH_XY_R8( seaIceMass, myThid )
       IF ( ocnCpl_exchange_DIC ) THEN
         _EXCH_XY_R8( airCO2    , myThid )
         _EXCH_XY_R8( surfWSpeed , myThid )
         _EXCH_XY_R8( fracIce   , myThid )
       ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
