C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_cpl_read_pickup.F,v 1.6 2009/08/27 18:00:01 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
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
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#endif

C     == Routine arguments ==
C     myIter  :: time-step number
C     myThid  :: Thread number for this instance of the routine
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE
C     == Local variables ==
      INTEGER prec, rec_ind
      CHARACTER*(MAX_LEN_FNAM) fn

       WRITE(fn,'(A,I10.10)') 'pickup_cpl.',myIter
       prec = precFloat64
       rec_ind=0

       IF ( .NOT. cpl_earlyExpImpCall ) THEN
         CALL READ_REC_3D_RL( fn,prec,1,atmSLPr   , 1,myIter,myThid )
         CALL READ_REC_3D_RL( fn,prec,1,HeatFlux  , 2,myIter,myThid )
         CALL READ_REC_3D_RL( fn,prec,1,qShortWave, 3,myIter,myThid )
         CALL READ_REC_3D_RL( fn,prec,1,tauX      , 4,myIter,myThid )
         CALL READ_REC_3D_RL( fn,prec,1,tauY      , 5,myIter,myThid )
         CALL READ_REC_3D_RL( fn,prec,1,FWFlux    , 6,myIter,myThid )
         CALL READ_REC_3D_RL( fn,prec,1,iceSaltFlx, 7,myIter,myThid )
         CALL READ_REC_3D_RL( fn,prec,1,seaIceMass, 8,myIter,myThid )
         rec_ind=8
       ENDIF

       IF ( ocnCpl_exchange_DIC ) THEN
c        CALL READ_REC_3D_RL( fn,prec,1,airCO2    , 9,myIter,myThid )
c        CALL READ_REC_3D_RL( fn,prec,1,surfWSpeed,10,myIter,myThid )
c        CALL READ_REC_3D_RL( fn,prec,1,fracIce   ,11,myIter,myThid )
#ifdef ALLOW_PTRACERS
         IF (nIter0.GT.PTRACERS_Iter0) THEN
          rec_ind = rec_ind+1
          CALL READ_REC_3D_RL( fn, prec, 1,
     &                         fluxCO2cpl, rec_ind, myIter, myThid )
         ENDIF
#endif
       ENDIF

c      CALL READ_REC_3D_RL( fn,prec,1,ocMxlD2cpl, 9,myIter,myThid )
c      CALL READ_REC_3D_RL( fn,prec,1,SSTocn2cpl,10,myIter,myThid )
c      CALL READ_REC_3D_RL( fn,prec,1,SSSocn2cpl,11,myIter,myThid )
c      CALL READ_REC_3D_RL( fn,prec,1,vSqocn2cpl,12,myIter,myThid )

C--   Fill in the halo region:
       IF ( .NOT. cpl_earlyExpImpCall ) THEN
         _EXCH_XY_RL( atmSLPr   , myThid )
         _EXCH_XY_RL( HeatFlux  , myThid )
         _EXCH_XY_RL( qShortWave, myThid )
         CALL EXCH_UV_XY_RL( tauX, tauY, .TRUE., myThid )
         _EXCH_XY_RL( FWFlux    , myThid )
         _EXCH_XY_RL( iceSaltFlx, myThid )
         _EXCH_XY_RL( seaIceMass, myThid )
       ENDIF
       IF ( ocnCpl_exchange_DIC ) THEN
c        _EXCH_XY_RL( airCO2    , myThid )
c        _EXCH_XY_RL( surfWSpeed, myThid )
c        _EXCH_XY_RL( fracIce   , myThid )
         _EXCH_XY_RL( fluxCO2cpl, myThid )
       ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
