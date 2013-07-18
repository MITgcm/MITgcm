C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_cpl_read_pickup.F,v 1.8 2013/07/18 20:02:08 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: OCN_CPL_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE OCN_CPL_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OCN_CPL_READ_PICKUP
C     | o Reads fields from a pickup file for a restart
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myIter :: Iteration number
C     myThid :: my Thread Id number
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE

C     !LOCAL VARIABLES:
      INTEGER prec, rec_ind
      CHARACTER*(MAX_LEN_FNAM) fn

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( pickupSuff.EQ.' ' ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_cpl.',myIter
       ELSE
         WRITE(fn,'(A,A10)') 'pickup_cpl.', pickupSuff
       ENDIF
       prec = precFloat64
       rec_ind=0

c      IF ( .NOT. cpl_earlyExpImpCall ) THEN
c        CALL READ_REC_3D_RL( fn,prec,1,atmSLPr   , 1,myIter,myThid )
c        CALL READ_REC_3D_RL( fn,prec,1,HeatFlux  , 2,myIter,myThid )
c        CALL READ_REC_3D_RL( fn,prec,1,qShortWave, 3,myIter,myThid )
c        CALL READ_REC_3D_RL( fn,prec,1,tauX      , 4,myIter,myThid )
c        CALL READ_REC_3D_RL( fn,prec,1,tauY      , 5,myIter,myThid )
c        CALL READ_REC_3D_RL( fn,prec,1,FWFlux    , 6,myIter,myThid )
c        CALL READ_REC_3D_RL( fn,prec,1,iceSaltFlx, 7,myIter,myThid )
c        CALL READ_REC_3D_RL( fn,prec,1,seaIceMass, 8,myIter,myThid )
c        rec_ind=8
c      ENDIF

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

C--   Fill in the halo region:
c      IF ( .NOT. cpl_earlyExpImpCall ) THEN
c        _EXCH_XY_RL( atmSLPr   , myThid )
c        _EXCH_XY_RL( HeatFlux  , myThid )
c        _EXCH_XY_RL( qShortWave, myThid )
c        CALL EXCH_UV_XY_RL( tauX, tauY, .TRUE., myThid )
c        _EXCH_XY_RL( FWFlux    , myThid )
c        _EXCH_XY_RL( iceSaltFlx, myThid )
c        _EXCH_XY_RL( seaIceMass, myThid )
c      ENDIF
       IF ( ocnCpl_exchange_DIC ) THEN
c        _EXCH_XY_RL( airCO2    , myThid )
c        _EXCH_XY_RL( surfWSpeed, myThid )
c        _EXCH_XY_RL( fracIce   , myThid )
         _EXCH_XY_RL( fluxCO2cpl, myThid )
       ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
