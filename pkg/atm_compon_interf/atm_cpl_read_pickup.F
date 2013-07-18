C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_cpl_read_pickup.F,v 1.4 2013/07/18 20:03:10 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_CPL_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE ATM_CPL_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_CPL_READ_PICKUP
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
#include "ATMIDS.h"
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myIter  :: Current time-step number
C     myThid  :: my Thread Id number
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     == Local variables ==
      INTEGER prec
      INTEGER i,j,bi,bj
      CHARACTER*(MAX_LEN_FNAM) fn

      WRITE(fn,'(A,I10.10)') 'pickup_cpl.',myIter
      prec = precFloat64

c     IF ( cpl_oldPickup ) THEN
C     old pickup from before checkpoint 52m_post
c       CALL READ_REC_3D_RL(fn,prec,1,SSTocn    , 1,myIter,myThid)
c       CALL READ_REC_3D_RL(fn,prec,1,HeatFlux  , 2,myIter,myThid)
c       CALL READ_REC_3D_RL(fn,prec,1,tauX      , 3,myIter,myThid)
c       CALL READ_REC_3D_RL(fn,prec,1,tauY      , 4,myIter,myThid)
c       CALL READ_REC_3D_RL(fn,prec,1,EvMPrFlux , 5,myIter,myThid)
c       CALL READ_REC_3D_RL(fn,prec,1,RunOffFlux, 6,myIter,myThid)
c       RETURN
c     ENDIF
        CALL READ_REC_3D_RL( fn,prec,1, ocMxlD    ,  1, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, SSTocn    ,  2, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, SSSocn    ,  3, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, vSqocn    ,  4, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, atmSLPr   ,  5, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, HeatFlux  ,  6, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, qShortWave,  7, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, tauX      ,  8, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, tauY      ,  9, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, EvMPrFlux , 10, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, RunOffFlux, 11, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, RunOffEnFx, 12, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, iceSaltFlx, 13, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, seaIceMass, 14, myIter,myThid )
      IF ( atmCpl_exchange_DIC ) THEN
        CALL READ_REC_3D_RL( fn,prec,1, flxCO2ocn , 15, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, airCO2    , 16, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, sWSpeed   , 17, myIter,myThid )
        CALL READ_REC_3D_RL( fn,prec,1, fracIce   , 18, myIter,myThid )
      ENDIF

      IF ( cpl_oldPickup ) THEN
        _BARRIER
C-    EmP & RunOff were (before checkpoint59h) in m/s , but are now in kg/m2/s:
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            EvMPrFlux (i,j,bi,bj) = EvMPrFlux (i,j,bi,bj)*rhoConstFresh
            RunOffFlux(i,j,bi,bj) = RunOffFlux(i,j,bi,bj)*rhoConstFresh
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
