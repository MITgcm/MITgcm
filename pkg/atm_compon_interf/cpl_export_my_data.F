C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/cpl_export_my_data.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_EXPORT_MY_DATA( 
     I                               myCurrentIter, 
     I                               myCurrentTime, 
     I                               myThid )
C     /==========================================================\
C     | SUBROUTINE CPL_EXPORT_MY_DATA                            |
C     | o Routine for controlling export of coupling data to     |
C     |   coupler layer.                                         |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     \==========================================================/
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "CPL_PARAMS.h"

C     == Routine arguments ==
C     myCurrentIter - Current timestep number.
C     myCurrentTime - Current internal time.
C     myThid        - Thread number for this instance 
C                     of the routine.
      INTEGER myCurrentIter
      _RL     myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==

C     Send data to coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous posting of data, so ordering has to be
C     consistent with coupling layer ordering.
      IF ( (myCurrentIter/cplSendFrq_iter)*cplSendFrq_iter 
     &      .EQ. myCurrentIter ) THEN
       WRITE(0,*) ' Exporting atmospheric fluxes at iteration ',
     &             myCurrentIter
       CALL ATM_EXPORT_TAUX(       myThid )
       CALL ATM_EXPORT_TAUY(       myThid )
       CALL ATM_EXPORT_HEATFLUX(   myThid )
       CALL ATM_EXPORT_EVMPR(      myThid )
       CALL ATM_EXPORT_RUNOFF(     myThid )
       CALL ATM_EXPORT_UVELGROUND( myThid )
       CALL ATM_EXPORT_VVELGROUND( myThid )
       CALL ATM_EXPORT_QLATENT(    myThid )
       CALL ATM_EXPORT_QSENSIBLE(  myThid )
       CALL ATM_EXPORT_QLONGWAVE(  myThid )
       CALL ATM_EXPORT_QSHORTWAVE( myThid )
      ENDIF

      RETURN
      END
