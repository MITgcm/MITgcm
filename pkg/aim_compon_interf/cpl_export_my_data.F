C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/cpl_export_my_data.F,v 1.2 2004/05/21 20:00:48 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_EXPORT_MY_DATA( 
     I               myIter, myTime, myThid )
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
C     myIter - Current timestep number.
C     myTime - Current internal time.
C     myThid - Thread number for this instance of the routine.
      INTEGER myIter
      _RL     myTime
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE
C     == Local variables ==

C     Send data to coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous posting of data, so ordering has to be
C     consistent with coupling layer ordering.
      IF ( (myIter/cplSendFrq_iter)*cplSendFrq_iter .EQ. myIter ) THEN
       WRITE(0,*) ' Exporting atmospheric fluxes at iter. ', myIter
       CALL ATM_EXPORT_ATMSLP(     myThid )
       CALL ATM_EXPORT_HEATFLUX(   myThid )
       CALL ATM_EXPORT_QSHORTWAVE( myThid )
c      CALL ATM_EXPORT_QLATENT(    myThid )
c      CALL ATM_EXPORT_QSENSIBLE(  myThid )
c      CALL ATM_EXPORT_QLONGWAVE(  myThid )
c      CALL ATM_EXPORT_UVELGROUND( myThid )
c      CALL ATM_EXPORT_VVELGROUND( myThid )
       CALL ATM_EXPORT_TAUX(       myThid )
       CALL ATM_EXPORT_TAUY(       myThid )
       CALL ATM_EXPORT_EVMPR(      myThid )
       CALL ATM_EXPORT_RUNOFF(     myThid )
       CALL ATM_EXPORT_ROENFX(     myThid )
       CALL ATM_EXPORT_SALTFX(     myThid )
       CALL ATM_EXPORT_SEAICE(     myThid )
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
