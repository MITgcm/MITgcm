C $ Header: $
C $ Name:   $

      PROGRAM COUPLER
C     /==========================================================\
C     | PROGRAM COUPLER                                          |
C     | o Main routine for 'Coupler' component. 'Coupler'        |
C     |   component coordiantes the exchange of data between     |
C     |   component models in a coupled model experiment.        |
C     |==========================================================|
C     | This version uses the MIT Coupler "checkpoint1" library  |
C     | calls.                                                   |
C     \==========================================================/
      IMPLICIT NONE
#include "mpif.h"
#include "CPL_PARAMS.h"

C     == Local variables ==
C     I                - Loop counter
C     MPI_COMM_Coupler
C     rc               - MPI return code
      INTEGER I
      INTEGER rc

C     Initialise the coupler component
      CALL INITIALISE

C     Perform registration with other components
      CALL ACCEPT_COMPONENT_REGISTRATIONS

C     Coordinate the transfer configuration information 
C     between components
      CALL RECEIVE_COMPONENT_CONFIGS
      CALL SENDOUT_COMPONENT_CONFIGS

      DO I=1,nCouplingSteps

C      Receive updated state
       CALL CPL_RECV_OCN_SST
       CALL CPL_RECV_ATM_TAUX
       CALL CPL_RECV_ATM_TAUY
       CALL CPL_RECV_ATM_HEATFLUX
       CALL CPL_RECV_ATM_EVMPR
       CALL CPL_RECV_ATM_RUNOFF
       CALL CPL_RECV_ATM_UVELGROUND
       CALL CPL_RECV_ATM_VVELGROUND
       CALL CPL_RECV_ATM_QLATENT
       CALL CPL_RECV_ATM_QSENSIBLE
       CALL CPL_RECV_ATM_QLONGWAVE
       CALL CPL_RECV_ATM_QSHORTWAVE

C      Send out fields
       CALL CPL_SEND_ATM_SST
       CALL CPL_SEND_OCN_TAUX
       CALL CPL_SEND_OCN_TAUY
       CALL CPL_SEND_OCN_HEATFLUX
       CALL CPL_SEND_OCN_FWFLUX
       CALL CPL_SEND_OCN_UVELGROUND
       CALL CPL_SEND_OCN_VVELGROUND
       CALL CPL_SEND_OCN_QLATENT
       CALL CPL_SEND_OCN_QSENSIBLE
       CALL CPL_SEND_OCN_QLONGWAVE
       CALL CPL_SEND_OCN_QSHORTWAVE

      ENDDO

C     o Finalize MPI
C     First wait for everybody to finish. Nobody should call
C     MPI_Finalize before all the component modules are
C     ready to finish. On some systems once one participant
C     gets to MPI_Finalize then its unclear what will
C     happen after that. If everybody does on MPI_Barrier
C     on COMM_WORLD then we will be OK.
      CALL MPI_BARRIER( MPI_COMM_WORLD, rc )
      CALL MPI_FINALIZE(rc)

      STOP
      END
