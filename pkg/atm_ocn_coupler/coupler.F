C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/coupler.F,v 1.3 2006/06/15 23:29:17 jmc Exp $
C $Name:  $

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
       CALL CPL_RECV_OCN_FIELDS
       CALL CPL_RECV_ATM_FIELDS

C      Send out fields
       CALL CPL_SEND_ATM_FIELDS
       CALL CPL_SEND_OCN_FIELDS

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
