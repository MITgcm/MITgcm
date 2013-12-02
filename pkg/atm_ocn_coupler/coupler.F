C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/coupler.F,v 1.4 2013/12/02 21:55:06 jmc Exp $
C $Name:  $

CBOP 0
C !ROUTINE: COUPLER

C !INTERFACE:
      PROGRAM COUPLER

C !DESCRIPTION:
C     *==========================================================*
C     | PROGRAM COUPLER
C     | o Main routine for 'Coupler' component. 'Coupler'
C     |   component coordiantes the exchange of data between
C     |   component models in a coupled model experiment.
C     *==========================================================*
C     | This version uses the MIT Coupler "checkpoint1" library
C     | calls.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "mpif.h"
#include "CPL_PARAMS.h"

C !LOCAL VARIABLES:
C     I       :: Loop counter
C     rc      :: MPI return code
C     msgUnit :: log-file I/O unit
      INTEGER I
      INTEGER rc
      INTEGER msgUnit
CEOP

C     Initialise the coupler component
      CALL INITIALISE(
     O                 msgUnit )

C     Perform registration with other components
      CALL ACCEPT_COMPONENT_REGISTRATIONS

C     Coordinate the transfer configuration information
C     between components
      CALL EXCH_COMPONENT_CONFIGS(
     I                             msgUnit )

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
