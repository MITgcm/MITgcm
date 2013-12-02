C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/initialise.F,v 1.4 2013/12/02 21:55:06 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: INITIALISE

C !INTERFACE:
      SUBROUTINE INITIALISE(
     O                       msgUnit )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE INITIALISE
C     | o Routine to initialise coupling component.
C     *==========================================================*
C     | Initialisation involves starting up MPI and then joining
C     | the coupled configuration as a component with the special
C     | name "Coupler". With the present MIT Coupler library
C     | only one process can register with the name "Coupler".
C     | This component is the process responsible for
C     | coordinating the transfer of data between other
C     | components and for mapping data from one components
C     | representation to anothers representation as appropriate.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     === GLobal variables ==
#include "CPLIDS.h"

C !INPUT/OUTPUT PARAMETERS:
C     msgUnit          :: log-file I/O unit
      INTEGER msgUnit

C !LOCAL VARIABLES:
C     iErr             :: Error code
C     MPI_COMM_Coupler :: Communicatior used by this component.
C                         Stored internally by the coupling library.
      INTEGER iErr
      INTEGER MPI_COMM_Coupler
CEOP

      CALL MPI_Init(iErr)

C--   Set the running directory (= rank_0 )
      CALL SETDIR( 0 )

C     Register this process as the "coupler" participant. Currently there can
C     only be one coupler participant all others are "component" participants.
      CALL MITCOUPLER_INIT(
     I                      cplCompName,
     O                      MPI_COMM_Coupler, msgUnit )

      CALL CPL_READ_PARAMS( msgUnit )

      CALL SET_RUNOFFMAP( msgUnit )

      RETURN
      END
