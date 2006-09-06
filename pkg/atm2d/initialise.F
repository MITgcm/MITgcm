#include "ATM2D_OPTIONS.h"
#ifdef ATM2D_MPI_ON

CStartOfInterface
      SUBROUTINE INITIALISE
C     /==========================================================\
C     | SUBROUTINE INITIALISE                                    |
C     | o Routine to initialise coupling component.              |
C     |==========================================================|
C     | Initialisation involves starting up MPI and then joining |
C     | the coupled configuration as a component with the special|
C     | name "Coupler". With the present MIT Coupler library     |
C     | only one process can register with the name "Coupler".   |
C     | This component is the process responsible for            |
C     | coordinating the transfer of data between other          |
C     | components and for mapping data from one components      |
C     | representation to anothers representation as appropriate.|
C     \==========================================================/
      IMPLICIT NONE
C     === GLobal variables ==
#include "CPLIDS.h"

CEndOfInterface

C     === Local variables ===
C     iErr             - Error code
C     MPI_COMM_Coupler - Communicatior used by this component.
C                        Stored internally by the coupling library.
      INTEGER iErr
      INTEGER MPI_COMM_Coupler

      CALL MPI_Init(iErr)

C--   Set the running directory (= rank_0 )
      CALL SETDIR( 0 )

C     Register this process as the "coupler" participant. Currently there can
C     only be one coupler participant all others are "component" participants.
      CALL MITCOUPLER_INIT( cplCompName, MPI_COMM_Coupler )

      RETURN
      END
#endif
