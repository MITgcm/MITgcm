#include "BATS_OPTIONS.h"

CBOP
C     !ROUTINE: BATS_TR_REGISTER
C     !INTERFACE:
      SUBROUTINE BATS_TR_REGISTER(
     U           tracer_num, sepFTr_num,
     I           myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE BATS_TR_REGISTER
C     | o Register BATS tracer indices
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     tracer_num :: current number of registered tracers
C     sepFTr_num :: number of registered tracers that use Separate Forcing
C     myThid     :: my Thread Id number
      INTEGER tracer_num
      INTEGER sepFTr_num
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf     :: message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER BATS_Tr_num
CEOP

      BATS_Tr_num = 36

      WRITE(msgBuf,'(2A,2I5)') ' BATS_TR_REGISTER:',
     &     ' number of BATS tracers=', BATS_Tr_num
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A,2I5)') ' BATS_TR_REGISTER:',
     &     '   starting at pTrc num =', tracer_num + 1
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      tracer_num = tracer_num + BATS_Tr_num
      sepFTr_num = sepFTr_num + BATS_Tr_num

      WRITE(msgBuf,'(2A,2I5)') ' BATS_TR_REGISTER:',
     &     ' Numb. Trac & SepForc Trac:', tracer_num, sepFTr_num
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      RETURN
      END
