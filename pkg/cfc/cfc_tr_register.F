#include "GCHEM_OPTIONS.h"

CBOP
C     !ROUTINE: CFC_TR_REGISTER
C     !INTERFACE:
      SUBROUTINE CFC_TR_REGISTER(
     U                            tracer_num, sepFTr_num,
     I                            myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CFC_TR_REGISTER
C     | o Register CFC tracer indices
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
#include "CFC_SIZE.h"

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
CEOP

      CFC_pTr_i1 = tracer_num + 1
      tracer_num = tracer_num + CFC_Tr_num

      WRITE(msgBuf,'(2A,2I5)') '  CFC_TR_REGISTER: ',
     &     ' number of CFC tracers=', CFC_Tr_num
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A,2I5)') '  CFC_TR_REGISTER: ',
     &     '  starting at pTrc num=', CFC_pTr_i1
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      WRITE(msgBuf,'(2A,2I5)') '  CFC_TR_REGISTER: ',
     &     ' Numb. Trac & SepForc Trac:', tracer_num, sepFTr_num
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      RETURN
      END
