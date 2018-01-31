#include "DIC_OPTIONS.h"

CBOP
C     !ROUTINE: DIC_TR_REGISTER
C     !INTERFACE:
      SUBROUTINE DIC_TR_REGISTER(
     U                            tracer_num, sepFTr_num,
     I                            myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE DIC_TR_REGISTER
C     | o Register DIC tracer indices
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
c#include "DIC_VARS.h"

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
      INTEGER DIC_Tr_num
CEOP

#ifdef ALLOW_FE
      DIC_Tr_num = 6
#else /* ALLOW_FE */
# ifdef ALLOW_O2
      DIC_Tr_num = 5
# else /* ALLOW_O2 */
      DIC_Tr_num = 4
# endif /* ALLOW_O2 */
#endif /* ALLOW_FE */

      WRITE(msgBuf,'(2A,2I5)') '  DIC_TR_REGISTER: ',
     &     ' number of DIC tracers=', DIC_Tr_num
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A,2I5)') '  DIC_TR_REGISTER: ',
     &     '  starting at pTrc num=', tracer_num + 1
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      tracer_num = tracer_num + DIC_Tr_num
      sepFTr_num = sepFTr_num + DIC_Tr_num

      WRITE(msgBuf,'(2A,2I5)') '  DIC_TR_REGISTER: ',
     &     ' Numb. Trac & SepForc Trac:', tracer_num, sepFTr_num
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      RETURN
      END
