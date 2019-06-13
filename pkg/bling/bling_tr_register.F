#include "BLING_OPTIONS.h"

CBOP
C     !ROUTINE: BLING_TR_REGISTER
C     !INTERFACE:
      SUBROUTINE BLING_TR_REGISTER(
     U           tracer_num, sepFTr_num,
     I           myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE BLING_TR_REGISTER
C     | o Register BLING tracer indices
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
      INTEGER BLING_Tr_num
CEOP

#ifndef USE_BLING_V1
# ifdef USE_SIBLING
      BLING_Tr_num = 9
# else
      BLING_Tr_num = 8
# endif
#else
      BLING_Tr_num = 6
#endif
#ifdef ADVECT_PHYTO
      BLING_Tr_num = BLING_Tr_num+1
#endif

      WRITE(msgBuf,'(2A,2I5)') ' BLING_TR_REGISTER:',
     &     ' number of BLING tracers=', BLING_Tr_num
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A,2I5)') ' BLING_TR_REGISTER:',
     &     '   starting at pTrc num =', tracer_num + 1
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      tracer_num = tracer_num + BLING_Tr_num
      sepFTr_num = sepFTr_num + BLING_Tr_num

      WRITE(msgBuf,'(2A,2I5)') ' BLING_TR_REGISTER:',
     &     ' Numb. Trac & SepForc Trac:', tracer_num, sepFTr_num
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      RETURN
      END
