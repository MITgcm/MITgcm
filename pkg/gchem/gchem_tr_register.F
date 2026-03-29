#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_TR_REGISTER

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_TR_REGISTER( myThid )

C !DESCRIPTION:
C     Register GCHEM tracer indices

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GCHEM.h"

C !INPUT PARAMETERS: ===================================================
C  myThid         :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_GCHEM

C !LOCAL VARIABLES: ====================================================
C  iUnit          :: unit number for I/O
C  msgBuf         :: message buffer
c     INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(2A,2I5)') 'GCHEM_TR_REGISTER: ',
     &     ' Start registering GCHEM tracers'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Initialise
      gchem_Tracer_num = 0
      gchem_sepFTr_num = 0

C--   Register GCHEM tracers: start with those using Separate Forcing
C                             and finish with those using gchem-tendency

#ifdef ALLOW_DIC
      IF ( useDIC ) THEN
        CALL DIC_TR_REGISTER(
     U              gchem_Tracer_num, gchem_sepFTr_num,
     I              myThid )
      ENDIF
#endif
#ifdef ALLOW_BLING
      IF ( useBLING ) THEN
        CALL BLING_TR_REGISTER(
     U                gchem_Tracer_num, gchem_sepFTr_num,
     I                myThid )
      ENDIF
#endif
#ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
        CALL DARWIN_TR_REGISTER(
     U                 gchem_Tracer_num, gchem_sepFTr_num,
     I                 myThid )
      ENDIF
#endif

#ifdef ALLOW_CFC
      IF ( useCFC ) THEN
        CALL CFC_TR_REGISTER(
     U              gchem_Tracer_num, gchem_sepFTr_num,
     I              myThid )
      ENDIF
#endif
#ifdef ALLOW_SPOIL
      IF ( useSPOIL ) THEN
        CALL SPOIL_TR_REGISTER(
     U                gchem_Tracer_num, gchem_sepFTr_num,
     I                myThid )
      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      WRITE(msgBuf,'(2A,2I5)') 'GCHEM_TR_REGISTER: ',
     &     ' Numb. Trac & SepForc Trac:',
     &     gchem_Tracer_num, gchem_sepFTr_num
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)
      _BARRIER

#endif /* ALLOW_GCHEM */

      RETURN
      END
