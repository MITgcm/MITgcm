#include "OBSFIT_OPTIONS.h"

CBOP
C     !ROUTINE: OBSFIT_INI_IO

C     !INTERFACE: 
      SUBROUTINE OBSFIT_INI_IO( myThid )

C     !DESCRIPTION:
C     ==================================================================
C     | Create directory for obsfit output if needed
C     ==================================================================

C     !USES: 
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "OBSFIT_SIZE.h"
#include "OBSFIT.h"

C     !INPUT PARAMETERS: 
C     myThid    :: my Thread Id number
      INTEGER myThid

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
CEOP

#ifdef ALLOW_OBSFIT
C !LOCAL VARIABLES: 
#ifndef HAVE_SYSTEM
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
      CHARACTER*(MAX_LEN_FNAM) namBuf
      INTEGER iL, pIL

      _BEGIN_MASTER( myThid )

      IF ( obsfitDir .NE. ' ' ) THEN
       iL = ILNBLNK( obsfitDir )
C Append "/", if necessary
       IF ( iL.LT.MAX_LEN_FNAM .AND. obsfitDir(iL:iL).NE.'/' ) THEN
        namBuf(1:iL) = obsfitDir(1:iL)
        WRITE(obsfitDir(1:iL+1),'(2A)') namBuf(1:iL),'/'
       ENDIF
#ifdef HAVE_SYSTEM
C Create directory
       iL = ILNBLNK( obsfitDir ) -1
       WRITE(namBuf,'(2A)') ' mkdir -p ', obsfitDir(1:iL)
       pIL = 1 + ILNBLNK( namBuf )
       WRITE(standardMessageUnit,'(3A)')
     & '==> SYSTEM CALL (from OBSFIT_INI_IO): >',namBuf(1:pIL),'<'
       CALL SYSTEM( namBuf(1:pIL) )
#else
       WRITE(msgBuf,'(2A)') '** WARNING ** OBSFIT_INI_IO: cannot ',
     &       'call mkdir -> please create obsfitDir manually'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
#endif
      ENDIF

      _END_MASTER( myThid )

#endif /* ALLOW_OBSFIT */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|