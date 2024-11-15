C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PROFILES_OPTIONS.h"

CBOP
C     !ROUTINE: PROFILES_INI_IO
C     !INTERFACE:
      SUBROUTINE PROFILES_INI_IO( myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | S/R PROFILES_INI_IO
C     | o create directory for profiles output if needed
C     *==================================================================
C     \ev

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "PROFILES_SIZE.h"
#include "profiles.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid -  Number of this instances
      INTEGER myThid

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
#ifndef HAVE_SYSTEM
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
      CHARACTER*(MAX_LEN_FNAM) namBuf
      INTEGER iL, pIL
CEOP

      _BEGIN_MASTER( myThid )

      IF (profilesDir .NE. ' ') THEN
       iL = ILNBLNK( profilesDir )
C      append "/", if necessary
       IF ( iL.LT.MAX_LEN_FNAM .AND. profilesDir(iL:iL).NE.'/' ) THEN
        namBuf(1:iL) = profilesDir(1:iL)
        WRITE(profilesDir(1:iL+1),'(2A)') namBuf(1:iL),'/'
       ENDIF
#ifdef HAVE_SYSTEM
C      create directory
       iL = ILNBLNK( profilesDir ) -1
       WRITE(namBuf,'(2A)') ' mkdir -p ', profilesDir(1:iL)
       pIL = 1 + ILNBLNK( namBuf )
       WRITE(standardMessageUnit,'(3A)')
     & '==> SYSTEM CALL (from PROFILES_INI_IO): >',namBuf(1:pIL),'<'
       CALL SYSTEM( namBuf(1:pIL) )
#else
       WRITE(msgBuf,'(2A)') '** WARNING ** PROFILES_INI_IO: cannot ',
     &       'call mkdir -> please create profilesDir manually'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
#endif
      ENDIF

      _END_MASTER( myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
