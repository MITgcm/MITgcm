#include "PROFILES_OPTIONS.h"
#include "AD_CONFIG.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_NF_ERROR

C !INTERFACE:
      SUBROUTINE PROFILES_NF_ERROR( message, STATUS, bi, bj, myThid )

C     !DESCRIPTION:
C     Print NetCDF error message

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#ifdef ALLOW_PROFILES
# include "SIZE.h"
# include "EEPARAMS.h"
# include "PARAMS.h"
# include "netcdf.inc"
#endif

C     !INPUT PARAMETERS:
C     message   :: optional message
C     STATUS    :: NetCDF error status
C     bi,bj     :: Tile indices
C     myThid: my thread ID number
      CHARACTER*(*) message
      INTEGER STATUS, bi, bj, myThid

C     !OUTPUT PARAMETERS:
CEOP

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
#ifdef ALLOW_PROFILES
      INTEGER IL
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF (debugLevel .GE. debLevA .AND. STATUS .NE. NF_NOERR) THEN
        IL = ILNBLNK(message)
        IF ( IL .GT. 0 ) THEN
          WRITE(msgBuf,'(A,A,2I3,1X,A)')
     &     'NF_MESSAGE: PROFILES_', message(1:IL),
     &     bi, bj, NF_STRERROR(STATUS)
        ELSE
          WRITE(msgBuf,'(A,2I3,1X,A)') 'NF_MESSAGE: PROFILES_',
     &     bi, bj, NF_STRERROR(STATUS)
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT,myThid )
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
#endif /* ALLOW_PROFILES */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_NC_CLOSE

C !INTERFACE:
      SUBROUTINE PROFILES_NC_CLOSE( myThid )

C     !DESCRIPTION:
C     Close NetCDF files

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#ifdef ALLOW_PROFILES
# include "SIZE.h"
# include "EEPARAMS.h"
# include "PROFILES_SIZE.h"
# include "profiles.h"
# include "netcdf.inc"
#endif

C     !INPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid

C     !OUTPUT PARAMETERS:
CEOP

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
#ifdef ALLOW_PROFILES
C     bi,bj :: Tile indices
      INTEGER bi, bj
      INTEGER IL, numFile
      INTEGER STATUS
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF (profilesDoNcOutput) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO numFile = 1, NFILESPROFMAX
              IL = ILNBLNK( profilesfiles(numFile) )
              IF (IL.NE.0) THEN
                WRITE(msgBuf,'(A,A,5(1X,I8))') 
     &           'S/R PROFILES_NC_CLOSE:',
     &           ' Closing '//profilesfiles(numFile)(1:IL), numFile,
     &           fiddata(numFile,bi,bj),fidforward(numFile,bi,bj),
     &           fidadjoint(numFile,bi,bj),fidtangent(numFile,bi,bj)
                CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &               SQUEEZE_RIGHT,myThid )
C Data file
                STATUS = NF_CLOSE( fiddata(numFile,bi,bj) )
                WRITE(msgBuf,'(A,I3,I8)')
     &           'NC_CLOSE: NF_CLOSE data unit',
     &           numFile, fiddata(numFile,bi,bj)
                CALL PROFILES_NF_ERROR( msgBuf, STATUS, bi, bj, myThid )

C Forward equivalent (.equi) file
                STATUS = NF_CLOSE( fidforward(numFile,bi,bj) )
                WRITE(msgBuf,'(A,I3,I8)')
     &           'NC_CLOSE: NF_CLOSE fwd unit',
     &           numFile, fidforward(numFile,bi,bj)
                CALL PROFILES_NF_ERROR( msgBuf, STATUS, bi, bj, myThid )
#ifdef ALLOW_ADJOINT_RUN
C Adjoint equivalent (.equi) file
                STATUS = NF_CLOSE( fidadjoint(numFile,bi,bj) )
                WRITE(msgBuf,'(A,I3,I8)') 
     &           'NC_CLOSE: NF_CLOSE adj unit',
     &           numFile, fidadjoint(numFile,bi,bj)
                CALL PROFILES_NF_ERROR( msgBuf, STATUS, bi, bj, myThid )
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
C Tangent linear equivalent (.equi) file
                STATUS = NF_CLOSE( fidtangent(numFile,bi,bj) )
                WRITE(msgBuf,'(A,I3,I8)') 
     &           'NC_CLOSE: NF_CLOSE tlm unit',
     &           numFile, fidtangent(numFile,bi,bj)
                CALL PROFILES_NF_ERROR( msgBuf, STATUS, bi, bj, myThid )
#endif
              ENDIF

            ENDDO
          ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_PROFILES */

      RETURN
      END
