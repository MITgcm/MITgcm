#include "OBSFIT_OPTIONS.h"
#include "AD_CONFIG.h"

C--  File obsfit_nc_utils.F:
C--   Contents
C--   o OBSFIT_NF_ERROR
C--   o OBSFIT_NC_CLOSE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: OBSFIT_NF_ERROR

C !INTERFACE:
      SUBROUTINE OBSFIT_NF_ERROR( message, STATUS, bi, bj, myThid )

C     !DESCRIPTION:
C     Print NetCDF error message

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#ifdef ALLOW_OBSFIT
# include "SIZE.h"
# include "EEPARAMS.h"
# include "PARAMS.h"
# include "netcdf.inc"
#endif

C     !INPUT PARAMETERS:
C     message   :: optional message
C     STATUS    :: NetCDF error status
C     bi,bj     :: Tile indices
C     myThid    :: my thread ID number
      CHARACTER*(*) message
      INTEGER STATUS, bi, bj, myThid

C     !OUTPUT PARAMETERS:
CEOP

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
#ifdef ALLOW_OBSFIT
      INTEGER IL
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF ( debugLevel .GE. debLevA .AND. STATUS .NE. NF_NOERR ) THEN
        IL = ILNBLNK(message)
        IF ( IL .GT. 0 ) THEN
          IF ( (bi.GT.0).AND.(bj.GT.0) ) THEN
            WRITE( msgBuf,'(A,A,2I3,1X,A)' )
     &       'NF_MESSAGE: OBSFIT_', message(1:IL),
     &       bi, bj, NF_STRERROR(STATUS)
          ELSE
             WRITE( msgBuf,'(A,A,1X,A)' )
     &       'NF_MESSAGE: OBSFIT_', message(1:IL),
     &       NF_STRERROR(STATUS)
          ENDIF
        ELSE
          IF ( (bi.GT.0).AND.(bj.GT.0) ) THEN
            WRITE( msgBuf,'(A,2I3,1X,A)' ) 'NF_MESSAGE: OBSFIT ',
     &       bi, bj, NF_STRERROR(STATUS)
          ELSE
            WRITE( msgBuf,'(A,1X,A)' ) 'NF_MESSAGE: OBSFIT ',
     &       NF_STRERROR(STATUS)
          ENDIF
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT,myThid )
      ENDIF
#endif /* ALLOW_OBSFIT */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: OBSFIT_NC_CLOSE

C !INTERFACE:
      SUBROUTINE OBSFIT_NC_CLOSE( myThid )

C     !DESCRIPTION:
C     Close NetCDF files

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#ifdef ALLOW_OBSFIT
# include "SIZE.h"
# include "EEPARAMS.h"
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
# include "netcdf.inc"
#endif

C     !INPUT PARAMETERS:
C     myThid :: my thread ID number
      INTEGER myThid

C     !OUTPUT PARAMETERS:
CEOP

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
#ifdef ALLOW_OBSFIT
C     bi,bj :: tile indices
      INTEGER bi, bj
      INTEGER IL, num_file
      INTEGER STATUS
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER( myThid )

      DO num_file = 1, NFILESMAX_OBS
        IL = ILNBLNK( obsfitFiles(num_file) )

        IF ( IL.NE.0 ) THEN
          WRITE( msgBuf,'(A,A,5(1X,I8))' )
     &         'S/R OBSFIT_NC_CLOSE:',
     &         ' Closing '//obsfitFiles(num_file)(1:IL), num_file,
     &         fiddata_obs(num_file)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT,myThid )
C Data file
          IF ( fiddata_obs(num_file) .GT. 0 ) THEN
            STATUS = NF_CLOSE( fiddata_obs(num_file) )
            WRITE( msgBuf,'(A,I3,I8)' )
     &           'NC_CLOSE: NF_CLOSE data unit',
     &           num_file, fiddata_obs(num_file)
            CALL OBSFIT_NF_ERROR( msgBuf, STATUS,
     &           0, 0, myThid )
          ENDIF

C Global equi file
          IF ( fidglobal(num_file) .GT. 0 ) THEN
            STATUS = NF_CLOSE( fidglobal(num_file) )
            WRITE( msgBuf,'(A,I3,I8)' )
     &           'NC_CLOSE: NF_CLOSE global equi unit',
     &           num_file, fidglobal(num_file)
            CALL OBSFIT_NF_ERROR( msgBuf, STATUS,
     &           0, 0, myThid )
          ENDIF

          IF (obsfitDoNcOutput) THEN
c            DO bj = myByLo(myThid), myByHi(myThid)
c               DO bi = myBxLo(myThid), myBxHi(myThid)
C Since this is only done by the master thread, we loop over the
C entire range of tiles
            DO bj = 1, nSy
              DO bi = 1, nSx
                WRITE( msgBuf,'(A,A,5(1X,I8))' )
     &           'S/R OBSFIT_NC_CLOSE:',
     &           ' Closing '//obsfitFiles(num_file)(1:IL), num_file,
     &           fidfwd_obs(num_file,bi,bj),
     &           fidadj_obs(num_file,bi,bj),
     &           fidtan_obs(num_file,bi,bj)
                CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &               SQUEEZE_RIGHT,myThid )

C Forward equivalent (.equi) file
                IF ( fidfwd_obs(num_file,bi,bj) .GT. 0 ) THEN
                 STATUS = NF_CLOSE( fidfwd_obs(num_file,bi,bj) )
                 WRITE( msgBuf,'(A,I3,I8)' )
     &                'NC_CLOSE: NF_CLOSE fwd unit',
     &                num_file, fidfwd_obs(num_file,bi,bj)
                 CALL OBSFIT_NF_ERROR( msgBuf, STATUS,
     &                bi, bj, myThid )
                ENDIF
#ifdef ALLOW_ADJOINT_RUN
C Adjoint equivalent (.equi) file
                IF ( fidadj_obs(num_file,bi,bj) .GT. 0 ) THEN
                 STATUS = NF_CLOSE( fidadj_obs(num_file,bi,bj) )
                 WRITE( msgBuf,'(A,I3,I8)' )
     &                'NC_CLOSE: NF_CLOSE adj unit',
     &                num_file, fidadj_obs(num_file,bi,bj)
                 CALL OBSFIT_NF_ERROR( msgBuf, STATUS,
     &                bi, bj, myThid )
                ENDIF
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
C Tangent linear equivalent (.equi) file
                IF ( fidtan_obs(num_file,bi,bj) .GT. 0 ) THEN
                 STATUS = NF_CLOSE( fidtan_obs(num_file,bi,bj) )
                 WRITE( msgBuf,'(A,I3,I8)' )
     &                'NC_CLOSE: NF_CLOSE tlm unit',
     &                num_file, fidtan_obs(num_file,bi,bj)
                 CALL OBSFIT_NF_ERROR( msgBuf, STATUS,
     &                bi, bj, myThid )
                ENDIF
#endif

              ENDDO
            ENDDO
          ENDIF
        ENDIF
      ENDDO

      _END_MASTER( myThid )
#endif /* ALLOW_OBSFIT */

      RETURN
      END

