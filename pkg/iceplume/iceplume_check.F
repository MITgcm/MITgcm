
#include "ICEPLUME_OPTIONS.h"

      SUBROUTINE ICEPLUME_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE ICEPLUME_CHECK                                |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ICEPLUME.h"
#include "GRID.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEPLUME_CHECK
      INTEGER myThid
      integer i,j,bi,bj,ccount

#ifdef ALLOW_ICEPLUME

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      ccount=0
      DO bi = 1,nSx
       DO bj = 1,nSy
        DO I = 1-Olx,sNx+Olx
         DO J = 1-Oly,sNy+Oly

          IF ( plumeMask(I,J,bi,bj) .GT. 0 ) THEN

            IF ( plumeLength(i,j,bi,bj) .GT. dyG(i,j,bi,bj) ) THEN
             plumeLength(i,j,bi,bj) =  dyG(i,j,bi,bj)
             ccount=ccount+1
            ENDIF

          ELSEIF( plumeMask(i,j,bi,bj) .LT. 0) THEN

            IF ( plumeLength(i,j,bi,bj) .GT. dxG(i,j,bi,bj) ) THEN
             plumeLength(i,j,bi,bj) =  dxG(i,j,bi,bj)
             ccount=ccount+1
            ENDIF

          ENDIF

          ENDDO
         ENDDO
        ENDDO
       ENDDO
       
      _BEGIN_MASTER(myThid)

           IF(ccount .GT. 0) THEN
           WRITE(msgBuf,'(3A)') '** WARNING ** ICEPLUME_CHECK:',
     &      'length of plume  exceeds cell width, ',
     &      'and has been set to cell width'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
           ENDIF

      WRITE(msgBuf,'(A)') 'ICEPLUME_CHECK: #define ALLOW_ICEPLUME'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

C    INSERT OTHER CHECKS HERE AS NECESSARY

       IF ( selectAddFluid .NE. 1 ) THEN
       WRITE(msgBuf,'(2A)')
     &  'Cannot use ICEPLUME if selectAddFluid not equal to 1'
                CALL PRINT_ERROR( msgBuf, 1 )
                STOP 'ABNORMAL END: S/R ICEPLUME_CHECK'
      ENDIF

      _END_MASTER(myThid)

#endif /* ALLOW_ICEPLUME */

      RETURN
      END
