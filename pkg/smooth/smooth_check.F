#include "SMOOTH_OPTIONS.h"

CBOP
C     !ROUTINE: SMOOTH_CHECK
C     !INTERFACE:
      SUBROUTINE SMOOTH_CHECK( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SMOOTH_CHECK
C     | o Checks for inconsistencies in the smooth package
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "SMOOTH.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf      :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER smoothOpNb
CEOP

C --- Check for legal mask names
      DO smoothOpNb=1,smoothOpNbMax
        IF ( .NOT.(
     &       (smooth2DmaskName(smoothOpNb)(1:5).EQ.'maskC').OR.
     &       (smooth2DmaskName(smoothOpNb)(1:5).EQ.'maskW').OR.
     &       (smooth2DmaskName(smoothOpNb)(1:5).EQ.'maskS').OR.
     &       (smooth2DmaskName(smoothOpNb)(1:5).EQ.'maskI'))) THEN
          WRITE(msgBuf,'(A,A,I2,A,A,A,A)') 'SMOOTH_CHECK: ',
     &      'Unrecognized option for smooth2DmaskName(', smoothOpNb,
     &      '): ', smooth2DmaskName(smoothOpNb), ' - must be: ',
     &      'maskC, maskW, maskS, or maskI'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R SMOOTH_CHECK'
        ENDIF

        IF ( .NOT.(
     &       (smooth3DmaskName(smoothOpNb)(1:5).EQ.'maskC').OR.
     &       (smooth3DmaskName(smoothOpNb)(1:5).EQ.'maskW').OR.
     &       (smooth3DmaskName(smoothOpNb)(1:5).EQ.'maskS'))) THEN
          WRITE(msgBuf,'(A,A,I2,A,A,A,A)') 'SMOOTH_CHECK: ',
     &      'Unrecognized option for smooth3DmaskName(', smoothOpNb,
     &      '): ', smooth3DmaskName(smoothOpNb), ' - must be: ',
     &      'maskC, maskW, or maskS'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R SMOOTH_CHECK'
        ENDIF

#ifndef ALLOW_SHELFICE
C --- make sure proper macros are defined for maskI (iceshelf domain)
        IF (smooth2DmaskName(smoothOpNb)(1:5).EQ.'maskI') THEN
          WRITE(msgBuf,'(A,A,I2,A,A,A,A)') 'SMOOTH_CHECK: ',
     &      'Must compile with shelfice package, ALLOW_SHELFICE'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R SMOOTH_CHECK'
        ENDIF
#endif
      ENDDO

      RETURN
      END
