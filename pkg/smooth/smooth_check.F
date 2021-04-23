#include "SMOOTH_OPTIONS.h"
#include "CTRL_OPTIONS.h"

      subroutine smooth_check( myThid )

C     *==========================================================*
C     | SUBROUTINE smooth_check
C     | o Routine that checks for inconsistencies in the smooth package
C     *==========================================================*

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"

#include "SMOOTH.h"

c     == routine arguments ==

      integer myThid

c     == local variables ==
C     msgBuf      - Informational/error message buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      integer smoothOpNb

c     == end of interface ==


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
