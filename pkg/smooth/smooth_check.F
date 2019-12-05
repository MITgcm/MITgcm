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
        IF ( (smooth2DmaskName(smoothOpNb)(1:5).ne.'maskC').or.
     &       (smooth2DmaskName(smoothOpNb)(1:5).ne.'maskW').or.
     &       (smooth2DmaskName(smoothOpNb)(1:5).ne.'maskS').or.
     &       (smooth2DmaskName(smoothOpNb)(1:5).ne.'maskI') ) then
          WRITE(msgBuf,'(A,A,I2,A,A,A,A)') 'SMOOTH_CHECK: ',
     &      'Unrecognized option for smooth2DmaskName(', smoothOpNb, 
     &      '): ', smooth2DmaskName(smoothOpNb), ' - must be: ',
     &      'maskC, maskW, maskS, or maskI'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R SMOOTH_CHECK'
        ENDIF

C --- only maskC is allowed for smoothing 3D fields... for now I guess.
        IF ( (smooth3DmaskName(smoothOpNb)(1:5).ne.'maskC') ) then
          WRITE(msgBuf,'(A,A,I2,A,A,A,A)') 'SMOOTH_CHECK: ',
     &      'Only maskC implemented for 3D smoothing'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R SMOOTH_CHECK'
        ENDIF

        IF ( (smooth3DmaskName(smoothOpNb)(1:5).ne.'maskC').or.
     &       (smooth3DmaskName(smoothOpNb)(1:5).ne.'maskW').or.
     &       (smooth3DmaskName(smoothOpNb)(1:5).ne.'maskS').or.
     &       (smooth3DmaskName(smoothOpNb)(1:5).ne.'maskI') ) then
          WRITE(msgBuf,'(A,A,I2,A,A,A,A)') 'SMOOTH_CHECK: ',
     &      'Unrecognized option for smooth3DmaskName(', smoothOpNb, 
     &      '): ', smooth3DmaskName(smoothOpNb), ' - must be: ',
     &      'maskC, maskW, maskS, or maskI'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R SMOOTH_CHECK'
        ENDIF

C --- make sure proper macros are defined for maskI (iceshelf domain)
#if !(defined ALLOW_SHI2D_CONTROL) || !(defined ALLOW_SHIFWFLX_CONTROL)
        IF ( (smooth2DmaskName(smoothOpNb)(1:5).eq.'maskI') .or. 
     &       (smooth3DmaskName(smoothOpNb)(1:5).eq.'maskI') )then
          WRITE(msgBuf,'(A,A,I2,A,A,A,A)') 'SMOOTH_CHECK: ',
     &      'Must compile with option ALLOW_SHI2D_CONTROL or ',
     &      'ALLOW_SHIFWFLX_CONTROL in SHELFICE_OPTIONS.h ',
     &      '(and CTRL_OPTIONS.h) to define maskI'
          STOP 'ABNORMAL END: S/R SMOOTH_CHECK'
        ENDIF
#endif
      ENDDO

      RETURN
      END
