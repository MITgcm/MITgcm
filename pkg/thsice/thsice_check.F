C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_check.F,v 1.3 2007/09/17 21:48:25 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

      SUBROUTINE THSICE_CHECK( myThid )

c     ==================================================================
c     SUBROUTINE THSICE_CHECK
c     ==================================================================
c
      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"

c     == routine arguments ==

c     myThid :: thread number for this instance of the routine.

      INTEGER myThid

c     == local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf

c     == end of interface ==

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'THSICE_CHECK: #define THSICE'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )

#ifdef ALLOW_AUTODIFF_TAMC
      IF ( nitMaxTsf .GT. MaxTsf ) THEN
        WRITE(msgBuf,'(A)') 'Need to set MaxTsf >= nitMaxTsf for TAF'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R THSICE_CHECK'
      ENDIF
#endif

      _END_MASTER(myThid)

      RETURN
      END
