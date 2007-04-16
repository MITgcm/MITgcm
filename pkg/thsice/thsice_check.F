c $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_check.F,v 1.1 2007/04/16 22:38:24 heimbach Exp $

#include "THSICE_OPTIONS.h"

      subroutine THSICE_CHECK( mythid )

c     ==================================================================
c     SUBROUTINE THSICE_CHECK
c     ==================================================================
c
      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer mythid

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

      end
