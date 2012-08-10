C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_check.F,v 1.8 2012/08/10 19:36:02 jmc Exp $
C $Name:  $

#include "COST_OPTIONS.h"

      SUBROUTINE COST_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE CTRK_CHECK                                    |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'COST_CHECK: cost package'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
#ifdef ALLOW_ECCO

#if ((defined (ALLOW_HFLUX_CONTROL) && \
      !defined (ALLOW_HFLUX_COST_CONTRIBUTION)) || \
     (!defined (ALLOW_HFLUX_CONTROL) && \
      defined (ALLOW_HFLUX_COST_CONTRIBUTION)))
c      WRITE(msgBuf,'(A,A)')
c     &     'Must have both ',
c     &     'ALLOW_HFLUX_CONTROL & ALLOW_HFLUX_COST_CONTRIBUTION'
c      CALL PRINT_ERROR( msgBuf , 1)
cph      STOP 'ABNORMAL END: S/R COST_CHECK'
#elif ((defined (ALLOW_ATEMP_CONTROL) && \
      !defined (ALLOW_ATEMP_COST_CONTRIBUTION)) || \
     (!defined (ALLOW_ATEMP_CONTROL) && \
      defined (ALLOW_ATEMP_COST_CONTRIBUTION)))
c      WRITE(msgBuf,'(A,A)')
c     &     'Must have both ',
c     &     'ALLOW_ATEMP_CONTROL & ALLOW_ATEMP_COST_CONTRIBUTION'
c      CALL PRINT_ERROR( msgBuf , 1)
cph      STOP 'ABNORMAL END: S/R COST_CHECK'
#endif

#if ((defined (ALLOW_SFLUX_CONTROL) && \
      !defined (ALLOW_SFLUX_COST_CONTRIBUTION)) || \
     (!defined (ALLOW_SFLUX_CONTROL) && \
      defined (ALLOW_SFLUX_COST_CONTRIBUTION)))
c      WRITE(msgBuf,'(A,A)')
c     &     'Must have both ',
c     &     'ALLOW_SFLUX_CONTROL & ALLOW_SFLUX_COST_CONTRIBUTION'
c      CALL PRINT_ERROR( msgBuf , 1)
cph      STOP 'ABNORMAL END: S/R COST_CHECK'
#elif ((defined (ALLOW_AQH_CONTROL) && \
      !defined (ALLOW_AQH_COST_CONTRIBUTION)) || \
     (!defined (ALLOW_AQH_CONTROL) && \
      defined (ALLOW_AQH_COST_CONTRIBUTION)))
c      WRITE(msgBuf,'(A,A)')
c     &     'Must have both ',
c     &     'ALLOW_AQH_CONTROL & ALLOW_AQH_COST_CONTRIBUTION'
c      CALL PRINT_ERROR( msgBuf , 1)
cph      STOP 'ABNORMAL END: S/R COST_CHECK'
#endif

#if ((defined (ALLOW_USTRESS_CONTROL) && \
      !defined (ALLOW_USTRESS_COST_CONTRIBUTION)) || \
     (!defined (ALLOW_USTRESS_CONTROL) && \
      defined (ALLOW_USTRESS_COST_CONTRIBUTION)))
c      WRITE(msgBuf,'(A,A)')
c     &     'Must have both ',
c     &     'ALLOW_USTRESS_CONTROL & ALLOW_USTRESS_COST_CONTRIBUTION'
c      CALL PRINT_ERROR( msgBuf , 1)
cph      STOP 'ABNORMAL END: S/R COST_CHECK'
#elif ((defined (ALLOW_UWIND_CONTROL) && \
      !defined (ALLOW_UWIND_COST_CONTRIBUTION)) || \
     (!defined (ALLOW_UWIND_CONTROL) && \
      defined (ALLOW_UWIND_COST_CONTRIBUTION)))
c      WRITE(msgBuf,'(A,A)')
c     &     'Must have both ',
c     &     'ALLOW_UWIND_CONTROL & ALLOW_UWIND_COST_CONTRIBUTION'
c      CALL PRINT_ERROR( msgBuf , 1)
cph      STOP 'ABNORMAL END: S/R COST_CHECK'
#endif

#if ((defined (ALLOW_VSTRESS_CONTROL) && \
      !defined (ALLOW_VSTRESS_COST_CONTRIBUTION)) || \
     (!defined (ALLOW_VSTRESS_CONTROL) && \
      defined (ALLOW_VSTRESS_COST_CONTRIBUTION)))
c      WRITE(msgBuf,'(A,A)')
c     &     'Must have both ',
c     &     'ALLOW_VSTRESS_CONTROL & ALLOW_VSTRESS_COST_CONTRIBUTION'
c      CALL PRINT_ERROR( msgBuf , 1)
cph      STOP 'ABNORMAL END: S/R COST_CHECK'
#elif ((defined (ALLOW_VWIND_CONTROL) && \
      !defined (ALLOW_VWIND_COST_CONTRIBUTION)) || \
     (!defined (ALLOW_VWIND_CONTROL) && \
      defined (ALLOW_VWIND_COST_CONTRIBUTION)))
c      WRITE(msgBuf,'(A,A)')
c     &     'Must have both ',
c     &     'ALLOW_VWIND_CONTROL & ALLOW_VWIND_COST_CONTRIBUTION'
c      CALL PRINT_ERROR( msgBuf , 1)
cph      STOP 'ABNORMAL END: S/R COST_CHECK'
#endif

#endif /* ALLOW_ECCO */

      return
      end
