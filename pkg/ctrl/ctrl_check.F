
#include "CTRL_CPPOPTIONS.h"

      SUBROUTINE CTRL_CHECK( myThid )
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

#include "ctrl.h"

C     === Routine arguments ===
C     myThid -  Number of this instance
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'CTRL_CHECK: ctrl package'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

C     KPP needs convection turned off (will be packaged later)
C     Can't have both atmos. state and flux field as control

#ifdef ALLOW_TANGENTLINEAR_RUN
      if ( yadmark .NE. 'g_' ) then
         WRITE(msgBuf,'(A)') 
     &        'yadmark very likely wrong in data.ctrl for case '
         CALL PRINT_ERROR( msgBuf , 1)
         WRITE(msgBuf,'(A)') 
     &        'ALLOW_TANGENTLINEAR_RUN; should be g_ '
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R CTRL_CHECK'
      end if
#endif

#if (defined (ALLOW_HFLUX_CONTROL) && defined (ALLOW_ATEMP_CONTROL))
      WRITE(msgBuf,'(A)') 
     &     'Can't have both ALLOW_HFLUX_CONTROL & ALLOW_ATEMP_CONTROL'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif

#if (defined (ALLOW_SFLUX_CONTROL) && defined (ALLOW_AQH_CONTROL))
      WRITE(msgBuf,'(A)') 
     &     'Can't have both ALLOW_SFLUX_CONTROL & ALLOW_AQH_CONTROL'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif

#if (defined (ALLOW_USTRESS_CONTROL) && defined (ALLOW_UWIND_CONTROL))
      WRITE(msgBuf,'(A)') 
     &     'Can't have both ALLOW_USTRESS_CONTROL & ALLOW_UWIND_CONTROL'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif

#if (defined (ALLOW_VSTRESS_CONTROL) && defined (ALLOW_VWIND_CONTROL))
      WRITE(msgBuf,'(A)') 
     &     'Can't have both ALLOW_VSTRESS_CONTROL & ALLOW_VWIND_CONTROL'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif

#ifdef  ALLOW_ATEMP_CONTROL
#ifndef ALLOW_ATM_TEMP
      WRITE(msgBuf,'(A)') 
     &     'ALLOW_ATEMP_CONTROL used without ALLOW_ATM_TEMP'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif
#endif

#ifdef  ALLOW_AQH_CONTROL
#ifndef ALLOW_ATM_TEMP
      WRITE(msgBuf,'(A)') 
     &     'ALLOW_AQH_CONTROL used without ALLOW_ATM_TEMP'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif
#endif

#ifdef  ALLOW_UWIND_CONTROL
#ifndef ALLOW_ATM_WIND
      WRITE(msgBuf,'(A)') 
     &     'ALLOW_UWIND_CONTROL used without ALLOW_ATM_WIND'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif
#endif

#ifdef  ALLOW_VWIND_CONTROL
#ifndef ALLOW_ATM_WIND
      WRITE(msgBuf,'(A)') 
     &     'ALLOW_VWIND_CONTROL used without ALLOW_ATM_WIND'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif
#endif

      return
      end
