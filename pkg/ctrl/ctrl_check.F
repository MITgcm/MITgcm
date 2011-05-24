C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_check.F,v 1.9 2011/05/24 21:13:53 jmc Exp $
C $Name:  $

#include "CTRL_CPPOPTIONS.h"
#ifdef ALLOW_ECCO_EVOLUTION
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#endif

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
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif

C     === Routine arguments ===
C     myThid -  Number of this instance
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL solve4Stress

      WRITE(msgBuf,'(A)') 'CTRL_CHECK: ctrl package'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

C     KPP needs convection turned off (will be packaged later)
C     Can not have both atmos. state and flux field as control

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
     &     'Cannot have both ALLOW_HFLUX_CONTROL & ALLOW_ATEMP_CONTROL'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif

#if (defined (ALLOW_SFLUX_CONTROL) && defined (ALLOW_AQH_CONTROL))
      WRITE(msgBuf,'(A)') 
     &     'Cannot have both ALLOW_SFLUX_CONTROL & ALLOW_AQH_CONTROL'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif

#if (defined (ALLOW_USTRESS_CONTROL) && defined (ALLOW_UWIND_CONTROL))
      WRITE(msgBuf,'(A)') 
     &     'Cannot have both ALLOW_USTRESS_CONTROL & ALLOW_UWIND_CONTROL'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif

#if (defined (ALLOW_VSTRESS_CONTROL) && defined (ALLOW_VWIND_CONTROL))
      WRITE(msgBuf,'(A)') 
     &     'Cannot have both ALLOW_VSTRESS_CONTROL & ALLOW_VWIND_CONTROL'
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

#ifdef ALLOW_SEAICE
C     pkg/seaice requires a particular set of control parameters
C     which in turn require a particular set of input fields
# if ( defined ALLOW_HFLUX_CONTROL || defined ALLOW_SFLUX_CONTROL )
      WRITE(msgBuf,'(A)') 
     &     'The SEAICE adjoint does not allow the surface fluxes'
      CALL PRINT_ERROR( msgBuf , 1)
      WRITE(msgBuf,'(A)') '  ALLOW_HFLUX_CONTROL'
      CALL PRINT_ERROR( msgBuf , 1)
      WRITE(msgBuf,'(A)') '  ALLOW_SFLUX_CONTROL'
      CALL PRINT_ERROR( msgBuf , 1)
      WRITE(msgBuf,'(A)') 'to be control variables'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
# endif
# if ( defined ALLOW_USTRESS_CONTROL || defined ALLOW_VSTRESS_CONTROL )
#  ifdef ALLOW_BULK_LARGEYEAGER04
      solve4Stress = wspeedfile .NE. ' '
#  else
      solve4Stress = .FALSE.
#  endif
      if (.NOT.solve4Stress) then
        WRITE(msgBuf,'(A)')
     &     'The SEAICE adjoint does not allow the surface fluxes'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)') '  ALLOW_USTRESS_CONTROL'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)') '  ALLOW_VSTRESS_CONTROL'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)') 'to be control variables'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)') 'except if wspeedfile is specified'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)') 'and ifdef ALLOW_BULK_LARGEYEAGER04'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CTRL_CHECK'
      endif
# endif
#endif /* ALLOW_SEAICE */
      return
      end
