C $Header: /u/gcmpack/MITgcm/model/src/config_check.F,v 1.2 2002/01/31 15:02:18 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CONFIG_CHECK
C     !INTERFACE:
      SUBROUTINE CONFIG_CHECK( myThid )
C     !DESCRIPTION: \bv
C     *=========================================================*
C     | SUBROUTINE CONFIG_CHECK
C     | o Check model parameter settings.
C     *=========================================================*
C     | This routine help to prevent the use of parameters
C     | that are not compatible with the model configuration.
C     *=========================================================*
C     \ev                                                           

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instances of CONFIG_CHECK
      INTEGER myThid
CEndOfInterface

C     !LOCAL VARIABLES:
C     == Local variables ==
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C-  check that CPP option is "defined" when running-flag parameter is on:

#ifndef ALLOW_NONHYDROSTATIC
      IF (nonHydrostatic) THEN 
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: #undef ALLOW_NONHYDROSTATIC and'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonHydrostatic is TRUE'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef EXACT_CONSERV
      IF (exactConserv) THEN 
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: #undef EXACT_CONSERV and'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: exactConserv is TRUE'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

#ifndef NONLIN_FRSURF
      IF (nonlinFreeSurf.NE.0) THEN 
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: #undef NONLIN_FRSURF and'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: nonlinFreeSurf is non-zero'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

C-  check parameter consistency :

#ifdef USE_NATURAL_BCS
      IF (rigidLid) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: #define USE_NATURAL_BCS and'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rigidLid are not compatible'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
      IF (exactConserv) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: #define USE_NATURAL_BCS with'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: exactConserv not yet implemented'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

      IF (rigidLid .AND. exactConserv) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: exactConserv not compatible with'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: rigidLid (meaningless in that case)'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      IF (nonlinFreeSurf.NE.0 .AND. .NOT.exactConserv) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: nonlinFreeSurf cannot be used'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: without exactConserv'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

C- note : not implemented in Release1_beta1 but it's done now (since 01-30-02)
c     IF (nonlinFreeSurf.NE.0 .AND. useOBCS ) THEN
c       WRITE(msgBuf,'(A)') 
c    &   'CONFIG_CHECK: nonlinFreeSurf not yet implemented'
c       CALL PRINT_ERROR( msgBuf , 1)
c       WRITE(msgBuf,'(A)')
c    &   'CONFIG_CHECK: in OBC package'
c       CALL PRINT_ERROR( msgBuf , 1)
c       STOP 'ABNORMAL END: S/R CONFIG_CHECK'
c     ENDIF

      IF (nonlinFreeSurf.NE.0 .AND. nonHydrostatic) THEN
        WRITE(msgBuf,'(A)') 
     &   'CONFIG_CHECK: nonlinFreeSurf not yet implemented'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &   'CONFIG_CHECK: in nonHydrostatic code'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF

      WRITE(msgBuf,'(A)') 'CONFIG_CHECK: OK'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)

      RETURN
      END
