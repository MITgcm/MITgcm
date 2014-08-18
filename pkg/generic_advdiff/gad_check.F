C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_check.F,v 1.10 2014/08/18 14:29:29 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_CHECK
C     !INTERFACE:
      SUBROUTINE GAD_CHECK( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GAD_CHECK
C     | o Check consistency with model configuration
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid :: my Thread Id number
      INTEGER myThid

#ifdef ALLOW_GENERIC_ADVDIFF
C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER minOlSize, n
CEOP

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'GAD_CHECK: #define ALLOW_GENERIC_ADVDIFF'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )

C--  Print out some kee parameters :
C    (better chance to know why it is stopping if print first & check after)

C--  Check Options:
#ifdef DISABLE_MULTIDIM_ADVECTION
c     IF ( useMultiDimAdvec ) THEN
      IF ( useMultiDimAdvec .OR.
     &     tempSOM_Advection .OR. saltSOM_Advection
     &   ) THEN
        WRITE(msgBuf,'(2A)') 'GAD_CHECK: ',
     &  'trying to use Multi-Dim. Advection code that is not compiled'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A)') 'GAD_CHECK: ',
     &  'Re-compile with:  #undef DISABLE_MULTIDIM_ADVECTION'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R GAD_CHECK'
      ENDIF
#endif /* DISABLE_MULTIDIM_ADVECTION */

#ifndef GAD_ALLOW_TS_SOM_ADV
      IF ( tempAdvScheme.EQ.ENUM_SOM_PRATHER
     & .OR.saltAdvScheme.EQ.ENUM_SOM_PRATHER
     & .OR.tempAdvScheme.EQ.ENUM_SOM_LIMITER
     & .OR.saltAdvScheme.EQ.ENUM_SOM_LIMITER ) THEN
        WRITE(msgBuf,'(2A)') 'GAD_CHECK: ',
     &  'trying to use 2nd.Order-Moment Advection that is not compiled'
        WRITE(msgBuf,'(2A)') 'GAD_CHECK: ',
     &  'Re-compile with:  #define GAD_ALLOW_TS_SOM_ADV'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R GAD_CHECK'
      ENDIF
#endif /* ndef GAD_ALLOW_TS_SOM_ADV */

C--  Check parameters:

C--  Check internal wave dynamics stability regarding active tracer time-stepping
      IF ( .NOT.(staggerTimeStep.OR.implicitIntGravWave) ) THEN
        IF ( ( tempIsActiveTr .AND. .NOT.AdamsBashforthGt )
     &  .OR. ( saltIsActiveTr .AND. .NOT.AdamsBashforthGs ) ) THEN
C-      issue a warning in Error msg file:
           WRITE(msgBuf,'(2A)') '** WARNING ** GAD_CHECK: ',
     &       'potentially unstable time-stepping (Internal Wave)'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT , myThid)
           WRITE(msgBuf,'(2A)') '** WARNING ** GAD_CHECK: ',
     &       'need "staggerTimeStep=.TRUE." in "data", nml PARM01'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT , myThid)
        ENDIF
      ENDIF

C--   Check size of overlap region
C--   Note: checking minimum size of overlap due to tracer limitations moved here;
C     previously done in GAD_INIT_FIXED, PTRACERS_INIT_FIXED and GMREDI_CHECK;
C     for mom limitations, done in CONFIG_CHECK, but can move to MOM_INIT_FIXED

      minOlSize = MAX( GAD_OlMinSize(1)+GAD_OlMinSize(2),
     &                 GAD_OlMinSize(2)+GAD_OlMinSize(3),
     &                 GAD_OlMinSize(1)*GAD_OlMinSize(3) )
      IF ( OLx.LT.minOlSize .OR. OLy.LT.minOlSize ) THEN
        WRITE(msgBuf,'(A,2I3,A)') 'GAD_CHECK: Overlap Size OLx,OLy=',
     &                             OLx, OLy,' too small'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'GAD_CHECK: ',
     &     'One tracer (T,S,pTrac, ...) advection scheme'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I3,A,3I2,A)') 'GAD_CHECK: ',
     &     'needs at least OLx,OLy=', minOlSize,
     &     ' (OlMinSize=', (GAD_OlMinSize(n),n=1,3), ')'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_CHECK'
      ENDIF

C Check compatibility with adjoint
#ifdef ALLOW_AUTODIFF
      IF ( tempAdvScheme.EQ.ENUM_OS7MP .OR.
     &     saltAdvScheme.EQ.ENUM_OS7MP ) THEN
        WRITE(msgBuf,'(A,I3,A)') 'GAD_CHECK: advection scheme OS7MP ',
     &     ENUM_OS7MP,
     &     ' not yet implemented for adjoint'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_CHECK'
      ENDIF
#endif /* ALLOW_AUTODIFF */

      _END_MASTER(myThid)

#endif /* ALLOW_GENERIC_ADVDIFF */
      RETURN
      END
