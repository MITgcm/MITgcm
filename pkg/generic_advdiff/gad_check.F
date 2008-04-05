C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_check.F,v 1.2 2008/04/05 21:28:32 jmc Exp $
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
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'GAD_CHECK: #define ALLOW_GENERIC_ADVDIFF'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )

C--  Print out some kee parameters :
C    (better chance to know why it's stopping if print first & check after)

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

#ifndef GAD_ALLOW_SOM_ADVECT
      IF ( tempAdvScheme.EQ.ENUM_SOM_PRATHER
     & .OR.saltAdvScheme.EQ.ENUM_SOM_PRATHER
     & .OR.tempAdvScheme.EQ.ENUM_SOM_LIMITER
     & .OR.saltAdvScheme.EQ.ENUM_SOM_LIMITER ) THEN
        WRITE(msgBuf,'(2A)') 'GAD_CHECK: ',
     &  'trying to use 2nd.Order-Moment Advection that is not compiled'
        WRITE(msgBuf,'(2A)') 'GAD_CHECK: ',
     &  'Re-compile with:  #define GAD_ALLOW_SOM_ADVECT'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R GAD_CHECK'
      ENDIF
#endif /* ndef GAD_ALLOW_SOM_ADVECT */

C--  Check parameters:

C-   Check internal wave dynamics stability regarding active tracer time-stepping
      IF ( .NOT.staggerTimeStep ) THEN
        IF ( tempIsActiveTr .AND.
     &                      .NOT.(AdamsBashforthGt.OR.AdamsBashforth_T)
     &  .OR. saltIsActiveTr .AND.
     &                      .NOT.(AdamsBashforthGs.OR.AdamsBashforth_S)
     &     ) THEN
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

C--- note: minimum size of overlap due to tracer limitations is checked in
C     GAD_INIT, PTRACERS_INIT_FIXED and GMREDI_CHECK ; can all move here.
C     for mom limitations, done in CONFIG_CHECK, but can move to MOM_INIT_FIXED
#ifndef ALLOW_GENERIC_ADVDIFF
C--- to be modified: --------
C     Check size of overlap region
      IF ( GM_AdvForm .AND. .NOT.GM_AdvSeparate
     &       .AND. GM_Visbeck_alpha.NE.0.
     &       .AND. useMultiDimAdvec
     &       .AND. (Olx.LT.3 .OR. Oly.LT.3) ) THEN
C       Visbeck variable K requires 1 more row/column in the overlap:
C       might need to increase Olx,Oly from 2 to 3 if GM advective

C       a) using a 5 points stencil advection scheme ; or
C       b) using a 3 points stencil advection scheme on CS-grid
C note: not clear how to check (b) since none of the advection scheme
C       currently implemented falls in this category, except if
C       GAD_FLUX_LIMITER.h is changed to use a "constant" flux-limiter
C       (from Lax-Wendroff to 1rst Order upwind).
C-------
c       IF ( (useCubedSphereExchange
c    &       .AND.(    tempAdvScheme.EQ.ENUM_LAXWENDROFF
c    &            .OR. saltAdvScheme.EQ.ENUM_LAXWENDROFF )
c    &       ).OR.(    tempAdvScheme.EQ.ENUM_FLUX_LIMIT
c    &            .OR. saltAdvScheme.EQ.ENUM_FLUX_LIMIT
c    &            .OR. tempAdvScheme.EQ.ENUM_DST3_FLUX_LIMIT
c    &            .OR. saltAdvScheme.EQ.ENUM_DST3_FLUX_LIMIT
c    &            .OR. tempAdvScheme.EQ.ENUM_DST3
c    &            .OR. saltAdvScheme.EQ.ENUM_DST3 )
c    &     ) THEN
          WRITE(msgBuf,'(A,A)')
     &     'GAD_CHECK: Visbeck + GM_AdvForm in MultiDimAdvec'
          CALL PRINT_ERROR( msgBuf , myThid )
          WRITE(msgBuf,'(A)') 'GAD_CHECK: need at least Olx,Oly = 3'
          CALL PRINT_ERROR( msgBuf , myThid )
          STOP 'ABNORMAL END: S/R GAD_CHECK'
c       ENDIF
      ENDIF
#endif /* ALLOW_GENERIC_ADVDIFF */

      _END_MASTER(myThid)

#endif /* ALLOW_GENERIC_ADVDIFF */
      RETURN
      END
