C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_check.F,v 1.10 2012/07/07 22:23:46 jmc Exp $
C $Name:  $
#include "KPP_OPTIONS.h"

      SUBROUTINE KPP_CHECK( myThid )

C     *==========================================================*
C     | SUBROUTINE KPP_CHECK
C     | o Validate basic package setup and inter-package
C     |   dependencies.
C     *==========================================================*

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPP_PARAMS.h"

C     === Routine arguments ===
C     myThid :: my Thread Id Number
      INTEGER myThid

#ifdef ALLOW_KPP
C     === Local variables ===
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'KPP_CHECK: #define ALLOW_KPP'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , myThid )

C     KPP needs convection turned off (will be packaged later)
      IF (cAdjFreq.NE.0.  .OR.
     &     ivdc_kappa.NE.0.) THEN
         WRITE(msgBuf,'(A)') 'Some form of convection has been enabled'
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF

C     KPP needs implicit vertical diffusion and viscosity
      IF (.NOT.implicitDiffusion ) THEN
         WRITE(msgBuf,'(A)') 'KPP needs implicitDiffusion to be enabled'
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF
      IF ( .NOT.implicitViscosity .AND. momStepping ) THEN
         WRITE(msgBuf,'(A)') 'KPP needs implicitViscosity to be enabled'
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF

C     Shortwave heating should be defined with KPP
#ifndef SHORTWAVE_HEATING
      WRITE(msgBuf,'(A)') 'KPP needs CPP flag SHORTWAVE_HEATING enabled'
      CALL PRINT_ERROR( msgBuf , myThid )
      STOP 'ABNORMAL END: S/R KPP_CHECK'
#endif /* SHORTWAVE_HEATING */

C     Check that overlap region is adequate.
C     When horizontal smoothing is turned on,
C     KPP_CALC computes vertical viscosity and diffusivity for region
C     (-2:sNx+3,-2:sNy+3) as required by CALC_DIFFUSIVITY and requires
C     values of uVel, vVel, surfaceForcingU, surfaceForcingV in the
C     region (-2:sNx+4,-2:sNy+4).
C     Hence overlap region needs to be set OLx=4, OLy=4.
#if defined(KPP_SMOOTH_SHSQ) || defined(KPP_SMOOTH_DVSQ)
#  define KPP_NEEDS_OVERLAP_4
#endif
#if defined(KPP_SMOOTH_DBLOC) || defined(KPP_SMOOTH_DENS)
#  define KPP_NEEDS_OVERLAP_4
#endif
#ifdef KPP_NEEDS_OVERLAP_4
#ifdef KPP_REACTIVATE_OL4
      IF (OLx.LT.4 .OR. OLy.LT.4) THEN
       WRITE(msgBuf,'(A)') 'KPP horizontal smoothing requires OLx=OLy=4'
       CALL PRINT_ERROR( msgBuf , myThid )
       STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF
#endif
#endif

#ifdef EXCLUDE_KPP_DOUBLEDIFF
      IF ( KPPuseDoubleDiff ) THEN
       WRITE(msgBuf,'(A,A)') 'KPPuseDoubleDiff=.TRUE. requires ',
     &      'that EXCLUDE_KPP_DOUBLEDIFF is undefined'
       CALL PRINT_ERROR( msgBuf , myThid )
       STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF
#endif /* EXCLUDE_KPP_DOUBLEDIFF */

      _END_MASTER(myThid)

#endif /* ALLOW_KPP */
      RETURN
      END
