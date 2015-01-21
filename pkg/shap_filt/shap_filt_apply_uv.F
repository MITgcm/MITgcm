C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_apply_uv.F,v 1.13 2015/01/21 14:36:01 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

CBOP
C     !ROUTINE: SHAP_FILT_APPLY_UV
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_APPLY_UV(
     U                     uFld, vFld,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHAP_FILT_cwAPPLY_UV
C     | o Apply Shapiro filter on momentum :
C     |   filter the argments uFld & vFld.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "DYNVARS.h"
#include "GRID.h"
#include "SHAP_FILT.h"
#ifdef ALLOW_FRICTION_HEATING
# include "FFIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     uFld :: velocity field (U component) on which filter applies
C     vFld :: velocity field (V component) on which filter applies
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of SHAP_FILT_APPLY_UV
      _RL  uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

C     !LOCAL VARIABLES:
C     == Local variables ==
#ifdef USE_OLD_SHAPIRO_FILTERS
C     bi,bj,k :: loop index
      INTEGER bi, bj, k
#else /* USE_OLD_SHAPIRO_FILTERS */
      LOGICAL diag_dKE
      CHARACTER*(MAX_LEN_MBUF) suff
#if ( defined ALLOW_FRICTION_HEATING ) || ( defined ALLOW_DIAGNOSTICS )
      INTEGER bi, bj, k, i, j
      _RL dKE_shap(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
#endif /* USE_OLD_SHAPIRO_FILTERS */
CEOP

      IF ( momStepping .AND. nShapUV.GT.0) THEN

#ifdef  USE_OLD_SHAPIRO_FILTERS
        _EXCH_XYZ_RL( uFld,myThid )
        _EXCH_XYZ_RL( vFld,myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1, Nr
            CALL SHAP_FILT_U( uFld,bi,bj,k,myTime,myThid )
            CALL SHAP_FILT_V( vFld,bi,bj,k,myTime,myThid )
          ENDDO
         ENDDO
        ENDDO

        _EXCH_XYZ_RL( uFld,myThid )
        _EXCH_XYZ_RL( vFld,myThid )
#else
        IF (Shap_funct.EQ.1) THEN
          CALL SHAP_FILT_UV_S1(
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
        ELSEIF (Shap_funct.EQ.2 .OR. Shap_funct.EQ.20) THEN
          CALL SHAP_FILT_UV_S2(
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
        ELSEIF (Shap_funct.EQ.4) THEN
          CALL SHAP_FILT_UV_S4(
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
c       ELSEIF (Shap_funct.EQ.20) THEN
c         CALL SHAP_FILT_UV_S2G(
c    U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
c    I           Nr, myTime, myThid )
        ELSEIF (Shap_funct.EQ.21) THEN
          CALL SHAP_FILT_UV_S2C(
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
        ELSE
           STOP 'SHAP_FILT_APPLY_UV: Ooops! Bad Shap_funct in UV block'
        ENDIF

C-----  Diagnostic of Shapiro Filter effect on Momentum :
C       Note: Shap_tmpFld1,2 from shap_filt_tracer_s2 (and not s1, s4)
C             are directly proportional to Delta-U,V due to the Filter
        IF ( Shap_funct.NE.1 .AND. Shap_funct.NE.4 .AND.
     &     DIFFERENT_MULTIPLE(Shap_diagFreq,myTime,deltaTClock)
     &     ) THEN
          WRITE(suff,'(I10.10)') myIter
          CALL WRITE_FLD_XYZ_RL( 'shap_dU.', suff, Shap_tmpFld1,
     &                            myIter,myThid)
          CALL WRITE_FLD_XYZ_RL( 'shap_dV.', suff, Shap_tmpFld2,
     &                            myIter,myThid)
        ENDIF

        IF ( Shap_funct.NE.1 .AND. Shap_funct.NE.4 .AND.
     &       ( addFrictionHeating .OR. useDiagnostics ) ) THEN
         diag_dKE = .FALSE.
#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) THEN
           CALL DIAGNOSTICS_FILL( Shap_tmpFld1, 'SHAP_dU ', 0, Nr,
     &                                           0, 1, 1, myThid )
           CALL DIAGNOSTICS_FILL( Shap_tmpFld2, 'SHAP_dV ', 0, Nr,
     &                                           0, 1, 1, myThid )
           diag_dKE = DIAGNOSTICS_IS_ON('SHAP_dKE',myThid)
         ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#if ( defined ALLOW_FRICTION_HEATING ) || ( defined ALLOW_DIAGNOSTICS )
         IF ( addFrictionHeating .OR. diag_dKE ) THEN
           DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
             DO k=1,Nr
              DO j=1,sNy
               DO i=1,sNx
                 dKE_shap(i,j) = 0.5 _d 0 *(
     &             (
     &               Shap_tmpFld1(i  ,j,k,bi,bj)*uFld(i  ,j,k,bi,bj)
     &                *_hFacW(i ,j, k,bi,bj)*rAw(i ,j, bi,bj)
     &              +Shap_tmpFld1(i+1,j,k,bi,bj)*uFld(i+1,j,k,bi,bj)
     &                *_hFacW(i+1,j,k,bi,bj)*rAw(i+1,j,bi,bj)
     &             )
     &           + (
     &               Shap_tmpFld2(i,j  ,k,bi,bj)*vFld(i,j  ,k,bi,bj)
     &                *_hFacS(i,j  ,k,bi,bj)*rAs(i,j  ,bi,bj)
     &              +Shap_tmpFld2(i,j+1,k,bi,bj)*vFld(i,j+1,k,bi,bj)
     &                *_hFacS(i,j+1,k,bi,bj)*rAs(i,j+1,bi,bj)
     &             )                       )*recip_rA(i,j,bi,bj)
               ENDDO
              ENDDO
#ifdef ALLOW_FRICTION_HEATING
              IF ( addFrictionHeating ) THEN
               DO j=1,sNy
                DO i=1,sNx
                   frictionHeating(i,j,k,bi,bj) =
     &                             frictionHeating(i,j,k,bi,bj)
     &                           - dKE_shap(i,j)*drF(k)*rUnit2mass
                ENDDO
               ENDDO
              ENDIF
#endif /* ALLOW_FRICTION_HEATING */
#ifdef ALLOW_DIAGNOSTICS
              IF ( diag_dKE ) THEN
               CALL DIAGNOSTICS_FILL( dKE_shap, 'SHAP_dKE',
     &                                k, 1, 2, bi, bj, myThid )
              ENDIF
#endif /* ALLOW_DIAGNOSTICS */
             ENDDO
            ENDDO
           ENDDO
         ENDIF
#endif /* ALLOW_FRICTION_HEATING or ALLOW_DIAGNOSTICS */
        ENDIF

#endif /* USE_OLD_SHAPIRO_FILTERS */

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
