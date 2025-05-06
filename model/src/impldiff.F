#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: IMPLDIFF
C     !INTERFACE:
      SUBROUTINE IMPLDIFF( bi, bj, iMin, iMax, jMin, jMax,
     I                     tracerId, KappaRX, recip_hFac,
     U                     gTracer,
     I                     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R IMPLDIFF
C     | o Solve implicit diffusion equation for vertical
C     |   diffusivity.
C     *==========================================================*
C     | o Recoded from 2d intermediate fields to 3d to reduce
C     |   TAF storage
C     | o Fixed missing masks for fields a(), c()
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#ifdef ALLOW_LONGSTEP
# include "LONGSTEP_PARAMS.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
#endif
c#ifdef ALLOW_AUTODIFF_TAMC
c#endif

C     !INPUT/OUTPUT PARAMETERS:
C     tracerId   :: tracer Identificator (if > 0) ; = -1 or -2 when
C                   solving vertical viscosity implicitly for U or V
C     KappaRk    :: vertical diffusion coefficient
C     recip_hFac :: Inverse of cell open-depth factor
C     gTracer    :: future tracer field
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER tracerId
      _RL KappaRX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS recip_hFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER i,j,k
      _RL deltaTX(Nr), locUpdate
      _RL locTr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL a(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL c(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL bet(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gam(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
#ifdef ALLOW_GENERIC_ADVDIFF
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
#endif
      LOGICAL     DIAGNOSTICS_IS_ON
      EXTERNAL    DIAGNOSTICS_IS_ON
      _RL recip_dT
      _RL df (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_DIAGNOSTICS */
CEOP

cph(
cph Not good for TAF: may create irreducible control flow graph
cph      IF (Nr.LE.1) RETURN
cph)

#ifdef ALLOW_PTRACERS
      IF ( tracerId.GE.GAD_TR1) THEN
        DO k=1,Nr
         deltaTX(k) = PTRACERS_dTLev(k)
        ENDDO
      ELSEIF ( tracerId.GE.1 ) THEN
#else
      IF ( tracerId.GE.1 ) THEN
#endif
        DO k=1,Nr
         deltaTX(k) = dTtracerLev(k)
        ENDDO
      ELSE
        DO k=1,Nr
         deltaTX(k) = deltaTMom
        ENDDO
      ENDIF

C--   Initialise
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         locTr(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C--   Old aLower
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         a(i,j,1) = 0. _d 0
       ENDDO
      ENDDO
      DO k=2,Nr
#ifdef TARGET_NEC_SX
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
#else
       DO j=jMin,jMax
        DO i=iMin,iMax
#endif
          a(i,j,k) = -deltaTX(k)*recip_hFac(i,j,k)*recip_drF(k)
     &               *recip_deepFac2C(k)*recip_rhoFacC(k)
     &               *KappaRX(i,j, k )*recip_drC( k )
     &               *deepFac2F(k)*rhoFacF(k)
          IF (recip_hFac(i,j,k-1).EQ.0.) a(i,j,k)=0.
        ENDDO
       ENDDO
      ENDDO

C--   Old aUpper
      DO k=1,Nr-1
#ifdef TARGET_NEC_SX
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
#else
       DO j=jMin,jMax
        DO i=iMin,iMax
#endif
          c(i,j,k) = -deltaTX(k)*recip_hFac(i,j,k)*recip_drF(k)
     &               *recip_deepFac2C(k)*recip_rhoFacC(k)
     &               *KappaRX(i,j,k+1)*recip_drC(k+1)
     &               *deepFac2F(k+1)*rhoFacF(k+1)
          IF (recip_hFac(i,j,k+1).EQ.0.) c(i,j,k)=0.
        ENDDO
       ENDDO
      ENDDO
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         c(i,j,Nr) = 0. _d 0
       ENDDO
      ENDDO

C--   Old aCenter
      DO k=1,Nr
#ifdef TARGET_NEC_SX
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
#else
       DO j=jMin,jMax
        DO i=iMin,iMax
#endif
          b(i,j,k) = 1. _d 0 - ( a(i,j,k) + c(i,j,k) )
C-    to recover older (prior to 2016-10-05) results:
c         b(i,j,k) = 1. _d 0 - c(i,j,k) - a(i,j,k)
        ENDDO
       ENDDO
      ENDDO

C--   Old and new gam, bet are the same
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          bet(i,j,k) = 1. _d 0
          gam(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C--   Only need do anything if Nr>1
      IF (Nr.GT.1) THEN

       k = 1
C--    Beginning of forward sweep (top level)
#ifdef TARGET_NEC_SX
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
#else
       DO j=jMin,jMax
        DO i=iMin,iMax
#endif
         IF (b(i,j,1).NE.0.) bet(i,j,1) = 1. _d 0 / b(i,j,1)
        ENDDO
       ENDDO

      ENDIF

C--   Middle of forward sweep
      IF (Nr.GE.2) THEN

CADJ loop = sequential
       DO k=2,Nr

#ifdef TARGET_NEC_SX
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#else
        DO j=jMin,jMax
         DO i=iMin,iMax
#endif
          gam(i,j,k) = c(i,j,k-1)*bet(i,j,k-1)
          IF ( ( b(i,j,k) - a(i,j,k)*gam(i,j,k) ) .NE. 0.)
     &        bet(i,j,k) = 1. _d 0 / ( b(i,j,k) - a(i,j,k)*gam(i,j,k) )
         ENDDO
        ENDDO

       ENDDO

      ENDIF

#ifdef TARGET_NEC_SX
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
#else
      DO j=jMin,jMax
       DO i=iMin,iMax
#endif
        locTr(i,j,1) = gTracer(i,j,1)*bet(i,j,1)
       ENDDO
      ENDDO
      DO k=2,Nr
#ifdef TARGET_NEC_SX
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
#else
       DO j=jMin,jMax
        DO i=iMin,iMax
#endif
         locTr(i,j,k) = bet(i,j,k)*
     &        (gTracer(i,j,k) - a(i,j,k)*locTr(i,j,k-1))
        ENDDO
       ENDDO
      ENDDO

C--    Backward sweep
CADJ loop = sequential
       DO k=Nr-1,1,-1
#ifdef TARGET_NEC_SX
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#else
        DO j=jMin,jMax
         DO i=iMin,iMax
#endif
          locTr(i,j,k) = locTr(i,j,k) - gam(i,j,k+1)*locTr(i,j,k+1)
         ENDDO
        ENDDO
       ENDDO

       DO k=1,Nr
#ifdef TARGET_NEC_SX
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#else
        DO j=jMin,jMax
         DO i=iMin,iMax
#endif
          locUpdate =  locTr(i,j,k) - gTracer(i,j,k)
          gTracer(i,j,k) = locTr(i,j,k)
          locTr(i,j,k) = locUpdate
         ENDDO
        ENDDO
       ENDDO

#ifdef ALLOW_DIAGNOSTICS
C--   Diagnostics of momentum dissipation/viscous tendency, implicit part:
      IF ( useDiagnostics .AND.
     &     ( tracerId.EQ. -1 .OR. tracerId.EQ. -2 ) ) THEN
        IF ( tracerId.EQ. -1 ) diagName = 'Um_ImplD'
        IF ( tracerId.EQ. -2 ) diagName = 'Vm_ImplD'
        IF ( DIAGNOSTICS_IS_ON(diagName,myThid) ) THEN
          recip_dT = 0. _d 0
          IF ( deltaTMom.GT.zeroRL ) recip_dT = oneRL / deltaTMom
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              locTr(i,j,k) = locTr(i,j,k)*recip_dT
            ENDDO
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL( locTr,diagName, 0,Nr,2,bi,bj, myThid )
        ENDIF
      ENDIF

C--   Diagnostics of vertical diffusion flux (or vertical viscous flux):
      IF ( useDiagnostics .AND.tracerId.NE.0 ) THEN
        IF ( tracerId.GE. 1 ) THEN
C--   Set diagnostic suffix for the current tracer
#ifdef ALLOW_GENERIC_ADVDIFF
          diagSufx = GAD_DIAG_SUFX( tracerId, myThid )
#else
          diagSufx = 'aaaa'
#endif
          diagName = 'DFrI'//diagSufx
        ELSEIF ( tracerId.EQ. -1 ) THEN
          diagName = 'VISrI_Um'
        ELSEIF ( tracerId.EQ. -2 ) THEN
          diagName = 'VISrI_Vm'
        ELSE
          STOP 'IMPLIDIFF: should never reach this point !'
        ENDIF
        IF ( DIAGNOSTICS_IS_ON(diagName,myThid) ) THEN
         DO k= 1,Nr
          IF ( k.EQ.1 ) THEN
C-  Note: Needs to call DIAGNOSTICS_FILL at level k=1 even if array == 0
C         otherwise counter is not incremented !!
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               df(i,j) = 0. _d 0
             ENDDO
            ENDDO
          ELSEIF ( tracerId.GE.1 ) THEN
#ifdef TARGET_NEC_SX
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
#else
            DO j=1,sNy
             DO i=1,sNx
#endif
               df(i,j) =
     &             -rA(i,j,bi,bj)*deepFac2F(k)*rhoFacF(k)
     &            * KappaRX(i,j,k)*recip_drC(k)*rkSign
     &            * (gTracer(i,j,k) - gTracer(i,j,k-1))
     &            * maskC(i,j,k,bi,bj)
     &            * maskC(i,j,k-1,bi,bj)
             ENDDO
            ENDDO
          ELSEIF ( tracerId.EQ.-1 ) THEN
#ifdef TARGET_NEC_SX
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
#else
            DO j=1,sNy
             DO i=1,sNx+1
#endif
               df(i,j) =
     &             -rAw(i,j,bi,bj)*deepFac2F(k)*rhoFacF(k)
     &            * KappaRX(i,j,k)*recip_drC(k)*rkSign
     &            * (gTracer(i,j,k) - gTracer(i,j,k-1))
     &            * _maskW(i,j,k,bi,bj)
     &            * _maskW(i,j,k-1,bi,bj)
             ENDDO
            ENDDO
          ELSEIF ( tracerId.EQ.-2 ) THEN
#ifdef TARGET_NEC_SX
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
#else
            DO j=1,sNy+1
             DO i=1,sNx
#endif
               df(i,j) =
     &             -rAs(i,j,bi,bj)*deepFac2F(k)*rhoFacF(k)
     &            * KappaRX(i,j,k)*recip_drC(k)*rkSign
     &            * (gTracer(i,j,k) - gTracer(i,j,k-1))
     &            * _maskS(i,j,k,bi,bj)
     &            * _maskS(i,j,k-1,bi,bj)
             ENDDO
            ENDDO
          ENDIF
          CALL DIAGNOSTICS_FILL(df,diagName, k,1, 2,bi,bj, myThid)
#ifdef ALLOW_LAYERS
          IF ( useLayers ) THEN
           CALL LAYERS_FILL( df, tracerId, 'DFR',
     &                           k, 1, 2,bi,bj, myThid )
          ENDIF
#endif /* ALLOW_LAYERS */
         ENDDO
        ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
