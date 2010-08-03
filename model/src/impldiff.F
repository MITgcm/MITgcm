C $Header: /u/gcmpack/MITgcm/model/src/impldiff.F,v 1.28 2010/08/03 12:46:44 mlosch Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: IMPLDIFF
C     !INTERFACE:
      SUBROUTINE IMPLDIFF( bi, bj, iMin, iMax, jMin, jMax,
     I                     tracerId, KappaRX, recip_hFac,
     U                     gXNm1,
     I                     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R IMPLDIFF                                              
C     | o Solve implicit diffusion equation for vertical          
C     |   diffusivity.                                            
C     *==========================================================*
C     | o Recoded from 2d intermediate fields to 3d to reduce     
C     |   TAMC storage                                            
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
#include "GAD.h"
#endif
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP_PARAMS.h"
#endif
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     tracerId :: tracer Identificator (if > 0) ; 
C      = 0 or < 0 when solving vertical viscosity implicitly for U or V
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER tracerId
      _RL KappaRX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RS recip_hFac(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL gXnm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k
      _RL deltaTX(Nr)
      _RL gYnm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL a(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL b(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL c(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL bet(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL gam(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
#ifdef ALLOW_GENERIC_ADVDIFF
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
#endif
      LOGICAL     DIAGNOSTICS_IS_ON
      EXTERNAL    DIAGNOSTICS_IS_ON
      _RL df (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
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
         deltaTX(k) = deltaTmom
        ENDDO
      ENDIF

C--   Initialise
      DO k=1,Nr
#ifdef TARGET_NEC_SX
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
#else
       DO j=jMin,jMax
        DO i=iMin,iMax
#endif
         gYNm1(i,j,k,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C--   Old aLower
#ifdef TARGET_NEC_SX
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
#else
      DO j=jMin,jMax
       DO i=iMin,iMax
#endif
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
          a(i,j,k) = -deltaTX(k)*recip_hFac(i,j,k,bi,bj)*recip_drF(k)
     &               *recip_deepFac2C(k)*recip_rhoFacC(k)
     &               *KappaRX(i,j, k )*recip_drC( k )
     &               *deepFac2F(k)*rhoFacF(k)
          IF (recip_hFac(i,j,k-1,bi,bj).EQ.0.) a(i,j,k)=0.
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
          c(i,j,k) = -deltaTX(k)*recip_hFac(i,j,k,bi,bj)*recip_drF(k)
     &               *recip_deepFac2C(k)*recip_rhoFacC(k)
     &               *KappaRX(i,j,k+1)*recip_drC(k+1)
     &               *deepFac2F(k+1)*rhoFacF(k+1)
          IF (recip_hFac(i,j,k+1,bi,bj).EQ.0.) c(i,j,k)=0.
        ENDDO
       ENDDO
      ENDDO
#ifdef TARGET_NEC_SX
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
#else
      DO j=jMin,jMax
       DO i=iMin,iMax
#endif
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
          b(i,j,k) = 1. _d 0 - c(i,j,k) - a(i,j,k)
        ENDDO
       ENDDO
      ENDDO

C--   Old and new gam, bet are the same
      DO k=1,Nr
#ifdef TARGET_NEC_SX
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
#else
       DO j=jMin,jMax
        DO i=iMin,iMax
#endif
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
        gYNm1(i,j,1,bi,bj) = gXNm1(i,j,1,bi,bj)*bet(i,j,1)
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
         gYnm1(i,j,k,bi,bj) = bet(i,j,k)*
     &        (gXnm1(i,j,k,bi,bj) - a(i,j,k)*gYnm1(i,j,k-1,bi,bj))
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
          gYnm1(i,j,k,bi,bj)=gYnm1(i,j,k,bi,bj)
     &              -gam(i,j,k+1)*gYnm1(i,j,k+1,bi,bj)
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
          gXnm1(i,j,k,bi,bj)=gYnm1(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDDO

#ifdef ALLOW_DIAGNOSTICS
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
     &            * KappaRX(i,j,k)*recip_drC(k)
     &            * (gXnm1(i,j,k,bi,bj) - gXnm1(i,j,k-1,bi,bj))*rkSign
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
     &            * KappaRX(i,j,k)*recip_drC(k)
     &            * (gXnm1(i,j,k,bi,bj) - gXnm1(i,j,k-1,bi,bj))*rkSign
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
     &            * KappaRX(i,j,k)*recip_drC(k)
     &            * (gXnm1(i,j,k,bi,bj) - gXnm1(i,j,k-1,bi,bj))*rkSign
     &            * _maskS(i,j,k,bi,bj)
     &            * _maskS(i,j,k-1,bi,bj)
             ENDDO
            ENDDO
          ENDIF
          CALL DIAGNOSTICS_FILL(df,diagName, k,1, 2,bi,bj, myThid)
         ENDDO
        ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
