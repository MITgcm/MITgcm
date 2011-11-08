C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_calc_stevens.F,v 1.6 2011/11/08 17:00:25 mlosch Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"
#undef CHECK_BALANCE

CBOP
C     !ROUTINE: OBCS_CALC_STEVENS
C     !INTERFACE:
      SUBROUTINE OBCS_CALC_STEVENS(
     I     futureTime, futureIter,
     I     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_CALC_STEVENS
C     | o Calculate future boundary data at open boundaries
C     |   at time = futureTime
C     |   from input data following Stevens(1990), and some
C     |   MOM3 legacy code
C     |
C     | o the code works like this
C     |  - the "barotropic" (= vertically averaged) velocity
C     |    normal to the boundary is assumed to be in
C     |    OBE/W/N/Su/v (normal) when this routine is entered
C     |  - the vertically averaged velocity is corrected
C     |    by the "baroclinic" (= deviation from vertically
C     |    averaged velocity) velocity to give a new OB?u/v;
C     |    the "barolinic" velocity is estimated from the previous
C     |    time step which makes this boundary condition depend on
C     |    a restart file. If OBCS_STEVENS_USE_INTERIOR_VELOCITY
C     |    is defined the velocity is simply copied from the model
C     |    interior to the boundary, thereby avoiding a restart
C     |    file or complicated reconstruction, but this solution
C     |    can give unexpected results.
C     |    (Note: in this context the terms barotropic and baroclinic
C     |    are MOM jargon and --- to my mind ---- should not be used)
C     |  - a wave phase speed is estimated from temporal and
C     |    horizontal variations of the tracer fields for each
C     |    tracer individually, this similar to Orlanski BCs
C     |  - velocity tangential to the boundary is always zero
C     |    (although this could be changed)
C     |  - a new tracer is computed from local advection equation
C     |    with an upwind scheme: tracer from the interior is
C     |    advected out of the domain, and tracer from the boundary
C     |    is "advected" into the domain by a restoring mechanism
C     |  - for the advection equation only values from the
C     |    the current (not the updated) time level are used
C     |
C     *==========================================================*
C     | Feb, 2009: started by Martin Losch (Martin.Losch@awi.de)
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#include "DYNVARS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      _RL futureTime
      INTEGER futureIter
      INTEGER myThid

#ifdef ALLOW_OBCS_STEVENS

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I,J,K        :: loop indices
C     msgBuf       :: Informational/error message buffer
C     uMer/vZonBar :: vertically averaged velocity at open boundary
C     drFBar       :: local depth for vertical average
C     uMer/vZonPri :: velocity anomalies applied to the open boundaries
C     gammat/s     :: restoring parameters (1./(T/SrelaxStevens - time scale))
C     u/vPhase     :: estimate of phase velocity for radiation condition
C     auxillary variables
C     clf          :: ratio of grid spacing and time step
C     dtracSpace   :: horizontal difference of tracer
C     aFac         :: switch (0 or 1) that turns on advective contribution
C     gFacM/Z      :: switch (0 or 1) that turns on restoring boundary condition
C     pFac         :: switch that turns on/off phase velocity contribution
      INTEGER bi, bj
      INTEGER I, J, K
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL uPhase
      _RL vPhase
      _RL dtracSpace
      _RL cflMer (1-OLy:sNy+OLy,Nr)
      _RL gFacM  (1-OLy:sNy+OLy,Nr)
      _RL cflZon (1-OLx:sNx+OLx,Nr)
      _RL gFacZ  (1-OLy:sNy+OLy,Nr)
      _RL uMerPri(1-OLy:sNy+OLy,Nr)
      _RL vZonPri(1-OLx:sNx+OLx,Nr)
      _RL uMerBar(1-OLy:sNy+OLy)
      _RL vZonBar(1-OLx:sNx+OLx)
      _RL drFBar 
      _RL gammat, gammas, pFac, aFac
#ifdef ALLOW_PTRACERS
c     INTEGER iTracer
#endif /* ALLOW_PTRACERS */
#ifdef CHECK_BALANCE
      _RL uVelLoc, vVelLoc
      _RL vPhase
#endif
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_CALC_STEVENS',myThid)
#endif

      aFac   = 1. _d 0
      IF (.NOT. useStevensAdvection ) aFac   = 0. _d 0
      pFac   = 1. _d 0
      IF (.NOT. useStevensPhaseVel )  pFac   = 0. _d 0
      gammat = 0. _d 0
      IF (TrelaxStevens .GT. 0. _d 0) gammat = 1./TrelaxStevens
      gammas = 0. _d 0
      IF (SrelaxStevens .GT. 0. _d 0) gammas = 1./SrelaxStevens

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_OBCS_EAST

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBEt(:,:,bi,bj)  = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE OBEs(:,:,bi,bj)  = comlev1_bibj, key=ikey, byte=isbyte
# endif
        IF ( useStevensEast ) THEN
C     Eastern OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: East',myThid)
#endif
C     compute vertical average and deviation from vertical
C     average for I_obe
C     first initialize some fields
         DO J=1-Oly,sNy+Oly
          uMerBar(J) = 0. _d 0
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           uMerPri(J,K) = 0. _d 0
          ENDDO
         ENDDO
         DO J=1-Oly,sNy+Oly
          I=OB_Ie(J,bi,bj)
          IF (I.NE.0) THEN
           drFbar = 0. _d 0
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerBar(J) = uMerBar(J) + uVel(I-1,J,K,bi,bj)
#else
            uMerBar(J) = uMerBar(J) + OBEuStevens(J,K,bi,bj)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(K)* _hFacW(I,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacW(I,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar(J) = uMerBar(J)/drFBar
          ENDIF
         ENDDO
         DO J=1-Oly,sNy+Oly
          I=OB_Ie(J,bi,bj)
          IF (I.NE.0) THEN
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerPri(J,K) = (uVel(I-1,J,K,bi,bj)-uMerBar(J))
#else 
            uMerPri(J,K) = (OBEuStevens(J,K,bi,bj)-uMerBar(J))
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskW(I,J,K,bi,bj)
           ENDDO
          ENDIF
         ENDDO
C     vertical average of input field
         DO J=1-Oly,sNy+Oly
          uMerBar(J) = 0. _d 0
         ENDDO
         DO J=1-Oly,sNy+Oly
          I=OB_Ie(J,bi,bj)
          IF (I.NE.0) THEN
           drFbar = 0. _d 0
           DO K=1,Nr
            uMerBar(J) = uMerBar(J) + OBEu(J,K,bi,bj)
     &           *drF(K)* _hFacW(I,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacW(I,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar(J) = uMerBar(J)/drFBar
          ENDIF
         ENDDO
C     Now the absolute velocity normal to the boundary is
C     uMerBar(J) + uMerPri(J,K).
         DO J=1-Oly,sNy+Oly
          I=OB_Ie(J,bi,bj)
          IF (I.NE.0) THEN
           DO K=1,Nr
            OBEu(J,K,bi,bj) = (uMerBar(J) + uMerPri(J,K))
     &           * _maskW(I,J,K,bi,bj)
CML            OBEv(J,K,bi,bj) = 0. _d 0
#ifdef ALLOW_NONHYDROSTATIC
            OBEw(J,K,bi,bj)=0.
#endif
           ENDDO
          ENDIF
         ENDDO
#ifdef NONLIN_FRSURF
C     this is a bit of a hack
         IF ( nonlinFreeSurf.GT.0 ) THEN
          DO J=1-Oly,sNy+Oly
           I=OB_Ie(J,bi,bj)
           IF (I.ne.0) THEN
            OBEeta(J,bi,bj) = etaN(I-1,J,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer!
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Ie(J,bi,bj)
           IF (I.NE.0) THEN
            cflMer(J,K) = 0.5 _d 0 * _dxC(I-1,J,bi,bj)/deltaTmom
CML         gFacM(J,K)  = 0. _d 0
CML         IF ( uVel(I,J,K,bi,bj) .LT. 0. _d 0 ) gFacM(J,K) = 1. _d 0
            gFacM(J,K)  = ABS(MIN(SIGN(1.D0,uVel(I,J,K,bi,bj)),0.D0))
           ELSE
            cflMer(J,K) = 0. _d 0
            gFacM (J,K) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Ie(J,bi,bj)
           IF (I.NE.0) THEN
C     Theta first:
            dTracSpace = (theta(I-1,J,K,bi,bj)-theta(I-2,J,K,bi,bj))
     &           * _maskW(I-1,J,K,bi,bj)
            uPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             uPhase = MIN( cflMer(J,K), 
#ifdef ALLOW_ADAMSBASHFORTH_3
     &        MAX( 0.D0, -cflMer(J,K)*gtNm(I-1,J,K,bi,bj,1)/dTracSpace )
#else
     &        MAX( 0.D0, -cflMer(J,K)*gtNm1(I-1,J,K,bi,bj)/dTracSpace )
#endif
     &            ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
            OBEt(J,K,bi,bj) = theta(I,J,K,bi,bj) + dTtracerLev(K)
     &          * _maskW(I,J,K,bi,bj)
     &          * (
     &          - ( aFac*MAX(0.D0,uVel(I,J,K,bi,bj)) + uPhase )
     &          *(theta(I,J,K,bi,bj)-theta(I-1,J,K,bi,bj))
     &          * _recip_dxC(I,J,bi,bj)
     &          - gFacM(J,K) * gammat 
     &           * (theta(I,J,K,bi,bj)-OBEt(J,K,bi,bj)) )
C     Now salinity:
            dTracSpace = (salt(I-1,J,K,bi,bj)-salt(I-2,J,K,bi,bj))
     &          * _maskW(I-1,J,K,bi,bj)
            uPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             uPhase = MIN( cflMer(J,K),
#ifdef ALLOW_ADAMSBASHFORTH_3
     &        MAX( 0.D0, -cflMer(J,K)*gsNm(I-1,J,K,bi,bj,1)/dTracSpace )
#else
     &        MAX( 0.D0, -cflMer(J,K)*gsNm1(I-1,J,K,bi,bj)/dTracSpace )
#endif
     &           ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
            OBEs(J,K,bi,bj) = salt(I,J,K,bi,bj) + dTtracerLev(K)
     &          * _maskW(I,J,K,bi,bj)
     &          * (
     &          - ( aFac*MAX(0.D0,uVel(I,J,K,bi,bj)) + uPhase )
     &          *(salt(I,J,K,bi,bj)-salt(I-1,J,K,bi,bj))
     &          * _recip_dxC(I,J,bi,bj)
     &          - gFacM(J,K) * gammas 
     &           * (salt(I,J,K,bi,bj)-OBEs(J,K,bi,bj)) )
           ENDIF
          ENDDO
         ENDDO
C     IF ( useStevensEast ) THEN
        ENDIF
#endif /* ALLOW_OBCS_EAST */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_WEST

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBWt(:,:,bi,bj)  = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE OBWs(:,:,bi,bj)  = comlev1_bibj, key=ikey, byte=isbyte
# endif
        IF ( useStevensWest ) THEN
C     Western OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: West',myThid)
#endif
C     compute vertical average and deviation from vertical
C     average for I_obw+1
C     first initialize some fields
         DO J=1-Oly,sNy+Oly
          uMerBar(J) = 0. _d 0
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           uMerPri(J,K) = 0. _d 0
          ENDDO
         ENDDO
         DO J=1-Oly,sNy+Oly
          I=OB_Iw(J,bi,bj)
          IF (I.NE.0) THEN
           drFBar = 0. _d 0
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerBar(J) = uMerBar(J) + uVel(I+2,J,K,bi,bj)
#else
            uMerBar(J) = uMerBar(J) + OBWuStevens(J,K,bi,bj)
#endif /* OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(K)* _hFacW(I+1,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacW(I+1,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar(J) = uMerBar(J)/drFBar
          ENDIF
         ENDDO
         DO J=1-Oly,sNy+Oly
          I=OB_Iw(J,bi,bj)
          IF (I.NE.0) THEN
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerPri(J,K) = (uVel(I+2,J,K,bi,bj)-uMerBar(J))
#else
            uMerPri(J,K) = (OBWuStevens(J,K,bi,bj)-uMerBar(J))
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskW(I+1,J,K,bi,bj)
           ENDDO
          ENDIF
         ENDDO
C     vertical average of input field
         DO J=1-Oly,sNy+Oly
          uMerBar(J) = 0. _d 0
         ENDDO
         DO J=1-Oly,sNy+Oly
          I=OB_Iw(J,bi,bj)
          IF (I.NE.0) THEN
           drFBar  = 0. _d 0
           DO K=1,Nr
            uMerBar(J) = uMerBar(J) + OBWu(J,K,bi,bj)
     &           *drF(K)* _hFacW(I+1,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacW(I+1,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar(J) = uMerBar(J)/drFBar
          ENDIF
         ENDDO
C     Now the absolute velocity normal to the boundary is
C     uMerBar(J) + uMerPri(J,K).
         DO J=1-Oly,sNy+Oly
          I=OB_Iw(J,bi,bj)
          IF (I.NE.0) THEN
           DO K=1,Nr
            OBWu(J,K,bi,bj) = (uMerBar(J) + uMerPri(J,K))
     &          * _maskW(I+1,J,K,bi,bj)
CML            OBWv(J,K,bi,bj) = 0. _d 0
#ifdef ALLOW_NONHYDROSTATIC
            OBWw(J,K,bi,bj)=0.
#endif
           ENDDO
          ENDIF
         ENDDO
#ifdef NONLIN_FRSURF
C     this is a bit of a hack
         IF ( nonlinFreeSurf.GT.0 ) THEN
          DO J=1-Oly,sNy+Oly
           I=OB_Iw(J,bi,bj)
           IF (I.ne.0) THEN
            OBWeta(J,bi,bj) = etaN(I+1,J,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer! 
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Iw(J,bi,bj)
           IF (I.NE.0) THEN
            cflMer(J,K) = - 0.5 _d 0 * _dxC(I+2,J,bi,bj)/deltaTmom
CML         gFacM = 0. _d 0
CML         IF ( uVel(I+1,J,K,bi,bj) .GT. 0. _d 0 ) gFacM = 1. _d 0
            gFacM(J,K)  = ABS(MAX(SIGN(1.D0,uVel(I+1,J,K,bi,bj)),0.D0))
           ELSE
            cflMer(J,K) = 0. _d 0
            gFacM (J,K) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Iw(J,bi,bj)
           IF (I.NE.0) THEN
C     Theta first:
            dTracSpace = (theta(I+2,J,K,bi,bj)-theta(I+1,J,K,bi,bj))
     &        * _maskW(I+2,J,K,bi,bj)
            uPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             uPhase = MAX( cflMer(J,K),
#ifdef ALLOW_ADAMSBASHFORTH_3
     &       MIN( 0.D0, -cflMer(J,K)*gtNm(I+1,J,K,bi,bj,1)/dTracSpace )
#else
     &       MIN( 0.D0, -cflMer(J,K)*gtNm1(I+1,J,K,bi,bj)/dTracSpace )
#endif
     &         ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
            OBWt(J,K,bi,bj) = theta(I,J,K,bi,bj) + dTtracerLev(K)
     &        * _maskW(I+1,J,K,bi,bj)
     &        * (
     &        - ( aFac*MIN(0.D0,uVel(I+1,J,K,bi,bj)) + uPhase )
     &        *(theta(I+1,J,K,bi,bj)-theta(I,J,K,bi,bj))
     &        * _recip_dxC(I+1,J,bi,bj)
     &        - gFacM(J,K) * gammat 
     &           * (theta(I,J,K,bi,bj)-OBWt(J,K,bi,bj)) )
C     Now salinity:
            dTracSpace = (salt(I+2,J,K,bi,bj)-salt(I+1,J,K,bi,bj))
     &        * _maskW(I+2,J,K,bi,bj)
            uPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             uPhase = MAX( cflMer(J,K),
#ifdef ALLOW_ADAMSBASHFORTH_3
     &       MIN( 0.D0, -cflMer(J,K)*gsNm(I+1,J,K,bi,bj,1)/dTracSpace )
#else
     &       MIN( 0.D0, -cflMer(J,K)*gsNm1(I+1,J,K,bi,bj)/dTracSpace )
#endif
     &         ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
            OBWs(J,K,bi,bj) = salt(I,J,K,bi,bj) + dTtracerLev(K)
     &        * _maskW(I+1,J,K,bi,bj)
     &        * (
     &        - ( aFac*MIN(0.D0,uVel(I+1,J,K,bi,bj)) + uPhase )
     &        *(salt(I+1,J,K,bi,bj)-salt(I,J,K,bi,bj))
     &        * _recip_dxC(I+1,J,bi,bj)
     &        - gFacM(J,K) * gammas 
     &           * (salt(I,J,K,bi,bj)-OBWs(J,K,bi,bj)) )
           ENDIF
          ENDDO
         ENDDO
C      IF ( useStevensWest ) THEN
        ENDIF
#endif /* ALLOW_OBCS_WEST */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_NORTH
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBNt(:,:,bi,bj)  = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE OBNs(:,:,bi,bj)  = comlev1_bibj, key=ikey, byte=isbyte
# endif
        IF ( useStevensNorth ) THEN
C         Northern OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: North',myThid)
#endif
C     compute vertical average and deviation from vertical
C     average for J_obn
C     first initialize some fields
         DO I=1-Olx,sNx+Olx
          vZonBar(I) = 0. _d 0
         ENDDO
         DO K=1,Nr
          DO I=1-Olx,sNx+Olx
           vZonPri(I,K) = 0. _d 0
          ENDDO
         ENDDO
         DO I=1-Olx,sNx+Olx
          J=OB_Jn(I,bi,bj)
          IF (J.NE.0) THEN
           drFBar  = 0. _d 0
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonBar(I) = vZonBar(I) + vVel(I,J-1,K,bi,bj)
#else
            vZonBar(I) = vZonBar(I) + OBNvStevens(I,K,bi,bj)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(K)* _hFacS(I,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacS(I,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar(I) = vZonBar(I)/drFBar
          ENDIF
         ENDDO
         DO I=1-Olx,sNx+Olx
          J=OB_Js(I,bi,bj)
          IF (J.NE.0) THEN
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonPri(I,K) = (vVel(I,J-1,K,bi,bj)-vZonBar(I))
#else
            vZonPri(I,K) = (OBNvStevens(I,K,bi,bj)-vZonBar(I))
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskS(I,J,K,bi,bj)
           ENDDO
          ENDIF
         ENDDO
C     vertical average of input field
         DO I=1-Olx,sNx+Olx
          vZonBar(I) = 0. _d 0
         ENDDO
         DO I=1-Olx,sNx+Olx
          J=OB_Js(I,bi,bj)
          IF (J.NE.0) THEN
           drFBar  = 0. _d 0
           DO K=1,Nr
            vZonBar(I) = vZonBar(I) + OBNv(I,K,bi,bj)
     &           *drF(K)* _hFacS(I,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacS(I,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar(I) = vZonBar(I)/drFBar
          ENDIF
         ENDDO
C     Now the absolute velocity normal to the boundary is
C     vZonBar(I) + vZonPri(J,K).
         DO I=1-Olx,sNx+Olx
          J=OB_Jn(I,bi,bj)
          IF (J.NE.0) THEN
           DO K=1,Nr
            OBNv(I,K,bi,bj) = (vZonBar(I) + vZonPri(I,K))
     &          * _maskS(I,J,K,bi,bj)
CML            OBNu(I,K,bi,bj) = 0. _d 0
#ifdef ALLOW_NONHYDROSTATIC
            OBNw(I,K,bi,bj)=0.
#endif
           ENDDO
          ENDIF
         ENDDO
#ifdef NONLIN_FRSURF
C     this is a bit of a hack
         IF ( nonlinFreeSurf.GT.0 ) THEN
          DO I=1-Olx,sNx+Olx
           J=OB_Jn(I,bi,bj)
           IF (J.ne.0) THEN
            OBNeta(I,bi,bj) = etaN(I,J-1,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer! 
         DO K=1,Nr
          DO I=1-Olx,sNx+Olx
           J=OB_Jn(I,bi,bj)
           IF (J.NE.0) THEN
            cflZon(I,K) = 0.5 _d 0 * _dyC(I,J-1,bi,bj)/deltaTmom
CML         gFacZ(I,K) = 0. _d 0
CML         IF ( vVel(I,J,K,bi,bj) .LT. 0. _d 0 ) gFacZ(I,K) = 1. _d 0
            gFacZ(I,K)  = ABS(MIN(SIGN(1.D0,vVel(I,J,K,bi,bj)),0.D0))
           ELSE
            cflZon(I,K) = 0. _d 0
            gFacZ (I,K) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
         DO K=1,Nr
          DO I=1-Olx,sNx+Olx
           J=OB_Jn(I,bi,bj)
           IF (J.NE.0) THEN
C     Theta first:
            dTracSpace = (theta(I,J-1,K,bi,bj)-theta(I,J-2,K,bi,bj))
     &        * _maskS(I,J-1,K,bi,bj)
            vPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             vPhase = MIN( cflZon(I,K),
#ifdef ALLOW_ADAMSBASHFORTH_3
     &       MAX( 0.D0, -cflZon(I,K)*gtNm(I,J-1,K,bi,bj,1)/dTracSpace )
#else
     &       MAX( 0.D0, -cflZon(I,K)*gtNm1(I,J-1,K,bi,bj)/dTracSpace )
#endif
     &         ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
            OBNt(I,K,bi,bj) = theta(I,J,K,bi,bj) + dTtracerLev(K)
     &        * _maskS(I,J,K,bi,bj)
     &        * (
     &        - ( aFac*MAX(0.D0,vVel(I,J,K,bi,bj)) + vPhase )
     &        *(theta(I,J,K,bi,bj)-theta(I,J-1,K,bi,bj))
     &        * _recip_dyC(I,J,bi,bj)
     &        - gFacZ(I,K) * gammat 
     &           * (theta(I,J,K,bi,bj)-OBNt(I,K,bi,bj)) )
C     Now salinity:
            dTracSpace = (salt(I,J-1,K,bi,bj)-salt(I,J-2,K,bi,bj))
     &        * _maskS(I,J-1,K,bi,bj)
            vPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             vPhase = MIN( cflZon(I,K),
#ifdef ALLOW_ADAMSBASHFORTH_3
     &       MAX( 0.D0, -cflZon(I,K)*gsNm(I,J-1,K,bi,bj,1)/dTracSpace )
#else
     &       MAX( 0.D0, -cflZon(I,K)*gsNm1(I,J-1,K,bi,bj)/dTracSpace )
#endif
     &         ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
            OBNs(I,K,bi,bj) = salt(I,J,K,bi,bj) + dTtracerLev(K)
     &        * _maskS(I,J,K,bi,bj)
     &        * (
     &        - ( aFac*MAX(0.D0,vVel(I,J,K,bi,bj)) + vPhase )
     &        *(salt(I,J,K,bi,bj)-salt(I,J-1,K,bi,bj))
     &        * _recip_dyC(I,J,bi,bj)
     &        - gFacZ(I,K) * gammat 
     &           * (salt(I,J,K,bi,bj)-OBNs(I,K,bi,bj)) )
           ENDIF
          ENDDO
         ENDDO
C      IF ( useStevensNorth ) THEN
        ENDIF
#endif /* ALLOW_OBCS_NORTH */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_SOUTH

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBSt(:,:,bi,bj)  = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE OBSs(:,:,bi,bj)  = comlev1_bibj, key=ikey, byte=isbyte
# endif
        IF ( useStevensSouth ) THEN
C         Southern OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: South',myThid)
#endif
C     compute vertical average and deviation from vertical
C     average for J_obs+1
C     first initialize some fields
         DO I=1-Olx,sNx+Olx
          vZonBar(I) = 0. _d 0
         ENDDO
         DO K=1,Nr
          DO I=1-Olx,sNx+Olx
           vZonPri(I,K) = 0. _d 0
          ENDDO
         ENDDO
         DO I=1-Olx,sNx+Olx
          J=OB_Js(I,bi,bj)
          IF (J.NE.0) THEN
           drFBar  = 0. _d 0
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonBar(I) = vZonBar(I) + vVel(I,J+2,K,bi,bj)
#else
            vZonBar(I) = vZonBar(I) + OBSvStevens(I,K,bi,bj)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(K)* _hFacS(I,J+1,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacS(I,J+1,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar(I) = vZonBar(I)/drFBar
          ENDIF
         ENDDO
         DO I=1-Olx,sNx+Olx
          J=OB_Js(I,bi,bj)
          IF (J.NE.0) THEN
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonPri(I,K) = (vVel(I,J+2,K,bi,bj)-vZonBar(I))
#else
            vZonPri(I,K) = (OBSvStevens(I,K,bi,bj)-vZonBar(I))
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskS(I,J+1,K,bi,bj)
           ENDDO
          ENDIF
         ENDDO
C     vertical average of input field
         DO I=1-Olx,sNx+Olx
          vZonBar(I) = 0. _d 0
         ENDDO
         DO I=1-Olx,sNx+Olx
          J=OB_Js(I,bi,bj)
          IF (J.NE.0) THEN
           drFBar  = 0. _d 0
           DO K=1,Nr
            vZonBar(I) = vZonBar(I) + OBSv(I,K,bi,bj)
     &           *drF(K)* _hFacS(I,J+1,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacS(I,J+1,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar(I) = vZonBar(I)/drFBar
          ENDIF
         ENDDO
C     Now the absolute velocity normal to the boundary is
C     vZonBar(I) + vZonPri(I,K).
         DO I=1-Olx,sNx+Olx
          J=OB_Js(I,bi,bj)
          IF (J.NE.0) THEN
           DO K=1,Nr
            OBSv(I,K,bi,bj) = (vZonBar(I) + vZonPri(I,K))
     &          * _maskS(I,J+1,K,bi,bj)
CML            OBSu(I,K,bi,bj) = 0. _d 0
#ifdef ALLOW_NONHYDROSTATIC
            OBSw(I,K,bi,bj)=0.
#endif
           ENDDO
          ENDIF
         ENDDO
#ifdef NONLIN_FRSURF
C     this is a bit of a hack
         IF ( nonlinFreeSurf.GT.0 ) THEN
          DO I=1-Olx,sNx+Olx
           J=OB_Js(I,bi,bj)
           IF (J.ne.0) THEN
            OBSeta(I,bi,bj) = etaN(I,J+1,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer! 
         DO K=1,Nr
          DO I=1-Olx,sNx+Olx
           J=OB_Js(I,bi,bj)
           IF (J.NE.0) THEN
            cflZon(I,K) = - 0.5 _d 0 * _dyC(I,J+2,bi,bj)/deltaTmom
CML         gFacZ = 0. _d 0
CML         IF ( vVel(I,J+1,K,bi,bj) .GT. 0. _d 0 ) gFacZ = 1. _d 0
            gFacZ(I,K)  = ABS(MAX(SIGN(1.D0,vVel(I,J+1,K,bi,bj)),0.D0))
           ELSE
            cflZon(I,K) = 0. _d 0
            gFacZ (I,K) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
         DO K=1,Nr
          DO I=1-Olx,sNx+Olx
           J=OB_Js(I,bi,bj)
           IF (J.NE.0) THEN
C     Theta first:
            dTracSpace = (theta(I,J+2,K,bi,bj)-theta(I,J+1,K,bi,bj))
     &        * _maskS(I,J+2,K,bi,bj)
            vPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             vPhase = MAX( cflZon(I,K),
#ifdef ALLOW_ADAMSBASHFORTH_3
     &       MIN( 0.D0, -cflZon(I,K)*gtNm(I,J+1,K,bi,bj,1)/dTracSpace )
#else
     &       MIN( 0.D0, -cflZon(I,K)*gtNm1(I,J+1,K,bi,bj)/dTracSpace )
#endif
     &         ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
            OBSt(I,K,bi,bj) = theta(I,J,K,bi,bj) + dTtracerLev(K)
     &        * _maskS(I,J+1,K,bi,bj)
     &        * (
     &        - ( aFac*MIN(0.D0,vVel(I,J+1,K,bi,bj)) + vPhase )
     &        *(theta(I,J+1,K,bi,bj)-theta(I,J,K,bi,bj))
     &        * _recip_dyC(I,J+1,bi,bj)
     &        - gFacZ(I,K) * gammat 
     &           * (theta(I,J,K,bi,bj)-OBSt(I,K,bi,bj)) )
C     Now salinity:
            dTracSpace = (salt(I,J+2,K,bi,bj)-salt(I,J+1,K,bi,bj))
     &        * _maskS(I,J+2,K,bi,bj)
            vPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             vPhase = MAX( cflZon(I,K),
#ifdef ALLOW_ADAMSBASHFORTH_3
     &       MIN( 0.D0, -cflZon(I,K)*gsNm(I,J+1,K,bi,bj,1)/dTracSpace )
#else
     &       MIN( 0.D0, -cflZon(I,K)*gsNm1(I,J+1,K,bi,bj)/dTracSpace )
#endif
     &         ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
            OBSs(I,K,bi,bj) = salt(I,J,K,bi,bj) + dTtracerLev(K)
     &        * _maskS(I,J+1,K,bi,bj)
     &        * (
     &        - ( aFac*MIN(0.D0,vVel(I,J+1,K,bi,bj)) + vPhase )
     &        *(salt(I,J+1,K,bi,bj)-salt(I,J,K,bi,bj))
     &        * _recip_dyC(I,J+1,bi,bj)
     &        - gFacZ(I,K) * gammat 
     &           * (salt(I,J,K,bi,bj)-OBSs(I,K,bi,bj)) )
           ENDIF
          ENDDO
         ENDDO
C     IF ( useStevensSouth ) THEN
        ENDIF
#endif /* ALLOW_OBCS_SOUTH */

C     end bi/bj-loops
       ENDDO
      ENDDO

C ------------------------------------------------------------------------------

#ifdef CHECK_BALANCE
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        uPhase=0.
        vPhase=0.
        uVelLoc = 0.
        DO J=1-Oly,sNy+Oly
         uMerBar(J)=0. _d 0
        ENDDO
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          I=OB_Ie(J,bi,bj)
          uPhase = uPhase + OBEu(J,K,bi,bj)
     &         *drF(k)* _hFacW(I,J,K,bi,bj)*dyG(I,J,bi,bj)
          I=OB_Iw(J,bi,bj)
          vPhase = vPhase + OBWu(J,K,bi,bj)
     &         *drF(k)* _hFacW(I+1,J,K,bi,bj)*dyG(I+1,J,bi,bj)
          uVelLoc = uVelLoc + uMerPri(J,K)
     &         *drF(k)* _hFacW(I+1,J,K,bi,bj)*dyG(I+1,J,bi,bj)
          uMerBar(J)=uMerBar(J) + uMerPri(J,K)
     &         *drF(k)* _hFacW(I+1,J,K,bi,bj)
         ENDDO
        ENDDO
C     end bi/bj-loops
       ENDDO
      ENDDO
      _GLOBAL_SUM_RL( uPhase, myThid )
      _GLOBAL_SUM_RL( vPhase, myThid )
      _GLOBAL_SUM_RL( uVelLoc, myThid )
      print *, 'ml-obcs: OBE  = ',  uPhase*1 _d -6, ' Sv'
      print *, 'ml-obcs: OBW  = ',  vPhase*1 _d -6, ' Sv'
      print *, 'ml-obcs: OBWp = ', uVelLoc*1 _d -6, ' Sv'
      print *, 'ml-obcs: uBar = ', (j,uMerBar(J),J=1,sNy)
CML      DO K=1,5
CML      print '(I2,5E12.4)',K, (umerpri(J,K),J=21,25)
CML      ENDDO
CML      DO K=1,5
CML      print '(I2,5E12.4)',K, (hFacW(2,J,K)*drF(K),J=21,25)
CML      ENDDO
#endif /* CHECK_BALANCE */

#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_LEAVE('OBCS_CALC_STEVENS',myThid)
#endif

#endif /* ALLOW_OBCS_STEVENS */
      RETURN
      END
