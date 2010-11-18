C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_calc_stevens.F,v 1.2 2010/11/18 19:48:33 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"
#undef CHECK_BALANCE

CBOP
C     !ROUTINE: OBCS_CALC_STEVENS

C     !INTERFACE:
      SUBROUTINE OBCS_CALC_STEVENS(
     I     futureTime, futureIter,
     I     myThid )
C     !DESCRIPTION:
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
C     |    the "barolinic" velocity is simply copied from the model
C     |    interior to the boundary
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

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"
#include "DYNVARS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

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
C     gFac         :: switch (0 or 1) that turns on restoring boundary condition
C     pFac         :: switch that turns on/off phase velocity contribution
      INTEGER bi, bj
      INTEGER I, J, K
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL uPhase
c     _RL vPhase
      _RL cfl, dtracSpace
      _RL uMerPri(1-OLy:sNy+OLy,Nr)
c     _RL vZonPri(1-OLx:sNx+OLx,Nr)
      _RL uMerBar(1-OLy:sNy+OLy)
c     _RL vZonBar(1-OLx:sNx+OLx)
      _RL drFBar(1-OLy:sNy+OLy)
      _RL gammat, gammas, gFac, pFac, aFac
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

#ifdef ALLOW_OBCS_EAST
        IF ( useStevensEast ) THEN
C     Eastern OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: East',myThid)
#endif
C     compute vertical average and deviation from vertical
C     average for I_obe-1
C     first initialize some fields
         DO J=1-Oly,sNy+Oly
          uMerBar(J) = 0. _d 0
          drFBar(J)  = 0. _d 0
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           uMerPri(J,K) = 0. _d 0
          ENDDO
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Ie(J,bi,bj)
           IF (I.NE.0) THEN
            uMerBar(J) = uMerBar(J) + uVel(I-1,J,K,bi,bj)
     &           *drF(K)* _hFacW(I,J,K,bi,bj)
            drFBar(J) = drFBar(J) + drF(K)* _hFacW(I,J,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
         DO J=1-Oly,sNy+Oly
          IF ( drFBar(J) .GT. 0. _d 0 )
     &         uMerBar(J) = uMerBar(J)/drFBar(J)
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Ie(J,bi,bj)
           IF (I.NE.0) uMerPri(J,K) = (uVel(I-1,J,K,bi,bj)-uMerBar(J))
     &          * _maskW(I,J,K,bi,bj)
          ENDDO
         ENDDO
C     vertical average of input field
         DO J=1-Oly,sNy+Oly
          uMerBar(J) = 0. _d 0
          drFBar(J)  = 0. _d 0
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Ie(J,bi,bj)
           IF (I.NE.0) THEN
            uMerBar(J) = uMerBar(J) + OBEu(J,K,bi,bj)
     &           *drF(K)* _hFacW(I,J,K,bi,bj)
            drFBar(J) = drFBar(J) + drF(K)* _hFacW(I,J,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
         DO J=1-Oly,sNy+Oly
          IF ( drFBar(J) .GT. 0. _d 0 )
     &         uMerBar(J) = uMerBar(J)/drFBar(J)
         ENDDO
C     Now the absolute velocity normal to the boundary is
C     uMerBar(J) + uMerPri(J,K).
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Ie(J,bi,bj)
           IF (I.NE.0) THEN
            OBEu(J,K,bi,bj) = (uMerBar(J) + uMerPri(J,K))
     &           * _maskW(I,J,K,bi,bj)
CML         OBEv(J,K,bi,bj) = 0. _d 0
#ifdef ALLOW_NONHYDROSTATIC
            OBEw(J,K,bi,bj)=0.
#endif
           ENDIF
          ENDDO
         ENDDO
C     Next, we compute the phase speed correction, which depends on the
C     tracer! Theta first:
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Ie(J,bi,bj)
           IF (I.NE.0) THEN
            dTracSpace = (theta(I-1,J,K,bi,bj)-theta(I-2,J,K,bi,bj))
     &           * _maskW(I-1,J,K,bi,bj)
            uPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             cfl = 0.5 _d 0 * _dxC(I-1,J,bi,bj)/deltaTmom
             uPhase = MIN( cfl,
#ifdef ALLOW_ADAMSBASHFORTH_3
     &            MAX( 0.D0, -cfl*gtNm(I-1,J,K,bi,bj,1)/dTracSpace )
#else
     &            MAX( 0.D0, -cfl*gtNm1(I-1,J,K,bi,bj)/dTracSpace )
#endif
     &            ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
CML         gFac = 0. _d 0
CML         IF ( uVel(I,J,K,bi,bj) .LT. 0. _d 0 ) gFac = 1. _d 0
            gFac = ABS(MIN(SIGN(1.D0,uVel(I,J,K,bi,bj)),0.D0))
            OBEt(J,K,bi,bj) = theta(I,J,K,bi,bj) + dTtracerLev(K)
     &          * _maskW(I,J,K,bi,bj)
     &          * (
     &          - ( aFac*MAX(0.D0,uVel(I,J,K,bi,bj)) + uPhase )
     &          *(theta(I,J,K,bi,bj)-theta(I-1,J,K,bi,bj))
     &          * _recip_dxC(I,J,bi,bj)
     &          - gFac * gammat * (theta(I,J,K,bi,bj)-OBEt(J,K,bi,bj)) )
           ENDIF
          ENDDO
         ENDDO
C     Now salinity:
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Ie(J,bi,bj)
           IF (I.NE.0) THEN
            dTracSpace = (salt(I-1,J,K,bi,bj)-salt(I-2,J,K,bi,bj))
     &          * _maskW(I-1,J,K,bi,bj)
            uPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             cfl = 0.5 _d 0 * _dxC(I-1,J,bi,bj)/deltaTmom
             uPhase = MIN( cfl,
#ifdef ALLOW_ADAMSBASHFORTH_3
     &           MAX( 0.D0, -cfl*gsNm(I-1,J,K,bi,bj,1)/dTracSpace )
#else
     &           MAX( 0.D0, -cfl*gsNm1(I-1,J,K,bi,bj)/dTracSpace )
#endif
     &           ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
CML         gFac = 0. _d 0
CML         IF ( uVel(I,J,K,bi,bj) .LT. 0. _d 0 ) gFac = 1. _d 0
            gFac = ABS(MIN(SIGN(1.D0,uVel(I,J,K,bi,bj)),0.D0))
            OBEs(J,K,bi,bj) = salt(I,J,K,bi,bj) + dTtracerLev(K)
     &          * _maskW(I,J,K,bi,bj)
     &          * (
     &          - ( aFac*MAX(0.D0,uVel(I,J,K,bi,bj)) + uPhase )
     &          *(salt(I,J,K,bi,bj)-salt(I-1,J,K,bi,bj))
     &          * _recip_dxC(I,J,bi,bj)
     &          - gFac * gammas * (salt(I,J,K,bi,bj)-OBEs(J,K,bi,bj)) )
           ENDIF
          ENDDO
         ENDDO
C     IF ( useStevensEast ) THEN
        ENDIF
#endif /* ALLOW_OBCS_EAST */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_WEST
        IF ( useStevensWest ) THEN
C     Western OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: West',myThid)
#endif
C     compute vertical average and deviation from vertical
C     average for I_obw+2
C     first initialize some fields
         DO J=1-Oly,sNy+Oly
          uMerBar(J) = 0. _d 0
          drFBar(J)  = 0. _d 0
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           uMerPri(J,K) = 0. _d 0
          ENDDO
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Iw(J,bi,bj)
           IF (I.NE.0) THEN
            uMerBar(J) = uMerBar(J) + uVel(I+2,J,K,bi,bj)
     &        *drF(K)* _hFacW(I+1,J,K,bi,bj)
            drFBar(J) = drFBar(J) + drF(K)* _hFacW(I+1,J,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
         DO J=1-Oly,sNy+Oly
          IF ( drFBar(J) .GT. 0. _d 0 )
     &         uMerBar(J) = uMerBar(J)/drFBar(J)
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Iw(J,bi,bj)
           IF (I.NE.0) uMerPri(J,K) = (uVel(I+2,J,K,bi,bj)-uMerBar(J))
     &       * _maskW(I+1,J,K,bi,bj)
          ENDDO
         ENDDO
C     vertical average of input field
         DO J=1-Oly,sNy+Oly
          uMerBar(J) = 0. _d 0
          drFBar(J)  = 0. _d 0
         ENDDO
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Iw(J,bi,bj)
           IF (I.NE.0) THEN
            uMerBar(J) = uMerBar(J) + OBWu(J,K,bi,bj)
     &           *drF(K)* _hFacW(I+1,J,K,bi,bj)
            drFBar(J) = drFBar(J) + drF(K)* _hFacW(I+1,J,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
         DO J=1-Oly,sNy+Oly
          IF ( drFBar(J) .GT. 0. _d 0 )
     &         uMerBar(J) = uMerBar(J)/drFBar(J)
         ENDDO
C     Now the absolute velocity normal to the boundary is
C     uMerBar(J) + uMerPri(J,K).
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Iw(J,bi,bj)
           IF (I.NE.0) THEN
            OBWu(J,K,bi,bj) = (uMerBar(J) + uMerPri(J,K))
     &          * _maskW(I+1,J,K,bi,bj)
CML         OBWv(J,K,bi,bj) = 0. _d 0
#ifdef ALLOW_NONHYDROSTATIC
            OBWw(J,K,bi,bj)=0.
#endif
           ENDIF
          ENDDO
         ENDDO
C     Next, we compute the phase speed correction, which depends on the
C     tracer! Theta first:
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Iw(J,bi,bj)
           IF (I.NE.0) THEN
            dTracSpace = (theta(I+2,J,K,bi,bj)-theta(I+1,J,K,bi,bj))
     &        * _maskW(I+2,J,K,bi,bj)
            uPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             cfl = - 0.5 _d 0 * _dxC(I+2,J,bi,bj)/deltaTmom
             uPhase = MAX( cfl,
#ifdef ALLOW_ADAMSBASHFORTH_3
     &         MIN( 0.D0, -cfl*gtNm(I+1,J,K,bi,bj,1)/dTracSpace )
#else
     &         MIN( 0.D0, -cfl*gtNm1(I+1,J,K,bi,bj)/dTracSpace )
#endif
     &         ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
CML         gFac = 0. _d 0
CML         IF ( uVel(I,J,K,bi,bj) .GT. 0. _d 0 ) gFac = 1. _d 0
            gFac = ABS(MAX(SIGN(1.D0,uVel(I+1,J,K,bi,bj)),0.D0))
            OBWt(J,K,bi,bj) = theta(I,J,K,bi,bj) + dTtracerLev(K)
     &        * _maskW(I+1,J,K,bi,bj)
     &        * (
     &        - ( aFac*MIN(0.D0,uVel(I+1,J,K,bi,bj)) + uPhase )
     &        *(theta(I+1,J,K,bi,bj)-theta(I,J,K,bi,bj))
     &        * _recip_dxC(I+1,J,bi,bj)
     &        - gFac * gammat * (theta(I,J,K,bi,bj)-OBWt(J,K,bi,bj)) )
           ENDIF
          ENDDO
         ENDDO
C     Now salinity:
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I=OB_Iw(J,bi,bj)
           IF (I.NE.0) THEN
            dTracSpace = (salt(I+2,J,K,bi,bj)-salt(I+1,J,K,bi,bj))
     &        * _maskW(I+2,J,K,bi,bj)
            uPhase = 0. _d 0
            IF ( dTracSpace .NE. 0. _d 0 ) THEN
             cfl = - 0.5 _d 0 * _dxC(I+2,J,bi,bj)/deltaTmom
             uPhase = MAX( cfl,
#ifdef ALLOW_ADAMSBASHFORTH_3
     &         MIN( 0.D0, -cfl*gsNm(I+1,J,K,bi,bj,1)/dTracSpace )
#else
     &         MIN( 0.D0, -cfl*gsNm1(I+1,J,K,bi,bj)/dTracSpace )
#endif
     &         ) * pFac
            ENDIF
C     Update the tracer here with a simple Euler forward step; not
C     sure if this will be stable
CML         gFac = 0. _d 0
CML         IF ( uVel(I,J,K,bi,bj) .GT. 0. _d 0 ) gFac = 1. _d 0
            gFac = ABS(MAX(SIGN(1.D0,uVel(I+1,J,K,bi,bj)),0.D0))
            OBWs(J,K,bi,bj) = salt(I,J,K,bi,bj) + dTtracerLev(K)
     &        * _maskW(I+1,J,K,bi,bj)
     &        * (
     &        - ( aFac*MIN(0.D0,uVel(I+1,J,K,bi,bj)) + uPhase )
     &        *(salt(I+1,J,K,bi,bj)-salt(I,J,K,bi,bj))
     &        * _recip_dxC(I+1,J,bi,bj)
     &        - gFac * gammas * (salt(I,J,K,bi,bj)-OBWs(J,K,bi,bj)) )
           ENDIF
          ENDDO
         ENDDO
C      IF ( useStevensWest ) THEN
        ENDIF
#endif /* ALLOW_OBCS_WEST */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_NORTH
        IF ( useStevensNorth ) THEN
C         Northern OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: North',myThid)
#endif
         STOP 'OBCS_NORTH Stevens not yet implemented'
CML      DO K=1,Nr
CML       DO I=1-Olx,sNx+Olx
CML        vZonPri(I,K) = 0. _d 0
CML       ENDDO
CML      ENDDO
         DO K=1,Nr
          DO I=1-Olx,sNx+Olx
           J=OB_Jn(I,bi,bj)
           IF (J.NE.0) THEN
            OBNv(I,K,bi,bj)=0.
            OBNu(I,K,bi,bj)=0.
            OBNt(I,K,bi,bj)=tRef(K)
            OBNs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBNw(I,K,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
            OBNeta(I,bi,bj)=0.
#endif
           ENDIF
          ENDDO
         ENDDO
C      IF ( useStevensNorth ) THEN
        ENDIF
#endif /* ALLOW_OBCS_NORTH */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_SOUTH
        IF ( useStevensSouth ) THEN
C         Southern OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: South',myThid)
#endif
         STOP 'OBCS_SOUTH Stevens not yet implemented'
CML      DO K=1,Nr
CML       DO I=1-Olx,sNx+Olx
CML        vZonPri(I,K) = 0. _d 0
CML       ENDDO
CML      ENDDO
         DO K=1,Nr
          DO I=1-Olx,sNx+Olx
           J=OB_Js(I,bi,bj)
           IF (J.NE.0) THEN
            OBSu(I,K,bi,bj)=0.
            OBSv(I,K,bi,bj)=0.
            OBSt(I,K,bi,bj)=tRef(K)
            OBSs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBSw(I,K,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
            OBSeta(I,bi,bj)=0.
#endif
           ENDIF
          ENDDO
         ENDDO
C      IF ( useStevensSouth ) THEN
        ENDIF
#endif /* ALLOW_OBCS_SOUTH */

CML#ifdef ALLOW_PTRACERS
CML      IF ( usePTRACERS ) THEN
CMLC
CMLC     Calculate some default open boundary conditions for passive tracers:
CMLC     The default is a homogeneous v.Neumann conditions, that is, the
CMLC     tracer gradient across the open boundary is nearly zero;
CMLC     only nearly, because the boundary conditions are applied throughout
CMLC     the time step during which the interior field does change; therefore
CMLC     we have to use the values from the previous time step here. If you
CMLC     really want exact v.Neumann conditions, you have to modify
CMLC     obcs_apply_ptracer directly.
CMLC
CML# ifdef ALLOW_OBCS_EAST
CMLC     Eastern OB
CML#  ifdef ALLOW_DEBUG
CML       IF (debugMode)
CML     &      CALL DEBUG_MSG('OBCS_CALC_STEVENS: East, pTracers',myThid)
CML#  endif
CML       DO iTracer=1,PTRACERS_numInUse
CML        DO K=1,Nr
CML         DO J=1-Oly,sNy+Oly
CML          I=OB_Ie(J,bi,bj)
CML          IF (I.NE.0) THEN
CML           OBEptr(J,K,bi,bj,iTracer) =
CML     &          pTracer(I-1,J,K,bi,bj,iTracer)
CML     &          *_maskW(I,J,K,bi,bj)
CML          ENDIF
CML         ENDDO
CML        ENDDO
CML       ENDDO
CML# endif /* ALLOW_OBCS_EAST */
CML
CMLC ------------------------------------------------------------------------------
CML
CML# ifdef ALLOW_OBCS_WEST
CMLC     Western OB
CML#  ifdef ALLOW_DEBUG
CML       IF (debugMode)
CML     &      CALL DEBUG_MSG('OBCS_CALC_STEVENS: West, pTracers',myThid)
CML#  endif
CML       DO iTracer=1,PTRACERS_numInUse
CML        DO K=1,Nr
CML         DO J=1-Oly,sNy+Oly
CML          I=OB_Iw(J,bi,bj)
CML          IF (I.NE.0) THEN
CML           OBWptr(J,K,bi,bj,iTracer) =
CML     &          pTracer(I+1,J,K,bi,bj,iTracer)
CML     &          *_maskW(I+1,J,K,bi,bj)
CML          ENDIF
CML         ENDDO
CML        ENDDO
CML       ENDDO
CML# endif /* ALLOW_OBCS_WEST */
CML
CMLC ------------------------------------------------------------------------------
CML
CML# ifdef ALLOW_OBCS_NORTH
CMLC         Northern OB
CML#  ifdef ALLOW_DEBUG
CML       IF (debugMode)
CML     &     CALL DEBUG_MSG('OBCS_CALC_STEVENS: North, pTracers',myThid)
CML#  endif
CML       DO iTracer=1,PTRACERS_numInUse
CML        DO K=1,Nr
CML         DO I=1-Olx,sNx+Olx
CML          J=OB_Jn(I,bi,bj)
CML          IF (J.NE.0) THEN
CML           OBNptr(I,K,bi,bj,iTracer) =
CML     &          pTracer(I,J-1,K,bi,bj,iTracer)
CML     &          *_maskS(I,J,K,bi,bj)
CML          ENDIF
CML         ENDDO
CML        ENDDO
CML       ENDDO
CML# endif /* ALLOW_OBCS_NORTH */
CML
CMLC ------------------------------------------------------------------------------
CML
CML# ifdef ALLOW_OBCS_SOUTH
CMLC         Southern OB
CML# ifdef ALLOW_DEBUG
CML       IF (debugMode)
CML     &      CALL DEBUG_MSG('OBCS_CALC_STEVENS: South, pTracers',myThid)
CML#endif
CML       DO iTracer=1,PTRACERS_numInUse
CML        DO K=1,Nr
CML         DO I=1-Olx,sNx+Olx
CML          J=OB_Js(I,bi,bj)
CML          IF (J.NE.0) THEN
CML           OBSptr(I,K,bi,bj,iTracer) =
CML     &          pTracer(I,J+1,K,bi,bj,iTracer)
CML     &          *_maskS(I,J+1,K,bi,bj)
CML          ENDIF
CML         ENDDO
CML        ENDDO
CML       ENDDO
CML# endif /* ALLOW_OBCS_SOUTH */
CMLC     end if (usePTracers)
CML      ENDIF
CML#endif /* ALLOW_PTRACERS */

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
