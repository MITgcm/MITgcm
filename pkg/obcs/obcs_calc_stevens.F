C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_calc_stevens.F,v 1.14 2015/01/12 16:06:38 mlosch Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"
#undef CHECK_BALANCE

C--   File obcs_calc_stevens.F: 
C--    Contents
C--    o OBCS_CALC_STEVENS
C--    o OBCS_STEVENS_CALC_TRACER_EAST
C--    o OBCS_STEVENS_CALC_TRACER_WEST
C--    o OBCS_STEVENS_CALC_TRACER_NORTH
C--    o OBCS_STEVENS_CALC_TRACER_SOUTH
C--    o OBCS_STEVENS_SAVE_TRACER

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
C     |    tracer individually, this similar to Orlanski BCs,
C     |    but for simplicity the fields of the previous time step
C     |    are used, and the time derivative is estimated
C     |    independently of the time stepping procedure by simple
C     |    differencing
C     |  - velocity tangential to the boundary is always zero
C     |    (although this could be changed)
C     |  - a new tracer is computed from a local advection equation
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
C     auxillary variables
C     cflMer/Zon   :: ratio of grid spacing and time step
C     aFac         :: switch (0 or 1) that turns on advective contribution
C     gFacM/Z      :: switch (0 or 1) that turns on restoring boundary condition
C     pFac         :: switch that turns on/off phase velocity contribution
      INTEGER bi, bj
      INTEGER I, J, K
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL cflMer (1-OLy:sNy+OLy,Nr)
      _RL gFacM  (1-OLy:sNy+OLy,Nr)
      _RL uMerPri(Nr)
      _RL uMerBar
      _RL cflZon (1-OLx:sNx+OLx,Nr)
      _RL gFacZ  (1-OLx:sNx+OLx,Nr)
      _RL vZonPri(Nr)
      _RL vZonBar
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
         DO J=1-OLy,sNy+OLy
          I = OB_Ie(J,bi,bj)
          IF ( I.NE.OB_indexNone ) THEN
C     first initialize some fields
           drFbar  = 0. _d 0
           uMerBar = 0. _d 0
           DO K=1,Nr
            uMerPri(K) = 0. _d 0
           ENDDO
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerBar = uMerBar + uVel(I-1,J,K,bi,bj)
#else
            uMerBar = uMerBar + OBEuStevens(J,K,bi,bj)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(K)* _hFacW(I,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacW(I,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar = uMerBar/drFBar
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerPri(K) = (uVel(I-1,J,K,bi,bj)-uMerBar)
#else
            uMerPri(K) = (OBEuStevens(J,K,bi,bj)-uMerBar)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskW(I,J,K,bi,bj)
           ENDDO
C     vertical average of input field
           drFbar  = 0. _d 0
           uMerBar = 0. _d 0
           DO K=1,Nr
            uMerBar = uMerBar + OBEu(J,K,bi,bj)
     &           *drF(K)* _hFacW(I,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacW(I,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar = uMerBar/drFBar
C     Now the absolute velocity normal to the boundary is
C     uMerBar + uMerPri(K).
           DO K=1,Nr
            OBEu(J,K,bi,bj) = (uMerBar + uMerPri(K))
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
          DO J=1-OLy,sNy+OLy
           I = OB_Ie(J,bi,bj)
           IF ( I.NE.OB_indexNone ) THEN
            OBEeta(J,bi,bj) = etaN(I-1,J,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer!
         DO K=1,Nr
          DO J=1-OLy,sNy+OLy
           I = OB_Ie(J,bi,bj)
           IF ( I.NE.OB_indexNone ) THEN
            cflMer(J,K) = 0.5 _d 0 * _dxC(I-1,J,bi,bj)/dTtracerLev(K)
CML         gFacM(J,K)  = 0. _d 0
CML         IF ( uVel(I,J,K,bi,bj) .LT. 0. _d 0 ) gFacM(J,K) = 1. _d 0
            gFacM(J,K)  = ABS(MIN(SIGN(1.D0,uVel(I,J,K,bi,bj)),0.D0))
           ELSE
            cflMer(J,K) = 0. _d 0
            gFacM (J,K) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
C     theta
         CALL OBCS_STEVENS_CALC_TRACER_EAST(
     U        OBEt, 
     I        OBEtStevens, theta, gammat, 
     I        uVel, cflMer, gFacM, pFac, aFac,
     I        OB_Ie, OB_indexNone, bi, bj,
     I        futureTime, futureIter,
     I        myThid )
C     salinity
         CALL OBCS_STEVENS_CALC_TRACER_EAST(
     U        OBEs, 
     I        OBEsStevens, salt, gammas,
     I        uVel, cflMer, gFacM, pFac, aFac,
     I        OB_Ie, OB_indexNone, bi, bj,
     I        futureTime, futureIter,
     I        myThid )
C     Template for passive tracers, requires work
CML#ifdef ALLOW_PTRACERS
CMLC     ptracers
CML         IF ( usePtracers ) THEN
CML          DO itracer = 1, PTRACERnumInUse
CML           CALL OBCS_STEVENS_CALC_TRACER_EAST(
CML     O          OBEptr       (1-OLy,1,1,1,iTracer), 
CML     I          OBEpStevens  (1-OLy,1,1,1,iTracer), 
CML     I          pTracer(1-OLx,1-OLy,1,1,1,iTracer), gammas,
CML     I          uVel, cflMer, gFacM, pFac, aFac,
CML     I          OB_Ie, OB_indexNone, bi, bj,
CML     I          futureTime, futureIter,
CML     I          myThid )
CML          ENDDO
CML         ENDIF
CML#endif /* ALLOW_PTRACERS */
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
         DO J=1-OLy,sNy+OLy
          I = OB_Iw(J,bi,bj)
          IF ( I.NE.OB_indexNone ) THEN
C     first initialize some fields
           drFBar  = 0. _d 0
           uMerBar = 0. _d 0
           DO K=1,Nr
            uMerPri(K) = 0. _d 0
           ENDDO
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerBar = uMerBar + uVel(I+2,J,K,bi,bj)
#else
            uMerBar = uMerBar + OBWuStevens(J,K,bi,bj)
#endif /* OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(K)* _hFacW(I+1,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacW(I+1,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar = uMerBar/drFBar
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerPri(K) = (uVel(I+2,J,K,bi,bj)-uMerBar)
#else
            uMerPri(K) = (OBWuStevens(J,K,bi,bj)-uMerBar)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskW(I+1,J,K,bi,bj)
           ENDDO
C     vertical average of input field
           drFBar  = 0. _d 0
           uMerBar = 0. _d 0
           DO K=1,Nr
            uMerBar = uMerBar + OBWu(J,K,bi,bj)
     &           *drF(K)* _hFacW(I+1,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacW(I+1,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar = uMerBar/drFBar
C     Now the absolute velocity normal to the boundary is
C     uMerBar + uMerPri(K).
           DO K=1,Nr
            OBWu(J,K,bi,bj) = (uMerBar + uMerPri(K))
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
          DO J=1-OLy,sNy+OLy
           I = OB_Iw(J,bi,bj)
           IF ( I.NE.OB_indexNone ) THEN
            OBWeta(J,bi,bj) = etaN(I+1,J,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer!
         DO K=1,Nr
          DO J=1-OLy,sNy+OLy
           I = OB_Iw(J,bi,bj)
           IF ( I.NE.OB_indexNone ) THEN
            cflMer(J,K) = 0.5 _d 0 * _dxC(I+2,J,bi,bj)/dTtracerLev(K)
CML         gFacM = 0. _d 0
CML         IF ( uVel(I+1,J,K,bi,bj) .GT. 0. _d 0 ) gFacM = 1. _d 0
            gFacM(J,K)  = ABS(MAX(SIGN(1.D0,uVel(I+1,J,K,bi,bj)),0.D0))
           ELSE
            cflMer(J,K) = 0. _d 0
            gFacM (J,K) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
C     theta
         CALL OBCS_STEVENS_CALC_TRACER_WEST(
     U        OBWt, 
     I        OBWtStevens, theta, gammat, 
     I        uVel, cflMer, gFacM, pFac, aFac,
     I        OB_Iw, OB_indexNone, bi, bj,
     I        futureTime, futureIter,
     I        myThid )
C     salinity
         CALL OBCS_STEVENS_CALC_TRACER_WEST(
     U        OBWs, 
     I        OBWsStevens, salt, gammas,
     I        uVel, cflMer, gFacM, pFac, aFac,
     I        OB_Iw, OB_indexNone, bi, bj,
     I        futureTime, futureIter,
     I        myThid )
C     ptracers
C     IF ( useStevensWest ) THEN
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
         DO I=1-OLx,sNx+OLx
          J = OB_Jn(I,bi,bj)
          IF ( J.NE.OB_indexNone ) THEN
C     first initialize some fields
           drFBar  = 0. _d 0
           vZonBar = 0. _d 0
           DO K=1,Nr
            vZonPri(K) = 0. _d 0
           ENDDO
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonBar = vZonBar + vVel(I,J-1,K,bi,bj)
#else
            vZonBar = vZonBar + OBNvStevens(I,K,bi,bj)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(K)* _hFacS(I,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacS(I,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar = vZonBar/drFBar
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonPri(K) = (vVel(I,J-1,K,bi,bj)-vZonBar)
#else
            vZonPri(K) = (OBNvStevens(I,K,bi,bj)-vZonBar)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskS(I,J,K,bi,bj)
           ENDDO
C     vertical average of input field
           drFBar  = 0. _d 0
           vZonBar = 0. _d 0
           DO K=1,Nr
            vZonBar = vZonBar + OBNv(I,K,bi,bj)
     &           *drF(K)* _hFacS(I,J,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacS(I,J,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar = vZonBar/drFBar
C     Now the absolute velocity normal to the boundary is
C     vZonBar + vZonPri(K).
           DO K=1,Nr
            OBNv(I,K,bi,bj) = (vZonBar + vZonPri(K))
     &           * _maskS(I,J,K,bi,bj)
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
          DO I=1-OLx,sNx+OLx
           J = OB_Jn(I,bi,bj)
           IF ( J.NE.OB_indexNone ) THEN
            OBNeta(I,bi,bj) = etaN(I,J-1,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer!
         DO K=1,Nr
          DO I=1-OLx,sNx+OLx
           J = OB_Jn(I,bi,bj)
           IF ( J.NE.OB_indexNone ) THEN
            cflZon(I,K) = 0.5 _d 0 * _dyC(I,J-1,bi,bj)/dTtracerLev(K)
CML         gFacZ(I,K) = 0. _d 0
CML         IF ( vVel(I,J,K,bi,bj) .LT. 0. _d 0 ) gFacZ(I,K) = 1. _d 0
            gFacZ(I,K)  = ABS(MIN(SIGN(1.D0,vVel(I,J,K,bi,bj)),0.D0))
           ELSE
            cflZon(I,K) = 0. _d 0
            gFacZ (I,K) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
C     theta
         CALL OBCS_STEVENS_CALC_TRACER_NORTH(
     U        OBNt, 
     I        OBNtStevens, theta, gammat,
     I        vVel, cflZon, gFacZ, pFac, aFac, 
     I        OB_Jn, OB_indexNone, bi, bj,
     I        futureTime, futureIter,
     I        myThid )
C     salinity
         CALL OBCS_STEVENS_CALC_TRACER_NORTH(
     U        OBNs, 
     I        OBNsStevens, salt, gammas,
     I        vVel, cflZon, gFacZ, pFac, aFac, 
     I        OB_Jn, OB_indexNone, bi, bj,
     I        futureTime, futureIter,
     I        myThid )
C     ptracers
C     IF ( useStevensNorth ) THEN
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
         DO I=1-OLx,sNx+OLx
          J = OB_Js(I,bi,bj)
          IF ( J.NE.OB_indexNone ) THEN
C     first initialize some fields
           drFBar  = 0. _d 0
           vZonBar = 0. _d 0
           DO K=1,Nr
            vZonPri(K) = 0. _d 0
           ENDDO
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonBar = vZonBar + vVel(I,J+2,K,bi,bj)
#else
            vZonBar = vZonBar + OBSvStevens(I,K,bi,bj)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(K)* _hFacS(I,J+1,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacS(I,J+1,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar = vZonBar/drFBar
           DO K=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonPri(K) = (vVel(I,J+2,K,bi,bj)-vZonBar)
#else
            vZonPri(K) = (OBSvStevens(I,K,bi,bj)-vZonBar)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskS(I,J+1,K,bi,bj)
           ENDDO
C     vertical average of input field
           drFBar  = 0. _d 0
           vZonBar = 0. _d 0
           DO K=1,Nr
            vZonBar = vZonBar + OBSv(I,K,bi,bj)
     &           *drF(K)* _hFacS(I,J+1,K,bi,bj)
            drFBar = drFBar + drF(K)* _hFacS(I,J+1,K,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar = vZonBar/drFBar
C     Now the absolute velocity normal to the boundary is
C     vZonBar + vZonPri(K).
           DO K=1,Nr
            OBSv(I,K,bi,bj) = (vZonBar + vZonPri(K))
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
          DO I=1-OLx,sNx+OLx
           J = OB_Js(I,bi,bj)
           IF ( J.NE.OB_indexNone ) THEN
            OBSeta(I,bi,bj) = etaN(I,J+1,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer!
         DO K=1,Nr
          DO I=1-OLx,sNx+OLx
           J = OB_Js(I,bi,bj)
           IF ( J.NE.OB_indexNone ) THEN
            cflZon(I,K) = 0.5 _d 0 * _dyC(I,J+2,bi,bj)/dTtracerLev(K)
CML         gFacZ = 0. _d 0
CML         IF ( vVel(I,J+1,K,bi,bj) .GT. 0. _d 0 ) gFacZ = 1. _d 0
            gFacZ(I,K)  = ABS(MAX(SIGN(1.D0,vVel(I,J+1,K,bi,bj)),0.D0))
           ELSE
            cflZon(I,K) = 0. _d 0
            gFacZ (I,K) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
C     theta
         CALL OBCS_STEVENS_CALC_TRACER_SOUTH(
     U        OBSt, 
     I        OBStStevens, theta, gammat,
     I        vVel, cflZon, gFacZ, pFac, aFac, 
     I        OB_Js, OB_indexNone, bi, bj,
     I        futureTime, futureIter,
     I        myThid )
C     salinity
         CALL OBCS_STEVENS_CALC_TRACER_SOUTH(
     U        OBSs, 
     I        OBSsStevens, salt, gammas,
     I        vVel, cflZon, gFacZ, pFac, aFac, 
     I        OB_Js, OB_indexNone, bi, bj,
     I        futureTime, futureIter,
     I        myThid )
C     ptracers
C     IF ( useStevensSouth ) THEN
        ENDIF
#endif /* ALLOW_OBCS_SOUTH */

C     end bi/bj-loops
       ENDDO
      ENDDO

C     save the tracer fields of the previous time step for the next time step
      CALL OBCS_STEVENS_SAVE_TRACERS(
     I     futureTime, futureIter,
     I     myThid )
C ------------------------------------------------------------------------------

#ifdef CHECK_BALANCE
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        uPhase=0.
        vPhase=0.
        uVelLoc = 0.
        DO J=1-OLy,sNy+OLy
         uMerBar=0. _d 0
         DO K=1,Nr
          I = OB_Ie(J,bi,bj)
          IF ( I.EQ.OB_indexNone ) I = 1
          uPhase = uPhase + OBEu(J,K,bi,bj)
     &         *drF(k)* _hFacW(I,J,K,bi,bj)*dyG(I,J,bi,bj)
          I = OB_Iw(J,bi,bj)
          IF ( I.EQ.OB_indexNone ) I = 1
          vPhase = vPhase + OBWu(J,K,bi,bj)
     &         *drF(k)* _hFacW(I+1,J,K,bi,bj)*dyG(I+1,J,bi,bj)
CML          uVelLoc = uVelLoc + uMerPri(J,K)
CML     &         *drF(k)* _hFacW(I+1,J,K,bi,bj)*dyG(I+1,J,bi,bj)
CML          uMerBar(J)=uMerBar(J) + uMerPri(J,K)
CML     &         *drF(k)* _hFacW(I+1,J,K,bi,bj)
         ENDDO
CML         print *, 'ml-obcs: uBar = ', j,uMerBar(J)
        ENDDO
C     end bi/bj-loops
       ENDDO
      ENDDO
      _GLOBAL_SUM_RL( uPhase, myThid )
      _GLOBAL_SUM_RL( vPhase, myThid )
CML      _GLOBAL_SUM_RL( uVelLoc, myThid )
      print *, 'ml-obcs: OBE  = ',  uPhase*1 _d -6, ' Sv'
      print *, 'ml-obcs: OBW  = ',  vPhase*1 _d -6, ' Sv'
CML      print *, 'ml-obcs: OBWp = ', uVelLoc*1 _d -6, ' Sv'
#endif /* CHECK_BALANCE */

#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_LEAVE('OBCS_CALC_STEVENS',myThid)
#endif

#endif /* ALLOW_OBCS_STEVENS */
      RETURN
      END

CBOP
C     !ROUTINE: OBCS_STEVENS_CALC_TRACER_EAST
C     !INTERFACE:
      SUBROUTINE OBCS_STEVENS_CALC_TRACER_EAST(
     U     OBEf, 
     I     OBE_Stevens, tracer, gammaf,
     I     uVel, cflMer, gFacM, pFac, aFac, 
     I     OB_I, OB_indexNone, bi, bj,
     I     futureTime, futureIter,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_STEVENS_CALC_TRACER_EAST
C     | Calculate tracer value at the eastern OB location
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C    myThid    :: my Thread Id number
C    bi, bj    :: indices of current tile
      _RL futureTime
      INTEGER futureIter
      INTEGER myThid
      INTEGER bi, bj
      INTEGER OB_indexNone
      INTEGER OB_I             (1-OLy:sNy+OLy,nSx,nSy)
      _RL cflMer               (1-OLy:sNy+OLy,Nr)
      _RL gFacM                (1-OLy:sNy+OLy,Nr)
      _RL gammaf, pFac, aFac
      _RL OBEf                 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBE_Stevens          (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL tracer (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL uVel   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)

#ifdef ALLOW_OBCS_STEVENS
C     !LOCAL VARIABLES:
C     i,j,k        :: loop indices
C     uPhase       :: estimate of phase velocity for radiation condition
C     dtracSpace   :: horizontal difference of tracer
C     dtracTime    :: temporal difference of tracer
      INTEGER i,j,k
      _RL uPhase
      _RL dtracSpace
      _RL dTracTime
CEOP
      DO K=1,Nr
       DO J=1-OLy,sNy+OLy
        I = OB_I(J,bi,bj)
        IF ( I.NE.OB_indexNone ) THEN
         dTracSpace = (tracer(I-1,J,K,bi,bj)-tracer(I-2,J,K,bi,bj))
     &        * _maskW(I-1,J,K,bi,bj)
         dTracTime  = (tracer(I-1,J,K,bi,bj)-OBE_Stevens(J,K,bi,bj))
         uPhase = cflMer(J,K) * pFac
         IF ( dTracSpace .NE. 0. _d 0 ) THEN
          uPhase = MIN( cflMer(J,K),
     &         MAX( 0.D0, -cflMer(J,K)*dTracTime/dTracSpace )
     &         ) * pFac
         ENDIF
C     Compute the tracer tendency here, the tracer will be updated
C     with a simple Euler forward step in S/R obcs_apply_ts
         OBEf(J,K,bi,bj) = _maskW(I,J,K,bi,bj) * (
     &        - ( aFac*MAX(0.D0,uVel(I,J,K,bi,bj)) + uPhase )
     &        *(tracer(I,J,K,bi,bj)-tracer(I-1,J,K,bi,bj))
     &        * _recip_dxC(I,J,bi,bj)
     &        - gFacM(J,K) * gammaf
     &        * (tracer(I,J,K,bi,bj)-OBEf(J,K,bi,bj)) )
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_OBCS_STEVENS */
      RETURN
      END

CBOP
C     !ROUTINE: OBCS_STEVENS_CALC_TRACER_WEST
C     !INTERFACE:
      SUBROUTINE OBCS_STEVENS_CALC_TRACER_WEST(
     U     OBWf, 
     I     OBW_Stevens, tracer, gammaf,
     I     uVel, cflMer, gFacM, pFac, aFac, 
     I     OB_I, OB_indexNone, bi, bj,
     I     futureTime, futureIter,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_STEVENS_CALC_TRACER_WEST
C     | Calculate tracer value at the western OB location
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C    myThid    :: my Thread Id number
C    bi, bj    :: indices of current tile
      _RL futureTime
      INTEGER futureIter
      INTEGER myThid
      INTEGER bi, bj
      INTEGER OB_indexNone
      INTEGER OB_I             (1-OLy:sNy+OLy,nSx,nSy)
      _RL cflMer               (1-OLy:sNy+OLy,Nr)
      _RL gFacM                (1-OLy:sNy+OLy,Nr)
      _RL gammaf, pFac, aFac
      _RL OBWf                 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBW_Stevens          (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL tracer (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL uVel   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)

#ifdef ALLOW_OBCS_STEVENS
C     !LOCAL VARIABLES:
C     i,j,k        :: loop indices
C     uPhase       :: estimate of phase velocity for radiation condition
C     dtracSpace   :: horizontal difference of tracer
C     dtracTime    :: temporal difference of tracer
      INTEGER i,j,k
      _RL uPhase
      _RL dtracSpace
      _RL dTracTime
CEOP

      DO K=1,Nr
       DO J=1-OLy,sNy+OLy
        I = OB_I(J,bi,bj)
        IF ( I.NE.OB_indexNone ) THEN
         dTracSpace = (tracer(I+2,J,K,bi,bj)-tracer(I+1,J,K,bi,bj))
     &        * _maskW(I+2,J,K,bi,bj)
         dTracTime  = (tracer(I+1,J,K,bi,bj)-OBW_Stevens(J,K,bi,bj))
         uPhase = -cflMer(J,K) * pFac
         IF ( dTracSpace .NE. 0. _d 0 ) THEN
          uPhase = MAX( -cflMer(J,K),
     &         MIN( 0.D0, -cflMer(J,K)*dTracTime/dTracSpace )
     &         ) * pFac
         ENDIF
C     Compute the tracer tendency here, the tracer will be updated
C     with a simple Euler forward step in S/R obcs_apply_ts
         OBWf(J,K,bi,bj) = _maskW(I+1,J,K,bi,bj) * (
     &        - ( aFac*MIN(0.D0,uVel(I+1,J,K,bi,bj)) + uPhase )
     &        *(tracer(I+1,J,K,bi,bj)-tracer(I,J,K,bi,bj))
     &        * _recip_dxC(I+1,J,bi,bj)
     &        - gFacM(J,K) * gammaf
     &        * (tracer(I,J,K,bi,bj)-OBWf(J,K,bi,bj)) )
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_OBCS_STEVENS */
      RETURN
      END

CBOP
C     !ROUTINE: OBCS_STEVENS_CALC_TRACER_NORTH
C     !INTERFACE:
      SUBROUTINE OBCS_STEVENS_CALC_TRACER_NORTH(
     U     OBNf, 
     I     OBN_Stevens, tracer, gammaf,
     I     vVel, cflZon, gFacZ, pFac, aFac, 
     I     OB_J, OB_indexNone, bi, bj,
     I     futureTime, futureIter,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_STEVENS_CALC_TRACER_NORTH
C     | Calculate tracer value at the northern OB location
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C    myThid    :: my Thread Id number
C    bi, bj    :: indices of current tile
      _RL futureTime
      INTEGER futureIter
      INTEGER myThid
      INTEGER bi, bj
      INTEGER OB_indexNone
      INTEGER OB_J             (1-OLx:sNx+OLx,nSx,nSy)
      _RL cflZon               (1-OLx:sNx+OLx,Nr)
      _RL gFacZ                (1-OLx:sNx+OLx,Nr)
      _RL gammaf, pFac, aFac
      _RL OBNf                 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBN_Stevens          (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL tracer (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL vVel   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)

#ifdef ALLOW_OBCS_STEVENS
C     !LOCAL VARIABLES:
C     i,j,k        :: loop indices
C     vPhase       :: estimate of phase velocity for radiation condition
C     dtracSpace   :: horizontal difference of tracer
C     dtracTime    :: temporal difference of tracer
      INTEGER i,j,k
      _RL vPhase
      _RL dtracSpace
      _RL dTracTime
CEOP
      DO K=1,Nr
       DO I=1-OLx,sNx+OLx
        J = OB_J(I,bi,bj)
        IF ( J.NE.OB_indexNone ) THEN
C     Theta first:
         dTracSpace = (tracer(I,J-1,K,bi,bj)-tracer(I,J-2,K,bi,bj))
     &        * _maskS(I,J-1,K,bi,bj)
         dTracTime  = (tracer(I,J-1,K,bi,bj)-OBN_Stevens(I,K,bi,bj))
         vPhase = cflZon(I,K) * pFac
         IF ( dTracSpace .NE. 0. _d 0 ) THEN
          vPhase = MIN( cflZon(I,K),
     &         MAX( 0.D0, -cflZon(I,K)*dTracTime/dTracSpace )
     &         ) * pFac
         ENDIF
C     Compute the tracer tendency here, the tracer will be updated
C     with a simple Euler forward step in S/R obcs_apply_ts
         OBNf(I,K,bi,bj) = _maskS(I,J,K,bi,bj) * (
     &        - ( aFac*MAX(0.D0,vVel(I,J,K,bi,bj)) + vPhase )
     &        *(tracer(I,J,K,bi,bj)-tracer(I,J-1,K,bi,bj))
     &        * _recip_dyC(I,J,bi,bj)
     &        - gFacZ(I,K) * gammaf
     &        * (tracer(I,J,K,bi,bj)-OBNf(I,K,bi,bj)) )
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_OBCS_STEVENS */
      RETURN
      END

CBOP
C     !ROUTINE: OBCS_STEVENS_CALC_TRACER_SOUTH
C     !INTERFACE:
      SUBROUTINE OBCS_STEVENS_CALC_TRACER_SOUTH(
     U     OBSf, 
     I     OBS_Stevens, tracer, gammaf,
     I     vVel, cflZon, gFacZ, pFac, aFac, 
     I     OB_J, OB_indexNone, bi, bj,
     I     futureTime, futureIter,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_STEVENS_CALC_TRACER_SOUTH
C     | Calculate tracer value at the southern OB location
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C    myThid    :: my Thread Id number
C    bi, bj    :: indices of current tile
      _RL futureTime
      INTEGER futureIter
      INTEGER myThid
      INTEGER bi, bj
      INTEGER OB_indexNone
      INTEGER OB_J             (1-OLx:sNx+OLx,nSx,nSy)
      _RL cflZon               (1-OLx:sNx+OLx,Nr)
      _RL gFacZ                (1-OLx:sNx+OLx,Nr)
      _RL gammaf, pFac, aFac
      _RL OBSf                 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBS_Stevens          (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL tracer (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL vVel   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)

#ifdef ALLOW_OBCS_STEVENS
C     !LOCAL VARIABLES:
C     i,j,k        :: loop indices
C     vPhase       :: estimate of phase velocity for radiation condition
C     dtracSpace   :: horizontal difference of tracer
C     dtracTime    :: temporal difference of tracer
      INTEGER i,j,k
      _RL vPhase
      _RL dtracSpace
      _RL dTracTime
CEOP
      DO K=1,Nr
       DO I=1-OLx,sNx+OLx
        J = OB_J(I,bi,bj)
        IF ( J.NE.OB_indexNone ) THEN
         dTracSpace = (tracer(I,J+2,K,bi,bj)-tracer(I,J+1,K,bi,bj))
     &        * _maskS(I,J+2,K,bi,bj)
         dTracTime  = (tracer(I,J+1,K,bi,bj)-OBS_Stevens(I,K,bi,bj))
         vPhase = -cflZon(I,K) * pFac
         IF ( dTracSpace .NE. 0. _d 0 ) THEN
          vPhase = MAX( -cflZon(I,K),
     &         MIN( 0.D0, -cflZon(I,K)*dTracTime/dTracSpace )
     &         ) * pFac
         ENDIF
C     Compute the tracer tendency here, the tracer will be updated
C     with a simple Euler forward step in S/R obcs_apply_ts
         OBSf(I,K,bi,bj) = _maskS(I,J+1,K,bi,bj) * (
     &        - ( aFac*MIN(0.D0,vVel(I,J+1,K,bi,bj)) + vPhase )
     &        *(tracer(I,J+1,K,bi,bj)-tracer(I,J,K,bi,bj))
     &        * _recip_dyC(I,J+1,bi,bj)
     &        - gFacZ(I,K) * gammaf
     &        * (tracer(I,J,K,bi,bj)-OBSf(I,K,bi,bj)) )
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_OBCS_STEVENS */
      RETURN
      END

CBOP
C     !ROUTINE: OBCS_STEVENS_SAVE_TRACERS
C     !INTERFACE:
      SUBROUTINE OBCS_STEVENS_SAVE_TRACERS(
     I     futureTime, futureIter,
     I     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_STEVENS_SAVE_TRACERS
C     | Save tracers (of previous time step) at the OB location
C     | to be used in the next time step for Stevens boundary
C     | conditions
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#include "DYNVARS.h"
CML#ifdef ALLOW_PTRACERS
CML#include "PTRACERS_SIZE.h"
CML#include "PTRACERS_PARAMS.h"
CML#include "PTRACERS_FIELDS.h"
CML#include "OBCS_PTRACERS.h"
CML#endif /* ALLOW_PTRACERS */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C    myThid    :: my Thread Id number
      _RL futureTime
      INTEGER futureIter
      INTEGER myThid

#ifdef ALLOW_OBCS_STEVENS
C     !LOCAL VARIABLES:
C    bi, bj    :: indices of current tile
C    i,j,k     :: loop indices
C    Iobc,Jobc :: position-index of open boundary
      INTEGER bi,bj,i,j,k,Iobc,Jobc
CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef ALLOW_OBCS_NORTH
        IF ( tileHasOBN(bi,bj) .AND. useStevensNorth ) THEN
C Northern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Jn(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone ) THEN
           DO k = 1,Nr
            OBNtStevens(i,k,bi,bj) = theta(i,Jobc-1,k,bi,bj)
     &           *maskC(i,Jobc+1,k,bi,bj)
            OBNsStevens(i,k,bi,bj) = salt(i,Jobc-1,k,bi,bj)
     &           *maskC(i,Jobc+1,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
        IF ( tileHasOBS(bi,bj) .AND. useStevensSouth ) THEN
C Southern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Js(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone ) THEN
           DO k = 1,Nr
            OBStStevens(i,k,bi,bj) = theta(i,Jobc+1,k,bi,bj)
     &           *maskC(i,Jobc+1,k,bi,bj)
            OBSsStevens(i,k,bi,bj) = salt(i,Jobc+1,k,bi,bj)
     &           *maskC(i,Jobc+1,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
        IF ( tileHasOBE(bi,bj) .AND. useStevensEast ) THEN
C Eastern boundary
         DO j=1-OLy,sNy+OLy
          Iobc = OB_Ie(j,bi,bj)
          IF ( Iobc.NE.OB_indexNone ) THEN
           DO k = 1,Nr
            OBEtStevens(j,k,bi,bj) = theta(Iobc-1,j,k,bi,bj)
     &           *maskC(Iobc-1,j,k,bi,bj)
            OBEsStevens(j,k,bi,bj) = salt(Iobc-1,j,k,bi,bj)
     &           *maskC(Iobc-1,j,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
        IF ( tileHasOBW(bi,bj) .AND. useStevensWest ) THEN
C Western boundary
         DO j=1-OLy,sNy+OLy
          Iobc = OB_Iw(j,bi,bj)
          IF ( Iobc.NE.OB_indexNone ) THEN
           DO k = 1,Nr
            OBWtStevens(j,k,bi,bj) = theta(Iobc+1,j,k,bi,bj)
     &           *maskC(Iobc+1,j,k,bi,bj)
            OBWsStevens(j,k,bi,bj) = salt(Iobc+1,j,k,bi,bj)
     &           *maskC(Iobc+1,j,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_WEST */
       ENDDO
      ENDDO
#endif /* ALLOW_OBCS_STEVENS */
      RETURN
      END
