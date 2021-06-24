#include "OBCS_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
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
# include "tamc.h"
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
      INTEGER i, j, k
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
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3, ikey
#endif /* ALLOW_AUTODIFF_TAMC */
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
CADJ STORE OBEt(:,:,bi,bj)  = comlev1_bibj, key=ikey, kind=isbyte
CADJ STORE OBEs(:,:,bi,bj)  = comlev1_bibj, key=ikey, kind=isbyte
# endif
        IF ( useStevensEast ) THEN
C     Eastern OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: East',myThid)
#endif
C     compute vertical average and deviation from vertical
C     average for I_obe
         DO j=1-OLy,sNy+OLy
          i = OB_Ie(j,bi,bj)
          IF ( i.NE.OB_indexNone ) THEN
C     first initialize some fields
           drFbar  = 0. _d 0
           uMerBar = 0. _d 0
           DO k=1,Nr
            uMerPri(k) = 0. _d 0
           ENDDO
           DO k=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerBar = uMerBar + uVel(i-1,j,k,bi,bj)
#else
            uMerBar = uMerBar + OBEuStevens(j,k,bi,bj)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(k)* _hFacW(i,j,k,bi,bj)
            drFBar = drFBar + drF(k)* _hFacW(i,j,k,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar = uMerBar/drFBar
           DO k=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerPri(k) = (uVel(i-1,j,k,bi,bj)-uMerBar)
#else
            uMerPri(k) = (OBEuStevens(j,k,bi,bj)-uMerBar)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskW(i,j,k,bi,bj)
           ENDDO
C     vertical average of input field
           drFbar  = 0. _d 0
           uMerBar = 0. _d 0
           DO k=1,Nr
            uMerBar = uMerBar + OBEu(j,k,bi,bj)
     &           *drF(k)* _hFacW(i,j,k,bi,bj)
            drFBar = drFBar + drF(k)* _hFacW(i,j,k,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar = uMerBar/drFBar
C     Now the absolute velocity normal to the boundary is
C     uMerBar + uMerPri(K).
           DO k=1,Nr
            OBEu(j,k,bi,bj) = (uMerBar + uMerPri(k))
     &           * _maskW(i,j,k,bi,bj)
CML            OBEv(J,K,bi,bj) = 0. _d 0
#ifdef ALLOW_NONHYDROSTATIC
            OBEw(j,k,bi,bj)=0.
#endif
           ENDDO
          ENDIF
         ENDDO
#ifdef NONLIN_FRSURF
C     this is a bit of a hack
         IF ( nonlinFreeSurf.GT.0 ) THEN
          DO j=1-OLy,sNy+OLy
           i = OB_Ie(j,bi,bj)
           IF ( i.NE.OB_indexNone ) THEN
            OBEeta(j,bi,bj) = etaN(i-1,j,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer!
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           i = OB_Ie(j,bi,bj)
           IF ( i.NE.OB_indexNone ) THEN
            cflMer(j,k) = 0.5 _d 0 * _dxC(i-1,j,bi,bj)/dTtracerLev(k)
CML         gFacM(J,K)  = 0. _d 0
CML         IF ( uVel(I,J,K,bi,bj) .LT. 0. _d 0 ) gFacM(J,K) = 1. _d 0
            gFacM(j,k)  = ABS(MIN(SIGN(1.D0,uVel(i,j,k,bi,bj)),0.D0))
           ELSE
            cflMer(j,k) = 0. _d 0
            gFacM (j,k) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cflMer, gFacM = comlev1_bibj, key=ikey, kind=isbyte
# endif
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
CADJ STORE OBWt(:,:,bi,bj)  = comlev1_bibj, key=ikey, kind=isbyte
CADJ STORE OBWs(:,:,bi,bj)  = comlev1_bibj, key=ikey, kind=isbyte
# endif
        IF ( useStevensWest ) THEN
C     Western OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: West',myThid)
#endif
C     compute vertical average and deviation from vertical
C     average for I_obw+1
         DO j=1-OLy,sNy+OLy
          i = OB_Iw(j,bi,bj)
          IF ( i.NE.OB_indexNone ) THEN
C     first initialize some fields
           drFBar  = 0. _d 0
           uMerBar = 0. _d 0
           DO k=1,Nr
            uMerPri(k) = 0. _d 0
           ENDDO
           DO k=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerBar = uMerBar + uVel(i+2,j,k,bi,bj)
#else
            uMerBar = uMerBar + OBWuStevens(j,k,bi,bj)
#endif /* OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(k)* _hFacW(i+1,j,k,bi,bj)
            drFBar = drFBar + drF(k)* _hFacW(i+1,j,k,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar = uMerBar/drFBar
           DO k=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            uMerPri(k) = (uVel(i+2,j,k,bi,bj)-uMerBar)
#else
            uMerPri(k) = (OBWuStevens(j,k,bi,bj)-uMerBar)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskW(i+1,j,k,bi,bj)
           ENDDO
C     vertical average of input field
           drFBar  = 0. _d 0
           uMerBar = 0. _d 0
           DO k=1,Nr
            uMerBar = uMerBar + OBWu(j,k,bi,bj)
     &           *drF(k)* _hFacW(i+1,j,k,bi,bj)
            drFBar = drFBar + drF(k)* _hFacW(i+1,j,k,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) uMerBar = uMerBar/drFBar
C     Now the absolute velocity normal to the boundary is
C     uMerBar + uMerPri(K).
           DO k=1,Nr
            OBWu(j,k,bi,bj) = (uMerBar + uMerPri(k))
     &          * _maskW(i+1,j,k,bi,bj)
CML            OBWv(J,K,bi,bj) = 0. _d 0
#ifdef ALLOW_NONHYDROSTATIC
            OBWw(j,k,bi,bj)=0.
#endif
           ENDDO
          ENDIF
         ENDDO
#ifdef NONLIN_FRSURF
C     this is a bit of a hack
         IF ( nonlinFreeSurf.GT.0 ) THEN
          DO j=1-OLy,sNy+OLy
           i = OB_Iw(j,bi,bj)
           IF ( i.NE.OB_indexNone ) THEN
            OBWeta(j,bi,bj) = etaN(i+1,j,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer!
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           i = OB_Iw(j,bi,bj)
           IF ( i.NE.OB_indexNone ) THEN
            cflMer(j,k) = 0.5 _d 0 * _dxC(i+2,j,bi,bj)/dTtracerLev(k)
CML         gFacM = 0. _d 0
CML         IF ( uVel(I+1,J,K,bi,bj) .GT. 0. _d 0 ) gFacM = 1. _d 0
            gFacM(j,k)  = ABS(MAX(SIGN(1.D0,uVel(i+1,j,k,bi,bj)),0.D0))
           ELSE
            cflMer(j,k) = 0. _d 0
            gFacM (j,k) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cflMer, gFacM = comlev1_bibj, key=ikey, kind=isbyte
# endif
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
CADJ STORE OBNt(:,:,bi,bj)  = comlev1_bibj, key=ikey, kind=isbyte
CADJ STORE OBNs(:,:,bi,bj)  = comlev1_bibj, key=ikey, kind=isbyte
# endif
        IF ( useStevensNorth ) THEN
C         Northern OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: North',myThid)
#endif
C     compute vertical average and deviation from vertical
C     average for J_obn
         DO i=1-OLx,sNx+OLx
          j = OB_Jn(i,bi,bj)
          IF ( j.NE.OB_indexNone ) THEN
C     first initialize some fields
           drFBar  = 0. _d 0
           vZonBar = 0. _d 0
           DO k=1,Nr
            vZonPri(k) = 0. _d 0
           ENDDO
           DO k=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonBar = vZonBar + vVel(i,j-1,k,bi,bj)
#else
            vZonBar = vZonBar + OBNvStevens(i,k,bi,bj)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(k)* _hFacS(i,j,k,bi,bj)
            drFBar = drFBar + drF(k)* _hFacS(i,j,k,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar = vZonBar/drFBar
           DO k=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonPri(k) = (vVel(i,j-1,k,bi,bj)-vZonBar)
#else
            vZonPri(k) = (OBNvStevens(i,k,bi,bj)-vZonBar)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskS(i,j,k,bi,bj)
           ENDDO
C     vertical average of input field
           drFBar  = 0. _d 0
           vZonBar = 0. _d 0
           DO k=1,Nr
            vZonBar = vZonBar + OBNv(i,k,bi,bj)
     &           *drF(k)* _hFacS(i,j,k,bi,bj)
            drFBar = drFBar + drF(k)* _hFacS(i,j,k,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar = vZonBar/drFBar
C     Now the absolute velocity normal to the boundary is
C     vZonBar + vZonPri(K).
           DO k=1,Nr
            OBNv(i,k,bi,bj) = (vZonBar + vZonPri(k))
     &           * _maskS(i,j,k,bi,bj)
CML            OBNu(I,K,bi,bj) = 0. _d 0
#ifdef ALLOW_NONHYDROSTATIC
            OBNw(i,k,bi,bj)=0.
#endif
           ENDDO
          ENDIF
         ENDDO
#ifdef NONLIN_FRSURF
C     this is a bit of a hack
         IF ( nonlinFreeSurf.GT.0 ) THEN
          DO i=1-OLx,sNx+OLx
           j = OB_Jn(i,bi,bj)
           IF ( j.NE.OB_indexNone ) THEN
            OBNeta(i,bi,bj) = etaN(i,j-1,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer!
         DO k=1,Nr
          DO i=1-OLx,sNx+OLx
           j = OB_Jn(i,bi,bj)
           IF ( j.NE.OB_indexNone ) THEN
            cflZon(i,k) = 0.5 _d 0 * _dyC(i,j-1,bi,bj)/dTtracerLev(k)
CML         gFacZ(I,K) = 0. _d 0
CML         IF ( vVel(I,J,K,bi,bj) .LT. 0. _d 0 ) gFacZ(I,K) = 1. _d 0
            gFacZ(i,k)  = ABS(MIN(SIGN(1.D0,vVel(i,j,k,bi,bj)),0.D0))
           ELSE
            cflZon(i,k) = 0. _d 0
            gFacZ (i,k) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cflZon, gFacZ = comlev1_bibj, key=ikey, kind=isbyte
# endif
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
CADJ STORE OBSt(:,:,bi,bj)  = comlev1_bibj, key=ikey, kind=isbyte
CADJ STORE OBSs(:,:,bi,bj)  = comlev1_bibj, key=ikey, kind=isbyte
# endif
        IF ( useStevensSouth ) THEN
C         Southern OB
#ifdef ALLOW_DEBUG
         IF (debugMode)
     &        CALL DEBUG_MSG('OBCS_CALC_STEVENS: South',myThid)
#endif
C     compute vertical average and deviation from vertical
C     average for J_obs+1
         DO i=1-OLx,sNx+OLx
          j = OB_Js(i,bi,bj)
          IF ( j.NE.OB_indexNone ) THEN
C     first initialize some fields
           drFBar  = 0. _d 0
           vZonBar = 0. _d 0
           DO k=1,Nr
            vZonPri(k) = 0. _d 0
           ENDDO
           DO k=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonBar = vZonBar + vVel(i,j+2,k,bi,bj)
#else
            vZonBar = vZonBar + OBSvStevens(i,k,bi,bj)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           *drF(k)* _hFacS(i,j+1,k,bi,bj)
            drFBar = drFBar + drF(k)* _hFacS(i,j+1,k,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar = vZonBar/drFBar
           DO k=1,Nr
#ifdef OBCS_STEVENS_USE_INTERIOR_VELOCITY
            vZonPri(k) = (vVel(i,j+2,k,bi,bj)-vZonBar)
#else
            vZonPri(k) = (OBSvStevens(i,k,bi,bj)-vZonBar)
#endif /*  OBCS_STEVENS_USE_INTERIOR_VELOCITY */
     &           * _maskS(i,j+1,k,bi,bj)
           ENDDO
C     vertical average of input field
           drFBar  = 0. _d 0
           vZonBar = 0. _d 0
           DO k=1,Nr
            vZonBar = vZonBar + OBSv(i,k,bi,bj)
     &           *drF(k)* _hFacS(i,j+1,k,bi,bj)
            drFBar = drFBar + drF(k)* _hFacS(i,j+1,k,bi,bj)
           ENDDO
           IF ( drFBar .GT. 0. _d 0 ) vZonBar = vZonBar/drFBar
C     Now the absolute velocity normal to the boundary is
C     vZonBar + vZonPri(K).
           DO k=1,Nr
            OBSv(i,k,bi,bj) = (vZonBar + vZonPri(k))
     &          * _maskS(i,j+1,k,bi,bj)
CML            OBSu(I,K,bi,bj) = 0. _d 0
#ifdef ALLOW_NONHYDROSTATIC
            OBSw(i,k,bi,bj)=0.
#endif
           ENDDO
          ENDIF
         ENDDO
#ifdef NONLIN_FRSURF
C     this is a bit of a hack
         IF ( nonlinFreeSurf.GT.0 ) THEN
          DO i=1-OLx,sNx+OLx
           j = OB_Js(i,bi,bj)
           IF ( j.NE.OB_indexNone ) THEN
            OBSeta(i,bi,bj) = etaN(i,j+1,bi,bj)
           ENDIF
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
C     Next, we compute the phase speed correction, which depends on the
C     tracer!
         DO k=1,Nr
          DO i=1-OLx,sNx+OLx
           j = OB_Js(i,bi,bj)
           IF ( j.NE.OB_indexNone ) THEN
            cflZon(i,k) = 0.5 _d 0 * _dyC(i,j+2,bi,bj)/dTtracerLev(k)
CML         gFacZ = 0. _d 0
CML         IF ( vVel(I,J+1,K,bi,bj) .GT. 0. _d 0 ) gFacZ = 1. _d 0
            gFacZ(i,k)  = ABS(MAX(SIGN(1.D0,vVel(i,j+1,k,bi,bj)),0.D0))
           ELSE
            cflZon(i,k) = 0. _d 0
            gFacZ (i,k) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cflZon, gFacZ = comlev1_bibj, key=ikey, kind=isbyte
# endif
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
        DO j=1-OLy,sNy+OLy
         uMerBar=0. _d 0
         DO k=1,Nr
          i = OB_Ie(j,bi,bj)
          IF ( i.EQ.OB_indexNone ) i = 1
          uPhase = uPhase + OBEu(j,k,bi,bj)
     &         *drF(k)* _hFacW(i,j,k,bi,bj)*dyG(i,j,bi,bj)
          i = OB_Iw(j,bi,bj)
          IF ( i.EQ.OB_indexNone ) i = 1
          vPhase = vPhase + OBWu(j,k,bi,bj)
     &         *drF(k)* _hFacW(i+1,j,k,bi,bj)*dyG(i+1,j,bi,bj)
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
      _RL OBEf                 (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBE_Stevens          (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL uVel   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

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
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        i = OB_I(j,bi,bj)
        IF ( i.NE.OB_indexNone ) THEN
         dTracSpace = (tracer(i-1,j,k,bi,bj)-tracer(i-2,j,k,bi,bj))
     &        * _maskW(i-1,j,k,bi,bj)
         dTracTime  = (tracer(i-1,j,k,bi,bj)-OBE_Stevens(j,k,bi,bj))
         uPhase = cflMer(j,k) * pFac
         IF ( dTracSpace .NE. 0. _d 0 ) THEN
          uPhase = MIN( cflMer(j,k),
     &         MAX( 0.D0, -cflMer(j,k)*dTracTime/dTracSpace )
     &         ) * pFac
         ENDIF
C     Compute the tracer tendency here, the tracer will be updated
C     with a simple Euler forward step in S/R obcs_apply_ts
         OBEf(j,k,bi,bj) = _maskW(i,j,k,bi,bj) * (
     &        - ( aFac*MAX(0.D0,uVel(i,j,k,bi,bj)) + uPhase )
     &        *(tracer(i,j,k,bi,bj)-tracer(i-1,j,k,bi,bj))
     &        * _recip_dxC(i,j,bi,bj)
     &        - gFacM(j,k) * gammaf
     &        * (tracer(i,j,k,bi,bj)-OBEf(j,k,bi,bj)) )
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
      _RL OBWf                 (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBW_Stevens          (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL uVel   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

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

      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        i = OB_I(j,bi,bj)
        IF ( i.NE.OB_indexNone ) THEN
         dTracSpace = (tracer(i+2,j,k,bi,bj)-tracer(i+1,j,k,bi,bj))
     &        * _maskW(i+2,j,k,bi,bj)
         dTracTime  = (tracer(i+1,j,k,bi,bj)-OBW_Stevens(j,k,bi,bj))
         uPhase = -cflMer(j,k) * pFac
         IF ( dTracSpace .NE. 0. _d 0 ) THEN
          uPhase = MAX( -cflMer(j,k),
     &         MIN( 0.D0, -cflMer(j,k)*dTracTime/dTracSpace )
     &         ) * pFac
         ENDIF
C     Compute the tracer tendency here, the tracer will be updated
C     with a simple Euler forward step in S/R obcs_apply_ts
         OBWf(j,k,bi,bj) = _maskW(i+1,j,k,bi,bj) * (
     &        - ( aFac*MIN(0.D0,uVel(i+1,j,k,bi,bj)) + uPhase )
     &        *(tracer(i+1,j,k,bi,bj)-tracer(i,j,k,bi,bj))
     &        * _recip_dxC(i+1,j,bi,bj)
     &        - gFacM(j,k) * gammaf
     &        * (tracer(i,j,k,bi,bj)-OBWf(j,k,bi,bj)) )
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
      _RL OBNf                 (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBN_Stevens          (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

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
      DO k=1,Nr
       DO i=1-OLx,sNx+OLx
        j = OB_J(i,bi,bj)
        IF ( j.NE.OB_indexNone ) THEN
C     Theta first:
         dTracSpace = (tracer(i,j-1,k,bi,bj)-tracer(i,j-2,k,bi,bj))
     &        * _maskS(i,j-1,k,bi,bj)
         dTracTime  = (tracer(i,j-1,k,bi,bj)-OBN_Stevens(i,k,bi,bj))
         vPhase = cflZon(i,k) * pFac
         IF ( dTracSpace .NE. 0. _d 0 ) THEN
          vPhase = MIN( cflZon(i,k),
     &         MAX( 0.D0, -cflZon(i,k)*dTracTime/dTracSpace )
     &         ) * pFac
         ENDIF
C     Compute the tracer tendency here, the tracer will be updated
C     with a simple Euler forward step in S/R obcs_apply_ts
         OBNf(i,k,bi,bj) = _maskS(i,j,k,bi,bj) * (
     &        - ( aFac*MAX(0.D0,vVel(i,j,k,bi,bj)) + vPhase )
     &        *(tracer(i,j,k,bi,bj)-tracer(i,j-1,k,bi,bj))
     &        * _recip_dyC(i,j,bi,bj)
     &        - gFacZ(i,k) * gammaf
     &        * (tracer(i,j,k,bi,bj)-OBNf(i,k,bi,bj)) )
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
      _RL OBSf                 (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBS_Stevens          (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

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
      DO k=1,Nr
       DO i=1-OLx,sNx+OLx
        j = OB_J(i,bi,bj)
        IF ( j.NE.OB_indexNone ) THEN
         dTracSpace = (tracer(i,j+2,k,bi,bj)-tracer(i,j+1,k,bi,bj))
     &        * _maskS(i,j+2,k,bi,bj)
         dTracTime  = (tracer(i,j+1,k,bi,bj)-OBS_Stevens(i,k,bi,bj))
         vPhase = -cflZon(i,k) * pFac
         IF ( dTracSpace .NE. 0. _d 0 ) THEN
          vPhase = MAX( -cflZon(i,k),
     &         MIN( 0.D0, -cflZon(i,k)*dTracTime/dTracSpace )
     &         ) * pFac
         ENDIF
C     Compute the tracer tendency here, the tracer will be updated
C     with a simple Euler forward step in S/R obcs_apply_ts
         OBSf(i,k,bi,bj) = _maskS(i,j+1,k,bi,bj) * (
     &        - ( aFac*MIN(0.D0,vVel(i,j+1,k,bi,bj)) + vPhase )
     &        *(tracer(i,j+1,k,bi,bj)-tracer(i,j,k,bi,bj))
     &        * _recip_dyC(i,j+1,bi,bj)
     &        - gFacZ(i,k) * gammaf
     &        * (tracer(i,j,k,bi,bj)-OBSf(i,k,bi,bj)) )
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
C     bi, bj     :: indices of current tile
C     i,j,k      :: loop indices
C     Iobc, Jobc :: position-index of open boundary
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
