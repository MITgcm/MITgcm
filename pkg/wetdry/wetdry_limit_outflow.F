#include "WETDRY_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_LIMIT_OUTFLOW

C !INTERFACE:
      SUBROUTINE WETDRY_LIMIT_OUTFLOW( myTime, myIter, myThid )

C     !DESCRIPTION:
C     Positivity (outflow) limiter, applied to the new velocity u(n+1)
C     after MOMENTUM_CORRECTION_STEP and before INTEGR_CONTINUITY:
C     for each column, the volume leaving through its faces during one
C     time step (outgoing transports only, all levels) may not exceed
C     the volume it holds above the film,  (D(n) - wdMinDepth)*rA.
C     If it does, all outgoing velocities of that column are scaled by
C       wdOutFac = (D(n) - wdMinDepth)*rA / (deltaT * outflow) < 1
C     A face is outgoing for only one of its two columns, so each face
C     is scaled at most once. Inflows are ignored, so after continuity
C     D(n+1) >= wdMinDepth (the film floor is then only a backstop).
C     Since continuity and tracer transports are both computed from the
C     limited velocity, volume and tracers stay exactly conserved.
C     Also accumulates (wdCumIn) the volume entering the interior
C     (maskInC) domain through its lateral boundary, from the same
C     transports as continuity, plus the interior mass source addMass
C     (selectAddFluid=1, e.g. a river), to close the volume budget.
C     With useRealFreshWaterFlux, net evaporation (EmPmR > 0) is an
C     outgoing flux of the column too: it is first ramped off on dry
C     columns (as the other surface forcing, wdCritDepth .. 2
C     wdCritDepth, with wdDryForcing), then scaled by wdOutFac with
C     the lateral outflows, and the limited EmPmR is what continuity
C     uses; rain (EmPmR < 0) is never limited. What was removed goes to
C     wdEvLim (diagnostic WDEVLIM); the budget counts the limited flux.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "WETDRY.h"

C     !INPUT PARAMETERS:
C     myTime :: time at the beginning of the current time step
C     myIter :: iteration number at the beginning of the time step
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_WETDRY
C     !LOCAL VARIABLES:
C     outVol :: volume leaving the column during one time step [m^3]
C     avail  :: volume of the column above the film [m^3]
      INTEGER i, j, k, bi, bj
      _RL outVol, avail, trW, trE, trS, trN, colD, fD
      LOGICAL useEmP
      _RL nLimT(nSx,nSy), nLim
      _RL inT(nSx,nSy), gIn

C--   Froude-number cap on thin fast sheets first (off by default)
      IF ( wdMaxFroude.GT.zeroRL )
     &  CALL WETDRY_LIMIT_FROUDE( myTime, myIter, myThid )

C     EmPmR moves volume (continuity uses it after the first step)
      useEmP = useRealFreshWaterFlux .AND. myIter.NE.nIter0

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        nLimT(bi,bj) = 0. _d 0

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          wdOutFac(i,j,bi,bj) = oneRL
          wdEvLim(i,j,bi,bj)  = zeroRL
         ENDDO
        ENDDO

        DO j=1,sNy
         DO i=1,sNx
          IF ( maskInC(i,j,bi,bj).NE.zeroRS ) THEN
           outVol = 0. _d 0
           DO k=1,Nr
C-    transports, positive when leaving the column
            trW = -uVel(i,j,k,bi,bj)*dyG(i,j,bi,bj)
     &           *drF(k)*hFacW(i,j,k,bi,bj)
            trE =  uVel(i+1,j,k,bi,bj)*dyG(i+1,j,bi,bj)
     &           *drF(k)*hFacW(i+1,j,k,bi,bj)
            trS = -vVel(i,j,k,bi,bj)*dxG(i,j,bi,bj)
     &           *drF(k)*hFacS(i,j,k,bi,bj)
            trN =  vVel(i,j+1,k,bi,bj)*dxG(i,j+1,bi,bj)
     &           *drF(k)*hFacS(i,j+1,k,bi,bj)
            outVol = outVol + MAX(trW,zeroRL) + MAX(trE,zeroRL)
     &                      + MAX(trS,zeroRL) + MAX(trN,zeroRL)
           ENDDO
           colD = etaH(i,j,bi,bj) + Ro_surf(i,j,bi,bj)
     &          - R_low(i,j,bi,bj)
           IF ( useEmP .AND. EmPmR(i,j,bi,bj).GT.zeroRL ) THEN
C-    no evaporation from a dry column (ramp as WETDRY_DRY_FORCING)
            IF ( wdDryForcing ) THEN
             fD = MIN( oneRL, MAX( zeroRL,
     &             (colD - wdCritDepth)/wdCritDepth ) )
             wdEvLim(i,j,bi,bj) = EmPmR(i,j,bi,bj)*(oneRL - fD)
             EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj)*fD
            ENDIF
            outVol = outVol
     &             + EmPmR(i,j,bi,bj)*mass2rUnit*rA(i,j,bi,bj)
           ENDIF
           outVol = outVol*deltaTFreeSurf
           avail  = MAX( zeroRL, colD - wdMinDepth )
     &            * rA(i,j,bi,bj)
           IF ( outVol.GT.avail ) THEN
            wdOutFac(i,j,bi,bj) = avail/outVol
            nLimT(bi,bj) = nLimT(bi,bj) + 1. _d 0
            IF ( useEmP .AND. EmPmR(i,j,bi,bj).GT.zeroRL ) THEN
             wdEvLim(i,j,bi,bj) = wdEvLim(i,j,bi,bj)
     &          + EmPmR(i,j,bi,bj)*(oneRL - wdOutFac(i,j,bi,bj))
             EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj)*wdOutFac(i,j,bi,bj)
            ENDIF
           ENDIF
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XY_RL( wdOutFac, myThid )
      IF ( useEmP ) CALL EXCH_XY_RL( EmPmR, myThid )

C--   Scale each outgoing velocity by the factor of its upstream column
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx+1
           IF ( uVel(i,j,k,bi,bj).GT.zeroRL ) THEN
            uVel(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)*wdOutFac(i-1,j,bi,bj)
           ELSE
            uVel(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)*wdOutFac(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
         DO j=1,sNy+1
          DO i=1,sNx
           IF ( vVel(i,j,k,bi,bj).GT.zeroRL ) THEN
            vVel(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)*wdOutFac(i,j-1,bi,bj)
           ELSE
            vVel(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)*wdOutFac(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Net inflow through the edge of the interior domain (e.g. OBCS);
C     faces 1:sNx (1:sNy) only, so that tile-edge faces count once
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        inT(bi,bj) = 0. _d 0
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           IF ( maskInC(i-1,j,bi,bj).NE.maskInC(i,j,bi,bj) )
     &      inT(bi,bj) = inT(bi,bj)
     &        + ( maskInC(i,j,bi,bj) - maskInC(i-1,j,bi,bj) )
     &        *uVel(i,j,k,bi,bj)*dyG(i,j,bi,bj)
     &        *drF(k)*hFacW(i,j,k,bi,bj)
           IF ( maskInC(i,j-1,bi,bj).NE.maskInC(i,j,bi,bj) )
     &      inT(bi,bj) = inT(bi,bj)
     &        + ( maskInC(i,j,bi,bj) - maskInC(i,j-1,bi,bj) )
     &        *vVel(i,j,k,bi,bj)*dxG(i,j,bi,bj)
     &        *drF(k)*hFacS(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_ADDFLUID
C--   Interior mass source (addMass, kg/s per cell), as in continuity
C     (INTEGR_CONTINUITY leaves it out at iteration 0)
      IF ( selectAddFluid.GE.1 .AND. myIter.NE.0 ) THEN
       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx
            inT(bi,bj) = inT(bi,bj) + mass2rUnit
     &                 * addMass(i,j,k,bi,bj)*maskInC(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_ADDFLUID */

C--   Surface fresh-water volume flux (the limited EmPmR), as continuity
      IF ( useEmP ) THEN
       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           inT(bi,bj) = inT(bi,bj) - mass2rUnit*EmPmR(i,j,bi,bj)
     &                *rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics )
     &  CALL DIAGNOSTICS_FILL( wdEvLim, 'WDEVLIM ', 0, 1, 0, 1, 1,
     &                         myThid )
#endif

      CALL GLOBAL_SUM_TILE_RL( nLimT, nLim, myThid )
      CALL GLOBAL_SUM_TILE_RL( inT, gIn, myThid )
      _BEGIN_MASTER(myThid)
      wdNlimit = NINT( nLim )
      wdCumIn  = wdCumIn + gIn*deltaTFreeSurf
      _END_MASTER(myThid)

#endif /* ALLOW_WETDRY */

      RETURN
      END
