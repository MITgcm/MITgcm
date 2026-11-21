#include "WETDRY_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_APPLY_FLOOR

C !INTERFACE:
      SUBROUTINE WETDRY_APPLY_FLOOR(
     I                       inStep, myTime, myIter, myThid )

C     !DESCRIPTION:
C     Keep a film of at least wdMinDepth in every wet column:
C       eta_floor = R_low - Ro_surf + wdMinDepth
C       IF etaH < eta_floor : etaH = eta_floor (same increment on etaN)
C     Within the time step (inStep=T) and with wdConserveVol=T, the
C     added volume is taken back from the neighbours connected through an
C     open face, in proportion to their depth in excess of wdCritDepth
C     (a donor never goes below wdCritDepth). What cannot be taken back
C     is accumulated in wdVolAdj and reported by the wetdry monitor.
C     At initialisation (inStep=F) etaN, etaH and etaHnm1 are simply
C     clipped and nothing is accumulated.
C     Also sets the dry-column flag wdDryC (D <= wdCritDepth: the
C     column cannot drain any further).

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "WETDRY.h"

C     !INPUT PARAMETERS:
C     inStep :: called from within the time step (not initialisation)
C     myTime :: time at the beginning of the current time step
C     myIter :: iteration number at the beginning of the time step
C     myThid :: my Thread Id number
      LOGICAL inStep
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_WETDRY
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     colD    :: total water depth, before then after the floor [m]
C     defic   :: depth added by the floor [m]
C     exces   :: depth above wdCritDepth, available to donate [m]
C     ratio   :: fraction of each donor's excess wanted by a clipped cell
C     take    :: volume taken from a donor [m^3]
C     volAdd/volTake/volTot/nDry :: tile sums
      INTEGER i, j, bi, bj
      _RL colD (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL defic(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL exces(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ratio(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sVol, want, avail, take
      _RL volAdd (nSx,nSy), volTake(nSx,nSy)
      _RL volTot (nSx,nSy), nDryT  (nSx,nSy)
      _RL gVolAdd, gVolTake, gVolTot, gNdry, gMinD
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      gMinD = -1. _d 20

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        volAdd (bi,bj) = 0. _d 0
        volTake(bi,bj) = 0. _d 0
        volTot (bi,bj) = 0. _d 0
        nDryT  (bi,bj) = 0. _d 0

C--   Depth, deficit below the film and excess above the critical depth
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          colD (i,j) = 0. _d 0
          defic(i,j) = 0. _d 0
          exces(i,j) = 0. _d 0
          ratio(i,j) = 0. _d 0
          IF ( maskInC(i,j,bi,bj).NE.zeroRS ) THEN
           colD (i,j) = etaH(i,j,bi,bj)
     &                + Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
           defic(i,j) = MAX( zeroRL, wdMinDepth - colD(i,j) )
           exces(i,j) = MAX( zeroRL, colD(i,j) - wdCritDepth )
          ENDIF
         ENDDO
        ENDDO

        IF ( inStep .AND. wdConserveVol ) THEN
C--   Share each clipped cell's volume among its open-face neighbours
C     (needs one row of halo for the clipped cell, two for its donors)
         DO j=0,sNy+1
          DO i=0,sNx+1
           IF ( defic(i,j).GT.zeroRL ) THEN
            sVol = exces(i-1,j)*rA(i-1,j,bi,bj)*wdMaskW(i,j,bi,bj)
     &           + exces(i+1,j)*rA(i+1,j,bi,bj)*wdMaskW(i+1,j,bi,bj)
     &           + exces(i,j-1)*rA(i,j-1,bi,bj)*wdMaskS(i,j,bi,bj)
     &           + exces(i,j+1)*rA(i,j+1,bi,bj)*wdMaskS(i,j+1,bi,bj)
            IF ( sVol.GT.zeroRL ) ratio(i,j) =
     &           MIN( oneRL, defic(i,j)*rA(i,j,bi,bj)/sVol )
           ENDIF
          ENDDO
         ENDDO
         DO j=1,sNy
          DO i=1,sNx
           IF ( exces(i,j).GT.zeroRL ) THEN
            avail = exces(i,j)*rA(i,j,bi,bj)
            want  = avail*( ratio(i-1,j)*wdMaskW(i,j,bi,bj)
     &                    + ratio(i+1,j)*wdMaskW(i+1,j,bi,bj)
     &                    + ratio(i,j-1)*wdMaskS(i,j,bi,bj)
     &                    + ratio(i,j+1)*wdMaskS(i,j+1,bi,bj) )
            take  = MIN( avail, want )
            IF ( take.GT.zeroRL ) THEN
             etaH(i,j,bi,bj) = etaH(i,j,bi,bj)
     &                      - take*recip_rA(i,j,bi,bj)
             etaN(i,j,bi,bj) = etaN(i,j,bi,bj)
     &                      - take*recip_rA(i,j,bi,bj)
             volTake(bi,bj) = volTake(bi,bj) + take
            ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDIF

C--   Apply the floor (interior only; halos filled by exchange below)
        DO j=1,sNy
         DO i=1,sNx
          IF ( defic(i,j).GT.zeroRL ) THEN
           etaH(i,j,bi,bj) = etaH(i,j,bi,bj) + defic(i,j)
           etaN(i,j,bi,bj) = etaN(i,j,bi,bj) + defic(i,j)
           IF ( .NOT.inStep )
     &       etaHnm1(i,j,bi,bj) = MAX( etaHnm1(i,j,bi,bj),
     &                                 etaH(i,j,bi,bj) )
           volAdd(bi,bj) = volAdd(bi,bj) + defic(i,j)*rA(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO

C--   Dry flag, depth after the floor, and tile statistics
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          wdDryC(i,j,bi,bj) = zeroRS
          IF ( maskInC(i,j,bi,bj).NE.zeroRS ) THEN
           colD(i,j) = etaH(i,j,bi,bj)
     &               + Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
           IF ( colD(i,j).LE.wdCritDepth ) wdDryC(i,j,bi,bj) = oneRS
          ENDIF
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
          IF ( maskInC(i,j,bi,bj).NE.zeroRS ) THEN
           volTot(bi,bj) = volTot(bi,bj) + colD(i,j)*rA(i,j,bi,bj)
           nDryT (bi,bj) = nDryT(bi,bj) + wdDryC(i,j,bi,bj)
           gMinD = MAX( gMinD, -colD(i,j) )
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics .AND. inStep ) THEN
          CALL DIAGNOSTICS_FILL( colD, 'WDdepth ', 0, 1, 2,
     &                           bi, bj, myThid )
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

       ENDDO
      ENDDO

      CALL EXCH_XY_RL( etaH, myThid )
      CALL EXCH_XY_RL( etaN, myThid )
      IF ( .NOT.inStep ) CALL EXCH_XY_RL( etaHnm1, myThid )
      CALL EXCH_XY_RS( wdDryC, myThid )

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND. inStep ) THEN
        CALL DIAGNOSTICS_FILL_RS( wdDryC, 'WDdryC  ', 0, 1, 0, 1, 1,
     &                            myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      CALL GLOBAL_SUM_TILE_RL( volAdd,  gVolAdd,  myThid )
      CALL GLOBAL_SUM_TILE_RL( volTake, gVolTake, myThid )
      CALL GLOBAL_SUM_TILE_RL( volTot,  gVolTot,  myThid )
      CALL GLOBAL_SUM_TILE_RL( nDryT,   gNdry,    myThid )
      _GLOBAL_MAX_RL( gMinD, myThid )
      gMinD = -gMinD

C--   Stop on NaN (the model would otherwise carry on to NORMAL END)
      IF ( gVolTot.NE.gVolTot .OR. gMinD.NE.gMinD ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(msgBuf,'(A,I10)')
     &   'WETDRY_APPLY_FLOOR: NaN in water depth at iteration', myIter
        CALL PRINT_ERROR( msgBuf, myThid )
        _END_MASTER(myThid)
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R WETDRY_APPLY_FLOOR'
      ENDIF

      _BEGIN_MASTER(myThid)
      IF ( .NOT.inStep ) wdVol0 = gVolTot
      IF ( inStep ) THEN
        wdVolAdjStep = gVolAdd - gVolTake
        wdVolAdj     = wdVolAdj + wdVolAdjStep
      ENDIF
      IF ( .NOT.inStep .OR.
     &     DIFFERENT_MULTIPLE( wdMonFreq, myTime,
     &                         deltaTClock ) ) THEN
        WRITE(msgBuf,'(A,I10,3(A,I8),A,1PE12.4)')
     &   '%WETDRY_MON: iter=', myIter, ' nDry=', NINT(gNdry),
     &   ' nFlips=', wdNflips, ' nLimit=', wdNlimit,
     &   ' minDepth=', gMinD
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,1P4E16.8)')
     &   '%WETDRY_MON: vol,dVolAdd,dVolTake,cumVolAdj=',
     &   gVolTot, gVolAdd, gVolTake, wdVolAdj
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
C-    volume budget: change = lateral inflow + film-floor adjustment
        WRITE(msgBuf,'(A,1P3E16.8)')
     &   '%WETDRY_MON: cumInflow,budgetErr,budgetErr/vol0=',
     &   wdCumIn, gVolTot - wdVol0 - wdCumIn - wdVolAdj,
     &   ( gVolTot - wdVol0 - wdCumIn - wdVolAdj )
     &   / MAX( wdVol0, oneRL )
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
      _END_MASTER(myThid)

#endif /* ALLOW_WETDRY */

      RETURN
      END
