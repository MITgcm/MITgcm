#include "WETDRY_OPTIONS.h"
#include "MOM_COMMON_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_CHECK

C !INTERFACE:
      SUBROUTINE WETDRY_CHECK( myThid )

C     !DESCRIPTION:
C     Check pkg/wetdry parameters and dependencies on the main model
C     configuration (non-linear free surface with r* coordinate).

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
#endif
#include "WETDRY.h"

C     !INPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_WETDRY
C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
C     errCount :: number of fatal parameter errors
C     maxRcol  :: largest resting column thickness in the domain [m]
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
#ifdef ALLOW_PTRACERS
      INTEGER iTr, nTrAB
#endif
      INTEGER i, j, bi, bj
      _RL maxRcol

C--   Largest resting column thickness (all threads, all procs)
      maxRcol = 0. _d 0
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          IF ( maskInC(i,j,bi,bj).NE.zeroRS ) maxRcol = MAX( maxRcol,
     &         Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj) )
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _GLOBAL_MAX_RL( maxRcol, myThid )

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'WETDRY_CHECK: #define ALLOW_WETDRY'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--  Print out some key parameters :
      CALL WRITE_0D_RL( wdMinDepth, INDEX_NONE, 'wdMinDepth =',
     &  ' /* thickness of the film kept in dry columns (m) */')
      CALL WRITE_0D_RL( wdCritDepth, INDEX_NONE, 'wdCritDepth =',
     &  ' /* face opens when surface > crest + this (m) */')
      CALL WRITE_0D_RL( wdDragDepth, INDEX_NONE, 'wdDragDepth =',
     &  ' /* velocity relaxed to 0 below this depth (m) */')
      CALL WRITE_0D_RL( wdAdvDepth, INDEX_NONE, 'wdAdvDepth =',
     &  ' /* momentum advection ramped off below this depth (m) */')
      CALL WRITE_0D_RL( wdGMDepth, INDEX_NONE, 'wdGMDepth =',
     &  ' /* GM/Redi tapered off below this depth (m) */')
      CALL WRITE_0D_RL( wdKPPDepth, INDEX_NONE, 'wdKPPDepth =',
     &  ' /* KPP non-local flux ramped off below this depth (m) */')
      CALL WRITE_0D_RL( wdKPPCapDepth, INDEX_NONE, 'wdKPPCapDepth =',
     &  ' /* KPP non-local fraction capped at 1 below this (m) */')
      CALL WRITE_0D_RL( wdManningN, INDEX_NONE, 'wdManningN =',
     &  ' /* Manning n for depth-dependent drag (<=0: off) */')
      CALL WRITE_0D_RL( wdDragMax, INDEX_NONE, 'wdDragMax =',
     &  ' /* upper limit of that drag coefficient */')
      CALL WRITE_0D_RL( wdMaxFroude, INDEX_NONE, 'wdMaxFroude =',
     &  ' /* cap |u| at this times sqrt(g D) (0: off) */')
      CALL WRITE_0D_RL( wdSmoothWidth, INDEX_NONE, 'wdSmoothWidth =',
     &  ' /* width of smooth face mask (m) */')
      CALL WRITE_0D_RL( wdMonFreq, INDEX_NONE, 'wdMonFreq =',
     &  ' /* wetdry monitor frequency (s) */')
      CALL WRITE_0D_RL( wdMaxSpeed, INDEX_NONE, 'wdMaxSpeed =',
     &  ' /* stop if an open-face speed exceeds this (m/s) */')
      CALL WRITE_0D_L( wdConserveVol, INDEX_NONE, 'wdConserveVol =',
     &  ' /* take film-floor volume from wet neighbours */')
      CALL WRITE_0D_L( wdCarryVel, INDEX_NONE, 'wdCarryVel =',
     &  ' /* opened face takes upstream u* */')
      CALL WRITE_0D_L( wdUpwindFace, INDEX_NONE, 'wdUpwindFace =',
     &  ' /* NLFS face thickness from upwind surface */')
      CALL WRITE_0D_L( wdDryForcing, INDEX_NONE, 'wdDryForcing =',
     &  ' /* no surface forcing on dry columns */')
      CALL WRITE_0D_C( wdManningFile, -1, INDEX_NONE,
     &  'wdManningFile =', ' /* extra Manning roughness map */')
      CALL WRITE_0D_RL( wdManningT1, INDEX_NONE, 'wdManningT1 =',
     &  ' /* extra roughness full until this time [s] */')
      CALL WRITE_0D_RL( wdManningT2, INDEX_NONE, 'wdManningT2 =',
     &  ' /* extra roughness gone at this time [s] */')

C--  Check parameters:
      errCount = 0
#ifndef ALLOW_BOTTOMDRAG_ROUGHNESS
      IF ( wdManningN.GT.zeroRL .OR. wdManningFile.NE.' ' ) THEN
        WRITE(msgBuf,'(2A)') 'WETDRY_CHECK: wdManningN/File needs',
     &   ' #define ALLOW_BOTTOMDRAG_ROUGHNESS (MOM_COMMON_OPTIONS.h)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#else
      IF ( (wdManningN.GT.zeroRL .OR. wdManningFile.NE.' ')
     &     .AND. zRoughBot.GT.zeroRL ) THEN
        WRITE(msgBuf,'(2A)') 'WETDRY_CHECK: wdManningN and zRoughBot',
     &   ' both set the bottom drag coefficient; use one'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifndef NONLIN_FRSURF
      WRITE(msgBuf,'(A)')
     &  'WETDRY_CHECK: needs #define NONLIN_FRSURF'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif
      IF ( .NOT.usingZCoords ) THEN
        WRITE(msgBuf,'(A)')
     &  'WETDRY_CHECK: only works with height coordinates'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( nonlinFreeSurf.LT.3 ) THEN
C-    the CG2D matrix must be rebuilt every step to follow the face mask
        WRITE(msgBuf,'(A,I3,A)') 'WETDRY_CHECK: nonlinFreeSurf=',
     &   nonlinFreeSurf, ' but pkg/wetdry needs nonlinFreeSurf >= 3'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( select_rStar.LT.0 .OR. selectSigmaCoord.NE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &  'WETDRY_CHECK: needs select_rStar >= 0 and no sigma coord.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( select_rStar.EQ.0 .AND. hFacInf*drF(1).GT.wdMinDepth ) THEN
C-    surface-level NLFS clips the surface at Rmin_surf (hFacInf) without
C     telling wetdry: the film must be thicker than that clip
        WRITE(msgBuf,'(A,1PE10.3,A)')
     &   'WETDRY_CHECK: select_rStar=0 needs'
     &   //' hFacInf <=', wdMinDepth/drF(1), ' (wdMinDepth/drF(1))'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( .NOT.exactConserv ) THEN
        WRITE(msgBuf,'(A)') 'WETDRY_CHECK: needs exactConserv=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( .NOT.implicitDiffusion ) THEN
        WRITE(msgBuf,'(A)')
     &  'WETDRY_CHECK: needs implicitDiffusion=T (thin films)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( wdMinDepth.LE.zeroRL ) THEN
        WRITE(msgBuf,'(A)') 'WETDRY_CHECK: wdMinDepth must be > 0'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( wdCritDepth.LE.wdMinDepth ) THEN
        WRITE(msgBuf,'(A)')
     &  'WETDRY_CHECK: wdCritDepth must be > wdMinDepth'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( wdCarryVel .AND. Nr.GT.1 .AND. .NOT.momImplVertAdv ) THEN
C-    a face opening at full speed into a thin r* column drives a large
C     vertical velocity through cm-thin layers: explicit vertical
C     advection of momentum blows up
        WRITE(msgBuf,'(A)')
     &  'WETDRY_CHECK: wdCarryVel with Nr > 1 needs momImplVertAdv=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
C-    Combinations that pkg/wetdry does not handle
C-    useRealFreshWaterFlux: supported (evaporation in
C     the outflow limiter and the dry-column ramp, EmPmR in the budget)
      IF ( useRealFreshWaterFlux ) THEN
        WRITE(msgBuf,'(2A)') 'WETDRY_CHECK: useRealFreshWaterFlux:',
     &   ' evaporation limited on thin and dry columns (WDEVLIM)'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( useAbsVorticity .AND. momAdvection .AND.
     &     vectorInvariantMomentum ) THEN
C-    f is then inside the advection term that wetdry ramps off in thin water
        WRITE(msgBuf,'(A)') 'WETDRY_CHECK: useAbsVorticity=.TRUE. would'
     &   //' ramp off Coriolis in thin water; not supported'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( implicDiv2DFlow.LT.oneRL ) THEN
C-    etaH halos are then not updated before WETDRY_APPLY_FLOOR
        WRITE(msgBuf,'(A)') 'WETDRY_CHECK: needs implicDiv2DFlow=1'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( applyExchUV_early ) THEN
C-    uVel halos would miss the outflow limiter
        WRITE(msgBuf,'(A)')
     &   'WETDRY_CHECK: applyExchUV_early=.TRUE. is not supported'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useThSIce ) THEN
        WRITE(msgBuf,'(A)') 'WETDRY_CHECK: pkg/thsice is not supported'
     &   //' (ice would grow on dry columns)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useSALT_PLUME ) THEN
        WRITE(msgBuf,'(A)')
     &  'WETDRY_CHECK: pkg/salt_plume is not supported'
     &   //' (brine bypasses the dry-column ramp)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useSEAICE ) THEN
C-    sea ice on drying flats (masks, grounding, dry-column growth) is
C     not part of this version of pkg/wetdry
        WRITE(msgBuf,'(A)') 'WETDRY_CHECK: pkg/seaice is not supported'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useGCHEM ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** WETDRY_CHECK: pkg/gchem ',
     &   'air-sea fluxes are not ramped off on dry columns'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( wdSmoothWidth.GT.zeroRL ) THEN
        WRITE(msgBuf,'(A)')
     &  'WETDRY_CHECK: wdSmoothWidth > 0 is not implemented yet'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#ifdef ALLOW_SHELFICE_REMESHING
      IF ( useShelfIce ) THEN
        WRITE(msgBuf,'(A)')
     &  'WETDRY_CHECK: is not compatible with shelfice remeshing'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

C-   The r* "too small" STOP in CALC_R_STAR is bypassed with wetdry, so a
C    hFacInf above the film fraction is harmless, but worth noting
      IF ( maxRcol.GT.zeroRL .AND.
     &     hFacInf*maxRcol.GT.wdMinDepth ) THEN
        WRITE(msgBuf,'(2A,1PE12.4,A)') '** WARNING ** WETDRY_CHECK: ',
     &   'hFacInf >', wdMinDepth/maxRcol,
     &   ' = wdMinDepth/max(Rcol) ; r* STOP is bypassed by wetdry'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

C-   Momentum advection in thin columns:
C    flux form divides by hFacW and blows up at the drying front ;
C    vector-invariant needs upwindShear when Nr > 1
      IF ( momStepping .AND. momAdvection ) THEN
       IF ( .NOT.vectorInvariantMomentum ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** WETDRY_CHECK: flux-form ',
     &   'momentum advection is unstable at drying fronts ;'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** WETDRY_CHECK: ',
     &   'use vectorInvariantMomentum=.TRUE.'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ELSEIF ( Nr.GT.1 .AND. .NOT.upwindShear ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** WETDRY_CHECK: centred ',
     &   'vertical shear is unstable in thin columns ;'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** WETDRY_CHECK: ',
     &   'use upwindShear=.TRUE.'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDIF

C-    explicit quadratic bottom drag on a layer of thickness h is unstable
C     when Cd |u| dt / h > 2 ; thin partial cells in drying columns reach
C     this (e.g. Cd=2.5e-3, |u|=1 m/s, dt=6 s: h < 7.5 mm)
      IF ( momStepping .AND. Nr.GT.1 .AND. selectImplicitDrag.EQ.0
     &     .AND. bottomDragQuadratic.GT.zeroRL ) THEN
        WRITE(msgBuf,'(2A,1PE10.3,A)') '** WARNING ** WETDRY_CHECK: ',
     &   'explicit quadratic bottom drag is unstable in layers < ',
     &   bottomDragQuadratic*deltaTMom/2. _d 0, ' m (|u| = 1 m/s) ;'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** WETDRY_CHECK: ',
     &   'with thin partial cells use selectImplicitDrag=2'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** WETDRY_CHECK: ',
     &   '(#define ALLOW_SOLVE4_PS_AND_DRAG) or a larger hFacMin'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

#ifdef ALLOW_GENERIC_ADVDIFF
C-   Adams-Bashforth extrapolation of the tracer tendency is inconsistent
C    with the fast volume changes of thin columns: a uniform tracer does
C    not stay uniform (and can blow up with films of a few mm).
      IF ( ( tempStepping .AND. (AdamsBashforthGt.OR.AdamsBashforth_T) )
     & .OR.( saltStepping .AND. (AdamsBashforthGs.OR.AdamsBashforth_S) )
     &   ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** WETDRY_CHECK: ',
     &   'T or S uses an Adams-Bashforth advection scheme ;'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** WETDRY_CHECK: ',
     &   'prefer a 2-level scheme (e.g. 33, 77) in drying areas'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
#endif
#ifdef ALLOW_PTRACERS
      IF ( usePTRACERS ) THEN
        IF ( wdDryForcing .AND. PTRACERS_addSrelax2EmP ) THEN
C-    WETDRY_DRY_FORCING keeps PmEpR*(EvPrRn - pTracer) unscaled, but the
C     add2EmP part of that term is scaled with the other fluxes
          WRITE(msgBuf,'(3A)') '** WARNING ** WETDRY_CHECK: ',
     &     'PTRACERS_addSrelax2EmP: its pTracer term',
     &     ' follows the dry ramp'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF
        nTrAB = 0
        DO iTr=1,PTRACERS_numInUse
          IF ( PTRACERS_AdamsBashGtr(iTr) .OR.
     &         PTRACERS_AdamsBash_Tr(iTr) ) nTrAB = nTrAB + 1
        ENDDO
        IF ( nTrAB.GT.0 ) THEN
          WRITE(msgBuf,'(2A,I4,A)') '** WARNING ** WETDRY_CHECK: ',
     &     'pTracers:', nTrAB, ' use an Adams-Bashforth scheme ;'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(2A)') '** WARNING ** WETDRY_CHECK: ',
     &     'prefer a 2-level scheme (e.g. 33, 77) in drying areas'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF
#endif

      IF ( errCount.GT.0 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'WETDRY_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R WETDRY_CHECK'
      ENDIF

      _END_MASTER(myThid)

#endif /* ALLOW_WETDRY */

      RETURN
      END
