#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_CHECK
C     !INTERFACE:
      SUBROUTINE OBCS_CHECK( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_CHECK
C     | o Check OBC parameters and set-up
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_SEAICE.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
# include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid   :: My thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     msgBuf  :: Informational/error message buffer
C     bi,bj   :: tile indices
C     i, j    :: Loop counters
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj
      INTEGER i, j
      INTEGER ln
      INTEGER ioUnit
      INTEGER errCount
#ifdef ALLOW_OBCS_BALANCE
C     number of tiles with open boundaries
      INTEGER nTileWithActiveOB
#endif

      ioUnit = standardMessageUnit

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Print OBCS set-up summary:

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_OBCS'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

#ifdef ALLOW_OBCS_BALANCE
      IF ( OBCSbalanceSurf .AND. .NOT. useRealFreshWaterFlux ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** OBCS_CHECK: ',
     &       'OBCSbalanceSurf requires useRealFreshWaterFlux=T'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** OBCS_CHECK: ',
     &       'resetting OBCSbalanceSurf to False'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        OBCSbalanceSurf = .FALSE.
      ENDIF
#endif /* ALLOW_OBCS_BALANCE */

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// OBCS_CHECK: OBCS configuration summary'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
C
      CALL WRITE_0D_L( useOBCSprescribe, INDEX_NONE,
     &   'useOBCSprescribe =', ' /* prescribe OB values */')
      CALL WRITE_0D_L( useOBCSbalance, INDEX_NONE,
     &   'useOBCSbalance =', ' /* balance the flow through OB */')
      IF ( useOBCSbalance ) THEN
        CALL WRITE_0D_L( OBCSbalanceSurf, INDEX_NONE,
     &   'OBCSbalanceSurf =', ' /* include surf. flux in balance */')
        CALL WRITE_0D_RL( OBCS_balanceFacN, INDEX_NONE,
     &   'OBCS_balanceFacN =',
     &   ' /* Northern OB Factor for balancing OB flow [-] */')
        CALL WRITE_0D_RL( OBCS_balanceFacS, INDEX_NONE,
     &   'OBCS_balanceFacS =',
     &   ' /* Southern OB Factor for balancing OB flow [-] */')
        CALL WRITE_0D_RL( OBCS_balanceFacE, INDEX_NONE,
     &   'OBCS_balanceFacE =',
     &   ' /* Eastern  OB Factor for balancing OB flow [-] */')
        CALL WRITE_0D_RL( OBCS_balanceFacW, INDEX_NONE,
     &   'OBCS_balanceFacW =',
     &   ' /* Western  OB Factor for balancing OB flow [-] */')
      ENDIF
C
      CALL WRITE_0D_L( OBCSfixTopo,   INDEX_NONE,
     & 'OBCSfixTopo =',
     & '  /* mod. topo to have zero gradient across boundaries. */')
      CALL WRITE_0D_RL( OBCS_uvApplyFac, INDEX_NONE,
     &   'OBCS_uvApplyFac =',
     &   ' /* Factor to apply to U,V 2nd column/row */')
      CALL WRITE_0D_I( OBCS_u1_adv_T, INDEX_NONE,
     &   'OBCS_u1_adv_T =', ' /* Temp uses upwind adv-scheme @ OB */')
      CALL WRITE_0D_I( OBCS_u1_adv_S, INDEX_NONE,
     &   'OBCS_u1_adv_S =', ' /* Salt uses upwind adv-scheme @ OB */')
#ifdef ALLOW_PTRACERS
      IF ( usePTRACERS ) THEN
       CALL WRITE_1D_I( OBCS_u1_adv_Tr, PTRACERS_numInUse, INDEX_NONE,
     &   'OBCS_u1_adv_Tr =', ' /* pTr uses upwind adv-scheme @ OB */')
      ENDIF
#endif /* ALLOW_PTRACERS */
      CALL WRITE_0D_RL( OBCS_monitorFreq, INDEX_NONE,
     &   'OBCS_monitorFreq =', ' /* monitor output frequency [s] */')
      CALL WRITE_0D_I( OBCS_monSelect, INDEX_NONE, 'OBCS_monSelect =',
     &                  ' /* select group of variables to monitor */')
      CALL WRITE_0D_L( OBCSprintDiags,   INDEX_NONE,
     &   'OBCSprintDiags =', '  /* print some OBCS diagnostics. */')
C
      CALL WRITE_0D_L( useOBCStides, INDEX_NONE,
     &   'useOBCStides =', ' /* apply tidal forcing through OB */')
      IF ( useOBCStides ) THEN
       CALL WRITE_0D_I( OBCS_nTidalComp, INDEX_NONE,
     &   'OBCS_nTidalComp =', ' /* number of tidal-Comp to use */')
       CALL WRITE_1D_RL( OBCS_tidalPeriod, OBCS_tideCompSize, INDEX_K,
     &   'OBCS_tidalPeriod = ', ' /* (s) */')
      ENDIF
C
      CALL WRITE_0D_L( useOrlanskiNorth,   INDEX_NONE,
     & 'useOrlanskiNorth =', '  /* use Orlanski for northern bound. */')
      CALL WRITE_0D_L( useOrlanskiSouth,   INDEX_NONE,
     & 'useOrlanskiSouth =', '  /* use Orlanski for southern bound. */')
      CALL WRITE_0D_L( useOrlanskiEast,    INDEX_NONE,
     & 'useOrlanskiEast  =', '  /* use Orlanski for eastern bound. */')
      CALL WRITE_0D_L( useOrlanskiWest,    INDEX_NONE,
     & 'useOrlanskiWest  =', '  /* use Orlanski for western bound. */')
C
      CALL WRITE_0D_L( useStevensNorth,   INDEX_NONE,
     & 'useStevensNorth =', '  /* use Stevens for northern bound. */')
      CALL WRITE_0D_L( useStevensSouth,   INDEX_NONE,
     & 'useStevensSouth =', '  /* use Stevens for southern bound. */')
      CALL WRITE_0D_L( useStevensEast,    INDEX_NONE,
     & 'useStevensEast  =', '  /* use Stevens for eastern bound. */')
      CALL WRITE_0D_L( useStevensWest,    INDEX_NONE,
     & 'useStevensWest  =', '  /* use Stevens for western bound. */')
      CALL WRITE_0D_L( useStevensPhaseVel,    INDEX_NONE,
     & 'useStevensPhaseVel  =', '  /* include phase vel. term. */')
      CALL WRITE_0D_L( useStevensAdvection,    INDEX_NONE,
     & 'useStevensAdvection  =', '  /* include advection term. */')
      CALL WRITE_0D_RS( TrelaxStevens, INDEX_NONE,
     & 'TrelaxStevens =',
     & ' /* relaxation time scale for theta ( s ). */')
      CALL WRITE_0D_RS( SrelaxStevens, INDEX_NONE,
     & 'SrelaxStevens =',
     & ' /* relaxation time scale for salinity ( s ). */')
C
      CALL WRITE_0D_L( useOBCSsponge,   INDEX_NONE,
     & 'useOBCSSponge =', '  /* use sponge along boundaries */')
      IF ( useOBCSSponge ) THEN
       CALL WRITE_0D_L( OBCSsponge_N,   INDEX_NONE,
     & 'OBCSSponge_N =', '  /* use sponge for northern boundary */')
       CALL WRITE_0D_L( OBCSsponge_S,   INDEX_NONE,
     & 'OBCSSponge_S =', '  /* use sponge for southern boundary */')
       CALL WRITE_0D_L( OBCSsponge_E,    INDEX_NONE,
     & 'OBCSSponge_E =', '  /* use sponge for eastern boundary */')
       CALL WRITE_0D_L( OBCSsponge_W,    INDEX_NONE,
     & 'OBCSSponge_W =', '  /* use sponge for western boundary */')
       CALL WRITE_0D_L( OBCSsponge_UatNS,   INDEX_NONE,
     & 'OBCSSponge_UatNS =', '  /* use u-vel. sponge at N/S bound. */')
       CALL WRITE_0D_L( OBCSsponge_UatEW,   INDEX_NONE,
     & 'OBCSSponge_UatEW =', '  /* use u-vel. sponge at E/W bound. */')
       CALL WRITE_0D_L( OBCSsponge_VatNS,   INDEX_NONE,
     & 'OBCSSponge_VatNS =', '  /* use v-vel. sponge at N/S bound. */')
       CALL WRITE_0D_L( OBCSsponge_UatEW,   INDEX_NONE,
     & 'OBCSSponge_VatEW =', '  /* use v-vel. sponge at E/W bound. */')
       CALL WRITE_0D_L( OBCSsponge_Theta,   INDEX_NONE,
     & 'OBCSSponge_Theta =', '  /* use sponge for Theta. */')
       CALL WRITE_0D_L( OBCSsponge_Salt,   INDEX_NONE,
     & 'OBCSSponge_Salt =', '  /* use sponge for salinity. */')
       CALL WRITE_0D_L( useLinearSponge,   INDEX_NONE,
     & 'useLinearSponge =', '  /* use linear sponge. */')
       CALL WRITE_0D_I( spongeThickness, INDEX_NONE,
     & 'spongeThickness =',
     & ' /* number grid points in sponge */')
       CALL WRITE_0D_RS( Urelaxobcsinner, INDEX_NONE,
     &  'Urelaxobcsinner =',
     & ' /* innermost relaxation time scale, u-velocity ( s ). */')
       CALL WRITE_0D_RS( Urelaxobcsbound, INDEX_NONE,
     &  'Urelaxobcsbound =',
     & ' /* boudnary relaxation time scale, u-velocity ( s ). */')
       CALL WRITE_0D_RS( Vrelaxobcsinner, INDEX_NONE,
     &  'Vrelaxobcsinner =',
     & ' /* innermost relaxation time scale, v-velocity ( s ). */')
       CALL WRITE_0D_RS( Vrelaxobcsbound, INDEX_NONE,
     &  'Vrelaxobcsbound =',
     & ' /* boundary relaxation time scale, v-velocity ( s ). */')
      ENDIF
      CALL WRITE_0D_L( useSeaiceSponge, INDEX_NONE,
     & 'useSeaiceSponge =', '  /* use sponge for sea ice variables */')
      CALL WRITE_0D_L( useSeaiceNeumann, INDEX_NONE,
     & 'useSeaiceNeumann =',
     & '  /* use Neumann conditions for sea ice variables */')
C
      ln = ILNBLNK(insideOBmaskFile)
      IF ( ln.GT.0 ) THEN
        CALL WRITE_0D_C( insideOBmaskFile, ln, INDEX_NONE,
     &   'insideOBmaskFile =',
     &   ' /* used to specify Inside OB region mask */')
      ENDIF
      CALL WRITE_0D_I( OB_indexNone, INDEX_NONE, 'OB_indexNone =',
     &                 ' /* null value for OB index (i.e. no OB) */')

      IF ( debugLevel.GE.debLevA ) THEN
       DO bj = 1,nSy
        DO bi = 1,nSx
         WRITE(msgBuf,'(A,2(I4,A))')
     &     '======== Tile bi=', bi, ' , bj=', bj, ' ========'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(A)') ' OB_Jn = /* Northern OB local indices */'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         CALL PRINT_LIST_I( OB_Jn(1-OLx,bi,bj), 1-OLx, sNx+OLx, INDEX_I,
     &                     .FALSE., .TRUE., ioUnit )
         WRITE(msgBuf,'(A)') ' OB_Js = /* Southern OB local indices */'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         CALL PRINT_LIST_I( OB_Js(1-OLx,bi,bj), 1-OLx, sNx+OLx, INDEX_I,
     &                     .FALSE., .TRUE., ioUnit )
         WRITE(msgBuf,'(A)') ' OB_Ie = /* Eastern OB local indices */'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         CALL PRINT_LIST_I( OB_Ie(1-OLy,bi,bj), 1-OLy, sNy+OLy, INDEX_J,
     &                     .FALSE., .TRUE., ioUnit )
         WRITE(msgBuf,'(A)') ' OB_Iw = /* Western OB local indices */'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         CALL PRINT_LIST_I( OB_Iw(1-OLy,bi,bj), 1-OLy, sNy+OLy, INDEX_J,
     &                     .FALSE., .TRUE., ioUnit )
        ENDDO
       ENDDO
      ENDIF

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// End of OBCS config. summary'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Check OBCS set-up

C-    Check each tile set-up
      errCount = 0
      _BEGIN_MASTER( myThid )
      DO bj = 1,nSy
       DO bi = 1,nSx
#ifndef ALLOW_OBCS_NORTH
         IF ( tileHasOBN(bi,bj) ) THEN
           WRITE(msgBuf,'(A)')
     &      'OBCS_CHECK: #undef ALLOW_OBCS_NORTH and'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,2I4,A)')
     &      'OBCS_CHECK: tile bi,bj=',bi,bj, ' has Northern OB'
           CALL PRINT_ERROR( msgBuf, myThid )
           errCount = errCount + 1
         ENDIF
#endif
#ifndef ALLOW_OBCS_SOUTH
         IF ( tileHasOBS(bi,bj) ) THEN
           WRITE(msgBuf,'(A)')
     &      'OBCS_CHECK: #undef ALLOW_OBCS_SOUTH and'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,2I4,A)')
     &      'OBCS_CHECK: tile bi,bj=',bi,bj, ' has Southern OB'
           CALL PRINT_ERROR( msgBuf, myThid )
           errCount = errCount + 1
         ENDIF
#endif
#ifndef ALLOW_OBCS_EAST
         IF ( tileHasOBE(bi,bj) ) THEN
           WRITE(msgBuf,'(A)')
     &      'OBCS_CHECK: #undef ALLOW_OBCS_EAST and'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,2I4,A)')
     &      'OBCS_CHECK: tile bi,bj=',bi,bj, ' has Eastern OB'
           CALL PRINT_ERROR( msgBuf, myThid )
           errCount = errCount + 1
         ENDIF
#endif
#ifndef ALLOW_OBCS_WEST
         IF ( tileHasOBW(bi,bj) ) THEN
           WRITE(msgBuf,'(A)')
     &      'OBCS_CHECK: #undef ALLOW_OBCS_WEST and'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,2I4,A)')
     &      'OBCS_CHECK: tile bi,bj=',bi,bj, ' has Western OB'
           CALL PRINT_ERROR( msgBuf, myThid )
           errCount = errCount + 1
         ENDIF
#endif
       ENDDO
      ENDDO
      _END_MASTER(myThid)
      CALL GLOBAL_SUM_INT( errCount, myThid )
      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I6,A)')
     &       'OBCS_CHECK:', errCount,' errors in tile OB set-up'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF

#ifdef ALLOW_OBCS_BALANCE
      IF ( OBCSbalanceSurf ) THEN
C     check if there is any open boundary to balance the flow with
        nTileWithActiveOB  = 0
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
C     Eastern boundary
          IF ( tileHasOBE(bi,bj) .AND. OBCS_balanceFacE .GT. 0. _d 0 )
     &         nTileWithActiveOB = nTileWithActiveOB + 1
C     Western boundary
          IF ( tileHasOBW(bi,bj) .AND. OBCS_balanceFacW .GT. 0. _d 0 )
     &         nTileWithActiveOB = nTileWithActiveOB + 1
C     Northern boundary
          IF ( tileHasOBN(bi,bj) .AND. OBCS_balanceFacN .GT. 0. _d 0 )
     &         nTileWithActiveOB = nTileWithActiveOB + 1
C     Southern boundary
          IF ( tileHasOBS(bi,bj) .AND. OBCS_balanceFacS .GT. 0. _d 0 )
     &         nTileWithActiveOB = nTileWithActiveOB + 1
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_INT( nTileWithActiveOB, myThid )
      ENDIF
#endif /* ALLOW_OBCS_BALANCE */

C----------------------------------
C-    Check options and parameters
      _BEGIN_MASTER( myThid )
      errCount = 0

#ifdef ALLOW_CD_CODE
      IF ( useCDscheme ) THEN
        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &      'OBCS not yet implemented in CD-Scheme (useCDscheme=T)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_CD_CODE */

#ifdef ALLOW_ORLANSKI
      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_ORLANSKI'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
#else
      IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.useOrlanskiWest) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: #undef ALLOW_ORLANSKI  and'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: one of useOrlanski* logicals is true'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_ORLANSKI */

      IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.useOrlanskiWest) THEN
       IF (nonlinFreeSurf.GT.0) THEN
        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &      'nonlinFreeSurf not yet implemented in Orlanski OBC'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF (usePTracers) THEN
        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &      ' useOrlanski* OBC not yet implemented for pTracers'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF (useSEAICE) THEN
        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &      ' useOrlanski* OBC not yet implemented for SEAICE'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

#ifdef ALLOW_OBCS_STEVENS
      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_OBCS_STEVENS'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
C     check compatibility with Orlanski
      IF (      ( useStevensNorth.AND.useOrlanskiNorth )
     &     .OR. ( useStevensSouth.AND.useOrlanskiSouth )
     &     .OR. (  useStevensEast.AND.useOrlanskiEast )
     &     .OR. (  useStevensWest.AND.useOrlanskiWest ) ) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: both useOrlanski* and useStevens* logicals'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &      'OBCS_CHECK: are true for at least one boundary'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF (useStevensNorth.OR.useStevensSouth.OR.
     &    useStevensEast.OR.useStevensWest) THEN
       IF (nonlinFreeSurf.GT.0) THEN
        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &    'useStevens OBC with nonlinFreeSurf not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       IF (usePTracers) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** OBCS_CHECK: ',
     &    'useStevens OBC with pTracers not yet implemented'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** OBCS_CHECK: ',
     &    'therefore expect the unexpected'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
       IF (useSEAICE) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** OBCS_CHECK: ',
     &    'useStevens OBC with SEAICE not yet implemented'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** OBCS_CHECK: ',
     &    'therefore expect the unexpected'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
CML       IF (usePTracers) THEN
CML        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
CML     &  'useStevens* OBC with pTracers not yet implemented'
CML        CALL PRINT_ERROR( msgBuf, myThid )
CML        errCount = errCount + 1
CML       ENDIF
CML       IF (useSEAICE) THEN
CML        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
CML     &  'useStevens* OBC with SEAICE not yet implemented'
CML        CALL PRINT_ERROR( msgBuf, myThid )
CML        errCount = errCount + 1
CML       ENDIF
      ENDIF
#else
      IF (useStevensNorth.OR.useStevensSouth.OR.
     &    useStevensEast.OR.useStevensWest) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: #undef OBCS_ALLOW_STEVENS  and'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: one of useStevens* logicals is true'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_OBCS_STEVENS */

#ifndef ALLOW_OBCS_PRESCRIBE
      IF (useOBCSprescribe) THEN
        WRITE(msgBuf,'(A)')
     &       'OBCS_CHECK: useOBCSprescribe = .TRUE. for'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &       'OBCS_CHECK: undef ALLOW_OBCS_PRESCRIBE'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_OBCS_PRESCRIBE */

#ifndef ALLOW_OBCS_SPONGE
      IF (useOBCSsponge) THEN
        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &       'Cannot set useOBCSsponge=.TRUE. (data.obcs)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &       'with #undef ALLOW_OBCS_SPONGE (OBCS_OPTIONS.h)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_OBCS_SPONGE */

#ifndef ALLOW_OBCS_SEAICE_SPONGE
      IF (useSeaiceSponge) THEN
        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &       'Cannot set useSeaiceSponge=.TRUE. (data.obcs)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &       'with #undef ALLOW_OBCS_SEAICE_SPONGE (OBCS_OPTIONS.h)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_OBCS_SEAICE_SPONGE */

#ifdef ALLOW_OBCS_BALANCE
      IF ( OBCSbalanceSurf ) THEN
C     The surface correction is not applied when none of the
C     four OBCS_balanceFacE,W,N,S is positive.
        IF ( nTileWithActiveOB .EQ. 0 ) THEN
         WRITE(msgBuf,'(A)')
     &    'OBCS_CHECK: OBCSbalanceSurf=T requires to have at least'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A)')
     &        'OBCS_CHECK: one of OBCS_balanceFac[E,W,N,S] > 0',
     &        ' on an active boundary to work.'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
      ENDIF
#else /* ALLOW_OBCS_BALANCE */
      IF ( useOBCSbalance ) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: useOBCSbalance requires to define'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ALLOW_OBCS_BALANCE in "OBCS_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_OBCS_BALANCE */

#ifndef ALLOW_OBCS_TIDES
      IF ( useOBCStides ) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: useOBCStides requires to define'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ALLOW_OBCS_TIDES in "OBCS_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_OBCS_TIDES */

      IF ( .NOT.nonHydrostatic .AND.
     &         ( OBNwFile.NE.' ' .OR. OBSwFile.NE.' ' .OR.
     &           OBEwFile.NE.' ' .OR. OBWwFile.NE.' ' )
     &   ) THEN
        WRITE(msgBuf,'(2A)')
     &  'OBCS_CHECK: OB*wFile only allowed with nonHydrostatic'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( nonlinFreeSurf.EQ.0 .AND.
     &         ( OBNetaFile.NE.' ' .OR. OBSetaFile.NE.' ' .OR.
     &           OBEetaFile.NE.' ' .OR. OBWetaFile.NE.' ' )
     &   ) THEN
        WRITE(msgBuf,'(2A)')
     &  'OBCS_CHECK: OB*etaFile(s) only allowed with nonlinFreeSurf'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF (useSEAICE .AND. .NOT. useEXF) THEN
        WRITE(msgBuf,'(A)')
     &   'OBCS_CHECK: for SEAICE OBCS, use'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'OBCS_CHECK: pkg/exf to read input files'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#ifndef OBCS_UVICE_OLD
c     IF ( useSEAICE .AND. SEAICEuseDYNAMICS ) THEN
      IF ( useSEAICE ) THEN
#if ( defined (OBCS_SEAICE_COMPUTE_UVICE)     || \
      defined (OBCS_SEAICE_AVOID_CONVERGENCE) || \
      defined (OBCS_SEAICE_SMOOTH_UVICE_PERP) || \
      defined (OBCS_SEAICE_SMOOTH_UVICE_PAR) )
        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &       'needs to define OBCS_UVICE_OLD in OBCS_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &       'to activate such OBCS_SEAICE_[] option'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
#endif
      ENDIF
#endif /* ndef OBCS_UVICE_OLD */

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'OBCS_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: set-up OK'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Check if Interior mask is consistent with OB list of indices
c     IF ( insideOBmaskFile.NE.' ' ) THEN
       errCount = 0
       WRITE(msgBuf,'(2A)') 'S/R OBCS_CHECK: ',
     &              'Inside Mask and OB locations disagree :'
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

         DO j=1,sNy
C-    Eastern boundary
          i = OB_Ie(j,bi,bj)
          IF ( i.NE.OB_indexNone ) THEN
           IF ( maskInC(i,j,bi,bj).NE.0. ) THEN
            IF ( errCount.EQ.0 ) CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(2(A,I3),2(A,I5))')
     &        ' from OB_Ie (bi,bj=', bi, ',', bj,
     &        ') expects Mask=0 @ i,j=', i, ',', j
            CALL PRINT_ERROR( msgBuf, myThid )
            errCount = errCount + 1
           ENDIF
          ENDIF
          DO i=1,sNx+1
           IF ( maskInC(i-1,j,bi,bj).GT.maskInC(i,j,bi,bj)
     &          .AND.kSurfW(i,j,bi,bj).LE.Nr
     &          .AND. i.NE.OB_Ie(j,bi,bj) ) THEN
            IF ( errCount.EQ.0 ) CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(2(A,I3),2(A,I5))')
     &        ' from insideMask (bi,bj=', bi, ',', bj,
     &        ') expects OBE=', i, ' @ j=', j
            CALL PRINT_ERROR( msgBuf, myThid )
            errCount = errCount + 1
           ENDIF
          ENDDO
C-    Western boundary
          i = OB_Iw(j,bi,bj)
          IF ( i.NE.OB_indexNone ) THEN
           IF ( maskInC(i,j,bi,bj).NE.0. ) THEN
            IF ( errCount.EQ.0 ) CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(2(A,I3),2(A,I5))')
     &        ' from OB_Iw (bi,bj=', bi, ',', bj,
     &        ') expects Mask=0 @ i,j=', i, ',', j
            CALL PRINT_ERROR( msgBuf, myThid )
            errCount = errCount + 1
           ENDIF
          ENDIF
          DO i=1,sNx+1
           IF ( maskInC(i-1,j,bi,bj).LT.maskInC(i,j,bi,bj)
     &          .AND.kSurfW(i,j,bi,bj).LE.Nr
     &          .AND. i.NE.OB_Iw(j,bi,bj)+1 ) THEN
            IF ( errCount.EQ.0 ) CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(2(A,I3),2(A,I5))')
     &        ' from insideMask (bi,bj=', bi, ',', bj,
     &        ') expects OBW=', i-1, ' @ j=', j
            CALL PRINT_ERROR( msgBuf, myThid )
            errCount = errCount + 1
           ENDIF
          ENDDO
         ENDDO

         DO i=1,sNx
C-    Northern boundary
          j = OB_Jn(i,bi,bj)
          IF ( j.NE.OB_indexNone ) THEN
           IF ( maskInC(i,j,bi,bj).NE.0. ) THEN
            IF ( errCount.EQ.0 ) CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(2(A,I3),2(A,I5))')
     &        ' from OB_Jn (bi,bj=', bi, ',', bj,
     &        ') expects Mask=0 @ i,j=', i, ',', j
            CALL PRINT_ERROR( msgBuf, myThid )
            errCount = errCount + 1
           ENDIF
          ENDIF
          DO j=1,sNy+1
           IF ( maskInC(i,j-1,bi,bj).GT.maskInC(i,j,bi,bj)
     &          .AND.kSurfS(i,j,bi,bj).LE.Nr
     &          .AND. j.NE.OB_Jn(i,bi,bj) ) THEN
            IF ( errCount.EQ.0 ) CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(2(A,I3),2(A,I5))')
     &        ' from insideMask (bi,bj=', bi, ',', bj,
     &        ') expects OBN=', j, ' @ i=', i
            CALL PRINT_ERROR( msgBuf, myThid )
            errCount = errCount + 1
           ENDIF
          ENDDO
C-    Southern boundary
          j = OB_Js(i,bi,bj)
          IF ( j.NE.OB_indexNone ) THEN
           IF ( maskInC(i,j,bi,bj).NE.0. ) THEN
            IF ( errCount.EQ.0 ) CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(2(A,I3),2(A,I5))')
     &        ' from OB_Js (bi,bj=', bi, ',', bj,
     &        ') expects Mask=0 @ i,j=', i, ',', j
            CALL PRINT_ERROR( msgBuf, myThid )
            errCount = errCount + 1
           ENDIF
          ENDIF
          DO j=1,sNy+1
           IF ( maskInC(i,j-1,bi,bj).LT.maskInC(i,j,bi,bj)
     &          .AND.kSurfS(i,j,bi,bj).LE.Nr
     &          .AND. j.NE.OB_Js(i,bi,bj)+1 ) THEN
            IF ( errCount.EQ.0 ) CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(2(A,I3),2(A,I5))')
     &        ' from insideMask (bi,bj=', bi, ',', bj,
     &        ') expects OBS=', j-1, ' @ i=', i
            CALL PRINT_ERROR( msgBuf, myThid )
            errCount = errCount + 1
           ENDIF
          ENDDO
         ENDDO

        ENDDO
       ENDDO
       CALL GLOBAL_SUM_INT( errCount, myThid )
       IF ( errCount.GE.1 ) THEN
         WRITE(msgBuf,'(A,I6,A)')
     &       'OBCS_CHECK:', errCount,' errors in OB location vs Mask'
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( myThid )
         STOP 'ABNORMAL END: S/R OBCS_CHECK'
       ELSE
         _BEGIN_MASTER(myThid)
         WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &              'check Inside Mask and OB locations: OK'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(A)') ' '
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         _END_MASTER(myThid)
       ENDIF
c     ENDIF

#endif /* ALLOW_OBCS */
      RETURN
      END
