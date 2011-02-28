C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_check.F,v 1.19 2011/02/28 15:22:09 jmc Exp $
C $Name:  $

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
#include "OBCS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid   :: My thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi,bj
      INTEGER ioUnit

      ioUnit = standardMessageUnit

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_OBCS'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Print OBCS set-up summary:

      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)') 'OBCS_CHECK: start summary:'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      CALL WRITE_0D_L( useOBCSbalance, INDEX_NONE,
     &   'useOBCSbalance =', ' /* balance the flow through OB */')
      IF ( useOBCSbalance ) THEN
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
      CALL WRITE_0D_RL( OBCS_monitorFreq, INDEX_NONE,
     &   'OBCS_monitorFreq =', ' /* monitor output frequency [s] */')
      CALL WRITE_0D_I( OBCS_monSelect, INDEX_NONE, 'OBCS_monSelect =',
     &                  ' /* select group of variables to monitor */')

      IF ( debugLevel.GE.debLevA ) THEN
       DO bj = 1,nSy
        DO bi = 1,nSx
         WRITE(msgBuf,'(A,2(I4,A))')
     &     '======== Tile bi=', bi, ' , bj=', bj, ' ========'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(A)') ' OB_Jn = /* Northern OB local indices */'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         CALL PRINT_LIST_I( OB_Jn(1-Olx,bi,bj), 1-Olx, sNx+Olx, INDEX_I,
     &                     .FALSE., .TRUE., ioUnit )
         WRITE(msgBuf,'(A)') ' OB_Js = /* Southern OB local indices */'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         CALL PRINT_LIST_I( OB_Js(1-Olx,bi,bj), 1-Olx, sNx+Olx, INDEX_I,
     &                     .FALSE., .TRUE., ioUnit )
         WRITE(msgBuf,'(A)') ' OB_Ie = /* Eastern OB local indices */'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         CALL PRINT_LIST_I( OB_Ie(1-Oly,bi,bj), 1-Oly, sNy+Oly, INDEX_J,
     &                     .FALSE., .TRUE., ioUnit )
         WRITE(msgBuf,'(A)') ' OB_Iw = /* Western OB local indices */'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         CALL PRINT_LIST_I( OB_Iw(1-Oly,bi,bj), 1-Oly, sNy+Oly, INDEX_J,
     &                     .FALSE., .TRUE., ioUnit )
        ENDDO
       ENDDO
      ENDIF

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: end summary.'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Check OBCS set-up

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
           STOP 'ABNORMAL END: S/R OBCS_CHECK'
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
           STOP 'ABNORMAL END: S/R OBCS_CHECK'
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
           STOP 'ABNORMAL END: S/R OBCS_CHECK'
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
           STOP 'ABNORMAL END: S/R OBCS_CHECK'
         ENDIF
#endif
        ENDDO
       ENDDO
      _END_MASTER(myThid)

#ifdef ALLOW_CD_CODE
      IF ( useCDscheme ) THEN
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useCDscheme = .TRUE.'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: The CD-scheme does not work with OBCS.'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: Sorry, not yet implemented.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF
#endif /* ALLOW_CD_CODE */

#ifdef ALLOW_ORLANSKI
      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_ORLANSKI'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
#else
      IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.useOrlanskiWest) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: #undef OBCS_RADIATE_ORLANSKI  and'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: one of useOrlanski* logicals is true'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF
#endif /* ALLOW_ORLANSKI */

      IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.useOrlanskiWest) THEN
       IF (nonlinFreeSurf.GT.0) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useOrlanski* Rad OBC with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: nonlinFreeSurf not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CHECK'
       ENDIF
       IF (usePTracers) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useOrlanski* Rad OBC with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: pTracers not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CHECK'
       ENDIF
       IF (useSEAICE) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useOrlanski* Rad OBC with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: SEAICE not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CHECK'
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
     & 'OBCS_CHECK: ERROR: both useOrlanski* and useStevens* logicals'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &      'OBCS_CHECK: ERROR: are true for at least one boundary'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF
      IF (useStevensNorth.OR.useStevensSouth.OR.
     &    useStevensEast.OR.useStevensWest) THEN
       IF (nonlinFreeSurf.GT.0) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useStevens* OBC with'
         CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: nonlinFreeSurf not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CHECK'
       ENDIF
       IF (usePTracers) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: WARNING: useStevens* OBC with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: WARNING: pTracers not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: WARNING: therefore expect the unexpected'
        CALL PRINT_ERROR( msgBuf, myThid )
CML         STOP 'ABNORMAL END: S/R OBCS_CHECK'
       ENDIF
       IF (useSEAICE) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: WARNING: useStevens* OBC with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: WARNING: SEAICE not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: WARNING: therefore expect the unexpected'
        CALL PRINT_ERROR( msgBuf, myThid )
CML         STOP 'ABNORMAL END: S/R OBCS_CHECK'
       ENDIF
CML       IF (usePTracers) THEN
CML        WRITE(msgBuf,'(A)')
CML     &  'OBCS_CHECK: ERROR: useStevens* OBC with'
CML        CALL PRINT_ERROR( msgBuf, myThid )
CML        WRITE(msgBuf,'(A)')
CML     &  'OBCS_CHECK: ERROR: pTracers not yet implemented'
CML         CALL PRINT_ERROR( msgBuf, myThid )
CML         STOP 'ABNORMAL END: S/R OBCS_CHECK'
CML       ENDIF
CML       IF (useSEAICE) THEN
CML        WRITE(msgBuf,'(A)')
CML     &  'OBCS_CHECK: ERROR: useStevens* OBC with'
CML         CALL PRINT_ERROR( msgBuf, myThid )
CML        WRITE(msgBuf,'(A)')
CML     &  'OBCS_CHECK: ERROR: SEAICE not yet implemented'
CML         CALL PRINT_ERROR( msgBuf, myThid )
CML         STOP 'ABNORMAL END: S/R OBCS_CHECK'
CML       ENDIF
      ENDIF
#else
      IF (useStevensNorth.OR.useStevensSouth.OR.
     &    useStevensEast.OR.useStevensWest) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: #undef OBCS_ALLOW_STEVENS  and'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: one of useStevens* logicals is true'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF
#endif /* ALLOW_OBCS_STEVENS */

#ifndef ALLOW_OBCS_PRESCRIBE
      IF (useOBCSprescribe) THEN
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useOBCSprescribe = .TRUE. for'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: undef ALLOW_OBCS_PRESCRIBE'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF
#endif /* ALLOW_OBCS_PRESCRIBE */

#ifndef ALLOW_OBCS_SPONGE
      IF (useOBCSsponge) THEN
       WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &      'Cannot set useOBCSsponge=.TRUE. (data.obcs)'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A)') 'OBCS_CHECK: ',
     &  'with ALLOW_OBCS_SPONGE left undefined (OBCS_OPTIONS.h)'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF
#endif /* ALLOW_OBCS_SPONGE */

#ifndef ALLOW_OBCS_BALANCE
      IF ( useOBCSbalance ) THEN
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: useOBCSbalance requires to define'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ALLOW_OBCS_BALANCE in "OBCS_OPTIONS.h"'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF
#endif /* ALLOW_OBCS_BALANCE */

      IF ( .NOT.nonHydrostatic .AND.
     &         ( OBNwFile.NE.' ' .OR. OBSwFile.NE.' ' .OR.
     &           OBEwFile.NE.' ' .OR. OBWwFile.NE.' ' )
     &   ) THEN
        WRITE(msgBuf,'(2A)')
     &  'OBCS_CHECK: OB*wFile only allowed with nonHydrostatic'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF
      IF ( nonlinFreeSurf.EQ.0 .AND.
     &         ( OBNetaFile.NE.' ' .OR. OBSetaFile.NE.' ' .OR.
     &           OBEetaFile.NE.' ' .OR. OBWetaFile.NE.' ' )
     &   ) THEN
        WRITE(msgBuf,'(2A)')
     &  'OBCS_CHECK: OB*etaFile(s) only allowed with nonlinFreeSurf'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF

      IF (useSEAICE .AND. .NOT. useEXF) THEN
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: for SEAICE OBCS, use'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: pkg/exf to read input files'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: OK'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

#endif /* ALLOW_OBCS */
      RETURN
      END
