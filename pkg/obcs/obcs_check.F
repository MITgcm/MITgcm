C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_check.F,v 1.16 2010/10/25 22:47:26 jmc Exp $
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
      INTEGER i,j,bi,bj
      INTEGER k1,k2

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_OBCS'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      IF ( debugLevel.GE.debLevB ) THEN
       _BEGIN_MASTER( myThid )
       DO bj = 1,nSy
        DO bi = 1,nSx
         WRITE(msgBuf,'(A,2I4,A)') 'OBC N : bi,bj=',bi,bj,' , OB_Jn:'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         DO k1=1,sNx,100
          k2 = MIN(sNx,k1+99)
          WRITE(standardMessageUnit,*) '=',(OB_Jn(i,bi,bj),i=k1,k2)
         ENDDO
         WRITE(msgBuf,'(A,2I4,A)') 'OBC S : bi,bj=',bi,bj,' , OB_Js:'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         DO k1=1,sNx,100
          k2 = MIN(sNx,k1+99)
          WRITE(standardMessageUnit,*) '=',(OB_Js(i,bi,bj),i=k1,k2)
         ENDDO
         WRITE(msgBuf,'(A,2I4,A)') 'OBC E : bi,bj=',bi,bj,' , OB_Ie:'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         DO k1=1,sNy,100
          k2 = MIN(sNy,k1+99)
          WRITE(standardMessageUnit,*) '=',(OB_Ie(j,bi,bj),j=k1,k2)
         ENDDO
         WRITE(msgBuf,'(A,2I4,A)') 'OBC W : bi,bj=',bi,bj,' , OB_Iw:'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         DO k1=1,sNy,100
          k2 = MIN(sNy,k1+99)
          WRITE(standardMessageUnit,*) '=',(OB_Iw(j,bi,bj),j=k1,k2)
         ENDDO
         WRITE(msgBuf,'(A)') ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDDO
       ENDDO
       _END_MASTER(myThid)
      ENDIF

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
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)
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
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,
     &                    SQUEEZE_RIGHT,myThid )

#endif /* ALLOW_OBCS */
      RETURN
      END
