C $Header: /u/gcmpack/MITgcm/model/src/load_ref_files.F,v 1.1 2008/09/05 20:15:28 jmc Exp $
C $Name:  $

c #include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: LOAD_REF_FILES
C     !INTERFACE:
      SUBROUTINE LOAD_REF_FILES( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LOAD_REF_FILES
C     | o Read reference vertical profile from files
C     |   (Pot.Temp., Salinity/Specif.Humid., density ... )
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid     :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     k          :: loop index
C     msgBuf     :: Informational/error message buffer
C     tmp4vRef   :: temporary arrays to read in ref. vertical profile
C     tmp8vRef   :: temporary arrays to read in ref. vertical profile
C     iUnit      :: Work variable for IO unit number
      REAL*4 tmp4vRef(Nr)
      REAL*8 tmp8vRef(Nr)
      _RL    tracerDefault
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER  k, kLen, iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BEGIN_MASTER( myThid )

C--   Set reference Potential Temperature
      IF ( tRefFile .EQ. ' ' ) THEN
C-    set default vertical profile for temperature: tRef
        tracerDefault = 20.
        IF ( fluidIsAir ) tracerDefault = 300.
        DO k=1,Nr
          IF (tRef(k).EQ.UNSET_RL) tRef(k) = tracerDefault
          tracerDefault = tRef(k)
        ENDDO
      ELSE
C-    check for multiple definitions:
        DO k=1,Nr
         IF (tRef(k).NE.UNSET_RL) THEN
          WRITE(msgBuf,'(2A,I4,A)') 'S/R LOAD_REF_FILES:',
     &      ' Cannot set both tRef(k=', k, ') and tRefFile'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R INI_PARMS'
         ENDIF
        ENDDO
      ENDIF
C-    read from file:
      IF ( tRefFile .NE. ' ' ) THEN
        CALL MDSFINDUNIT( iUnit, myThid )
        kLen = ILNBLNK(tRefFile)
        IF (readBinaryPrec.EQ.precFloat32) THEN
         OPEN(iUnit, FILE=tRefFile(1:kLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=WORDLENGTH*Nr)
         READ(iUnit,rec=1) tmp4vRef
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR4( Nr, tmp4vRef )
#endif
         DO k=1,Nr
           tRef(k) = tmp4vRef(k)
         ENDDO
        ELSEIF (readBinaryPrec.EQ.precFloat64) THEN
         OPEN(iUnit, FILE=tRefFile(1:kLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=WORDLENGTH*2*Nr)
         READ(iUnit,rec=1) tmp8vRef
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR8( Nr, tmp8vRef )
#endif
         DO k=1,Nr
           tRef(k) = tmp8vRef(k)
         ENDDO
        ENDIF
        WRITE(msgBuf,'(3A)') 'S/R LOAD_REF_FILES:',
     &    ' tRef loaded from file: ', tRefFile(1:kLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF

C--   Set reference Salinity/Specific Humidity
      IF ( sRefFile .EQ. ' ' ) THEN
C-    set default vertical profile for salinity/water-vapour: sRef
        tracerDefault = 30.
        IF ( fluidIsAir ) tracerDefault = 0.
        DO k=1,Nr
          IF (sRef(k).EQ.UNSET_RL) sRef(k) = tracerDefault
          tracerDefault = sRef(k)
        ENDDO
      ELSE
C-    check for multiple definitions:
        DO k=1,Nr
         IF (sRef(k).NE.UNSET_RL) THEN
          WRITE(msgBuf,'(2A,I4,A)') 'S/R LOAD_REF_FILES:',
     &      ' Cannot set both sRef(k=', k, ') and sRefFile'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R INI_PARMS'
         ENDIF
        ENDDO
      ENDIF
C-    read from file:
      IF ( sRefFile .NE. ' ' ) THEN
        CALL MDSFINDUNIT( iUnit, myThid )
        kLen = ILNBLNK(sRefFile)
        IF (readBinaryPrec.EQ.precFloat32) THEN
         OPEN(iUnit, FILE=sRefFile(1:kLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=WORDLENGTH*Nr)
         READ(iUnit,rec=1) tmp4vRef
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR4( Nr, tmp4vRef )
#endif
         DO k=1,Nr
           sRef(k) = tmp4vRef(k)
         ENDDO
        ELSEIF (readBinaryPrec.EQ.precFloat64) THEN
         OPEN(iUnit, FILE=sRefFile(1:kLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=WORDLENGTH*2*Nr)
         READ(iUnit,rec=1) tmp8vRef
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR8( Nr, tmp8vRef )
#endif
         DO k=1,Nr
           sRef(k) = tmp8vRef(k)
         ENDDO
        ENDIF
        WRITE(msgBuf,'(3A)') 'S/R LOAD_REF_FILES:',
     &    ' sRef loaded from file: ', sRefFile(1:kLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF

C--   Set reference Density
      IF ( rhoRefFile .NE. ' ' ) THEN
C-    read from file:
        CALL MDSFINDUNIT( iUnit, myThid )
        kLen = ILNBLNK(rhoRefFile)
        IF (readBinaryPrec.EQ.precFloat32) THEN
         OPEN(iUnit, FILE=rhoRefFile(1:kLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=WORDLENGTH*Nr)
         READ(iUnit,rec=1) tmp4vRef
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR4( Nr, tmp4vRef )
#endif
         DO k=1,Nr
           rho1Ref(k) = tmp4vRef(k)
         ENDDO
        ELSEIF (readBinaryPrec.EQ.precFloat64) THEN
         OPEN(iUnit, FILE=rhoRefFile(1:kLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=WORDLENGTH*2*Nr)
         READ(iUnit,rec=1) tmp8vRef
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR8( Nr, tmp8vRef )
#endif
         DO k=1,Nr
           rho1Ref(k) = tmp8vRef(k)
         ENDDO
        ENDIF
        WRITE(msgBuf,'(3A)') 'S/R LOAD_REF_FILES:',
     &    ' rho1Ref loaded from file: ', rhoRefFile(1:kLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
      ENDIF

      _END_MASTER(myThid)
C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
