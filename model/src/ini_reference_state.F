C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_reference_state.F,v 1.1 2006/11/28 22:47:13 jmc Exp $
C $Name:  $

c #include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_REFERENCE_STATE
C     !INTERFACE:
      SUBROUTINE INI_REFERENCE_STATE( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_REF_STATE
C     | o Initialise reference vertical profile for state
C     |   variables (Pot.Temp., Salinity/Specif.Humid., ....)
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
      _RL    rhoRef(Nr)
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
          WRITE(msgBuf,'(A,I4,A)')
     &      'S/R INI_PARMS: Cannot set both tRef(k=',k,') and tRefFile'
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
        WRITE(msgBuf,'(3A)') 'S/R INI_REFERENCE_STATE:',
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
          WRITE(msgBuf,'(A,I4,A)')
     &      'S/R INI_PARMS: Cannot set both sRef(k=',k,') and sRefFile'
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
        WRITE(msgBuf,'(3A)') 'S/R INI_REFERENCE_STATE:',
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
           rhoRef(k) = tmp4vRef(k)
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
           rhoRef(k) = tmp8vRef(k)
         ENDDO
        ENDIF
        WRITE(msgBuf,'(3A)') 'S/R INI_REFERENCE_STATE:',
     &    ' rhoRef loaded from file: ', rhoRefFile(1:kLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
      ENDIF

C--   Set refrence state density ratio between layer k and top layer
      DO k=1,Nr
        rhoFacC(k) = 1. _d 0
        recip_rhoFacC(k) = 1. _d 0
      ENDDO
      DO k=1,Nr+1
        rhoFacF(k) = 1. _d 0
        recip_rhoFacF(k) = 1. _d 0
      ENDDO

      IF ( rhoRefFile .NE. ' ' ) THEN
        DO k=1,Nr
          rhoFacC(k)=rhoRef(k)/rhoConst
          recip_rhoFacC(k)=1. _d 0/rhoFacC(k)
        ENDDO
        rhoFacF(1)=rhoFacC(1)
C       The first layer ratio has to be 1 both for C and F
        DO k=2,Nr
          rhoFacF(k)=(rhoFacC(k-1)*delR(k)+rhoFacC(k)*delR(k-1))
     &                                        /(delR(k)+delR(k-1))
          recip_rhoFacF(k)=1. _d 0/rhoFacF(k)
        ENDDO
        rhoFacF(Nr+1)=2*rhoFacC(Nr)-rhoFacF(Nr)
        recip_rhoFacF(Nr+1)=1. _d 0/rhoFacF(Nr+1)
      ENDIF

      _END_MASTER(myThid)
C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
