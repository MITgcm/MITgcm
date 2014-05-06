C $Header: /u/gcmpack/MITgcm/model/src/load_ref_files.F,v 1.3 2014/05/06 15:47:05 jmc Exp $
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

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     k          :: loop index
C     msgBuf     :: Informational/error message buffer
      _RL    tracerDefault
      INTEGER  k, kLen
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BEGIN_MASTER( myThid )

C--   Set reference Potential Temperature
      IF ( tRefFile .EQ. ' ' ) THEN
C-    set default vertical profile for temperature: tRef
        tracerDefault = 20.
        IF ( fluidIsAir ) tracerDefault = 300.
        IF ( thetaConst.NE.UNSET_RL ) tracerDefault = thetaConst
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
        kLen = ILNBLNK(tRefFile)
        CALL READ_GLVEC_RL( tRefFile, ' ', tRef, Nr, 1, myThid )
        WRITE(msgBuf,'(3A)') 'S/R LOAD_REF_FILES:',
     &    ' tRef loaded from file: ', tRefFile(1:kLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF
      IF ( thetaConst.EQ.UNSET_RL ) thetaConst = tRef(1)

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
        kLen = ILNBLNK(sRefFile)
        CALL READ_GLVEC_RL( sRefFile, ' ', sRef, Nr, 1, myThid )
        WRITE(msgBuf,'(3A)') 'S/R LOAD_REF_FILES:',
     &    ' sRef loaded from file: ', sRefFile(1:kLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF

C--   Set reference Density
      IF ( rhoRefFile .NE. ' ' ) THEN
        kLen = ILNBLNK(rhoRefFile)
C-    read from file:
        CALL READ_GLVEC_RL( rhoRefFile, ' ', rho1Ref, Nr, 1, myThid )
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
