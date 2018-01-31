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
#include "GRID.h"

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
          WRITE(msgBuf,'(2A,I4,A)') 'S/R LOAD_REF_FILES: ',
     &      'Cannot set both tRef(k=', k, ') and tRefFile'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R INI_PARMS'
         ENDIF
        ENDDO
      ENDIF
C-    read from file:
      IF ( tRefFile .NE. ' ' ) THEN
        kLen = ILNBLNK(tRefFile)
        CALL READ_GLVEC_RL( tRefFile, ' ', tRef, Nr, 1, myThid )
        WRITE(msgBuf,'(3A)') 'S/R LOAD_REF_FILES: ',
     &    'tRef loaded from file: ', tRefFile(1:kLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
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
          WRITE(msgBuf,'(2A,I4,A)') 'S/R LOAD_REF_FILES: ',
     &      'Cannot set both sRef(k=', k, ') and sRefFile'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R INI_PARMS'
         ENDIF
        ENDDO
      ENDIF
C-    read from file:
      IF ( sRefFile .NE. ' ' ) THEN
        kLen = ILNBLNK(sRefFile)
        CALL READ_GLVEC_RL( sRefFile, ' ', sRef, Nr, 1, myThid )
        WRITE(msgBuf,'(3A)') 'S/R LOAD_REF_FILES: ',
     &    'sRef loaded from file: ', sRefFile(1:kLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

C--   Set reference Density
      IF ( rhoRefFile .NE. ' ' ) THEN
        kLen = ILNBLNK(rhoRefFile)
C-    read from file:
        CALL READ_GLVEC_RL( rhoRefFile, ' ', rho1Ref, Nr, 1, myThid )
        WRITE(msgBuf,'(3A)') 'S/R LOAD_REF_FILES: ',
     &    'rho1Ref loaded from file: ', rhoRefFile(1:kLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Set gravity vertical profile
      IF ( gravityFile .NE. ' ' ) THEN
        kLen = ILNBLNK(gravityFile)
C-    read from file and store, for now, in gravFacC:
        CALL READ_GLVEC_RL( gravityFile, ' ', gravFacC, Nr, 1, myThid )
        WRITE(msgBuf,'(3A)') 'S/R LOAD_REF_FILES: ',
     &    'gravity vert. prof. loaded from file: ', gravityFile(1:kLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )

C-    Set gravity vertical profile factor: assume surface-interface
C     density-factor to be 1 (grav. acceleration @ rF(1) = gravity)
C- Note: done here (instead of in set_ref_state.F) since gravity fac 
C       might be needed to initialise EOS coeffs (in ini_eos.F)
        gravFacF(1)   = 1. _d 0
C       gravFac(k) = gravity ratio between layer k and top interface
        DO k=1,Nr
          gravFacC(k) = gravFacC(k)*recip_gravity
        ENDDO
        DO k=2,Nr
C       since rkSign=-1, recip_drC(k) = 1./(rC(k-1)-rC(k))
          gravFacF(k) = ( gravFacC(k-1)*(rF(k)-rC(k))
     &                  + gravFacC(k)*(rC(k-1)-rF(k)) )*recip_drC(k)
        ENDDO
C       extrapolate down to the bottom:
        gravFacF(Nr+1) = ( gravFacC(Nr)*(rF(Nr+1)-rF(Nr))
     &                   + gravFacF(Nr)*(rC(Nr)-rF(Nr+1))
     &                   ) / (rC(Nr)-rF(Nr))
C-      set reciprocal gravity-factor:
        DO k=1,Nr
          recip_gravFacC(k) = 1. _d 0/gravFacC(k)
        ENDDO
        DO k=1,Nr+1
          recip_gravFacF(k) = 1. _d 0/gravFacF(k)
        ENDDO

      ELSE
C-    Initialise gravity vertical profile factor:
        DO k=1,Nr
          gravFacC(k) = 1. _d 0
          recip_gravFacC(k) = 1. _d 0
        ENDDO
        DO k=1,Nr+1
          gravFacF(k) = 1. _d 0
          recip_gravFacF(k) = 1. _d 0
        ENDDO
      ENDIF

      _END_MASTER(myThid)
C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
