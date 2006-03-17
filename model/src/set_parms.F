C $Header: /u/gcmpack/MITgcm/model/src/set_parms.F,v 1.1 2006/03/17 16:12:55 jmc Exp $
C $Name:  $

c #include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SET_PARMS
C     !INTERFACE:
      SUBROUTINE SET_PARMS( myThid )

C     !DESCRIPTION:
C     Set model "parameters" that might depend on the use of some pkgs;
C     called from INITIALISE_FIXED, after INI_PARMS & PACKAGES_READPARAMS
C     NOTES: After leaving this S/R, parameters will not change anymore.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid - Number of this instance of SET_PARMS
      INTEGER myThid

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
c     INTEGER IL, iUnit
c     INTEGER  ILNBLNK
c     EXTERNAL ILNBLNK

CEOP

      _BEGIN_MASTER(myThid)

      IF ( useOffLine ) THEN
        WRITE(msgBuf,'(A,A)') 'S/R SET_PARMS: ',
     &    ' Off-Line => turn off Temp,Salt & Mom_Stepping flags'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , 1)
        tempStepping = .FALSE.
        saltStepping = .FALSE.
        momStepping  = .FALSE.
      ENDIF

C--   Set (or reset) On/Off flags :

C--   On/Off flags for each terms of the momentum equation
      nonHydrostatic   = momStepping .AND. nonHydrostatic
      quasiHydrostatic = momStepping .AND. quasiHydrostatic
      momAdvection = momStepping .AND. momAdvection
      momViscosity = momStepping .AND. momViscosity
      momForcing   = momStepping .AND. momForcing
      useCoriolis  = momStepping .AND. useCoriolis
      useCDscheme  = momStepping .AND. useCDscheme
      momPressureForcing= momStepping .AND. momPressureForcing
      implicitIntGravWave=momPressureForcing .AND. implicitIntGravWave
      momImplVertAdv   = momAdvection .AND. momImplVertAdv
      implicitViscosity= momViscosity .AND. implicitViscosity
      use3Dsolver      = nonHydrostatic.OR. implicitIntGravWave
C--   Momentum viscosity on/off flag.
      IF ( momViscosity        ) THEN
       vfFacMom = 1.D0
      ELSE
       vfFacMom = 0.D0
      ENDIF
C--   Momentum advection on/off flag.
      IF ( momAdvection        ) THEN
       afFacMom = 1.D0
      ELSE
       afFacMom = 0.D0
      ENDIF
C--   Momentum forcing on/off flag.
      IF ( momForcing ) THEN
       foFacMom = 1.D0
      ELSE
       foFacMom = 0.D0
      ENDIF
C--   Coriolis term on/off flag.
      IF ( useCoriolis ) THEN
       cfFacMom = 1.D0
      ELSE
       cfFacMom = 0.D0
      ENDIF
C--   Pressure term on/off flag.
      IF ( momPressureForcing ) THEN
       pfFacMom = 1.D0
      ELSE
       pfFacMom = 0.D0
      ENDIF
C--   Metric terms on/off flag.
      IF ( metricTerms ) THEN
       mTFacMom = 1.D0
      ELSE
       mTFacMom = 0.D0
      ENDIF

C--   Advection and Forcing for Temp and salt  on/off flags
      tempAdvection = tempStepping .AND. tempAdvection
      tempForcing   = tempStepping .AND. tempForcing
      saltAdvection = saltStepping .AND. saltAdvection
      saltForcing   = saltStepping .AND. saltForcing
      tempImplVertAdv = tempAdvection .AND. tempImplVertAdv
      saltImplVertAdv = saltAdvection .AND. saltImplVertAdv

C--   When using the dynamical pressure in EOS (with Z-coord.),
C     needs to activate specific part of the code (restart & exchange)
c     useDynP_inEos_Zc = .FALSE.
      useDynP_inEos_Zc = ( fluidIsWater .AND. usingZCoords
     &              .AND. ( eosType .EQ. 'JMD95P' .OR.
     &                      eosType .EQ. 'UNESCO' .OR.
     &                      eosType .EQ. 'MDJWF'       )  )

C--  After this point, main model parameters are not supposed to be modified.
       WRITE(msgBuf,'(A,A)') 'SET_PARMS: done'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , 1)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be set
      _BARRIER

      RETURN
      END
