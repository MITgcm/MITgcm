C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_readparms.F,v 1.13 2013/07/18 20:02:08 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_READPARMS
C     !INTERFACE:
      SUBROUTINE CPL_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CPL_READPARMS
C     | o Read Coupling parameters that control import/export
C     |   from/to the coupler layer
C     *==========================================================*
C     |   this version is specific to 1 component (ocean)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "OCNIDS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myThid     :: my Thread Id. number
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE

C     !FUNCTIONS:
c     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C     == Local Variables ==
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
C     k          :: loop counter
C     iL         :: Work variable for length of file-name
C     cpl_earlyExpImpCall :: retired; always call coupler early in call sequence

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
c     INTEGER k, iL
      LOGICAL cpl_earlyExpImpCall

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Coupling parameters:
C     cpl_earlyExpImpCall :: call coupler early in the time stepping call sequence
C     useImportHFlx :: True => use the Imported HeatFlux from couler
C     useImportFW   :: True => use the Imported Fresh Water flux fr cpl
C     useImportTau  :: True => use the Imported Wind-Stress from couler
C     useImportSLP  :: True => use the Imported Sea-level Atmos. Pressure
C     useImportSIce :: True => use the Imported Sea-Ice loading
C     useImportFIce :: True => use the Imported Sea-Ice fraction (DIC-only)
C     useImportCO2  :: True => use the Imported atmos. CO2 from coupler
C     useImportWSpd :: True => use the Imported surface Wind speed fr cpl
C     cpl_taveFreq  :: Frequency^-1 for time-Aver. output (s)
      NAMELIST /CPL_OCN_PARAM/
     &    cpl_earlyExpImpCall,
     &    useImportHFlx, useImportFW, useImportTau,
     &    useImportSLP, useImportSIce, useImportFIce,
     &    useImportCO2, useImportWSpd,
     &    cpl_taveFreq, cpl_snapshot_mnc, cpl_timeave_mnc

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C--   Open the data file
      WRITE(msgBuf,'(A)') ' CPL_READPARMS: opening data.cpl'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE( 'data.cpl', 'CPL_READPARMS',
     O                          iUnit, myThid )

C-    Set default value:
      cpl_earlyExpImpCall = .TRUE.
      useImportHFlx = .TRUE.
      useImportFW   = .TRUE.
      useImportTau  = .TRUE.
      useImportSLP  = .TRUE.
      useImportSIce = .TRUE.
      IF ( ocnCpl_exchange_DIC ) THEN
        useImportFIce = .TRUE.
        useImportCO2  = .TRUE.
        useImportWSpd = .TRUE.
      ELSE
        useImportFIce = .FALSE.
        useImportCO2  = .FALSE.
        useImportWSpd = .FALSE.
      ENDIF
      cpl_taveFreq  = taveFreq
#ifdef ALLOW_MNC
      cpl_snapshot_mnc = snapshot_mnc
      cpl_timeave_mnc  = timeave_mnc
#else
      cpl_snapshot_mnc = .FALSE.
      cpl_timeave_mnc  = .FALSE.
#endif

C--   Read parameters from open data file:

C-    Parameters for coupling interface:
      READ(UNIT=iUnit,NML=CPL_OCN_PARAM)

      WRITE(msgBuf,'(A)')
     &   ' CPL_READPARMS: finished reading data.cpl'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

C--   Close the open data file
      CLOSE(iUnit)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Check for retired parameters:
      IF ( .NOT.cpl_earlyExpImpCall ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &   'Parameter "cpl_earlyExpImpCall" has been retired;'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &   '=> always call coupler early in sequence of calls'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CPL_READPARMS'
      ENDIF

C--   Check parameters and model configuration

C-    If land_taveFreq is positive, then must compile the land-diagnostics code
#ifndef ALLOW_TIMEAVE
      IF (cpl_taveFreq.GT.0.) THEN
        WRITE(msgBuf,'(A)')
     &  'CPL_READPARMS: cpl_taveFreq > 0 but not compiled pkg/timeave'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(A)')
     &  'Re-compile with pkg "timeave" in packages.conf'
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R CPL_READPARMS'
      ENDIF
#endif /* ALLOW_TIMEAVE */

#ifndef ATMOSPHERIC_LOADING
      iUnit = errorMessageUnit
      IF ( useImportSLP ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CPL_READPARMS:',
     &                       ' useImportSLP is set but'
        CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( useImportSIce ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CPL_READPARMS:',
     &                       ' useImportSIce is set but'
        CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
      IF ( useImportSLP .OR. useImportSIce ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CPL_READPARMS:',
     &                       ' pressure loading code is not active.'
        CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
C        WRITE(msgBuf,'(2A)') '** WARNING **',
C     &              ' Re-compile with:  #define ATMOSPHERIC_LOADING'
C        CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
#endif /* ATMOSPHERIC_LOADING */

      IF ( ( useImportFice.OR.useImportCO2.OR.useImportWSpd ) .AND.
     &    (.NOT.ocnCpl_exchange_DIC) ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: useImport',
     &    ' with DIC variables requires ocnCpl_exchange_DIC TRUE'
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R CPL_READPARMS'
      ENDIF

C     Set IO flags
      cpl_snapshot_mdsio = .TRUE.
      cpl_timeave_mdsio  = .TRUE.
#ifdef ALLOW_MNC
      IF (useMNC) THEN
        IF ( .NOT.outputTypesInclusive
     &       .AND. cpl_snapshot_mnc )  cpl_snapshot_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. cpl_timeave_mnc )   cpl_timeave_mdsio  = .FALSE.
      ENDIF
#endif

C-    derive other parameters:

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Print out parameter values :

      iUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// Coupling package parameters :'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)

C- namelist CPL_OCN_PARAM:
       CALL WRITE_0D_L( useImportHFlx, INDEX_NONE,
     &                 'useImportHFlx=',
     &   ' /* use Imported Heat-Flx fr Coupler on/off flag */')
       CALL WRITE_0D_L( useImportFW  , INDEX_NONE,
     &                 'useImportFW  =',
     &   ' /* use Imported Fresh-Water fr Cpl. on/off flag */')
       CALL WRITE_0D_L( useImportTau , INDEX_NONE,
     &                 'useImportTau =',
     &   ' /* use Imported Wind-Stress fr Cpl. on/off flag */')
       CALL WRITE_0D_L( useImportSLP , INDEX_NONE,
     &                 'useImportSLP =',
     &   ' /* use Imported Sea-level Atm Press on/off flag */')
       CALL WRITE_0D_L( useImportSIce , INDEX_NONE,
     &                 'useImportSIce=',
     &   ' /* use Imported Sea-Ice loading on/off flag */')
       CALL WRITE_0D_L( useImportFIce , INDEX_NONE,
     &                 'useImportFIce=',
     &   ' /* use Imported Sea-Ice Fract fr Cpl. on/off flag */')
       CALL WRITE_0D_L( useImportCO2  , INDEX_NONE,
     &                 'useImportCO2 =',
     &   ' /* use Imported Atmos. CO2 fr Cpl. on/off flag */')
       CALL WRITE_0D_L( useImportWSpd , INDEX_NONE,
     &                 'useImportWSpd =',
     &   ' /* use Imported Windspeed fr Cpl. on/off flag */')
       CALL WRITE_0D_RL( cpl_taveFreq, INDEX_NONE, 'cpl_taveFreq =',
     &   ' /* Frequency^-1 for time-Aver. output (s) */')
       CALL WRITE_0D_L( cpl_timeave_mnc , INDEX_NONE,
     &                 'cpl_timeave_mnc =',
     &   ' /* write TimeAv to MNC file on/off flag */')
       CALL WRITE_0D_L( cpl_timeave_mdsio , INDEX_NONE,
     &                 'cpl_timeave_mdsio =',
     &   ' /* write TimeAv to MDSIO file on/off flag */')

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* COMPONENT_MODULE */

      RETURN
      END
