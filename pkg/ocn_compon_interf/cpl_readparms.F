#include "OCN_CPL_OPTIONS.h"

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
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid     :: my Thread Id. number
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     !FUNCTIONS:
c     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
C     k          :: loop counter
C     iL         :: Work variable for length of file-name
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
c     INTEGER k, iL

C-    retired parameters:
C     cpl_earlyExpImpCall :: retired; always call coupler early in call sequence
C     cpl_timeave_mnc :: retired;
C     cpl_taveFreq    :: retired; Frequency^-1 for time-Aver. output (s)
      LOGICAL cpl_earlyExpImpCall
      LOGICAL cpl_timeave_mnc
      _RL cpl_taveFreq

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Coupling parameters:
C     useImportHFlx :: True => use the Imported HeatFlux from couler
C     useImportFW   :: True => use the Imported Fresh Water flux fr cpl
C     useImportTau  :: True => use the Imported Wind-Stress from couler
C     useImportSLP  :: True => use the Imported Sea-level Atmos. Pressure
C     useImportRunOff :: True => use the Imported RunOff flux from coupler
C     useImportSIce   :: True => use the Imported Sea-Ice loading from coupler
C     useImportThSIce :: True => use the Imported thSIce state vars from coupler
C     useImportSltPlm :: True => use the Imported Salt-Plume flux from coupler
C     useImportFice   :: True => use the Imported Seaice fraction (DIC-only)
C     useImportCO2    :: True => use the Imported atmos. CO2 from coupler
C     useImportWSpd   :: True => use the Imported surf. Wind speed from coupler
      NAMELIST /CPL_OCN_PARAM/
     &    cpl_earlyExpImpCall,
     &    useImportHFlx, useImportFW, useImportTau,
     &    useImportSLP,  useImportRunOff,
     &    useImportSIce, useImportThSIce, useImportSltPlm,
     &    useImportFice, useImportCO2, useImportWSpd,
     &    cpl_taveFreq, cpl_snapshot_mnc, cpl_timeave_mnc

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C--   Open the data file
      WRITE(msgBuf,'(A)') ' CPL_READPARMS: opening data.cpl'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1 )

      CALL OPEN_COPY_DATA_FILE( 'data.cpl', 'CPL_READPARMS',
     O                          iUnit, myThid )

C-    Set default value:
      useImportHFlx   = .TRUE.
      useImportFW     = .TRUE.
      useImportTau    = .TRUE.
      useImportSLP    = .TRUE.
      useImportRunOff = cpl_exchange_RunOff.EQ.3
      useImportSIce   = cpl_exchange1W_sIce.EQ.3
      useImportThSIce = cpl_exchange2W_sIce.EQ.3
      useImportSltPlm = cpl_exchange_SaltPl.EQ.3
      useImportFice   =( cpl_exchange_DIC .EQ. 3 .AND.
     &                  cpl_exchange1W_sIce.EQ.3 )
c    &             .OR. cpl_exchange2W_sIce.EQ.3
      useImportCO2    =  cpl_exchange_DIC .EQ. 3
      useImportWSpd   =  cpl_exchange_DIC .EQ. 3
#ifdef ALLOW_MNC
      cpl_snapshot_mnc = useMNC .AND. snapshot_mnc
#else
      cpl_snapshot_mnc = .FALSE.
#endif
C-    retired params default:
      cpl_earlyExpImpCall = .TRUE.
      cpl_timeave_mnc = .FALSE.
      cpl_taveFreq    = UNSET_RL

C--   Read parameters from open data file:

C-    Parameters for coupling interface:
      READ(UNIT=iUnit,NML=CPL_OCN_PARAM)

      WRITE(msgBuf,'(A)')
     &   ' CPL_READPARMS: finished reading data.cpl'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1 )

C--   Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Check for retired parameters:
      IF ( .NOT.cpl_earlyExpImpCall ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &   'Parameter "cpl_earlyExpImpCall" has been retired;'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &   '=> always call coupler early in sequence of calls'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
      IF ( cpl_taveFreq .NE. UNSET_RL ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: "cpl_taveFreq"',
     &    ' is no longer allowed in file "data.cpl"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' since "pkg/timeave" has been removed.'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
      IF ( cpl_timeave_mnc ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: "cpl_timeave_mnc"',
     &    ' is no longer allowed in file "data.cpl"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' since "pkg/timeave" has been removed.'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF

C--   Check parameters and model configuration

#ifndef ALLOW_DIC
      IF ( ocn_cplExch_RunOff ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: needs',
     &    ' to compile pkg/dic to use: ocn_cplExch_RunOff=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' (set from Coupler "data.cpl": cpl_exchange_RunOff=1,3)'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
#endif /* ndef ALLOW_DIC */
#ifndef ALLOW_THSICE
      IF ( ocn_cplExch2W_sIce ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: needs',
     &    ' to compile pkg/thsice to use: ocn_cplExch2W_sIce=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' (set from Coupler "data.cpl": cpl_exchange2W_sIce=1,3)'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
#endif /* ndef ALLOW_THSICE */
#ifndef ALLOW_SALT_PLUME
      IF ( ocn_cplExch_SaltPl ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: needs',
     &    ' compiled pkg/salt_plume to use: ocn_cplExch_SaltPl=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' (set from Coupler "data.cpl": cpl_exchange_SaltPl=1,3)'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
#endif /* ndef ALLOW_SALT_PLUME */
#ifndef ALLOW_DIC
      IF ( ocn_cplExch_DIC ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: needs',
     &    ' to compile pkg/dic to use: ocn_cplExch_DIC = T'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' (set from Coupler "data.cpl": cpl_exchange_DIC = 1,3)'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
#endif /* ndef ALLOW_DIC */

#ifndef ATMOSPHERIC_LOADING
      iUnit = errorMessageUnit
      IF ( useImportSLP ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CPL_READPARMS:',
     &                       ' useImportSLP is set but'
        CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      ELSEIF ( useImportSIce ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CPL_READPARMS:',
     &                       ' useImportSIce is set but'
        CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( useImportSLP .OR. useImportSIce ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CPL_READPARMS:',
     &                       ' pressure loading code is not active.'
        CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
C        WRITE(msgBuf,'(2A)') '** WARNING **',
C     &              ' Re-compile with:  #define ATMOSPHERIC_LOADING'
C        CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
#endif /* ATMOSPHERIC_LOADING */

      IF ( useImportRunOff .AND. cpl_exchange_RunOff.NE.3 ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: useImportRunOff',
     &    ' requires setting'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' cpl_exchange_RunOff = 3 (in Coupler "data.cpl")'
        CALL PRINT_ERROR( msgBuf, myThid)
        cplErrorCount = cplErrorCount + 1
      ENDIF
      IF ( ( useImportSIce .OR.
     &       useImportFice.AND.cpl_exchange2W_sIce.NE.3 )
     &   .AND. cpl_exchange1W_sIce.NE.3 ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: useImport',
     &    ' SIce-Mass/Frac requires setting'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' cpl_exchange1W_sIce = 3 (in Coupler "data.cpl")'
        CALL PRINT_ERROR( msgBuf, myThid)
        cplErrorCount = cplErrorCount + 1
      ENDIF
      IF ( useImportThSIce .AND. .NOT.ocn_cplExch2W_sIce ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: useImportThSIce',
     &    ' requires setting'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' cpl_exchange2W_sIce=1,3 (in Coupler "data.cpl")'
        CALL PRINT_ERROR( msgBuf, myThid)
        cplErrorCount = cplErrorCount + 1
      ELSEIF ( useImportThSIce .AND. cpl_exchange2W_sIce.NE.3 ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CPL_READPARMS: ',
     &    'useImportThSIce useless without'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** CPL_READPARMS: ',
     &    ' cpl_exchange2W_sIce = 3 (in Coupler "data.cpl")'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( useImportSltPlm .AND. cpl_exchange_SaltPl.NE.3 ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: useImportSltPlm',
     &    ' requires setting'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' cpl_exchange_SaltPl = 3 (in Coupler "data.cpl")'
        CALL PRINT_ERROR( msgBuf, myThid)
        cplErrorCount = cplErrorCount + 1
      ENDIF
      IF ( ( useImportFice.AND.cpl_exchange2W_sIce.NE.3
     &       .OR. useImportCO2 .OR. useImportWSpd )
     &   .AND. cpl_exchange_DIC.NE.3 ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: useImport',
     &    ' with DIC variables requires setting'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' cpl_exchange_DIC = 3 (in Coupler "data.cpl")'
        CALL PRINT_ERROR( msgBuf, myThid)
        cplErrorCount = cplErrorCount + 1
      ENDIF

C     Set IO flags
      cpl_snapshot_mdsio = .TRUE.
#ifdef ALLOW_MNC
      cpl_snapshot_mnc = useMNC .AND. cpl_snapshot_mnc
      cpl_snapshot_mdsio = ( .NOT.cpl_snapshot_mnc )
     &                     .OR. outputTypesInclusive
#endif

C-    derive other parameters:

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Print out set-up summary

      iUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// Coupling set-up summary :'
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

C-    o Print Coupler-Exchange config (set from params in coupler 'data.cpl')
       WRITE(msgBuf,'(A)') '// -------'
       CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &        '// Coupler-exchange switch (received from coupler):'
       CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

       CALL WRITE_0D_L( ocn_cplSequential, INDEX_NONE,
     &                 'ocn_cplSequential =',
     &   ' /* use Sequential Coupling Exchange on/off flag */')

       CALL WRITE_0D_L( ocn_cplExch_RunOff, INDEX_NONE,
     &                 'ocn_cplExch_RunOff =',
     &     ' /* exchange RunOff fields with coupler on/off */')
       CALL WRITE_0D_L( ocn_cplExch1W_sIce, INDEX_NONE,
     &                 'ocn_cplExch1W_sIce =',
     &     ' /* 1-way exchange of seaice vars with coupler */')
       CALL WRITE_0D_L( ocn_cplExch2W_sIce, INDEX_NONE,
     &                 'ocn_cplExch2W_sIce =',
     &     ' /* 2-way exchange of ThSIce vars with coupler */')
       CALL WRITE_0D_L( ocn_cplExch_SaltPl, INDEX_NONE,
     &                 'ocn_cplExch_SaltPl =',
     &     ' /* exchange Salt-Plume fields with coupler */')
       CALL WRITE_0D_L( ocn_cplExch_DIC, INDEX_NONE,
     &                 'ocn_cplExch_DIC    =',
     &     ' /* exchange DIC    fields with coupler on/off */')

C-    print namelist parameter value:
       WRITE(msgBuf,'(A)') '// -------'
       CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &        '// Coupler parameters (from local param file):'
       CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

       CALL WRITE_0D_L( useImportHFlx, INDEX_NONE,
     &                 'useImportHFlx  =',
     &   ' /* use Imported Heat-Flx fr Coupler on/off flag */')
       CALL WRITE_0D_L( useImportFW  , INDEX_NONE,
     &                 'useImportFW    =',
     &   ' /* use Imported Fresh-Water fr Cpl. on/off flag */')
       CALL WRITE_0D_L( useImportTau , INDEX_NONE,
     &                 'useImportTau   =',
     &   ' /* use Imported Wind-Stress fr Cpl. on/off flag */')
       CALL WRITE_0D_L( useImportSLP , INDEX_NONE,
     &                 'useImportSLP   =',
     &   ' /* use Imported Sea-level Atm Press on/off flag */')
       CALL WRITE_0D_L( useImportRunOff , INDEX_NONE,
     &                 'useImportRunOff=',
     &   ' /* use Imported Run-Off fr Cpl. on/off flag */')
       CALL WRITE_0D_L( useImportSIce , INDEX_NONE,
     &                 'useImportSIce  =',
     &   ' /* use Imported Sea-Ice loading on/off flag */')
       CALL WRITE_0D_L( useImportThSIce , INDEX_NONE,
     &                 'useImportThSIce=',
     &   ' /* use Imported thSIce state-var on/off flag */')
       CALL WRITE_0D_L( useImportSltPlm , INDEX_NONE,
     &                 'useImportSltPlm=',
     &   ' /* use Imported Salt-Plume flux on/off flag */')
       CALL WRITE_0D_L( useImportFice , INDEX_NONE,
     &                 'useImportFice  =',
     &   ' /* use Imported Seaice Frac (DIC-only) flag */')
       CALL WRITE_0D_L( useImportCO2  , INDEX_NONE,
     &                 'useImportCO2   =',
     &   ' /* use Imported Atmos. CO2 fr Cpl. on/off flag */')
       CALL WRITE_0D_L( useImportWSpd , INDEX_NONE,
     &                 'useImportWSpd  =',
     &   ' /* use Imported Windspeed fr Cpl. on/off flag */')

      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// End of Coupling set-up summary'
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* COMPONENT_MODULE */

      RETURN
      END
