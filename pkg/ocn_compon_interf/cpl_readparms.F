C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_readparms.F,v 1.7 2006/09/15 15:45:18 jscott Exp $
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
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==       
C     myThid -  Number of this instance
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE

C Functions
      INTEGER ILNBLNK

C     == Local Variables == 
C     msgBuf     :: Informational/error meesage buffer
C     iUnit      :: Work variable for IO unit number
C     k          :: loop counter
C     iL         :: Work variable for length of file-name
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, k, iL

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Coupling parameters: 
C     cpl_earlyExpImpCall :: call coupler early in the time stepping call sequence
C     useImportHFlx :: True => use the Imported HeatFlux from couler
C     useImportFW   :: True => use the Imported Fresh Water flux fr cpl
C     useImportTau  :: True => use the Imported Wind-Stress from couler
C     useImportSLP  :: True => use the Imported Sea-level Atmos. Pressure
C     useImportSIce :: True => use the Imported Sea-Ice loading
C     cpl_taveFreq  :: Frequency^-1 for time-Aver. output (s)
      NAMELIST /CPL_OCN_PARAM/
     &    cpl_earlyExpImpCall,
     &    useImportHFlx, useImportFW, useImportTau,
     &    useImportSLP, useImportSIce,
     &    cpl_taveFreq, cpl_snapshot_mnc, cpl_timeave_mnc

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Set default value:
      cpl_earlyExpImpCall = .TRUE.
      useImportHFlx = .TRUE.
      useImportFW   = .TRUE.
      useImportTau  = .TRUE.
      useImportSLP  = .TRUE.
      useImportSIce = .TRUE.
      cpl_taveFreq  = taveFreq
#ifdef ALLOW_MNC
      cpl_snapshot_mnc = snapshot_mnc
      cpl_timeave_mnc  = timeave_mnc
#else
      cpl_snapshot_mnc = .FALSE.
      cpl_timeave_mnc  = .FALSE.
#endif

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' CPL_READPARMS: opening data.cpl'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE( 'data.cpl', 'CPL_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    Parameters for coupling interface:
      READ(UNIT=iUnit,NML=CPL_OCN_PARAM)

      WRITE(msgBuf,'(A)')
     &   ' CPL_READPARMS: finished reading data.cpl'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

C--   Close the open data file
      CLOSE(iUnit)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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
      IF ( useImportSLP ) THEN
        WRITE(msgBuf,'(A)')
     &  'CPL_READPARMS: useImportSLP is set but'
        CALL PRINT_ERROR( msgBuf , myThid)
      ELSEIF ( useImportSIce ) THEN
        WRITE(msgBuf,'(A)')
     &  'CPL_READPARMS: useImportSIce is set but'
        CALL PRINT_ERROR( msgBuf , myThid)
      ENDIF
      IF ( useImportSLP .OR. useImportSIce ) THEN
        WRITE(msgBuf,'(A)')
     &  'CPL_READPARMS: pressure loading code is not compiled.'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)')
     &  'Re-compile with:  #define ATMOSPHERIC_LOADING'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CPL_READPARMS'
      ENDIF
#endif

      IF ( .NOT.cpl_earlyExpImpCall .AND. staggerTimeStep ) THEN
        WRITE(msgBuf,'(A,L5,A,L5)')
     &  'CPL_READPARMS: staggerTimeStep=',staggerTimeStep,
     &  ' ; cpl_earlyExpImpCall=', cpl_earlyExpImpCall
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(A)')
     &  'CPL_READPARMS: taggerTimeStep requires cpl_earlyExpImpCall'
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
       CALL WRITE_0D_L( cpl_earlyExpImpCall, INDEX_NONE,
     &                 'cpl_earlyExpImpCall=',
     &   ' /* call coupler early in the time-stepping */')
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
       CALL WRITE_0D_R8( cpl_taveFreq, INDEX_NONE, 'cpl_taveFreq =',
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
