#include "ATM_CPL_OPTIONS.h"

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
C     |   this version is specific to 1 component (atmos)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"

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
      _RL  cpl_atmSendFrq, tmpLoc
      LOGICAL cpl_earlyExpImpCall

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Coupling parameters:
C     cpl_oldPickup  :: restart from an old pickup (= until checkpoint 59h)
C     useImportMxlD  :: True => use Imported Mix.Layer Detph from coupler
C     useImportSST   :: True => use the Imported SST from coupler
C     useImportSSS   :: True => use the Imported SSS from coupler
C     useImportVsq   :: True => use the Imported Surf. velocity^2
C     useImportThSIce :: True => use the Imported thSIce state vars from coupler
C     useImportFlxCO2 :: True => use the Imported air-sea CO2 flux from coupler
C     cpl_atmSendFrq :: Frequency^-1 for sending data to coupler (s)
      NAMELIST /CPL_ATM_PARAM/
     &    cpl_earlyExpImpCall,
     &    cpl_oldPickup,
     &    useImportMxlD, useImportSST, useImportSSS,
     &    useImportVsq, useImportThSIce, useImportFlxCO2,
     &    cpl_atmSendFrq,
     &    maxNumberPrint

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C--   Open the data file
      WRITE(msgBuf,'(A)') ' CPL_READPARMS: opening data.cpl'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1 )

      CALL OPEN_COPY_DATA_FILE( 'data.cpl', 'CPL_READPARMS',
     O                          iUnit, myThid )

C-    Set default value:
      cpl_earlyExpImpCall = .TRUE.
      cpl_oldPickup = .FALSE.
      useImportMxlD = .TRUE.
      useImportSST  = .TRUE.
      useImportSSS  = .TRUE.
      useImportVsq  = .TRUE.
      useImportThSIce = cpl_exchange2W_sIce.EQ.3
      useImportFlxCO2 =  cpl_exchange_DIC .EQ. 3
      cpl_atmSendFrq= deltaTClock
      maxNumberPrint= 100
      countPrtExp   = 0
      countPrtImp   = 0

C--   Read parameters from open data file:

C-    Parameters for coupling interface:
      READ(UNIT=iUnit,NML=CPL_ATM_PARAM)

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

C--   Check parameters and model configuration

#ifndef ALLOW_LAND
      IF ( atm_cplExch_RunOff ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: needs',
     &    ' to compile pkg/land to use: atm_cplExch_RunOff=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' (set from Coupler "data.cpl": cpl_exchange_RunOff > 1)'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
#endif /* ndef ALLOW_LAND */
#ifndef ALLOW_THSICE
      IF ( atm_cplExch1W_sIce ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: needs',
     &    ' to compile pkg/thsice to use: atm_cplExch1W_sIce=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' (set from Coupler "data.cpl": cpl_exchange1W_sIce > 1)'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
      IF ( atm_cplExch2W_sIce ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: needs',
     &    ' to compile pkg/thsice to use: atm_cplExch2W_sIce=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' (set from Coupler "data.cpl": cpl_exchange2W_sIce > 1)'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
      IF ( atm_cplExch_SaltPl ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: needs',
     &    ' to compile pkg/thsice to use: atm_cplExch_SaltPl=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' (set from Coupler "data.cpl": cpl_exchange_SaltPl > 1)'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
#endif /* ndef ALLOW_THSICE */
#ifndef ALLOW_AIM
      IF ( atm_cplExch_DIC ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: needs',
     &    ' to compile pkg/aim_v23 to use: atm_cplExch_DIC = T'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' (set from Coupler "data.cpl": cpl_exchange_DIC > 1)'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
#endif /* ndef ALLOW_AIM */

      IF ( useImportThSIce .AND. .NOT.atm_cplExch2W_sIce ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: useImportThSIce',
     &    ' requires setting'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' cpl_exchange2W_sIce > 1 (in Coupler "data.cpl")'
        CALL PRINT_ERROR( msgBuf, myThid )
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
      IF ( useImportFlxCO2 .AND. cpl_exchange_DIC.NE.3  ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: useImportFlxCO2',
     &    ' requires setting'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: ',
     &    ' cpl_exchange_DIC = 3 (in Coupler "data.cpl")'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF
      tmpLoc = NINT( cpl_atmSendFrq / deltaTClock )
      tmpLoc = ABS( tmpLoc - ( cpl_atmSendFrq / deltaTClock ) )
      IF ( tmpLoc.GT.1. _d -12 .OR. cpl_atmSendFrq.EQ.zeroRL ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: cpl_atmSendFrq',
     &    ' is not a multiple of deltaT'
        CALL PRINT_ERROR( msgBuf, myThid )
        cplErrorCount = cplErrorCount + 1
      ENDIF

C-    Derive other parameters:
      cplSendFrq_iter = NINT( cpl_atmSendFrq / deltaTClock )
      IF ( cplSendFrq_iter.LT.1 ) cplSendFrq_iter = 1

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

       CALL WRITE_0D_L( atm_cplSequential, INDEX_NONE,
     &                 'atm_cplSequential =',
     &   ' /* use Sequential Coupling Exchange on/off flag */')
       CALL WRITE_0D_L( atm_cplExch_RunOff, INDEX_NONE,
     &                 'atm_cplExch_RunOff =',
     &     ' /* exchange RunOff fields with coupler on/off */')
       CALL WRITE_0D_L( atm_cplExch1W_sIce, INDEX_NONE,
     &                 'atm_cplExch1W_sIce =',
     &     ' /* 1-way exchange of seaice vars with coupler */')
       CALL WRITE_0D_L( atm_cplExch2W_sIce, INDEX_NONE,
     &                 'atm_cplExch2W_sIce =',
     &     ' /* 2-way exchange of ThSIce vars with coupler */')
       CALL WRITE_0D_L( atm_cplExch_SaltPl, INDEX_NONE,
     &                 'atm_cplExch_SaltPl =',
     &     ' /* exchange Salt-Plume fields with coupler */')
       CALL WRITE_0D_L( atm_cplExch_DIC, INDEX_NONE,
     &                 'atm_cplExch_DIC    =',
     &     ' /* exchange DIC    fields with coupler on/off */')

C-    print namelist parameter value:
       WRITE(msgBuf,'(A)') '// -------'
       CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &        '// Coupler parameters (from local param file):'
       CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

       CALL WRITE_0D_L( cpl_oldPickup , INDEX_NONE,
     &                 'cpl_oldPickup =',
     &   ' /* restart from old pickup on/off flag */')
       CALL WRITE_0D_L( useImportMxlD , INDEX_NONE,
     &                 'useImportMxlD =',
     &   ' /* use Imported MxL. Depth from Coupler flag */')
       CALL WRITE_0D_L( useImportSST  , INDEX_NONE,
     &                 'useImportSST  =',
     &   ' /* use Imported SST from Coupler on/off flag */')
       CALL WRITE_0D_L( useImportSSS  , INDEX_NONE,
     &                 'useImportSSS  =',
     &   ' /* use Imported SSS from Coupler on/off flag */')
       CALL WRITE_0D_L( useImportVsq  , INDEX_NONE,
     &                 'useImportVsq  =',
     &   ' /* use Imported surf.Vel^2 from Coupler flag */')
       CALL WRITE_0D_L( useImportThSIce, INDEX_NONE,
     &                 'useImportThSIce=',
     &   ' /* use Imported thSIce state-var fr Cpl. flag */')
       CALL WRITE_0D_L( useImportFlxCO2, INDEX_NONE,
     &                 'useImportFlxCO2=',
     &   ' /* use Imported air-sea CO2 flux fr Cpl.  flag */')
       CALL WRITE_0D_RL( cpl_atmSendFrq, INDEX_NONE, 'cpl_atmSendFrq =',
     &   ' /* Frequency^o-1 for sending data to Coupler (s) */')
C     cpl_atmSendFrq  :: Frequency^-1 for sending data to coupler (s)
       CALL WRITE_0D_I( cplSendFrq_iter, INDEX_NONE,'cplSendFrq_iter =',
     &  ' /* send data to coupler every "cplSendFrq" iter */')
       CALL WRITE_0D_I( maxNumberPrint, INDEX_NONE, 'maxNumberPrint =',
     &  ' /* max number of printed Exp/Imp messages */')

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
