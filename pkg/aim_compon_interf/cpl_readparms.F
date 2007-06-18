C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/cpl_readparms.F,v 1.4 2007/06/18 21:28:53 jmc Exp $
C $Name:  $

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
#include "ATMIDS.h"

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
      _RL  cpl_atmSendFrq

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Coupling parameters:
C     cpl_earlyExpImpCall :: call coupler early in the time stepping call sequence
C     cpl_oldPickup  :: restart from an old pickup (= until checkpoint 52m_post)
C     useImportMxlD  :: True => use Imported Mix.Layer Detph from coupler
C     useImportSST   :: True => use the Imported SST from coupler
C     useImportSSS   :: True => use the Imported SSS from coupler
C     useImportVsq   :: True => use the Imported Surf. velocity^2
C     useImportFlxCO2 :: True => use the Imported air-sea CO2 fluxes from coupler
C     cpl_atmSendFrq :: Frequency^-1 for sending data to coupler (s)
      NAMELIST /CPL_ATM_PARAM/
     &    cpl_earlyExpImpCall,
     &    cpl_oldPickup,
     &    useImportMxlD, useImportSST, useImportSSS,
     &    useImportVsq, useImportFlxCO2,
     &    cpl_atmSendFrq

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Set default value:
      cpl_earlyExpImpCall = .TRUE.
      cpl_oldPickup = .FALSE.
      useImportMxlD = .TRUE.
      useImportSST  = .TRUE.
      useImportSSS  = .TRUE.
      useImportVsq  = .TRUE.
      IF ( atmCpl_exchange_DIC ) THEN
        useImportFlxCO2 = .TRUE.
      ELSE
        useImportFlxCO2 = .FALSE.
      ENDIF
      cpl_atmSendFrq= deltaTClock

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' CPL_READPARMS: opening data.cpl'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE( 'data.cpl', 'CPL_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    Parameters for coupling interface:
      READ(UNIT=iUnit,NML=CPL_ATM_PARAM)

      WRITE(msgBuf,'(A)')
     &   ' CPL_READPARMS: finished reading data.cpl'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

C--   Close the open data file
      CLOSE(iUnit)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Check parameters and model configuration
      IF ( useImportFlxCO2 .AND. .NOT.atmCpl_exchange_DIC ) THEN
        WRITE(msgBuf,'(2A)') 'CPL_READPARMS: useImportFlxCO2',
     &    ' requires atmCpl_exchange_DIC TRUE'
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R CPL_READPARMS'
      ENDIF

C-    derive other parameters:
      cplSendFrq_iter = NINT( cpl_atmSendFrq / deltaTClock )
      IF ( cplSendFrq_iter .LT. 1) cplSendFrq_iter = 1

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

C- namelist CPL_ATM_PARAM:
       CALL WRITE_0D_L( cpl_earlyExpImpCall, INDEX_NONE,
     &                 'cpl_earlyExpImpCall=',
     &   ' /* call coupler early in the time-stepping */')
       CALL WRITE_0D_L( cpl_oldPickup , INDEX_NONE,
     &                 'cpl_oldPickup =',
     &   ' /* restart from old pickup on/off flag */')
       CALL WRITE_0D_L( useImportMxlD , INDEX_NONE,
     &                 'useImportMxlD =',
     &   ' /* use Imported MxL. Depth from Coupler flag */')
       CALL WRITE_0D_L( useImportSST , INDEX_NONE,
     &                 'useImportSST =',
     &   ' /* use Imported SST from Coupler on/off flag */')
       CALL WRITE_0D_L( useImportSSS , INDEX_NONE,
     &                 'useImportSSS =',
     &   ' /* use Imported SSS from Coupler on/off flag */')
       CALL WRITE_0D_L( useImportVsq , INDEX_NONE,
     &                 'useImportVsq =',
     &   ' /* use Imported surf.Vel^2 from Coupler flag */')
       CALL WRITE_0D_L( useImportFlxCO2 , INDEX_NONE,
     &                 'useImportFlxCO2 =',
     &   ' /* use Imported air-sea CO2 flux from Coupler flag */')
       CALL WRITE_0D_R8( cpl_atmSendFrq, INDEX_NONE, 'cpl_atmSendFrq =',
     &   ' /* Frequency^o-1 for sending data to Coupler (s) */')
C     cpl_atmSendFrq  :: Frequency^-1 for sending data to coupler (s)
       CALL WRITE_0D_I( cplSendFrq_iter, INDEX_NONE,'cplSendFrq_iter =',
     &'   /* send data to coupler every "cplSendFrq" iter */')

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* COMPONENT_MODULE */

      RETURN
      END
