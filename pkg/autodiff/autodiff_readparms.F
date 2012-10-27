C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_readparms.F,v 1.8 2012/10/27 19:08:17 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: AUTODIFF_READPARMS

C !INTERFACE:
      SUBROUTINE AUTODIFF_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize AUTODIFF variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AUTODIFF_PARAMS.h"
#ifdef ALLOW_SEAICE
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_AUTODIFF

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /AUTODIFF_PARM01/
     &       dumpAdVarExch, mon_AdVarExch,
     &       dumpAdByRec,
     &       useKPPinAdMode, useGGL90inAdMode,
     &       useGMRediInAdMode, useSALT_PLUMEinAdMode,
     &       useSEAICEinAdMode,
     &       inAdExact,
     &       turnFreeDriftInAdMode, SEAICEuseDYNAMICSinAdMode

      _BEGIN_MASTER(myThid)

C--   Default values for AUTODIFF
      dumpAdVarExch      = 2
      mon_AdVarExch      = 2
C-    to recover old ad-monitor & ad-dump behaviour:
c     dumpAdVarExch      = 1
c     mon_AdVarExch      = 0
C--   default : write one file per record
      dumpAdByRec        = .FALSE.
      useKPPinAdMode     = .TRUE.
      useGMRediInAdMode  = .TRUE.
      useSEAICEinAdMode  = .TRUE.
      useGGL90inAdMode   = .TRUE.
      useSALT_PLUMEinAdMode = .TRUE.
      inAdExact          = .TRUE.
C--
C-- pkg/seaice related switches
      SEAICEuseDYNAMICSinAdMode = .TRUE.
      turnFreeDriftInAdMode     = .FALSE.

      WRITE(msgBuf,'(A)') 'AUTODIFF_READPARMS: opening data.autodiff'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.autodiff', 'AUTODIFF_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=AUTODIFF_PARM01)
      WRITE(msgBuf,'(A)')
     &    'AUTODIFF_READPARMS: finished reading data.autodiff'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C     Close the open data file
      CLOSE(iUnit)

C--   Flag for approximate adjoint
      IF ( inAdExact ) THEN
       inAdTrue  = .FALSE.
       inAdFalse = .FALSE.
      ELSE
       inAdTrue  = .TRUE.
       inAdFalse = .FALSE.
      ENDIF
      inAdMode   = .FALSE.

C--   packages which can be switched off in adjoint mode (approximate adjoint):
C     store value of usePKG as it will be reset in S/R ADAUTODIFF_INADMODE_UNSET
      useKPPinFwdMode    = useKPP
      useGMRediInFwdMode = useGMRedi
      useSEAICEinFwdMode = useSEAICE
      useGGL90inFwdMode   = useGGL90
      useSALT_PLUMEinFwdMode = useSALT_PLUME
#ifdef ALLOW_SEAICE
C- Note: should re-visit how to turn on/off pkg internal-params in adjoint mode
C  This will not work for PKGs which load their params AFTER this S/R is called
C     (currently, it is OK for SEAICE but not for all PKGs !)
      SEAICEuseDYNAMICSinFwdMode = SEAICEuseDYNAMICS
#endif

C     store value of usePKG as it will be set in S/R ADAUTODIFF_INADMODE_SET
      useKPPinAdMode    = useKPPinAdMode    .AND. useKPP
      useGMRediInAdMode = useGMRediInAdMode .AND. useGMRedi
      useSEAICEinAdMode = useSEAICEinAdMode .AND. useSEAICE
      useGGL90inAdMode    = useGGL90inAdMode    .AND. useGGL90
      useSALT_PLUMEinAdMode = useSALT_PLUMEinAdMode .AND. useSALT_PLUME
#ifdef ALLOW_SEAICE
      SEAICEuseDYNAMICSinAdMode = SEAICEuseDYNAMICSinAdMode
     &                             .AND. SEAICEuseDYNAMICS
#endif

C--   Print out some kee parameters :
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// AUTODIFF parameters :'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
       CALL WRITE_0D_L( inAdExact, INDEX_NONE,
     &  'inAdExact =', ' /* get an exact adjoint (no approximation) */')
       CALL WRITE_0D_L( useKPPinAdMode, INDEX_NONE,
     &   'useKPPinAdMode =',      ' /* use KPP in adjoint mode */')
       CALL WRITE_0D_L( useGMRediInAdMode, INDEX_NONE,
     &  'useGMRediInAdMode =', ' /* use GMRedi in adjoint mode */')
       CALL WRITE_0D_L( useSEAICEinAdMode, INDEX_NONE,
     &  'useSEAICEinAdMode =', ' /* use SEAICE in adjoint mode */')
       CALL WRITE_0D_L( useGGL90inAdMode, INDEX_NONE,
     &   'useGGL90inAdMode =',      ' /* use GGL90 in adjoint mode */')
       CALL WRITE_0D_L( useSALT_PLUMEinAdMode, INDEX_NONE,
     &   'useSALT_PLUMEinAdMode =',
     &   ' /* use SALT_PLUME in adjoint mode */')
#ifdef ALLOW_SEAICE
       CALL WRITE_0D_L( SEAICEuseDYNAMICSinAdMode, INDEX_NONE,
     &  'SEAICEuseDYNAMICSinAdMode =',
     &  ' /* use SEAICE dynamics in adjoint mode */')
       CALL WRITE_0D_L( turnFreeDriftInAdMode, INDEX_NONE,
     &  'turnFreeDriftInAdMode=',
     &             ' /* turn On/Off Free-Drift in adj/fwd mode */')
#endif /* ALLOW_SEAICE */
       CALL WRITE_0D_I( dumpAdVarExch, INDEX_NONE,
     &  'dumpAdVarExch =', ' /* control adexch before dumpinp */')
       CALL WRITE_0D_I( mon_AdVarExch, INDEX_NONE,
     &  'mon_AdVarExch =', ' /* control adexch before monitor */')
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Check parameters :

C- Note: would be safer to add a stop if both turnFreeDriftInAdMode and
C     SEAICEuseFREEDRIFT are set to true ; but better in seaice_check than here

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_AUTODIFF */

      RETURN
      END
