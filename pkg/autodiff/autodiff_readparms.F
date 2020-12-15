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

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_AUTODIFF

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER errCount

      NAMELIST /AUTODIFF_PARM01/
     &       dumpAdVarExch, mon_AdVarExch,
     &       dumpAdByRec,
     &       useKPPinAdMode, useGGL90inAdMode,
     &       useGMRediInAdMode, useSALT_PLUMEinAdMode,
     &       useSEAICEinAdMode, useSmoothCorrel2DinAdMode,
     &       inAdExact, SEAICEapproxLevInAd, viscFacInAd,
     &       SIregFacInAd, SIregFacInFw,
     &       SEAICEuseFREEDRIFTswitchInAd, SEAICEuseDYNAMICSswitchInAd

      IF ( .NOT.useAUTODIFF ) THEN
C-    pkg AUTODIFF is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.autodiff is found
         CALL PACKAGES_UNUSED_MSG( 'useAUTODIFF', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)
      errCount = 0

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
      SEAICEapproxLevInAd = 0
      viscFacInAd        = 1. _d 0
      SIregFacInAd       = UNSET_RL
      SIregFacInFw       = UNSET_RL

C-- pkg/seaice related switches
      SEAICEuseFREEDRIFTswitchInAd = .FALSE.
      SEAICEuseDYNAMICSswitchInAd  = .FALSE.
      useSmoothCorrel2DinAdMode = .TRUE.

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
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

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
      useGGL90inFwdMode  = useGGL90
      useSALT_PLUMEinFwdMode = useSALT_PLUME

C     store value of usePKG as it will be set in S/R ADAUTODIFF_INADMODE_SET
      useKPPinAdMode    = useKPPinAdMode    .AND. useKPP
      useGMRediInAdMode = useGMRediInAdMode .AND. useGMRedi
      useSEAICEinAdMode = useSEAICEinAdMode .AND. useSEAICE
      useGGL90inAdMode  = useGGL90inAdMode  .AND. useGGL90
      useSALT_PLUMEinAdMode = useSALT_PLUMEinAdMode .AND. useSALT_PLUME

#ifdef ALLOW_SEAICE
c     level of approximation in seaice thermodynamics adjoint
      if (useSEAICEinFwdMode.AND.(.NOT.useSEAICEinAdMode))
     &   SEAICEapproxLevInAd=MIN(SEAICEapproxLevInAd,0)
      if (useSEAICEinAdMode)
     &   SEAICEapproxLevInAd=MAX(SEAICEapproxLevInAd,0)
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
       CALL WRITE_0D_L( SEAICEuseDYNAMICSswitchInAd, INDEX_NONE,
     &  'SEAICEuseDYNAMICSswitchInAd =',
     &            ' /* switch On/Off SEAICE Dyn in AD mode */')
       CALL WRITE_0D_L( SEAICEuseFREEDRIFTswitchInAd, INDEX_NONE,
     &  'SEAICEuseFREEDRIFTswitchInAd=',
     &            ' /* switch On/Off Free-Drift in AD mode */')
       CALL WRITE_0D_I( SEAICEapproxLevInAd, INDEX_NONE,
     &  'SEAICEapproxLevInAd =',
     &  ' /* -1:SEAICE_FAKE, >0:other adjoint approximation */')
#endif /* ALLOW_SEAICE */
       CALL WRITE_0D_I( dumpAdVarExch, INDEX_NONE,
     &  'dumpAdVarExch =', ' /* control adexch before dumpinp */')
       CALL WRITE_0D_I( mon_AdVarExch, INDEX_NONE,
     &  'mon_AdVarExch =', ' /* control adexch before monitor */')
       CALL WRITE_0D_RL( viscFacInAd, INDEX_NONE,
     &  'viscFacInAd =', ' /* viscosity factor for adjoint */')
       CALL WRITE_0D_RL( SIregFacInAd, INDEX_NONE,
     &  'SIregFacInAd =', ' /* sea ice factor for adjoint model */')
       CALL WRITE_0D_RL( SIregFacInFw, INDEX_NONE,
     &  'SIregFacInFw =', ' /* sea ice factor for forward model */')
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Check parameters :
#ifdef ALLOW_SEAICE
      IF ( SEAICEuseFREEDRIFTswitchInAd .AND. .NOT.useSEAICE ) THEN
        WRITE(msgBuf,'(2A)') 'AUTODIFF_READPARMS: ',
     &  'SEAICEuseFREEDRIFTswitchInAd not usable without useSEAICE'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( SEAICEuseDYNAMICSswitchInAd .AND. .NOT.useSEAICE ) THEN
        WRITE(msgBuf,'(2A)') 'AUTODIFF_READPARMS: ',
     &  'SEAICEuseDYNAMICSswitchInAd not usable without useSEAICE'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#else /* ALLOW_SEAICE */
      SEAICEuseFREEDRIFTswitchInAd = .FALSE.
      SEAICEuseDYNAMICSswitchInAd  = .FALSE.
#endif /* ALLOW_SEAICE */

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &   'AUTODIFF_READPARMS: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R AUTODIFF_READPARMS'
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_AUTODIFF */

      RETURN
      END
