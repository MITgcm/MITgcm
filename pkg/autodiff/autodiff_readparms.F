#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_MOM_COMMON
# include "MOM_COMMON_OPTIONS.h"
#endif /* ALLOW_MOM_COMMON */

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
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif/* ALLOW_GENERIC_ADVDIFF */
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE_PARAMS.h"
#endif /* ALLOW_SEAICE */

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
#ifdef ALLOW_GENERIC_ADVDIFF
      LOGICAL validAdvScheme
#endif /* ALLOW_GENERIC_ADVDIFF */

      NAMELIST /AUTODIFF_PARM01/
     &       dumpAdVarExch, mon_AdVarExch,
     &       dumpAdByRec,
     &       useApproxAdvectionInAdMode,
     &       useKPPinAdMode, useGGL90inAdMode,
     &       useGMRediInAdMode, useSALT_PLUMEinAdMode,
     &       useSEAICEinAdMode, useSmoothCorrel2DinAdMode,
     &       inAdExact, SEAICEapproxLevInAd,
     &       viscFacInAd, viscFacInFw, SIregFacInAd, SIregFacInFw,
     &       SEAICEuseFREEDRIFTswitchInAd, SEAICEuseDYNAMICSswitchInAd,
     &       cg2dFullAdjoint

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
      cg2dFullAdjoint    = .FALSE.
      inAdExact          = .TRUE.
      useApproxAdvectionInAdMode = .FALSE.
      SEAICEapproxLevInAd = 0
      viscFacInFw        = 1. _d 0
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

C--   Flags for approximate adjoint. Flag inAdMode is set/unset in
C     autodiff_inadmode_set/unset_ad.F. Wherever inAdMode is used it requires
C     using inAdExact or useApproxAdvectionInAdMode along with it.
      IF ( .NOT.inAdExact ) THEN
C     reset to recover old behavior, in case that is ever needed
       useApproxAdvectionInAdMode = .TRUE.
       WRITE(msgBuf,'(A)') '**WARNING** AUTODIFF_READPARMS: '//
     &      'resetting useApproxAdvectionInAdMode to .TRUE., '//
     &      'because inAdExact=.FALSE.'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
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

C     set this parameter to something we want to be able specify in a namelist
      viscFacAdj = viscFacInFw

#ifdef ALLOW_SEAICE
c     level of approximation in seaice thermodynamics adjoint
      if (useSEAICEinFwdMode.AND.(.NOT.useSEAICEinAdMode))
     &   SEAICEapproxLevInAd=MIN(SEAICEapproxLevInAd,0)
      if (useSEAICEinAdMode)
     &   SEAICEapproxLevInAd=MAX(SEAICEapproxLevInAd,0)
#endif

C--   Print out some key parameters :
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
       CALL WRITE_0D_L( useApproxAdvectionInAdMode, INDEX_NONE,
     & 'useApproxAdvectionInAdMode =',' /* approximate AD-advection */')
       CALL WRITE_0D_L( cg2dFullAdjoint, INDEX_NONE,
     &   'cg2dFullAdjoint =',
     &   ' /* use full hand written cg2d adjoint (no approximation) */')
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
       CALL WRITE_0D_RL( viscFacInFw, INDEX_NONE,
     &  'viscFacInFw =', ' /* viscosity factor for forward model */')
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
      IF ( viscFacInAd .NE. 1. _d 0 .OR. viscFacInFw .NE. 1. _d 0 ) THEN
#ifndef AUTODIFF_ALLOW_VISCFACADJ
       WRITE(msgBuf,'(2A)') 'AUTODIFF_READPARMS: ',
     &'To use viscFacInFw/viscFacInAd, define AUTODIFF_ALLOW_VISCFACADJ'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
#endif /* AUTODIFF_ALLOW_VISCFACADJ */
#if (!defined ALLOW_3D_VISCAH && !defined ALLOW_3D_VISCA4)
       WRITE(msgBuf,'(2A)') 'AUTODIFF_READPARMS: ',
     &      'To use viscFacInFw/viscFacInAd, '//
     &      'define ALLOW_3D_VISCA4 and/or ALLOW_3D_VISCA4'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
#endif /* ALLOW_3D_VISCA */
#ifdef ALLOW_3D_VISCAH
       IF ( viscAhDfile .EQ. ' ' .AND. viscAhZfile .EQ. ' ' ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** AUTODIFF_READPARMS: ',
     &       'For viscFacInFw/viscFacInAd to have any effect, '//
     &       'specify at least one of viscAh[D/Z]file'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
       ENDIF
#endif /* ALLOW_3D_VISCAH */
#ifdef ALLOW_3D_VISCA4
       IF ( viscA4Dfile .EQ. ' ' .AND. viscA4Zfile .EQ. ' ' ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** AUTODIFF_READPARMS: ',
     &       'For viscFacInFw/viscFacInAd to have any effect, '//
     &       'specify at least one of viscA4[D/Z]file'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
       ENDIF
#endif /* ALLOW_3D_VISCA4 */
      ENDIF
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

      IF ( useApproxAdvectionInAdMode ) THEN
#ifdef ALLOW_GENERIC_ADVDIFF
C     so far this only works for flux limited DST3 scheme
       validAdvScheme = .FALSE.
       validAdvScheme = validAdvScheme
     &      .OR. tempAdvScheme.EQ.ENUM_DST3_FLUX_LIMIT
     &      .OR. saltAdvScheme.EQ.ENUM_DST3_FLUX_LIMIT
#ifdef ALLOW_SEAICE
       IF ( useSEAICE ) validAdvScheme = validAdvScheme
     &      .OR. SEAICEadvScheme .EQ.ENUM_DST3_FLUX_LIMIT
     &      .OR. SEAICEadvSchArea.EQ.ENUM_DST3_FLUX_LIMIT
     &      .OR. SEAICEadvSchHeff.EQ.ENUM_DST3_FLUX_LIMIT
     &      .OR. SEAICEadvSchSnow.EQ.ENUM_DST3_FLUX_LIMIT
     &      .OR. SEAICEadvSchSalt.EQ.ENUM_DST3_FLUX_LIMIT
#endif /* ALLOW_SEAICE */
       IF ( .NOT. validAdvScheme ) THEN
        WRITE(msgBuf,'(3A)') 'AUTODIFF_READPARMS: ',
     &       'useApproxAdvectionInAdMode = .TRUE. ',
     &       'only makes sense if at least '
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I2,A)') 'AUTODIFF_READPARMS: ',
     &       'one advection scheme is ', ENUM_DST3_FLUX_LIMIT,
     &       '. Current settings are:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I2)') 'AUTODIFF_READPARMS: ',
     &       'tempAdvScheme    = ', tempAdvScheme
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I2)') 'AUTODIFF_READPARMS: ',
     &       'saltAdvScheme    = ', saltAdvScheme
        CALL PRINT_ERROR( msgBuf, myThid )
#ifdef ALLOW_SEAICE
        IF ( useSEAICE ) THEN
         WRITE(msgBuf,'(2A,I2)') 'AUTODIFF_READPARMS: ',
     &        'SEAICEadvScheme  = ', SEAICEadvScheme
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A,I2)') 'AUTODIFF_READPARMS: ',
     &        'SEAICEadvSchArea = ', SEAICEadvSchArea
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A,I2)') 'AUTODIFF_READPARMS: ',
     &        'SEAICEadvSchHeff = ', SEAICEadvSchHeff
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A,I2)') 'AUTODIFF_READPARMS: ',
     &        'SEAICEadvSchSnow = ', SEAICEadvSchSnow
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A,I2)') 'AUTODIFF_READPARMS: ',
     &        'SEAICEadvSchSalt = ', SEAICEadvSchSalt
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
#endif /* ALLOW_SEAICE */
        errCount = errCount + 1
       ENDIF
#else
       WRITE(msgBuf,'(2A)') 'AUTODIFF_READPARMS: ',
     &  'useApproxAdvectionInAdMode=.TRUE. without pkg/generic_advdiff'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
#endif /* ALLOW_GENERIC_ADVDIFF */
      ENDIF

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
