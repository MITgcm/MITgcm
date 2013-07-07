C $Header: /u/gcmpack/MITgcm/pkg/offline/offline_check.F,v 1.1 2013/07/07 22:25:13 jmc Exp $
C $Name:  $

#include "OFFLINE_OPTIONS.h"
#ifdef ALLOW_DIC
#include "DIC_OPTIONS.h"
#endif
#ifdef ALLOW_DARWIN
#include "DARWIN_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: OFFLINE_CHECK

C !INTERFACE:
      SUBROUTINE OFFLINE_CHECK( myThid )

C     !DESCRIPTION:
C     Check dependances with other packages

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OFFLINE.h"
#include "OFFLINE_SWITCH.h"

C     !INPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_OFFLINE
C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'OFFLINE_CHECK: #define OFFLINE'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

C--   Print out some kee parameters :
       CALL WRITE_0D_L( offlineLoadGMRedi, INDEX_NONE,
     & 'offlineLoadGMRedi=', ' /* load (not compute) GMRedi tensor */')
       CALL WRITE_0D_L( offlineLoadKPP, INDEX_NONE,
     & 'offlineLoadKPP=', ' /* load (not compute) KPP mixing coeff */')
       CALL WRITE_0D_L( offlineLoadConvec, INDEX_NONE,
     & 'offlineLoadConvec=', ' /* load (not compute) Convect Mix */')
       CALL WRITE_0D_I( offlineIter0, INDEX_NONE,
     &   'offlineIter0 =', ' /* initial offline field timestep */')
       CALL WRITE_0D_I( offlineOffsetIter, INDEX_NONE,
     &   'offlineOffsetIter=', ' /* iteration number offset */')
       CALL WRITE_0D_I( offlineLoadPrec, INDEX_NONE,
     &   'offlineLoadPrec =',
     &   ' /* precision (32,64) of offline files */')
       CALL WRITE_0D_RL( deltaToffline, INDEX_NONE,
     &   'deltaToffline =', ' /* time-step to load offline files */')
       CALL WRITE_0D_RL( offlineForcingPeriod, INDEX_NONE,
     &   'offlineForcingPeriod=',
     &   ' /* time interval between reccords [s] */')
       CALL WRITE_0D_RL( offlineForcingCycle, INDEX_NONE,
     &   'offlineForcingCycle =', ' /* length of 1 cycle [s] */')
       CALL WRITE_0D_C( UvelFile, -1, INDEX_NONE, 'UvelFile =',
     &  ' /* file name of loaded uVel field */')

C--   Check parameters:
       errCount = 0

C-    Put stops here if some flags are wrongly chosen.

C-    better not to modify temperature that was loaded from file:
C     => disable allowFreezing
       IF ( allowFreezing ) THEN
        WRITE(msgBuf,'(A)') 'allowFreezing=T not allowed if useOffLine'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount =  errCount + 1
       ENDIF

C-    stop if files are specified but not used.
       IF ( HFluxFile.NE.' ' ) THEN
        WRITE(msgBuf,'(A)') 'HFluxFile is set but not used'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount =  errCount + 1
       ENDIF
#ifndef ALLOW_OLD_VIRTUALFLUX
       IF ( SFluxFile.NE.' ' ) THEN
        WRITE(msgBuf,'(A)') 'SFluxFile is set but not used'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount =  errCount + 1
       ENDIF
#endif /* ndef ALLOW_OLD_VIRTUALFLUX */
       IF ( IceFile.NE.' ' ) THEN
        WRITE(msgBuf,'(A)') 'IceFile is set but not used'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount =  errCount + 1
       ENDIF
       IF ( offlineLoadGMRedi .AND. .NOT.useGMRedi ) THEN
        WRITE(msgBuf,'(A)') 'some GMxxFile are set but not used'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount =  errCount + 1
       ENDIF
       IF ( offlineLoadKPP .AND. .NOT.useKPP ) THEN
        WRITE(msgBuf,'(A)') 'some KPP_xxxxxFile are set but not used'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount =  errCount + 1
       ENDIF
       IF ( offlineLoadConvec .AND. ivdc_kappa.EQ.0. ) THEN
        WRITE(msgBuf,'(A)') 'ConvFile is set but not used'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount =  errCount + 1
       ENDIF

       IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'OFFLINE_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R OFFLINE_CHECK'
       ENDIF

       WRITE(msgBuf,'(A)') 'OFFLINE_CHECK: done'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

#endif /* ALLOW_OFFLINE */

      RETURN
      END
