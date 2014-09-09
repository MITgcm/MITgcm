C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_readparms.F,v 1.47 2014/09/09 22:48:17 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PTRACERS_READPARMS

C     !INTERFACE:
      SUBROUTINE PTRACERS_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize PTRACERS parameters, read in data.ptracers

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP_PARAMS.h"
#endif
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS
C     !LOCAL VARIABLES:
C     k,iTracer  :: loop indices
C     iUnit      :: unit number for I/O
C     msgBuf     :: message buffer
      INTEGER k, iTracer
      INTEGER iUnit
      INTEGER ic
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL PTRACERS_diffKr(PTRACERS_num)
      _RL tauTr1ClimRelax

C     PTRACERS_taveFreq :: Frequency with which time-averaged PTRACERS
C                          are written to post-processing files.
C     tauTr1ClimRelax :: old parameter (will be removed 1 day)
      NAMELIST /PTRACERS_PARM01/
     &     tauTr1ClimRelax,
     &     PTRACERS_numInUse,
     &     PTRACERS_Iter0,
     &     PTRACERS_doAB_onGpTr,
     &     PTRACERS_addSrelax2EmP,
     &     PTRACERS_startStepFwd,
     &     PTRACERS_resetFreq,
     &     PTRACERS_resetPhase,
     &     PTRACERS_advScheme,
     &     PTRACERS_ImplVertAdv,
     &     PTRACERS_diffKh,
     &     PTRACERS_diffK4,
     &     PTRACERS_diffKr,
     &     PTRACERS_diffKrNr,
     &     PTRACERS_ref,
     &     PTRACERS_EvPrRn,
     &     PTRACERS_useGMRedi,
     &     PTRACERS_useDWNSLP,
     &     PTRACERS_useKPP,
     &     PTRACERS_initialFile,
     &     PTRACERS_names,
     &     PTRACERS_long_names,
     &     PTRACERS_units,
     &     PTRACERS_useRecords,
     &     PTRACERS_dumpFreq,
     &     PTRACERS_taveFreq,
     &     PTRACERS_monitorFreq,
     &     PTRACERS_timeave_mnc,
     &     PTRACERS_snapshot_mnc,
     &     PTRACERS_monitor_mnc,
     &     PTRACERS_pickup_write_mnc,
     &     PTRACERS_pickup_read_mnc

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.usePTRACERS ) THEN
C-    pkg PTRACERS is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.ptracers is found
         CALL PACKAGES_UNUSED_MSG( 'usePTRACERS', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C     Set ptracer IO & diagnostics labels (2 characters long)
      CALL PTRACERS_SET_IOLABEL(
     O                           PTRACERS_ioLabel,
     I                           PTRACERS_num, myThid )

C     Set defaults values for parameters in PTRACERS.h
      PTRACERS_dumpFreq    = dumpFreq
      PTRACERS_taveFreq    = taveFreq
      PTRACERS_monitorFreq = monitorFreq
      PTRACERS_Iter0   = 0
      PTRACERS_numInUse=-1
      DO iTracer=1,PTRACERS_num
        PTRACERS_startStepFwd(iTracer) = baseTime
        PTRACERS_resetFreq (iTracer) = 0.
        PTRACERS_resetPhase(iTracer) = 0.
        PTRACERS_advScheme(iTracer)=saltAdvScheme
        PTRACERS_ImplVertAdv(iTracer) = .FALSE.
        PTRACERS_diffKh(iTracer)=diffKhS
        PTRACERS_diffK4(iTracer)=diffK4S
        PTRACERS_diffKr(iTracer)=UNSET_RL
        DO k=1,Nr
          PTRACERS_diffKrNr(k,iTracer)=diffKrNrS(k)
          PTRACERS_ref     (k,iTracer)=0. _d 0
        ENDDO
        PTRACERS_EvPrRn(iTracer)=UNSET_RL
        PTRACERS_useGMRedi(iTracer)=useGMRedi
        PTRACERS_useDWNSLP(iTracer)=useDOWN_SLOPE
        PTRACERS_useKPP(iTracer)   =useKPP
        PTRACERS_initialFile(iTracer)=' '
        DO ic = 1,MAX_LEN_FNAM
          PTRACERS_names(iTracer)(ic:ic) = ' '
          PTRACERS_long_names(iTracer)(ic:ic) = ' '
          PTRACERS_units(iTracer)(ic:ic) = ' '
        ENDDO
      ENDDO
      PTRACERS_doAB_onGpTr      = doAB_onGtGs
      PTRACERS_addSrelax2EmP    = .FALSE.
      PTRACERS_useRecords       = .FALSE.
#ifdef ALLOW_MNC
      PTRACERS_timeave_mnc      = useMNC .AND. timeave_mnc
      PTRACERS_snapshot_mnc     = useMNC .AND. snapshot_mnc
      PTRACERS_monitor_mnc      = useMNC .AND. monitor_mnc
      PTRACERS_pickup_write_mnc = useMNC .AND. pickup_write_mnc
      PTRACERS_pickup_read_mnc  = useMNC .AND. pickup_read_mnc
#else
      PTRACERS_timeave_mnc      = .FALSE.
      PTRACERS_snapshot_mnc     = .FALSE.
      PTRACERS_monitor_mnc      = .FALSE.
      PTRACERS_pickup_write_mnc = .FALSE.
      PTRACERS_pickup_read_mnc  = .FALSE.
#endif
      tauTr1ClimRelax = 0.
      DO k = 1,Nr
#ifdef ALLOW_LONGSTEP
        PTRACERS_dTLev(k) = LS_nIter*dTtracerLev(k)
#else
        PTRACERS_dTLev(k) = dTtracerLev(k)
#endif
      ENDDO

C     Open and read the data.ptracers file
      WRITE(msgBuf,'(A)') ' PTRACERS_READPARMS: opening data.ptracers'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.ptracers', 'PTRACERS_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=PTRACERS_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' PTRACERS_READPARMS: finished reading data.ptracers'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

C     Close the open data file
      CLOSE(iUnit)

C     Check for retired parameters:
C     Tracer 1 climatology relaxation time scale (<- but the code is gone !)
      IF ( tauTr1ClimRelax .NE. 0. ) THEN
#ifndef ALLOW_COST
        WRITE(msgBuf,'(2A)')' PTRACERS_READPARMS: ',
     &       'tauTr1ClimRelax has been removed (code is gone)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R PTRACERS_READPARMS'
#else
       lambdaTr1ClimRelax = 1./tauTr1ClimRelax
      ELSE
       lambdaTr1ClimRelax = 0.
#endif
      ENDIF

C--   Now set-up any remaining parameters that result from other params

C     If PTRACERS_numInUse was not set in data.ptracers then we can
C     assume that all PTRACERS fields will be in use
      IF (PTRACERS_numInUse.LT.0) THEN
        PTRACERS_numInUse=PTRACERS_num
      ENDIF

C     Check we are not trying to use more tracers than allowed
      IF (PTRACERS_numInUse.GT.PTRACERS_num) THEN
        WRITE(msgBuf,'(A,I4,A,I4,A)')
     &       ' PTRACERS_READPARMS: You requested',PTRACERS_numInUse,
     &       ' tracers at run time when only',PTRACERS_num,
     &       ' were specified at compile time. Naughty! '
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R PTRACERS_READPARMS'
      ENDIF

C     Set vertical diffusion array
      DO iTracer=1,PTRACERS_numInUse
        IF ( PTRACERS_diffKr(iTracer).NE.UNSET_RL ) THEN
         DO k=1,Nr
          PTRACERS_diffKrNr(k,iTracer)=PTRACERS_diffKr(iTracer)
         ENDDO
        ENDIF
      ENDDO

C     Set flag to check when each tracer starts
      PTRACERS_startAllTrc = .TRUE.
      DO iTracer=1,PTRACERS_numInUse
        IF ( PTRACERS_startStepFwd(iTracer) .NE. baseTime )
     &    PTRACERS_startAllTrc = .FALSE.
      ENDDO
#ifdef ALLOW_AUTODIFF
      IF ( .NOT.PTRACERS_startAllTrc ) THEN
        WRITE(msgBuf,'(A)')
     &   'PTRACERS_READPARMS: Compiling AD code does not allow'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'to use different starting time (PTRACERS_startStepFwd)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R PTRACERS_READPARMS'
      ENDIF
#endif /* ALLOW_AUTODIFF */

C     Whether to compute linear surface correction (set by other packages)
      PTRACERS_calcSurfCor = .FALSE.

#ifdef ALLOW_MNC
      PTRACERS_timeave_mnc      = useMNC .AND. PTRACERS_timeave_mnc
      PTRACERS_snapshot_mnc     = useMNC .AND. PTRACERS_snapshot_mnc
      PTRACERS_monitor_mnc      = useMNC .AND. PTRACERS_monitor_mnc
      PTRACERS_pickup_write_mnc = useMNC .AND. PTRACERS_pickup_write_mnc
      PTRACERS_pickup_read_mnc  = useMNC .AND. PTRACERS_pickup_read_mnc

      PTRACERS_timeave_mdsio      = (.NOT. PTRACERS_timeave_mnc)
     &     .OR. outputTypesInclusive
      PTRACERS_snapshot_mdsio     = (.NOT. PTRACERS_snapshot_mnc)
     &     .OR. outputTypesInclusive
      PTRACERS_monitor_stdio      = (.NOT. PTRACERS_monitor_mnc)
     &     .OR. outputTypesInclusive
      PTRACERS_pickup_write_mdsio = (.NOT. PTRACERS_pickup_write_mnc)
     &     .OR. outputTypesInclusive
      PTRACERS_pickup_read_mdsio  = (.NOT. PTRACERS_pickup_read_mnc)
     &     .OR. outputTypesInclusive
#else
      PTRACERS_timeave_mnc        = .FALSE.
      PTRACERS_snapshot_mnc       = .FALSE.
      PTRACERS_monitor_mnc        = .FALSE.
      PTRACERS_pickup_write_mnc   = .FALSE.
      PTRACERS_pickup_read_mnc    = .FALSE.
      PTRACERS_timeave_mdsio      = .TRUE.
      PTRACERS_snapshot_mdsio     = .TRUE.
      PTRACERS_monitor_stdio      = .TRUE.
      PTRACERS_pickup_write_mdsio = .TRUE.
      PTRACERS_pickup_read_mdsio  = .TRUE.
#endif

C--   Print a summary of pTracer parameter values:

      _END_MASTER(myThid)
C     Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_PTRACERS */

      RETURN
      END
