C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_readparms.F,v 1.21 2005/07/30 23:53:48 jmc Exp $
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
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
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

C     PTRACERS_taveFreq :: Frequency with which time-averaged PTRACERS
C                          are written to post-processing files.
      NAMELIST /PTRACERS_PARM01/
     &     PTRACERS_dumpFreq,
     &     PTRACERS_taveFreq,
     &     PTRACERS_monitorFreq,
     &     PTRACERS_advScheme,
     &     PTRACERS_ImplVertAdv,
     &     PTRACERS_diffKh,
     &     PTRACERS_diffK4,
     &     PTRACERS_diffKr,
     &     PTRACERS_diffKrNr,
     &     PTRACERS_useGMRedi,
     &     PTRACERS_useKPP,
     &     PTRACERS_Iter0,
     &     PTRACERS_numInUse,
     &     PTRACERS_initialFile,
     &     PTRACERS_useRecords,
     &     PTRACERS_names,
     &     PTRACERS_long_names,
     &     PTRACERS_units,
     &     PTRACERS_timeave_mnc, PTRACERS_snapshot_mnc,
     &     PTRACERS_pickup_write_mnc, PTRACERS_pickup_read_mnc

C     This routine has been called by the main model so we set our
C     internal flag to indicate we are in business
      PTRACERSisON=.TRUE.

C     Set defaults values for parameters in PTRACERS.h
      PTRACERS_dumpFreq    = dumpFreq
      PTRACERS_taveFreq    = taveFreq
      PTRACERS_monitorFreq = monitorFreq
      PTRACERS_Iter0   = 0
      PTRACERS_numInUse=-1
      DO iTracer=1,PTRACERS_num
        PTRACERS_advScheme(iTracer)=saltAdvScheme
        PTRACERS_ImplVertAdv(iTracer) = .FALSE.
        PTRACERS_diffKh(iTracer)=diffKhS
        PTRACERS_diffK4(iTracer)=diffK4S
        PTRACERS_diffKr(iTracer)=UNSET_RL
        DO k=1,Nr
          PTRACERS_diffKrNr(k,iTracer)=diffKrNrS(k)
        ENDDO
        PTRACERS_useGMRedi(iTracer)=useGMRedi
        PTRACERS_useKPP(iTracer)=useKPP
        PTRACERS_initialFile(iTracer)=' '
        DO ic = 1,MAX_LEN_FNAM
          PTRACERS_names(iTracer)(ic:ic) = ' '
          PTRACERS_long_names(iTracer)(ic:ic) = ' '
          PTRACERS_units(iTracer)(ic:ic) = ' '
        ENDDO
      ENDDO
      PTRACERS_useRecords       = .FALSE.
#ifdef ALLOW_MNC
      PTRACERS_timeave_mnc      = timeave_mnc .AND. useMNC
      PTRACERS_snapshot_mnc     = snapshot_mnc .AND. useMNC
      PTRACERS_pickup_write_mnc = pickup_write_mnc .AND. useMNC
      PTRACERS_pickup_read_mnc  = pickup_read_mnc .AND. useMNC
#else
      PTRACERS_timeave_mnc      = .FALSE.
      PTRACERS_snapshot_mnc     = .FALSE.
      PTRACERS_pickup_write_mnc = .FALSE.
      PTRACERS_pickup_read_mnc  = .FALSE.
#endif

C     Open and read the data.ptracers file
      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)') ' PTRACERS_READPARMS: opening data.ptracers'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.ptracers', 'PTRACERS_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=PTRACERS_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' PTRACERS_READPARMS: finished reading data.ptracers'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

C     Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C     Everyone else must wait for the parameters to be loaded
      _BARRIER

C     Now set-up any remaining parameters that result from the input
C     parameters

C     If PTRACERS_numInUse was not set in data.ptracers then we can
C     assume that all PTRACERS fields will be in use
      IF (PTRACERS_numInUse.LT.0) THEN
        PTRACERS_numInUse=PTRACERS_num
      ENDIF
C     Check we are not trying to use more tracers than allowed
      IF (PTRACERS_numInUse.GT.PTRACERS_num) THEN
        WRITE(msgBuf,'(A,I2,A,I2,A)')
     &       ' PTRACERS_READPARMS: You requested ',PTRACERS_numInUse,
     &       ' tracers at run time when only ',PTRACERS_num,
     &       ' were specified at compile time. Naughty! '
        CALL PRINT_ERROR(msgBuf, 1)
        STOP 'ABNORMAL END: S/R PTRACERS_READPARMS'
      ENDIF
C     Check that enough parameters were specified
      DO iTracer=1,PTRACERS_numInUse
        IF (PTRACERS_advScheme(iTracer).EQ.0) THEN
          WRITE(msgBuf,'(A,A,I2)')
     &         ' PTRACERS_READPARMS: ',
     &         'No advect. scheme specified for tracer #',
     &         iTracer
          CALL PRINT_ERROR(msgBuf, 1)
          STOP 'ABNORMAL END: S/R PTRACERS_READPARMS'
        ENDIF
      ENDDO
#ifndef INCLUDE_IMPLVERTADV_CODE
      DO iTracer=1,PTRACERS_numInUse
       IF ( PTRACERS_ImplVertAdv(iTracer) ) THEN
        WRITE(msgBuf,'(A)')
     &   'PTRACERS_READPARMS: #undef INCLUDE_IMPLVERTADV_CODE'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A,I2,A)') 'PTRACERS_READPARMS:',
     &   ' but pTracers_ImplVertAdv(',iTracer,' ) is TRUE'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R PTRACERS_READPARMS'
       ENDIF
      ENDDO
#endif
      DO iTracer=1,PTRACERS_numInUse
        PTRACERS_useGMRedi(iTracer) = PTRACERS_useGMRedi(iTracer)
     &                           .AND.useGMRedi
        PTRACERS_useKPP(iTracer) = PTRACERS_useKPP(iTracer)
     &                        .AND.useKPP
        IF ( PTRACERS_diffKr(iTracer).NE.UNSET_RL ) THEN
         DO k=1,Nr
          PTRACERS_diffKrNr(k,iTracer)=PTRACERS_diffKr(iTracer)
         ENDDO
        ENDIF
      ENDDO

#ifdef ALLOW_MNC
      PTRACERS_timeave_mnc      = 
     &     PTRACERS_timeave_mnc .AND. useMNC
      PTRACERS_snapshot_mnc     = 
     &     PTRACERS_snapshot_mnc .AND. useMNC
      PTRACERS_pickup_write_mnc = 
     &     PTRACERS_pickup_write_mnc .AND. useMNC
      PTRACERS_pickup_read_mnc  = 
     &     PTRACERS_pickup_read_mnc .AND. useMNC

      PTRACERS_timeave_mdsio      = (.NOT. PTRACERS_timeave_mnc)
     &     .OR. outputTypesInclusive
      PTRACERS_snapshot_mdsio     = (.NOT. PTRACERS_snapshot_mnc)
     &     .OR. outputTypesInclusive
      PTRACERS_pickup_write_mdsio = (.NOT. PTRACERS_pickup_write_mnc)
     &     .OR. outputTypesInclusive
      PTRACERS_pickup_read_mdsio  = (.NOT. PTRACERS_pickup_read_mnc)
     &     .OR. outputTypesInclusive
      
#else
      PTRACERS_timeave_mnc        = .FALSE.
      PTRACERS_snapshot_mnc       = .FALSE.
      PTRACERS_pickup_write_mnc   = .FALSE.
      PTRACERS_pickup_read_mnc    = .FALSE.
      PTRACERS_timeave_mdsio      = .TRUE.
      PTRACERS_snapshot_mdsio     = .TRUE.
      PTRACERS_pickup_write_mdsio = .TRUE.
      PTRACERS_pickup_read_mdsio  = .TRUE.
#endif
      
C--   Print a summary of pTracer parameter values:
      iUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// PTRACERS parameters '
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , 1)
      CALL WRITE_0D_I( PTRACERS_numInUse, INDEX_NONE,
     &   'PTRACERS_numInUse =',
     &   ' /* number of tracers */')
      CALL WRITE_0D_I( PTRACERS_Iter0, INDEX_NONE,
     &   'PTRACERS_Iter0 =',
     &   ' /* timestep number when tracers are initialized */')
      CALL WRITE_0D_R8(PTRACERS_dumpFreq, INDEX_NONE,
     &   'PTRACERS_dumpFreq =',
     &   ' /* Frequency^-1 for snapshot output (s) */')
      CALL WRITE_0D_R8(PTRACERS_taveFreq, INDEX_NONE,
     &   'PTRACERS_taveFreq =',
     &   ' /* Frequency^-1 for time-Aver. output (s) */')
      CALL WRITE_0D_L( PTRACERS_useRecords, INDEX_NONE,
     &   'PTRACERS_useRecords =', ' /* all tracers in 1 file */')
      
      CALL WRITE_0D_L( PTRACERS_timeave_mnc, INDEX_NONE,
     &     'PTRACERS_timeave_mnc =',
     &     ' /* use MNC for Tave output */')
      CALL WRITE_0D_L( PTRACERS_snapshot_mnc, INDEX_NONE,
     &     'PTRACERS_snapshot_mnc =',
     &     ' /* use MNC for snapshot output */')
      CALL WRITE_0D_L( PTRACERS_pickup_write_mnc, INDEX_NONE,
     &     'PTRACERS_pickup_write_mnc =', 
     &     ' /* use MNC for writing pickups */')
      CALL WRITE_0D_L( PTRACERS_pickup_read_mnc, INDEX_NONE,
     &     'PTRACERS_pickup_read_mnc =', 
     &     ' /* use MNC for reading pickups */')

      DO iTracer=1,PTRACERS_numInUse
        WRITE(msgBuf,'(A)') ' -----------------------------------'
        CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A,I4)') ' tracer number : ',iTracer
        CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , 1)
        CALL WRITE_0D_I( PTRACERS_advScheme(iTracer), INDEX_NONE,
     &     'PTRACERS_advScheme =', ' /* Advection Scheme */')
        CALL WRITE_0D_L( PTRACERS_ImplVertAdv(iTracer), INDEX_NONE,
     &     'PTRACERS_ImplVertAdv =',
     &     ' /* implicit vert. advection flag */')
        CALL WRITE_0D_R8( PTRACERS_diffKh(iTracer), INDEX_NONE,
     &     'PTRACERS_diffKh =', ' /* Laplacian Diffusivity */')
        CALL WRITE_0D_R8( PTRACERS_diffK4(iTracer), INDEX_NONE,
     &     'PTRACERS_diffK4 =', ' /* Biharmonic Diffusivity */')
        CALL WRITE_1D_R8( PTRACERS_diffKrNr(1,iTracer), Nr, INDEX_K,
     &     'PTRACERS_diffKrNr =', ' /* Vertical Diffusivity */')
        CALL WRITE_0D_L( PTRACERS_useGMRedi(iTracer), INDEX_NONE,
     &     'PTRACERS_useGMRedi =', ' /* apply GM-Redi */')
        CALL WRITE_0D_L( PTRACERS_useKPP(iTracer), INDEX_NONE,
     &     'PTRACERS_useKPP =', ' /* apply KPP scheme */')

      ENDDO
        WRITE(msgBuf,'(A)') ' -----------------------------------'
        CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , 1)
#endif /* ALLOW_PTRACERS */

      RETURN
      END

