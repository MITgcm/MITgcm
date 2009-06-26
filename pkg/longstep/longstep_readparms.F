C $Header: /u/gcmpack/MITgcm/pkg/longstep/longstep_readparms.F,v 1.1 2009/06/26 23:10:10 jahn Exp $
C $Name:  $

#include "LONGSTEP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: LONGSTEP_READPARMS

C     !INTERFACE:
      SUBROUTINE LONGSTEP_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize longstep parameters, read in data.longstep

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "LONGSTEP_PARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_LONGSTEP

C     !LOCAL VARIABLES:
C     k          :: loop index
C     iUnit      :: unit number for I/O
C     msgBuf     :: message buffer
      INTEGER k,iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     LS_nIter    :: number of dynamics time steps between ptracer steps
C     LS_afterTS         :: use T,S at end of time step (after thermodynamics)
C     LS_staggerTimeStep :: use U,V,W,T,S at end of time step
      NAMELIST /LONGSTEP_PARM01/
     &     LS_nIter, LS_staggerTimeStep, LS_afterTS

      _BEGIN_MASTER(myThid)

C     Set defaults values for parameters in LONGSTEP_PARAMS.h
      LS_nIter = 1
      LS_staggerTimeStep = .FALSE.
      LS_afterTS = .FALSE.

C     Open and read the data.longstep file
      WRITE(msgBuf,'(A)') ' LONGSTEP_READPARMS: opening data.longstep'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.longstep', 'LONGSTEP_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=LONGSTEP_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' LONGSTEP_READPARMS: finished reading data.longstep'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

C     Close the open data file
      CLOSE(iUnit)

C     Now set-up any remaining parameters that result from the input
C     parameters

C     can only do LS_staggerTimeStep if staggerTimeStep
      IF ( LS_staggerTimeStep .AND. .NOT.staggerTimeStep ) THEN
        WRITE(msgBuf,'(2A)')
     &       ' LONGSTEP_READPARMS: LS_staggerTimeStep requires',
     &       ' staggerTimeStep.'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R LONGSTEP_READPARMS'
      ENDIF

C--   Print a summary of longstep parameter values:
      iUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') '// longstep parameters '
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT , myThid )
      CALL WRITE_0D_I( LS_nIter, INDEX_NONE,
     &   'LS_nIter =',
     &   ' /* number of dynamics steps between ptracer steps */')
      CALL WRITE_0D_L( LS_staggerTimeStep, INDEX_NONE,
     &   'LS_staggerTimeStep =',
     &   ' /* average U,V,W,T,S at end of time step */')
      CALL WRITE_0D_L( LS_afterTS, INDEX_NONE,
     &   'LS_afterTS =',
     &   ' /* average T,S at end of time step */')
      WRITE(msgBuf,'(A)') ' -----------------------------------'
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)
C     Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_LONGSTEP */

      RETURN
      END

