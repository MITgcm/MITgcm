C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_readparms.F,v 1.9 2014/05/27 23:41:28 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      SUBROUTINE FLT_READPARMS( myThid  )

C     ==================================================================
C     SUBROUTINE FLT_READPARMS
C     ==================================================================
C     o read float-pkg parameters from file "data.flt"
C     ==================================================================

C     !USES:
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FLT_SIZE.h"
#include "FLT.h"

C     == routine arguments ==
C     myThid - thread number for this instance of the routine.
      INTEGER myThid

C     == local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /FLT_NML/ flt_int_traj, flt_int_prof,
     &                   flt_selectTrajOutp, flt_selectProfOutp,
     &                   flt_noise, flt_deltaT, FLT_Iter0,
     &                   flt_file, mapIniPos2Index

C     == end of interface ==

      IF ( .NOT.useFLT ) THEN
C-    pkg FLT is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.flt is found
         CALL PACKAGES_UNUSED_MSG( 'useFLT', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C     Set default values.
      flt_deltaT   = deltaTClock
      FLT_Iter0 = 0
      flt_int_traj =  3600.
      flt_int_prof = 43200.
      flt_noise    = 0.0
      flt_file     = 'float_pos'
      mapIniPos2Index = .TRUE.
      flt_selectTrajOutp = 2
      flt_selectProfOutp = 2

      WRITE(msgBuf,'(A)') ' FLT_READPARMS: opening data.flt'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                          'data.flt', 'FLT_READPARMS',
     O                          iUnit,
     I                          myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=FLT_NML)
      WRITE(msgBuf,'(A)') ' FLT_READPARMS: finished reading data.flt'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C     Close the open data file
      CLOSE(iUnit)

C     Do some checks
c     IF ( useFLT .AND. useOBCS ) THEN
c       WRITE(msgBuf,'(A,A)')
c    &   'S/R FLT_READPARMS: floats currently not implemented',
c    &   ' with open boundaries'
c       CALL PRINT_ERROR( msgBuf , myThid)
c       STOP 'ABNORMAL END: S/R FLT_READPARMS'
c     ENDIF

      _END_MASTER(myThid)
      _BARRIER

      RETURN
      END
