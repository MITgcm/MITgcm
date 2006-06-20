C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_monitor.F,v 1.4 2006/06/20 20:57:06 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_MONITOR

C     !INTERFACE:
      SUBROUTINE SEAICE_MONITOR(
     I                  myTime, myIter, myThid )

C     !DESCRIPTION:
C     Print some statistics about input forcing fields.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
#include "GRID.h"
#ifdef ALLOW_MONITOR
# include "MONITOR.h"
#endif
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT PARAMETERS:
      INTEGER myIter
      _RL myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_MONITOR
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i,j,bi,bj
      _RL TMP_DIAG (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

      IF ( DIFFERENT_MULTIPLE(SEAICE_monFreq,myTime,deltaTclock)
     &   ) THEN

        IF (SEAICE_mon_stdio) THEN
          mon_write_stdout = .TRUE.
        ELSE
          mon_write_stdout = .FALSE.
        ENDIF
        mon_write_mnc = .FALSE.
#ifdef ALLOW_MNC
        IF (useMNC .AND. SEAICE_mon_mnc) THEN
          DO i = 1,MAX_LEN_MBUF
            mon_fname(i:i) = ' '
          ENDDO
          mon_fname(1:14) = 'monitor_seaice'
          CALL MNC_CW_APPEND_VNAME(
     &         'T', '-_-_--__-__t', 0,0, myThid)
          CALL MNC_CW_SET_UDIM(mon_fname, -1, myThid)
          CALL MNC_CW_I_W_S(
     &        'I',mon_fname,1,1,'T', myIter, myThid)
          CALL MNC_CW_SET_UDIM(mon_fname, 0, myThid)
          mon_write_mnc = .TRUE.
        ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_USE_MPI
        IF ( ( .NOT.useSingleCPUIO .OR. mpiMyId.EQ.0 ) .AND.
     &       mon_write_stdout ) THEN
#else /* ALLOW_USE_MPI */
        IF ( mon_write_stdout ) THEN
#endif /* ALLOW_USE_MPI */
          _BEGIN_MASTER(myThid)
          WRITE(msgBuf,'(2A)') '// ===========================',
     &         '============================'
          CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          WRITE(msgBuf,'(A)') '// Begin MONITOR SEAICE statistics'
          CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          WRITE(msgBuf,'(2A)') '// ===========================',
     &         '============================'
          CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          _END_MASTER(myThid)
        ENDIF

        CALL MON_SET_PREF('seaice',myThid)
        CALL MON_OUT_I ('_tsnumber', myIter,mon_string_none,myThid)
        CALL MON_OUT_RL('_time_sec', myTime,mon_string_none,myThid)

C       Print some statistics about input forcing fields
        DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
              DO J=1-OLy,sNy+OLy
                 DO I=1-OLx,sNx+OLx
                    TMP_DIAG(i,j,bi,bj)=UICE(i,j,1,bi,bj)
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
#ifdef SEAICE_CGRID
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_uice',
     &       maskW,maskW,rAw,drF,myThid)
#else
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_uice',
     &       UVM,UVM,rAz,drF,myThid)
#endif
        DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
              DO J=1-OLy,sNy+OLy
                 DO I=1-OLx,sNx+OLx
                    TMP_DIAG(i,j,bi,bj)=VICE(i,j,1,bi,bj)
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
#ifdef SEAICE_CGRID
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_vice',
     &       maskS,maskS,rAs,drF,myThid)
#else
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_vice',
     &       UVM,UVM,rAz,drF,myThid)
#endif
        DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
              DO J=1-OLy,sNy+OLy
                 DO I=1-OLx,sNx+OLx
                    TMP_DIAG(i,j,bi,bj)=AREA(i,j,1,bi,bj)
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_area',
     &       maskH,maskH,rA ,drF,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
              DO J=1-OLy,sNy+OLy
                 DO I=1-OLx,sNx+OLx
                    TMP_DIAG(i,j,bi,bj)=HEFF(i,j,1,bi,bj)
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
        CALL MON_PRINTSTATS_RL(1,TMP_DIAG,'_heff',
     &       maskH,maskH,rA ,drF,myThid)

#ifdef ALLOW_USE_MPI
        IF ( ( .NOT.useSingleCPUIO .OR. mpiMyId.EQ.0 ) .AND.
     &       mon_write_stdout ) THEN
#else /* ALLOW_USE_MPI */
        IF ( mon_write_stdout ) THEN
#endif /* ALLOW_USE_MPI */
          _BEGIN_MASTER(myThid)
          WRITE(msgBuf,'(2A)') '// ===========================',
     &         '============================'
          CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          WRITE(msgBuf,'(A)') '// End MONITOR SEAICE statistics'
          CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          WRITE(msgBuf,'(2A)') '// ===========================',
     &         '============================'
          CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          _END_MASTER(myThid)
        ENDIF

        mon_write_stdout = .FALSE.
        mon_write_mnc    = .FALSE.

      ENDIF

#endif /* ALLOW_MONITOR */

      RETURN
      END
