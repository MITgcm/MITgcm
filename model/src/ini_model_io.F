C $Header: /u/gcmpack/MITgcm/model/src/ini_model_io.F,v 1.22 2006/08/04 01:22:18 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_MODEL_IO

C     !INTERFACE:
      SUBROUTINE INI_MODEL_IO( myThid )

C     !DESCRIPTION: 
C     Pass specific setup data for mdsio/rw.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C-    Safety check:
      IF ( nPx*nPy.NE.1 .AND. globalFiles ) THEN
        _BEGIN_MASTER( myThid )
c       WRITE(msgBuf,'(2A)')
c    &   'INI_MODEL_IO: globalFiles=TRUE is not safe',
c    &   ' in Multi-processors (MPI) run'
c       CALL PRINT_ERROR( msgBuf , myThid)
c       WRITE(msgBuf,'(2A)')
c    &   'INI_MODEL_IO: use instead "useSingleCpuIO=.TRUE."'
c       CALL PRINT_ERROR( msgBuf , myThid)
c       STOP 'ABNORMAL END: S/R INI_MODEL_IO'
C------
C   GlobalFiles option with Multi-processors execution (with MPI) is not
C   safe: dependending on the platform & compiler, it may produce:
C    - incomplete output files (wrong size) 
C    - wrong isolated values in some output files
C    - missing tiles (all zeros) in some output files.
C   A safe alternative is to set "useSingleCpuIO=.TRUE." in file "data", 
C     namelist PARAM01  (and to keep the default value of globalFiles=FALSE)
C   or if you are really sure that the globalFile works well on our platform 
C     & compiler, comment out the above "stop"
C-----
        WRITE(msgBuf,'(2A)')
     &   '** WARNING ** INI_MODEL_IO: globalFiles=TRUE is not safe',
     &   ' in Multi-processors (MPI) run'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_MODEL_IO:',
     &   ' use instead "useSingleCpuIO=.TRUE."'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        _END_MASTER( myThid )
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Flags specific to RW and MDSIO

C-    Only Master-thread updates IO-parameter in Common blocks:
      _BEGIN_MASTER( myThid )

C     Set globalFiles flag for READ_WRITE_FLD package
      CALL SET_WRITE_GLOBAL_FLD( globalFiles )
C     Set globalFiles flag for READ_WRITE_REC package
      CALL SET_WRITE_GLOBAL_REC( globalFiles )
C     Set globalFiles flag for READ_WRITE_PICKUP 
      CALL SET_WRITE_GLOBAL_PICKUP( globalFiles )

      _END_MASTER( myThid )
C-    Everyone else must wait for the IO-parameters to be set
      _BARRIER

C-    MNC model-io initialisation

#ifdef ALLOW_MNC
      IF (useMNC) THEN

C       Write units/set precision/etc for I/O of variables/arrays
C       belonging to the core dynamical model
        CALL INI_MNC_VARS( myThid )

#ifdef ALLOW_AUTODIFF_MONITOR
        CALL AUTODIFF_INI_MODEL_IO( myThid )
#endif

      ENDIF
#endif /* ALLOW_MNC */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
