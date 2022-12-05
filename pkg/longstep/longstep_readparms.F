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
#include "PARAMS.h"
#include "LONGSTEP_PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_LONGSTEP

C     !LOCAL VARIABLES:
C     iUnit      :: unit number for I/O
C     msgBuf     :: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     LS_nIter        :: number of dynamics time steps between ptracer steps
C     LS_whenToSample :: when to sample dynamical fields for the longstep average
C                        0 - at beginning of timestep (reproduces offline results)
C                        1 - after first THERMODYNAMICS but before DYNAMICS
C                            (use use old U,V,W for advection, but new T,S for GCHEM if
C                            staggerTimeStep=.FALSE.; reproduces online with
C                            staggerTimeStep=.FALSE. for LS_nIter=1)
C                        2 - after DYNAMICS and second THERMODYNAMICS
C                            (use new U,V,W and T,S; reproduces online with
C                            staggerTimeStep=.TRUE. for LS_nIter=1)
      NAMELIST /LONGSTEP_PARM01/
     &     LS_nIter, LS_whenToSample

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.usePTRACERS ) THEN
C-    pkg LONGSTEP is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.longstep is found
         CALL PACKAGES_UNUSED_MSG(
     &        'usePTRACERS', 'LONGSTEP_READPARMS', 'longstep' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C     Set defaults values for parameters in LONGSTEP_PARAMS.h
      LS_nIter = 1
      LS_whenToSample = 0

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
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C     Now set-up any remaining parameters that result from the input
C     parameters

C     whether to average EmPmR or PmEpR in LS_fwFlux
#ifdef EXACT_CONSERV
      LS_usePmEpR = (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &              .AND. useRealFreshWaterFlux
#else /* EXACT_CONSERV */
      LS_usePmEpR = .FALSE.
#endif /* EXACT_CONSERV */

      _END_MASTER(myThid)
C     Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_LONGSTEP */

      RETURN
      END
