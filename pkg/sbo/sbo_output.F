C $Header: /u/gcmpack/MITgcm/pkg/sbo/sbo_output.F,v 1.4 2014/05/30 17:10:15 jmc Exp $
C $Name:  $

#include "SBO_OPTIONS.h"

CBOP
C !ROUTINE: SBO_OUTPUT

C !INTERFACE:
      SUBROUTINE SBO_OUTPUT( myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SBO_OUTPUT
C     | o Do SBO diagnostic output.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SBO.h"

C !INPUT PARAMETERS:
C     myTime    :: my time in simulation ( s )
C     myIter    :: my Iteration number
C     myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SBO
C !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C !LOCAL VARIABLES:
C     sbo_diag  :: vector of SBO diagnostics contains:
C                  model time (s), xoamc, yoamc, zoamc,
C                  xoamp, yoamp, zoamp, mass, xcom, ycom, zcom
C     fName     :: output file name
C     msgBuf    :: Informational/error message buffer
      CHARACTER*(MAX_LEN_FNAM) fName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER narr
      PARAMETER( narr = 24 )
      INTEGER irecord
      INTEGER ioUnit
      _RL     sbo_diag(narr)
      _RS     dummyRS(1)

C-----------------------------------------------------------------
C     Save angular momentum and mass variables at every time step
C-----------------------------------------------------------------

      sbo_diag(1)  = myTime
      sbo_diag(2)  = xoamc
      sbo_diag(3)  = yoamc
      sbo_diag(4)  = zoamc
      sbo_diag(5)  = xoamp
      sbo_diag(6)  = yoamp
      sbo_diag(7)  = zoamp
      sbo_diag(8)  = mass
      sbo_diag(9)  = xcom
      sbo_diag(10) = ycom
      sbo_diag(11) = zcom
      sbo_diag(12) = sboarea

      sbo_diag(13) = xoamc_si
      sbo_diag(14) = yoamc_si
      sbo_diag(15) = zoamc_si
      sbo_diag(16) = mass_si

      sbo_diag(17) = xoamp_fw
      sbo_diag(18) = yoamp_fw
      sbo_diag(19) = zoamp_fw
      sbo_diag(20) = mass_fw
      sbo_diag(21) = xcom_fw
      sbo_diag(22) = ycom_fw
      sbo_diag(23) = zcom_fw

      sbo_diag(24) = mass_gc

      irecord      = myIter - nIter0 + 1

#ifdef ALLOW_MDSIO
      _BEGIN_MASTER(myThid)
C-    multi-threaded: only write it once (by master thread)
       WRITE(fName,'(A,I10.10)') 'SBO_global.', nIter0
       ioUnit  = 0
       CALL MDS_WRITEVEC_LOC(
     I           fName, precFloat64, ioUnit,
     I           'RL', narr, sbo_diag, dummyRS,
     I           0, 0, irecord, myIter, myThid )
      _END_MASTER(myThid)
#endif /* ALLOW_MDSIO */

#ifdef ALLOW_TIMEAVE
C-----------------------------------------------------------------
C     NOT doing this anymore:
C     Save time-averaged bottom pressure at sbo_taveFreq intervals
C-----------------------------------------------------------------
#endif /* ALLOW_TIMEAVE */

      IF ( myIter .EQ. nIter0 .OR.
     &     DIFFERENT_MULTIPLE(sbo_monFreq,myTime,deltaTClock)
     &   ) THEN

        _BEGIN_MASTER(myThid)

        ioUnit = standardMessageUnit
        WRITE(msgBuf,'(A,1PE21.13)')
     &    '%SBO sbo_mass                     = ', mass
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,1PE21.13)')
     &    '%SBO sbo_mass_fw                  = ', mass_fw
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,1PE21.13)')
     &    '%SBO sbo_zoamc                    = ', zoamc
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,1PE21.13)')
     &    '%SBO sbo_zoamp                    = ', zoamp
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

        _END_MASTER(myThid)

      ENDIF

#endif /* ALLOW_SBO */

      RETURN
      END
