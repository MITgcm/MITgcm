C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_readparms.F,v 1.8 2014/05/27 23:41:27 jmc Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C     !ROUTINE: SALT_PLUME_READPARMS
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize SALT_PLUME variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SALT_PLUME.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME
C     !LOCAL VARIABLES:
C     msgBuf      - Informational/error message buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /SALT_PLUME_PARM01/
     &     SaltPlumeSouthernOcean,
     &     CriterionType, PlumeMethod, Npower,
     &     SaltPlumeCriterion, SPovershoot, SPsalFRAC
#ifdef SALT_PLUME_IN_LEADS
     &     , SPinflectionPoint
#endif
#ifdef SALT_PLUME_SPLIT_BASIN
     &     , SaltPlumeSplitBasin
#endif
#ifdef SALT_PLUME_VOLUME
     &     , SPbrineSconst, SPbrineSaltmax
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useSALT_PLUME ) THEN
C-    pkg SALT_PLUME is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.salt_plume is found
         CALL PACKAGES_UNUSED_MSG( 'useSALT_PLUME', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   Default values for SALT_PLUME
      SaltPlumeSouthernOcean = .TRUE.
      CriterionType       = 1
      PlumeMethod         = 1
      SaltPlumeCriterion  = UNSET_RL
      SPovershoot         = 1.0 _d 0
#ifdef SALT_PLUME_SPLIT_BASIN
      SaltPlumeSplitBasin = .FALSE.
      Npower(1)           = 0
      Npower(2)           = 0
      SPsalFRAC(1)        = 1.0 _d 0
      SPsalFRAC(2)        = 1.0 _d 0
#else
      Npower              = 0
      SPsalFRAC           = 1.0 _d 0
#endif
#ifdef SALT_PLUME_IN_LEADS
      SPinflectionPoint   = 0.8 _d 0
#endif
#ifdef SALT_PLUME_VOLUME
      SPbrineSconst       = 45. _d 0
      SPbrineSaltmax      = 100. _d 0
#endif

      WRITE(msgBuf,'(A)')'SALT_PLUME_READPARMS: opening data.salt_plume'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.salt_plume', 'SALT_PLUME_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=SALT_PLUME_PARM01)
      WRITE(msgBuf,'(A)')
     &    'SALT_PLUME_READPARMS: finished reading data.salt_plume'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
C     Close the open data file
      CLOSE(iUnit)

       IF ( SaltPlumeCriterion .EQ. UNSET_RL ) THEN
          IF ( CriterionType .EQ. 1 ) THEN
             SaltPlumeCriterion  = 0.4 _d 0
          ELSEIF ( CriterionType .EQ. 2 ) THEN
             SaltPlumeCriterion  = 0.005 _d 0
          ELSE
             WRITE(msgBuf,'(A)') 'S/R SALT_PLUME_READPARMS'
             CALL PRINT_ERROR( msgBuf, myThid )
             WRITE(msgBuf,'(A)') 'Invalid CriterionType'
             CALL PRINT_ERROR( msgBuf, myThid )
             STOP 'ABNORMAL END: S/R SALT_PLUME_READPARMS'
          ENDIF
       ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
