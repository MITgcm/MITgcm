C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc_readparms.F,v 1.2 2008/04/09 16:07:41 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: CFC_READPARMS
C !INTERFACE: ==========================================================
      SUBROUTINE CFC_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CFC_READPARMS
C     | o Initialise and read cfc package parameters
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CFC.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid    :: My Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_CFC

C     === Local variables ===
C     msgBuf    :: Informational/error message buffer
C     iUnit     :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      NAMELIST /CFC_FORCING/
     &          CFC_windFile, CFC_atmospFile, CFC_iceFile,
     &          CFC_forcingPeriod, CFC_forcingCycle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

       CFC_windFile  = ' '
       CFC_atmospFile= ' '
       CFC_iceFile   = ' '
c default periodic forcing to same as for physics
       CFC_forcingPeriod = externForcingPeriod
       CFC_forcingCycle  = externForcingCycle

      WRITE(msgBuf,'(A)') ' CFC_READPARMS: opening data.cfc'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE( 'data.cfc', 'CFC_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    forcing filenames and parameters
      READ(UNIT=iUnit,NML=CFC_FORCING)

      WRITE(msgBuf,'(A)')
     &   ' CFC_READPARMS: finished reading data.cfc'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

C--   Close the open data file
      CLOSE(iUnit)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_CFC */

      RETURN
      END
