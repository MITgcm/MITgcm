C $Header: /u/gcmpack/MITgcm/pkg/regrid/regrid_readparms.F,v 1.2 2014/05/28 00:13:16 jmc Exp $
C $Name:  $

#include "REGRID_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: REGRID_READPARMS

C     !INTERFACE:
      SUBROUTINE REGRID_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize REGRID variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "REGRID_SIZE.h"
#include "REGRID.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_REGRID

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) blank
      INTEGER iUnit, i

      NAMELIST /REGRID_PARM01/
     &     regrid_MNC, regrid_MDSIO,
     &     regrid_ngrids, regrid_fbname_in,
     &     regrid_nout

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useREGRID ) THEN
C-    pkg REGRID is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.regrid is found
         CALL PACKAGES_UNUSED_MSG( 'useREGRID', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

      DO i = 1,MAX_LEN_FNAM
        blank(i:i) = ' '
      ENDDO

C     Default values for REGRID
      REGRID_MNC      = useMNC
      REGRID_MDSIO    = .FALSE.
      regrid_ngrids   = 0
      DO i = 1,REGRID_NGRID_MAX
        REGRID_fbname_in(i)(1:MAX_LEN_FNAM) = blank(1:MAX_LEN_FNAM)
        REGRID_fbname_out(i)(1:MAX_LEN_FNAM) = blank(1:MAX_LEN_FNAM)
        regrid_nout(i) = 0
      ENDDO

      WRITE(msgBuf,'(A)') 'REGRID_READPARMS: opening data.regrid'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.regrid', 'REGRID_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=REGRID_PARM01)
      WRITE(msgBuf,'(A)')
     &     'REGRID_READPARMS: finished reading data.regrid'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
C     Close the open data file
      CLOSE(iUnit)

C     Make sure that we locally honor the global MNC on/off flag
      REGRID_MNC = REGRID_MNC .AND. useMNC
#ifndef ALLOW_MNC
C     Fix to avoid running without getting any output:
      REGRID_MNC = .FALSE.
#endif
      REGRID_MDSIO = (.NOT. REGRID_MNC) .OR. outputTypesInclusive

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_REGRID */

      RETURN
      END
