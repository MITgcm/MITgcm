C $Header: /u/gcmpack/MITgcm/model/src/Attic/get_offline_fields.F,v 1.7 2001/03/06 17:10:29 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE GET_OFFLINE_FIELDS( myThid, fSuff )
C     /==========================================================\
C     | SUBROUTINE GET_OFFLINE_FIELDS                            |
C     | o Make ee support calls for loading data for offline     |
C     |   experiments.                                           |
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
 
C     === Routine arguments ===
C     fSuff  - Suffix used to indicate which set of offline fields to load.
C     myThid - Thread no. that called this routine.
      CHARACTER*(*) fSuff
      INTEGER myThid
CEndOfInterface
      INTEGER  IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL IFNBLNK
      EXTERNAL ILNBLNK
 
C     === Local variables ===
C     msgBuf      - Error message buffer
C     filNam      - Parameter used to build file names.
C     I, Ilo, Ihi - Loop counter
C     iErr        - Error flag
      CHARACTER*(MAX_LEN_FNAM) filNam
      INTEGER I, Ilo, Ihi

C--   Build template file name
      Ilo = IFNBLNK(fSuff)
      Ihi = ILNBLNK(fSuff)
      Ilo = IFNBLNK(fSuff)
C     Note - Only the master thread writes to it filnam 
C            This presumes that only the master thread is going to do the
C            actual disk I/O.
      _BEGIN_MASTER(myThid)
      WRITE(filNam(2:),'(''_9yr_atlant_'',1024A)') 
     &  (fSuff(I:I),I=Ilo,Ihi)
      _END_MASTER(myThid)

C--   Call routine to load data
      filNam(1:1) = 'u'
      CALL READ_FIELD_XYZR8( 
     O                   uVel,
     I                   filNam, 'r4', myThid )
      filNam(1:1) = 'v'
      CALL READ_FIELD_XYZR8( 
     O                   vVel,
     I                   filNam, 'r4', myThid )
      filNam(1:1) = 'I'
C*P*  CALL READ_FIELD_XYZR8( 
C*P* O                   CINDEX,
C*P* I                   filNam, 'r4', myThid )

C  
      RETURN
      END
