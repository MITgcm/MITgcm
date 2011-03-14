C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_check4file.F,v 1.2 2011/03/14 01:34:17 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MDS_CHECK4FILE

C     !INTERFACE:
      SUBROUTINE MDS_CHECK4FILE(
     I                           filePfx, fileSfx, prtID,
     O                           fileName, fileExist,
     I                           useCurrentDir,
     I                           myThid )

C     !DESCRIPTION:
C     Check if file exist :
C     1rst check prefix alone, then prefix+suffix ; then prefix.tileNb+suffix

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C useCurrentDir :: always search for file in the current directory
C                  (even if "mdsioLocalDir" is set)
C     filePfx   :: file name prefix
C     fileSfx   :: file name suffix
C     prtID     :: print Identificator (in case file is not found)
C     fileName  :: file which has been found
C     fileExist :: True when file has been found
C     myThid    :: my Thread Id number
      LOGICAL useCurrentDir
      CHARACTER*(*) filePfx, fileSfx, prtID
      CHARACTER*(*) fileName
      LOGICAL fileExist
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  IFNBLNK, ILNBLNK
      EXTERNAL IFNBLNK, ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables in common block ==
      LOGICAL shareExist
      COMMON / LOCAL_MDS_CHECK4FILE / shareExist
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf, msgPfx
      INTEGER iG, jG
      INTEGER lp, ip, is, iL, i, ioUnit
CEOP

      ioUnit = errorMessageUnit
      fileName = ' '

C--   First check if fileName is long enough
      lp = ILNBLNK( mdsioLocalDir )
      IF ( useCurrentDir ) lp = 0
      ip = ILNBLNK(filePfx)
      is = ILNBLNK(fileSfx)
      IF ( ip.EQ.0 ) is = 0
       i  = LEN(fileName)
      IF ( i .LT. lp+ip+is+8 ) THEN
        WRITE(msgBuf,'(A,I6,A,I6)')
     &    'MDS_CHECK4FILE: file name length=', i,
     &    ' too small <', lp+ip+is+8
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R MDS_CHECK4FILE'
      ENDIF

      _BARRIER
      _BEGIN_MASTER( myThid )

C--   Check if file with various suffix exist
      fileExist = .FALSE.
      IF ( .NOT.fileExist .AND. ip.GE.1 ) THEN
C-    look for file = {filePfx}
        WRITE(fileName,'(A)') filePfx(1:ip)
        INQUIRE( FILE=fileName, EXIST=fileExist )
      ENDIF
      IF ( .NOT.fileExist .AND. is.GE.1 ) THEN
C-    look for file = {filePfx}{fileSfx}
        WRITE(fileName,'(2A)') filePfx(1:ip), fileSfx(1:is)
        INQUIRE( FILE=fileName, EXIST=fileExist )
      ENDIF
      IF ( .NOT.fileExist .AND. is.GE.1 ) THEN
C-    look for file = {filePfx}'.{iG}.{jG}'{fileSfx}
        iG = 1+(myXGlobalLo-1)/sNx
        jG = 1+(myYGlobalLo-1)/sNy
        IF ( lp.EQ.0 ) THEN
          WRITE(fileName,'(2A,I3.3,A,I3.3,A)')
     &                  filePfx(1:ip), '.', iG, '.', jG, fileSfx(1:is)
        ELSE
          WRITE(fileName,'(3A,I3.3,A,I3.3,A)') mdsioLocalDir(1:lp),
     &                  filePfx(1:ip), '.', iG, '.', jG, fileSfx(1:is)
        ENDIF
        INQUIRE( FILE=fileName, EXIST=fileExist )
      ENDIF
      IF ( .NOT.fileExist .AND. is.GE.1 ) THEN
C-    look for file = {filePfx}'.001.001'{fileSfx}
        IF ( lp.EQ.0 ) THEN
          WRITE(fileName,'(3A)')
     &                  filePfx(1:ip), '.001.001', fileSfx(1:is)
        ELSE
          WRITE(fileName,'(4A)') mdsioLocalDir(1:lp),
     &                  filePfx(1:ip), '.001.001', fileSfx(1:is)
        ENDIF
        INQUIRE( FILE=fileName, EXIST=fileExist )
      ENDIF

      IF ( .NOT.fileExist ) THEN
        ip = MAX(ILNBLNK(filePfx),1)
        is = MAX(is,1)
        i  = MAX(ILNBLNK(fileName),1)
        iL = ILNBLNK(prtID)
        IF ( iL.GE.1 ) THEN
          WRITE(msgPfx,'(2A)') 'WARNING >> ',prtID(1:iL)
        ELSE
          WRITE(msgPfx,'(2A)') 'WARNING >> MDS_CHECK4FILE'
        ENDIF
        iL = ILNBLNK(msgPfx)
        WRITE(msgBuf,'(7A)') msgPfx(1:iL), ': file: ',
     &       filePfx(1:ip), ' , ', fileSfx(1:is), ' , ', fileName(1:i)
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') msgPfx(1:iL), ': Files DO not exist'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        fileName = ' '
      ENDIF

      shareExist = fileExist

      _END_MASTER( myThid )
      _BARRIER

      fileExist = shareExist

      RETURN
      END
