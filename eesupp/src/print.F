#include "CPP_EEOPTIONS.h"

C--  File printf.F: Routines for performing formatted textual I/O
C--                 in the MITgcm UV implementation environment.
C--   Contents
C--   o PRINT_MESSAGE  Does IO with unhighlighted header
C--   o PRINT_ERROR    Does IO with **ERROR** highlighted header
C--   o PRINT_LIST_I   Prints one-dimensional list of INTEGER
C--                    numbers.
C--   o PRINT_LIST_L   Prints one-dimensional list of LOGICAL
C--                    variables.
C--   o PRINT_LIST_RL  Prints one-dimensional list of Real(_RL)
C--                    numbers.
C--   o PRINT_MAPRS    Formats ABCD... contour map of a Real(_RS) field
C--                    Uses print_message for writing
C--   o PRINT_MAPRL    Formats ABCD... contour map of a Real(_RL) field
C--                    Uses print_message for writing

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_MESSAGE
C     !INTERFACE:
      SUBROUTINE PRINT_MESSAGE( message, unit, sq , myThid )

C     !DESCRIPTION:
C     *============================================================*
C     | SUBROUTINE PRINT\_MESSAGE
C     | o Write out informational message using "standard" format.
C     *============================================================*
C     | Notes
C     | =====
C     | o Some system   I/O is not "thread-safe". For this reason
C     |   without the FMTFTN\_IO\_THREAD\_SAFE directive set a
C     |   critical region is defined around the write here. In some
C     |   cases  BEGIN\_CRIT() is approximated by only doing writes
C     |   for thread number 1 - writes for other threads are
C     |   ignored!
C     | o In a non-parallel form these routines can still be used.
C     |   to produce pretty printed output!
C     *============================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     message :: Message to write
C     unit    :: Unit number to write to
C     sq      :: Justification option
      CHARACTER*(*) message
      INTEGER       unit
      CHARACTER*(*) sq
      INTEGER  myThid

C     !FUNCTIONS:
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iStart, iEnd :: String indexing variables
C     idString     :: Temp. for building prefix.
C     fmtStr, iTmp :: Temp. for building prefix.
C     iTmpThid     :: Temp. for building prefix.
      INTEGER iStart
      INTEGER iEnd
      INTEGER iTmp, iTmpThid
      CHARACTER*13 fmtStr
      CHARACTER*13 idString
CEOP

C--   Find beginning and end of message
      IF ( sq .EQ. SQUEEZE_BOTH .OR.
     &     sq .EQ. SQUEEZE_LEFT ) THEN
       iStart = IFNBLNK( message )
      ELSE
       iStart = 1
      ENDIF
      IF ( sq .EQ. SQUEEZE_BOTH .OR.
     &     sq .EQ. SQUEEZE_RIGHT ) THEN
       iEnd   = ILNBLNK( message )
      ELSE
       iEnd   = LEN(message)
      ENDIF
C--   Test to see if in multi-process ( or multi-threaded ) mode.
C     If so include process or thread identifier.
      IF ( numberOfProcs .EQ. 0 .AND. nThreads .EQ. 1 ) THEN
C--    Write single process format
       IF ( message .EQ. ' ' ) THEN
        WRITE(unit,'(A)') ' '
       ELSE
        WRITE(unit,'(A)') message(iStart:iEnd)
       ENDIF
      ELSEIF ( pidIO .EQ. myProcId ) THEN
C--    Write multi-process format
C      PRINT can be called by several threads simultaneously.
C      The write statement may need to be marked as a critical section.
#ifndef FMTFTN_IO_THREAD_SAFE
# ifdef USE_OMP_THREADING
C$OMP CRITICAL
# else
       _BEGIN_CRIT(myThid)
# endif
#endif
       fmtStr = '(I4.4,A,I4.4)'
       IF ( nPx*nPy .GE. 10000 ) THEN
         iTmp     = 1 + INT(LOG10(DFLOAT(nPx*nPy)))
         iTmpThid = 1 + INT(LOG10(DFLOAT(MAX_NO_THREADS)))
         iTmpThid = MAX( iTmpThid, 2, 8-iTmp )
         WRITE(fmtStr,'(4(A,I1),A)')
     &        '(I',iTmp,'.',iTmp,',A,I',iTmpThid,'.',iTmpThid,')'
       ENDIF
       WRITE(idString,fmtStr) myProcId,'.',myThid
       iTmp = ILNBLNK( idString )
       IF ( message .EQ. ' ' ) THEN
        WRITE(unit,'(A,A,A,A,A,A)',ERR=999)
     &   '(',PROCESS_HEADER,' ',idString(1:iTmp),')',' '
       ELSE
        WRITE(unit,'(A,A,A,A,A,A,A)',ERR=999)
     &   '(',PROCESS_HEADER,' ',idString(1:iTmp),')',' ',
     &   message(iStart:iEnd)
       ENDIF
       IF ( debugMode ) THEN
        CALL MDS_FLUSH( unit, myThid )
       ENDIF
       GOTO 1000
  999  CONTINUE
       ioErrorCount(myThid) = ioErrorCount(myThid)+1
 1000  CONTINUE
#ifndef FMTFTN_IO_THREAD_SAFE
# ifdef USE_OMP_THREADING
C$OMP END CRITICAL
# else
       _END_CRIT(myThid)
# endif
#endif
      ENDIF

#ifndef DISABLE_WRITE_TO_UNIT_ZERO
C--   if error message, also write directly to unit 0 :
      IF ( numberOfProcs .EQ. 1 .AND. myThid .EQ. 1
     &     .AND. unit.EQ.errorMessageUnit
     &     .AND. message .NE. ' ' ) THEN
        IF ( nThreads.LE.1 ) THEN
          WRITE(0,'(A)') message(iStart:iEnd)
        ELSE
          WRITE(0,'(A,I4.4,A,A)') '(TID ', myThid, ') ',
     &                   message(iStart:iEnd)
        ENDIF
      ENDIF
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_ERROR
C     !INTERFACE:
      SUBROUTINE PRINT_ERROR( message , myThid )

C     !DESCRIPTION:
C     *============================================================*
C     | SUBROUTINE PRINT\_ERROR
C     | o Write out error message using "standard" format.
C     *============================================================*
C     | Notes
C     | =====
C     | o Some system   I/O is not "thread-safe". For this reason
C     |   without the FMTFTN\_IO\_THREAD\_SAFE directive set a
C     |   critical region is defined around the write here. In some
C     |   cases  BEGIN\_CRIT() is approximated by only doing writes
C     |   for thread number 1 - writes for other threads are
C     |   ignored!
C     | o In a non-parallel form these routines are still used
C     |   to produce pretty printed output. The process and thread
C     |   id prefix is omitted in this case.
C     *============================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     message :: Text string to print
C     myThid  :: Thread number of this instance
      CHARACTER*(*) message
      INTEGER       myThid

C     !FUNCTIONS:
c     INTEGER  IFNBLNK
c     EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iStart, iEnd :: Temps. for string indexing
C     idString     :: Temp. for building message prefix
C     fmtStr, iTmp :: Temp. for building prefix.
C     iTmpThid     :: Temp. for building prefix.
c     INTEGER iStart
      INTEGER iEnd
      INTEGER iTmp, iTmpThid
      CHARACTER*13 fmtStr
      CHARACTER*13 idString
CEOP

C--   Find beginning and end of message
c     iStart = IFNBLNK( message )
      iEnd   = ILNBLNK( message )
C--   Test to see if in multi-process ( or multi-threaded ) mode.
C     If so include process or thread identifier.
      IF ( numberOfProcs .EQ. 0 .AND. nThreads .EQ. 1 ) THEN
C--    Write single process format
       IF ( iEnd.EQ.0 ) THEN
        WRITE(errorMessageUnit,'(A,1X,A)') ERROR_HEADER, ' '
       ELSE
        WRITE(errorMessageUnit,'(A,1X,A)') ERROR_HEADER,
     &        message(1:iEnd)
c    &    message(iStart:iEnd)
       ENDIF
      ELSE
C       PRINT_ERROR can be called by several threads simulataneously.
C       The write statement may need to be marked as a critical section.
#ifndef FMTFTN_IO_THREAD_SAFE
# ifdef USE_OMP_THREADING
C$OMP CRITICAL
# else
       _BEGIN_CRIT(myThid)
# endif
#endif
       IF ( pidIO .EQ. myProcId ) THEN
C--    Write multi-process format
         fmtStr = '(I4.4,A,I4.4)'
         IF ( nPx*nPy .GE. 10000 ) THEN
           iTmp     = 1 + INT(LOG10(DFLOAT(nPx*nPy)))
           iTmpThid = 1 + INT(LOG10(DFLOAT(MAX_NO_THREADS)))
           iTmpThid = MAX( iTmpThid, 2, 8-iTmp )
           WRITE(fmtStr,'(4(A,I1),A)')
     &          '(I',iTmp,'.',iTmp,',A,I',iTmpThid,'.',iTmpThid,')'
         ENDIF
         WRITE(idString,fmtStr) myProcId,'.',myThid
         iTmp = ILNBLNK( idString )

         IF ( iEnd.EQ.0 ) THEN
          WRITE(errorMessageUnit,'(A,A,1X,A,A,A,A,A)',ERR=999)
     &    '(',PROCESS_HEADER,idString(1:iTmp),')',ERROR_HEADER,' ',
     &    ' '
         ELSE
          WRITE(errorMessageUnit,'(A,A,1X,A,A,A,A,A)',ERR=999)
     &    '(',PROCESS_HEADER,idString(1:iTmp),')',ERROR_HEADER,' ',
     &        message(1:iEnd)
c    &    message(iStart:iEnd)
         ENDIF
       ENDIF
       IF ( debugMode ) THEN
        CALL MDS_FLUSH( errorMessageUnit, myThid )
       ENDIF
       GOTO 1000
  999  CONTINUE
       ioErrorCount(myThid) = ioErrorCount(myThid)+1
 1000  CONTINUE

#ifndef DISABLE_WRITE_TO_UNIT_ZERO
C--    also write directly to unit 0 :
       IF ( numberOfProcs.EQ.1 .AND. iEnd.NE.0 ) THEN
        IF ( nThreads.LE.1 ) THEN
          WRITE(0,'(A)') message(1:iEnd)
        ELSE
          WRITE(0,'(A,I4.4,A,A)') '(TID ', myThid, ') ',
     &                   message(1:iEnd)
        ENDIF
       ENDIF
#endif

#ifndef FMTFTN_IO_THREAD_SAFE
# ifdef USE_OMP_THREADING
C$OMP END CRITICAL
# else
        _END_CRIT(myThid)
# endif
#endif
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_LIST_I
C     !INTERFACE:
      SUBROUTINE PRINT_LIST_I( fld, iFirst, iLast, index_type,
     &                         markEnd, compact, ioUnit )

C     !DESCRIPTION:
C     *==========================================================*
C     | o SUBROUTINE PRINT\_LIST\_I
C     *==========================================================*
C     | Routine for producing list of values for a field with
C     | duplicate values collected into
C     |    n \@ value
C     | record.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld     :: Data to be printed
C     iFirst  :: First element to print
C     iLast   :: Last element to print
C  index_type :: Flag indicating which type of index to print
C                  INDEX_K    => /* K = nnn */
C                  INDEX_I    => /* I = nnn */
C                  INDEX_J    => /* J = nnn */
C                  INDEX_NONE =>
C     markEnd :: Flag to control whether there is a separator after the
C                last element
C     compact :: Flag to control use of repeat symbol for same valued
C                fields.
C     ioUnit  :: Unit number for IO.
      INTEGER iFirst, iLast
      INTEGER fld(iFirst:iLast)
      INTEGER index_type
      LOGICAL markEnd
      LOGICAL compact
      INTEGER ioUnit

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iLo  - Range index holders for selecting elements with
C     iHi    with the same value
C     nDup - Number of duplicates
C     xNew, xOld - Hold current and previous values of field
C     punc - Field separator
C     msgBuf - IO buffer
C     index_lab - Index for labelling elements
C     K    - Loop counter
      INTEGER iLo
      INTEGER iHi
      INTEGER nDup
      INTEGER xNew, xOld
      CHARACTER punc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*2 commOpen,commClose
      CHARACTER*3 index_lab
      CHARACTER*25 fmt1, fmt2
      INTEGER K
CEOP

      IF     ( index_type .EQ. INDEX_I ) THEN
       index_lab = 'I ='
      ELSEIF ( index_type .EQ. INDEX_J ) THEN
       index_lab = 'J ='
      ELSEIF ( index_type .EQ. INDEX_K ) THEN
       index_lab = 'K ='
      ELSE
       index_lab = '?='
      ENDIF
C-    fortran format to write 1 or 2 indices:
      fmt1='(A,1X,A,I3,1X,A)'
      fmt2='(A,1X,A,I3,A,I3,1X,A)'
      IF ( iLast.GE.1000 ) THEN
        K = 1+INT(LOG10(FLOAT(iLast)))
        WRITE(fmt1,'(A,I1,A)')      '(A,1X,A,I',K,',1X,A)'
        WRITE(fmt2,'(A,I1,A,I1,A)') '(A,1X,A,I',K,',A,I',K,',1X,A)'
      ENDIF
      commOpen  = '/*'
      commClose = '*/'
      iLo = iFirst
      iHi = iFirst
      punc = ','
      xOld = fld(iFirst)
      DO K = iFirst+1,iLast
       xNew = fld(K  )
       IF ( .NOT. compact .OR. (xNew .NE. xOld) ) THEN
        nDup = iHi-iLo+1
        IF ( nDup .EQ. 1 ) THEN
         WRITE(msgBuf,'(A,I9,A)') '              ',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt1)
     &    commOpen,index_lab,iLo,commClose
        ELSE
         WRITE(msgBuf,'(I5,'' '',A,I9,A)') nDup,'@',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt2)
     &    commOpen,index_lab,iLo,':',iHi,commClose
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
        iLo  = K
        iHi  = K
        xOld = xNew
       ELSE
        iHi = K
       ENDIF
      ENDDO
      punc = ' '
      IF ( markEnd ) punc = ','
      nDup = iHi-iLo+1
      IF    ( nDup .EQ. 1 ) THEN
       WRITE(msgBuf,'(A,I9,A)') '              ',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt1)
     &    commOpen,index_lab,iLo,commClose
      ELSEIF( nDup .GT. 1 ) THEN
       WRITE(msgBuf,'(I5,'' '',A,I9,A)') nDup,'@',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt2)
     &    commOpen,index_lab,iLo,':',iHi,commClose
      ENDIF
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_LIST_L
C     !INTERFACE:
      SUBROUTINE PRINT_LIST_L( fld, iFirst, iLast, index_type,
     &                         markEnd, compact, ioUnit )

C     !DESCRIPTION:
C     *==========================================================*
C     | o SUBROUTINE PRINT\_LIST\_L
C     *==========================================================*
C     | Routine for producing list of values for a field with
C     | duplicate values collected into
C     |    n \@ value
C     | record.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld     :: Data to be printed
C     iFirst  :: First element to print
C     iLast   :: Last element to print
C  index_type :: Flag indicating which type of index to print
C                  INDEX_K    => /* K = nnn */
C                  INDEX_I    => /* I = nnn */
C                  INDEX_J    => /* J = nnn */
C                  INDEX_NONE =>
C     markEnd :: Flag to control whether there is a separator after the
C                last element
C     compact :: Flag to control use of repeat symbol for same valued
C                fields.
C     ioUnit  :: Unit number for IO.
      INTEGER iFirst, iLast
      LOGICAL fld(iFirst:iLast)
      INTEGER index_type
      LOGICAL markEnd
      LOGICAL compact
      INTEGER ioUnit

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iLo  - Range index holders for selecting elements with
C     iHi    with the same value
C     nDup - Number of duplicates
C     xNew, xOld - Hold current and previous values of field
C     punc - Field separator
C     msgBuf - IO buffer
C     index_lab - Index for labelling elements
C     K    - Loop counter
      INTEGER iLo
      INTEGER iHi
      INTEGER nDup
      LOGICAL xNew, xOld
      CHARACTER punc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*2 commOpen,commClose
      CHARACTER*3 index_lab
      CHARACTER*25 fmt1, fmt2
      INTEGER K
CEOP

      IF     ( index_type .EQ. INDEX_I ) THEN
       index_lab = 'I ='
      ELSEIF ( index_type .EQ. INDEX_J ) THEN
       index_lab = 'J ='
      ELSEIF ( index_type .EQ. INDEX_K ) THEN
       index_lab = 'K ='
      ELSE
       index_lab = '?='
      ENDIF
C-    fortran format to write 1 or 2 indices:
      fmt1='(A,1X,A,I3,1X,A)'
      fmt2='(A,1X,A,I3,A,I3,1X,A)'
      IF ( iLast.GE.1000 ) THEN
        K = 1+INT(LOG10(FLOAT(iLast)))
        WRITE(fmt1,'(A,I1,A)')      '(A,1X,A,I',K,',1X,A)'
        WRITE(fmt2,'(A,I1,A,I1,A)') '(A,1X,A,I',K,',A,I',K,',1X,A)'
      ENDIF
      commOpen  = '/*'
      commClose = '*/'
      iLo = iFirst
      iHi = iFirst
      punc = ','
      xOld = fld(iFirst)
      DO K = iFirst+1,iLast
       xNew = fld(K  )
       IF ( .NOT. compact .OR. (xNew .NEQV. xOld) ) THEN
        nDup = iHi-iLo+1
        IF ( nDup .EQ. 1 ) THEN
         WRITE(msgBuf,'(A,L5,A)') '              ',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt1)
     &    commOpen,index_lab,iLo,commClose
        ELSE
         WRITE(msgBuf,'(I5,'' '',A,L5,A)') nDup,'@',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt2)
     &    commOpen,index_lab,iLo,':',iHi,commClose
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
        iLo  = K
        iHi  = K
        xOld = xNew
       ELSE
        iHi = K
       ENDIF
      ENDDO
      punc = ' '
      IF ( markEnd ) punc = ','
      nDup = iHi-iLo+1
      IF    ( nDup .EQ. 1 ) THEN
       WRITE(msgBuf,'(A,L5,A)') '              ',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &  WRITE(msgBuf(45:),'(A,1X,A,I3,1X,A)')
     &    commOpen,index_lab,iLo,commClose
      ELSEIF( nDup .GT. 1 ) THEN
       WRITE(msgBuf,'(I5,'' '',A,L5,A)') nDup,'@',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &  WRITE(msgBuf(45:),'(A,1X,A,I3,A,I3,1X,A)')
     &  commOpen,index_lab,iLo,':',iHi,commClose
      ENDIF
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_LIST_RL
C     !INTERFACE:
      SUBROUTINE PRINT_LIST_RL( fld, iFirst, iLast, index_type,
     &                          markEnd, compact, ioUnit )

C     !DESCRIPTION:
C     *==========================================================*
C     | o SUBROUTINE PRINT\_LIST\_RL
C     *==========================================================*
C     | Routine for producing list of values for a field with
C     | duplicate values collected into
C     |    n \@ value
C     | record.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld     :: Data to be printed
C     iFirst  :: First element to print
C     iLast   :: Last element to print
C  index_type :: Flag indicating which type of index to print
C                  INDEX_K    => /* K = nnn */
C                  INDEX_I    => /* I = nnn */
C                  INDEX_J    => /* J = nnn */
C                  INDEX_NONE =>
C     markEnd :: Flag to control whether there is a separator after the
C                last element
C     compact :: Flag to control use of repeat symbol for same valued
C                fields.
C     ioUnit  :: Unit number for IO.
      INTEGER iFirst, iLast
      _RL     fld(iFirst:iLast)
      INTEGER index_type
      LOGICAL markEnd
      LOGICAL compact
      INTEGER ioUnit

C     !LOCA VARIABLES:
C     == Local variables ==
C     iLo  - Range index holders for selecting elements with
C     iHi    with the same value
C     nDup - Number of duplicates
C     xNew, xOld - Hold current and previous values of field
C     punc - Field separator
C     msgBuf - IO buffer
C     index_lab - Index for labelling elements
C     K    - Loop counter
      INTEGER iLo
      INTEGER iHi
      INTEGER nDup
      _RL     xNew, xOld
      CHARACTER punc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*2 commOpen,commClose
      CHARACTER*3 index_lab
      CHARACTER*25 fmt1, fmt2
      INTEGER K
CEOP

      IF     ( index_type .EQ. INDEX_I ) THEN
       index_lab = 'I ='
      ELSEIF ( index_type .EQ. INDEX_J ) THEN
       index_lab = 'J ='
      ELSEIF ( index_type .EQ. INDEX_K ) THEN
       index_lab = 'K ='
      ELSE
       index_lab = '?='
      ENDIF
C-    fortran format to write 1 or 2 indices:
      fmt1='(A,1X,A,I3,1X,A)'
      fmt2='(A,1X,A,I3,A,I3,1X,A)'
      IF ( iLast.GE.1000 ) THEN
        K = 1+INT(LOG10(FLOAT(iLast)))
        WRITE(fmt1,'(A,I1,A)')      '(A,1X,A,I',K,',1X,A)'
        WRITE(fmt2,'(A,I1,A,I1,A)') '(A,1X,A,I',K,',A,I',K,',1X,A)'
      ENDIF
      commOpen  = '/*'
      commClose = '*/'
      iLo = iFirst
      iHi = iFirst
      punc = ','
      xOld = fld(iFirst)
      DO K = iFirst+1,iLast
       xNew = fld(K  )
       IF ( .NOT. compact .OR. (xNew .NE. xOld) ) THEN
        nDup = iHi-iLo+1
        IF ( nDup .EQ. 1 ) THEN
         WRITE(msgBuf,'(A,1PE23.15,A)') '              ',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt1)
     &    commOpen,index_lab,iLo,commClose
        ELSE
         WRITE(msgBuf,'(I5,'' '',A,1PE23.15,A)') nDup,'@',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt2)
     &    commOpen,index_lab,iLo,':',iHi,commClose
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
        iLo  = K
        iHi  = K
        xOld = xNew
       ELSE
        iHi = K
       ENDIF
      ENDDO
      punc = ' '
      IF ( markEnd ) punc = ','
      nDup = iHi-iLo+1
      IF    ( nDup .EQ. 1 ) THEN
       WRITE(msgBuf,'(A,1PE23.15,A)') '              ',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &  WRITE(msgBuf(45:),fmt1)
     &    commOpen,index_lab,iLo,commClose
      ELSEIF( nDup .GT. 1 ) THEN
       WRITE(msgBuf,'(I5,'' '',A,1PE23.15,A)') nDup,'@',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &  WRITE(msgBuf(45:),fmt2)
     &  commOpen,index_lab,iLo,':',iHi,commClose
      ENDIF
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_MAPRS
C     !INTERFACE:
      SUBROUTINE PRINT_MAPRS ( fld, fldTitle, plotMode,
     I        iLo,   iHi,   jLo,   jHi,  kLo,  kHi, nBx, nBy,
     I       iMin,  iMax,  iStr,
     I       jMin,  jMax,  jStr,
     I       kMin, kMax,   kStr,
     I      bxMin, bxMax,  bxStr,
     I      byMin, byMax,  byStr )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE PRINT\_MAPRS
C     | o Does textual mapping printing of a field.
C     *==========================================================*
C     | This routine does the actual formatting of the data
C     | and printing to a file. It assumes an array using the
C     | MITgcm UV indexing scheme and base index variables.
C     | User code should call an interface routine like
C     | PLOT\_FIELD\_XYRS( ... ) rather than this code directly.
C     | Text plots can be oriented XY, YZ, XZ. An orientation
C     | is specficied through the "plotMode" argument. All the
C     | plots made by a single call to this routine will use the
C     | same contour interval. The plot range (iMin,...,byStr)
C     | can be three-dimensional. A separate plot is made for
C     | each point in the plot range normal to the orientation.
C     | e.g. if the orientation is XY (plotMode = PRINT\_MAP\_XY).
C     |      kMin =1, kMax = 5 and kStr = 2 will produce three XY
C     |      plots - one for K=1, one for K=3 and one for K=5.
C     |      Each plot would have extents iMin:iMax step iStr
C     |      and jMin:jMax step jStr.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        - Real*4 array holding data to be plotted
C     fldTitle   - Name of field to be plotted
C     plotMode   - Text string indicating plot orientation
C                  ( see - EEPARAMS.h for valid values ).
C     iLo, iHi,  - Dimensions of array fld. fld is assumed to
C     jLo, jHi     be five-dimensional.
C     kLo, kHi
C     nBx, nBy
C     iMin, iMax - Indexing for points to plot. Points from
C     iStr         iMin -> iMax in steps of iStr are plotted
C     jMin. jMax   and similarly for jMin, jMax, jStr and
C     jStr         kMin, kMax, kStr and bxMin, bxMax, bxStr
C     kMin, kMax   byMin, byMax, byStr.
C     kStr
      CHARACTER*(*) fldTitle
      CHARACTER*(*) plotMode
      INTEGER iLo, iHi
      INTEGER jLo, jHi
      INTEGER kLo, kHi
      INTEGER nBx, nBy
      _RS fld(iLo:iHi,jLo:jHi,kLo:kHi,nBx,nBy)
      INTEGER iMin, iMax, iStr
      INTEGER jMin, jMax, jStr
      INTEGER kMin, kMax, kStr
      INTEGER bxMin, bxMax, bxStr
      INTEGER byMin, byMax, byStr

C     !FUNCTIONS:
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     plotBuf - Buffer for building plot record
C     chList  - Character string used for plot
C     fMin, fMax - Contour min, max and range
C     fRange
C     val     - Value of element to be "plotted"
C     small   - Lowest range for which contours are plotted
C     accXXX  - Variables used in indexing accross page records.
C     dwnXXX    Variables used in indexing down the page.
C     pltXXX    Variables used in indexing multiple plots ( multiple
C               plots use same contour range).
C               Lab  - Label
C               Base - Base number for element indexing
C                      The process bottom, left coordinate in the
C                      global domain.
C               Step - Block size
C               Blo  - Start block
C               Bhi  - End block
C               Bstr - Block stride
C               Min  - Start index within block
C               Max  - End index within block
C               Str  - stride within block
      INTEGER MAX_LEN_PLOTBUF
      PARAMETER ( MAX_LEN_PLOTBUF = MAX_LEN_MBUF-20 )
      CHARACTER*(MAX_LEN_PLOTBUF) plotBuf
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER lChList
      PARAMETER ( lChList = 28 )
      CHARACTER*(lChList) chList
      _RL  fMin
      _RL  fMax
      _RL  fRange
      _RL  val
      _RL  small
      CHARACTER*2  accLab
      CHARACTER*7  dwnLab
      CHARACTER*3  pltLab
      INTEGER     accBase, dwnBase, pltBase
      INTEGER     accStep, dwnStep, pltStep
      INTEGER     accBlo,  dwnBlo,  pltBlo
      INTEGER     accBhi,  dwnBhi,  pltBhi
      INTEGER     accBstr, dwnBstr, pltBstr
      INTEGER     accMin,  dwnMin,  pltMin
      INTEGER     accMax,  dwnMax,  pltMax
      INTEGER     accStr,  dwnStr,  pltStr
      INTEGER I, J, K, iStrngLo, iStrngHi, iBuf, iDx
      INTEGER bi, bj, bk
      LOGICAL validRange
CEOP

      chList = '-abcdefghijklmnopqrstuvwxyz+'
      small  =  1. _d -15
      fMin   =  1. _d  32
      fMax   = -1. _d  32
      validRange = .FALSE.

C--   Calculate field range
      DO bj=byMin, byMax, byStr
       DO bi=bxMin, bxMax, bxStr
        DO K=kMin, kMax, kStr
         DO J=jMin, jMax, jStr
          DO I=iMin, iMax, iStr
           IF (printMapIncludesZeros .OR. fld(I,J,K,bi,bj) .NE. 0.) THEN
            IF ( fld(I,J,K,bi,bj) .LT. fMin )
     &       fMin = fld(I,J,K,bi,bj)
            IF ( fld(I,J,K,bi,bj) .GT. fMax )
     &       fMax = fld(I,J,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      fRange = fMax-fMin
      IF ( fRange .GT. small ) validRange = .TRUE.

C--   Write field title and statistics
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      iStrngLo = IFNBLNK(fldTitle)
      iStrngHi = ILNBLNK(fldTitle)
      IF ( iStrngLo .LE. iStrngHi ) THEN
       WRITE(msgBuf,'(A)') fldTitle(iStrngLo:iStrngHi)
      ELSE
       msgBuf = '// UNKNOWN FIELD'
      ENDIF
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1PE30.15)')
     & '// CMIN = ', fMin
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1PE30.15)')
     & '// CMAX = ', fMax
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      IF ( validRange ) THEN
       WRITE(msgBuf,'(A,1PE30.15)')
     &  '// CINT = ', fRange/FLOAT(lChlist-1)
      ELSE
       WRITE(msgBuf,'(A,1PE30.15)')
     &  '// CINT = ', 0.
      ENDIF
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1024A1)')
     & '// SYMBOLS (CMIN->CMAX): ',(chList(I:I),I=1,lChList)
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1024A1)')
     & '//                  0.0: ','.'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I6),A)')
     & '// RANGE I (Lo:Hi:Step):',
     &  '(',myXGlobalLo-1+(bxMin-1)*sNx+iMin,
     &  ':',myXGlobalLo-1+(bxMax-1)*sNx+iMax,
     &  ':',iStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I6),A)')
     & '// RANGE J (Lo:Hi:Step):',
     &  '(',myYGlobalLo-1+(byMin-1)*sNy+jMin,
     &  ':',myYGlobalLo-1+(byMax-1)*sNy+jMax,
     &  ':',jStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I4),A)')
     & '// RANGE K (Lo:Hi:Step):',
     &  '(',kMin,
     &  ':',kMax,
     &  ':',kStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)

c     if (Nx.gt.MAX_LEN_PLOTBUF-20) THEN
c      msgBuf =
c    &  'Model domain too big to print to terminal - skipping I/O'
c      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
c    &                   SQUEEZE_RIGHT, 1)
c      RETURN
c     endif

C--   Write field
C     Figure out slice type and set plotting parameters appropriately
C     acc = accross the page
C     dwn = down the page
      IF ( plotMode .EQ. PRINT_MAP_XY ) THEN
C      X across, Y down slice
       accLab  = 'I='
       accBase = myXGlobalLo
       accStep = sNx
       accBlo  = bxMin
       accBhi  = bxMax
       accBStr = bxStr
       accMin  = iMin
       accMax  = iMax
       accStr  = iStr
       dwnLab  = '|--J--|'
       dwnBase = myYGlobalLo
       dwnStep = sNy
       dwnBlo  = byMin
       dwnBhi  = byMax
       dwnBStr = byStr
       dwnMin  = jMin
       dwnMax  = jMax
       dwnStr  = jStr
       pltBlo  = 1
       pltBhi  = 1
       pltBstr = 1
       pltMin  = kMin
       pltMax  = kMax
       pltStr  = kStr
       pltBase = 1
       pltStep = 1
       pltLab  = 'K ='
      ELSEIF ( plotMode .EQ. PRINT_MAP_YZ ) THEN
C      Y across, Z down slice
       accLab  = 'J='
       accBase = myYGlobalLo
       accStep = sNy
       accBlo  = byMin
       accBhi  = byMax
       accBStr = byStr
       accMin  = jMin
       accMax  = jMax
       accStr  = jStr
       dwnLab  = '|--K--|'
       dwnBase = 1
       dwnStep = 1
       dwnBlo  = 1
       dwnBhi  = 1
       dwnBStr = 1
       dwnMin  = kMin
       dwnMax  = kMax
       dwnStr  = kStr
       pltBlo  = bxMin
       pltBhi  = bxMax
       pltBstr = bxStr
       pltMin  = iMin
       pltMax  = iMax
       pltStr  = iStr
       pltBase = myXGlobalLo
       pltStep = sNx
       pltLab  = 'I ='
      ELSEIF ( plotMode .EQ. PRINT_MAP_XZ ) THEN
C      X across, Z down slice
       accLab  = 'I='
       accBase = myXGlobalLo
       accStep = sNx
       accBlo  = bxMin
       accBhi  = bxMax
       accBStr = bxStr
       accMin  = iMin
       accMax  = iMax
       accStr  = iStr
       dwnLab  = '|--K--|'
       dwnBase = 1
       dwnStep = 1
       dwnBlo  = 1
       dwnBhi  = 1
       dwnBStr = 1
       dwnMin  = kMin
       dwnMax  = kMax
       dwnStr  = kStr
       pltBlo  = byMin
       pltBhi  = byMax
       pltBstr = byStr
       pltMin  = jMin
       pltMax  = jMax
       pltStr  = jStr
       pltBase = myYGlobalLo
       pltStep = sNy
       pltLab  = 'J ='
      ENDIF
C-    check if it fits into buffer (-10 should be enough but -12 is safer):
      IF ( (accMax-accMin+1)*(accBhi-accBlo+1).GT.MAX_LEN_PLOTBUF-12
     &     .AND. validRange ) THEN
       msgBuf =
     &  'Model domain too big to print to terminal - skipping I/O'
       CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       validRange = .FALSE.
      ENDIF
      IF ( validRange ) THEN
C      Header
C      Data
       DO bk=pltBlo, pltBhi, pltBstr
        DO K=pltMin,pltMax,pltStr
         WRITE(plotBuf,'(A,I4,I4,I4,I4)') pltLab,
     &   pltBase-1+(bk-1)*pltStep+K
         CALL PRINT_MESSAGE(plotBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, 1)
         plotBuf = ' '
         iBuf = 6
         DO bi=accBlo, accBhi, accBstr
          DO I=accMin, accMax, accStr
           iDx = accBase-1+(bi-1)*accStep+I
           iBuf = iBuf + 1
           IF ( 10*((iBuf-6)/10) .EQ. iBuf-6 ) THEN
            IF ( iDx .LT. 10 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I1)') accLab,iDx
            ELSEIF ( iDx .LT. 100 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I2)') accLab,iDx
            ELSEIF ( iDx .LT. 1000 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I3)') accLab,iDx
            ELSEIF ( iDx .LT. 10000 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I4)') accLab,iDx
            ENDIF
           ENDIF
          ENDDO
         ENDDO
         WRITE(msgBuf,'(A,A)') '// ',plotBuf
         CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, 1)
         plotBuf = dwnLab
         iBuf = 7
         DO bi=accBlo, accBhi, accBstr
          DO I=accMin, accMax, accStr
           iDx = accBase-1+(bi-1)*accStep+I
           iBuf = iBuf+1
           IF ( 10*((iBuf-7)/10) .EQ. iBuf-7 ) THEN
            WRITE(plotBuf(iBuf:),'(A)')  '|'
           ELSE
            WRITE(plotBuf(iBuf:iBuf),'(I1)') MOD(ABS(iDx),10)
           ENDIF
          ENDDO
         ENDDO
         WRITE(msgBuf,'(A,A)') '// ',plotBuf
         CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, 1)
         DO bj=dwnBlo, dwnBhi, dwnBStr
          DO J=dwnMin, dwnMax, dwnStr
           WRITE(plotBuf,'(1X,I5,1X)')
     &      dwnBase-1+(bj-1)*dwnStep+J
           iBuf = 7
           DO bi=accBlo,accBhi,accBstr
            DO I=accMin,accMax,accStr
             iBuf = iBuf + 1
             IF     ( plotMode .EQ. PRINT_MAP_XY ) THEN
              val = fld(I,J,K,bi,bj)
             ELSEIF ( plotMode .EQ. PRINT_MAP_XZ ) THEN
              val = fld(I,K,J,bi,bk)
             ELSEIF ( plotMode .EQ. PRINT_MAP_YZ ) THEN
              val = fld(K,I,J,bk,bi)
             ENDIF
             IF ( validRange .AND. val .NE. 0. ) THEN
              IDX = NINT(
     &              FLOAT( lChList-1 )*( val-fMin ) / (fRange)
     &             )+1
             ELSE
              IDX = 1
             ENDIF
             IF ( iBuf .LE. MAX_LEN_PLOTBUF )
     &        plotBuf(iBuf:iBuf) = chList(IDX:IDX)
             IF ( val .EQ. 0. ) THEN
              IF ( iBuf .LE. MAX_LEN_PLOTBUF )
     &         plotBuf(iBuf:iBuf) = '.'
             ENDIF
            ENDDO
           ENDDO
           WRITE(msgBuf,'(A,A)') '// ',plotBuf
           CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, 1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C--   Write delimiter
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// END OF FIELD                                          ='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf = ' '
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_MAPRL
C     !INTERFACE:
      SUBROUTINE PRINT_MAPRL ( fld, fldTitle, plotMode,
     I        iLo,   iHi,   jLo,   jHi,  kLo,  kHi, nBx, nBy,
     I       iMin,  iMax,  iStr,
     I       jMin,  jMax,  jStr,
     I       kMin, kMax,   kStr,
     I      bxMin, bxMax,  bxStr,
     I      byMin, byMax,  byStr )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE PRINT\_MAPRL
C     | o Does textual mapping printing of a field.
C     *==========================================================*
C     | This routine does the actual formatting of the data
C     | and printing to a file. It assumes an array using the
C     | MITgcm UV indexing scheme and base index variables.
C     | User code should call an interface routine like
C     | PLOT\_FIELD\_XYRL( ... ) rather than this code directly.
C     | Text plots can be oriented XY, YZ, XZ. An orientation
C     | is specficied through the "plotMode" argument. All the
C     | plots made by a single call to this routine will use the
C     | same contour interval. The plot range (iMin,...,byStr)
C     | can be three-dimensional. A separate plot is made for
C     | each point in the plot range normal to the orientation.
C     | e.g. if the orientation is XY (plotMode = PRINT\_MAP\_XY).
C     |      kMin =1, kMax = 5 and kStr = 2 will produce three XY
C     |      plots - one for K=1, one for K=3 and one for K=5.
C     |      Each plot would have extents iMin:iMax step iStr
C     |      and jMin:jMax step jStr.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        - Real*8 array holding data to be plotted
C     fldTitle   - Name of field to be plotted
C     plotMode   - Text string indicating plot orientation
C                  ( see - EEPARAMS.h for valid values ).
C     iLo, iHi,  - Dimensions of array fld. fld is assumed to
C     jLo, jHi     be five-dimensional.
C     kLo, kHi
C     nBx, nBy
C     iMin, iMax - Indexing for points to plot. Points from
C     iStr         iMin -> iMax in steps of iStr are plotted
C     jMin. jMax   and similarly for jMin, jMax, jStr and
C     jStr         kMin, kMax, kStr and bxMin, bxMax, bxStr
C     kMin, kMax   byMin, byMax, byStr.
C     kStr
      CHARACTER*(*) fldTitle
      CHARACTER*(*) plotMode
      INTEGER iLo, iHi
      INTEGER jLo, jHi
      INTEGER kLo, kHi
      INTEGER nBx, nBy
      _RL fld(iLo:iHi,jLo:jHi,kLo:kHi,nBx,nBy)
      INTEGER iMin, iMax, iStr
      INTEGER jMin, jMax, jStr
      INTEGER kMin, kMax, kStr
      INTEGER bxMin, bxMax, bxStr
      INTEGER byMin, byMax, byStr

C     !FUNCTIONS:
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     plotBuf - Buffer for building plot record
C     chList  - Character string used for plot
C     fMin, fMax - Contour min, max and range
C     fRange
C     val     - Value of element to be "plotted"
C     small   - Lowest range for which contours are plotted
C     accXXX  - Variables used in indexing accross page records.
C     dwnXXX    Variables used in indexing down the page.
C     pltXXX    Variables used in indexing multiple plots ( multiple
C               plots use same contour range).
C               Lab  - Label
C               Base - Base number for element indexing
C                      The process bottom, left coordinate in the
C                      global domain.
C               Step - Block size
C               Blo  - Start block
C               Bhi  - End block
C               Bstr - Block stride
C               Min  - Start index within block
C               Max  - End index within block
C               Str  - stride within block
      INTEGER MAX_LEN_PLOTBUF
      PARAMETER ( MAX_LEN_PLOTBUF = MAX_LEN_MBUF-20 )
      CHARACTER*(MAX_LEN_PLOTBUF) plotBuf
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER lChList
      PARAMETER ( lChList = 28 )
      CHARACTER*(lChList) chList
      _RL  fMin
      _RL  fMax
      _RL  fRange
      _RL  val
      _RL  small
      CHARACTER*2  accLab
      CHARACTER*7  dwnLab
      CHARACTER*3  pltLab
      INTEGER     accBase, dwnBase, pltBase
      INTEGER     accStep, dwnStep, pltStep
      INTEGER     accBlo,  dwnBlo,  pltBlo
      INTEGER     accBhi,  dwnBhi,  pltBhi
      INTEGER     accBstr, dwnBstr, pltBstr
      INTEGER     accMin,  dwnMin,  pltMin
      INTEGER     accMax,  dwnMax,  pltMax
      INTEGER     accStr,  dwnStr,  pltStr
      INTEGER I, J, K, iStrngLo, iStrngHi, iBuf, iDx
      INTEGER bi, bj, bk
      LOGICAL validRange
CEOP

      chList = '-abcdefghijklmnopqrstuvwxyz+'
      small  = 1. _d -15
      fMin   =  1. _d 32
      fMax   = -1. _d 32
      validRange = .FALSE.

C--   Calculate field range
      DO bj=byMin, byMax, byStr
       DO bi=bxMin, bxMax, bxStr
        DO K=kMin, kMax, kStr
         DO J=jMin, jMax, jStr
          DO I=iMin, iMax, iStr
           IF ( printMapIncludesZeros .OR. fld(I,J,K,bi,bj) .NE. 0. )
     &     THEN
            IF ( fld(I,J,K,bi,bj) .LT. fMin )
     &       fMin = fld(I,J,K,bi,bj)
            IF ( fld(I,J,K,bi,bj) .GT. fMax )
     &       fMax = fld(I,J,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      fRange = fMax-fMin
      IF ( fRange .GT. small ) validRange = .TRUE.

C--   Write field title and statistics
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      iStrngLo = IFNBLNK(fldTitle)
      iStrngHi = ILNBLNK(fldTitle)
      IF ( iStrngLo .LE. iStrngHi ) THEN
       WRITE(msgBuf,'(A)') fldTitle(iStrngLo:iStrngHi)
      ELSE
       msgBuf = '// UNKNOWN FIELD'
      ENDIF
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1PE30.15)')
     & '// CMIN = ', fMin
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1PE30.15)')
     & '// CMAX = ', fMax
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      IF ( validRange ) THEN
       WRITE(msgBuf,'(A,1PE30.15)')
     & '// CINT = ', fRange/FLOAT(lChlist-1)
      ELSE
       WRITE(msgBuf,'(A,1PE30.15)')
     & '// CINT = ', 0.
      ENDIF
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1024A1)')
     & '// SYMBOLS (CMIN->CMAX): ',(chList(I:I),I=1,lChList)
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1024A1)')
     & '//                  0.0: ','.'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I6),A)')
     & '// RANGE I (Lo:Hi:Step):',
     &  '(',myXGlobalLo-1+(bxMin-1)*sNx+iMin,
     &  ':',myXGlobalLo-1+(bxMax-1)*sNx+iMax,
     &  ':',iStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I6),A)')
     & '// RANGE J (Lo:Hi:Step):',
     &  '(',myYGlobalLo-1+(byMin-1)*sNy+jMin,
     &  ':',myYGlobalLo-1+(byMax-1)*sNy+jMax,
     &  ':',jStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I4),A)')
     & '// RANGE K (Lo:Hi:Step):',
     &  '(',kMin,
     &  ':',kMax,
     &  ':',kStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)

c     if (Nx.gt.MAX_LEN_PLOTBUF-20) THEN
c      msgBuf =
c    &  'Model domain too big to print to terminal - skipping I/O'
c      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
c    &                   SQUEEZE_RIGHT, 1)
c      RETURN
c     endif

C--   Write field
C     Figure out slice type and set plotting parameters appropriately
C     acc = accross the page
C     dwn = down the page
      IF ( plotMode .EQ. PRINT_MAP_XY ) THEN
C      X across, Y down slice
       accLab  = 'I='
       accBase = myXGlobalLo
       accStep = sNx
       accBlo  = bxMin
       accBhi  = bxMax
       accBStr = bxStr
       accMin  = iMin
       accMax  = iMax
       accStr  = iStr
       dwnLab  = '|--J--|'
       dwnBase = myYGlobalLo
       dwnStep = sNy
       dwnBlo  = byMin
       dwnBhi  = byMax
       dwnBStr = byStr
       dwnMin  = jMin
       dwnMax  = jMax
       dwnStr  = jStr
       pltBlo  = 1
       pltBhi  = 1
       pltBstr = 1
       pltMin  = kMin
       pltMax  = kMax
       pltStr  = kStr
       pltBase = 1
       pltStep = 1
       pltLab  = 'K ='
      ELSEIF ( plotMode .EQ. PRINT_MAP_YZ ) THEN
C      Y across, Z down slice
       accLab  = 'J='
       accBase = myYGlobalLo
       accStep = sNy
       accBlo  = byMin
       accBhi  = byMax
       accBStr = byStr
       accMin  = jMin
       accMax  = jMax
       accStr  = jStr
       dwnLab  = '|--K--|'
       dwnBase = 1
       dwnStep = 1
       dwnBlo  = 1
       dwnBhi  = 1
       dwnBStr = 1
       dwnMin  = kMin
       dwnMax  = kMax
       dwnStr  = kStr
       pltBlo  = bxMin
       pltBhi  = bxMax
       pltBstr = bxStr
       pltMin  = iMin
       pltMax  = iMax
       pltStr  = iStr
       pltBase = myXGlobalLo
       pltStep = sNx
       pltLab  = 'I ='
      ELSEIF ( plotMode .EQ. PRINT_MAP_XZ ) THEN
C      X across, Z down slice
       accLab  = 'I='
       accBase = myXGlobalLo
       accStep = sNx
       accBlo  = bxMin
       accBhi  = bxMax
       accBStr = bxStr
       accMin  = iMin
       accMax  = iMax
       accStr  = iStr
       dwnLab  = '|--K--|'
       dwnBase = 1
       dwnStep = 1
       dwnBlo  = 1
       dwnBhi  = 1
       dwnBStr = 1
       dwnMin  = kMin
       dwnMax  = kMax
       dwnStr  = kStr
       pltBlo  = byMin
       pltBhi  = byMax
       pltBstr = byStr
       pltMin  = jMin
       pltMax  = jMax
       pltStr  = jStr
       pltBase = myYGlobalLo
       pltStep = sNy
       pltLab  = 'J ='
      ENDIF
C-    check if it fits into buffer (-10 should be enough but -12 is safer):
      IF ( (accMax-accMin+1)*(accBhi-accBlo+1).GT.MAX_LEN_PLOTBUF-12
     &     .AND. validRange ) THEN
       msgBuf =
     &  'Model domain too big to print to terminal - skipping I/O'
       CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       validRange = .FALSE.
      ENDIF
      IF ( validRange ) THEN
C      Header
C      Data
       DO bk=pltBlo, pltBhi, pltBstr
        DO K=pltMin,pltMax,pltStr
         WRITE(plotBuf,'(A,I4,I4,I4,I4)') pltLab,
     &   pltBase-1+(bk-1)*pltStep+K
         CALL PRINT_MESSAGE(plotBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, 1)
         plotBuf = ' '
         iBuf = 6
         DO bi=accBlo, accBhi, accBstr
          DO I=accMin, accMax, accStr
           iDx = accBase-1+(bi-1)*accStep+I
           iBuf = iBuf + 1
           IF ( 10*((iBuf-6)/10) .EQ. iBuf-6 ) THEN
            IF ( iDx .LT. 10 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I1)') accLab,iDx
            ELSEIF ( iDx .LT. 100 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I2)') accLab,iDx
            ELSEIF ( iDx .LT. 1000 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I3)') accLab,iDx
            ELSEIF ( iDx .LT. 10000 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I4)') accLab,iDx
            ENDIF
           ENDIF
          ENDDO
         ENDDO
         CALL PRINT_MESSAGE(plotBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, 1)
         plotBuf = dwnLab
         iBuf = 7
         DO bi=accBlo, accBhi, accBstr
          DO I=accMin, accMax, accStr
           iDx = accBase-1+(bi-1)*accStep+I
           iBuf = iBuf+1
           IF ( 10*((iBuf-7)/10) .EQ. iBuf-7 ) THEN
            WRITE(plotBuf(iBuf:),'(A)')  '|'
           ELSE
            WRITE(plotBuf(iBuf:iBuf),'(I1)') MOD(ABS(iDx),10)
           ENDIF
          ENDDO
         ENDDO
         CALL PRINT_MESSAGE(plotBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, 1)
         DO bj=dwnBlo, dwnBhi, dwnBStr
          DO J=dwnMin, dwnMax, dwnStr
           WRITE(plotBuf,'(1X,I5,1X)')
     &      dwnBase-1+(bj-1)*dwnStep+J
           iBuf = 7
           DO bi=accBlo,accBhi,accBstr
            DO I=accMin,accMax,accStr
             iBuf = iBuf + 1
             IF     ( plotMode .EQ. PRINT_MAP_XY ) THEN
              val = fld(I,J,K,bi,bj)
             ELSEIF ( plotMode .EQ. PRINT_MAP_XZ ) THEN
              val = fld(I,K,J,bi,bk)
             ELSEIF ( plotMode .EQ. PRINT_MAP_YZ ) THEN
              val = fld(K,I,J,bk,bi)
             ENDIF
             IF ( validRange .AND. val .NE. 0. ) THEN
              IDX = NINT(
     &               FLOAT( lChList-1 )*( val-fMin ) / (fRange)
     &              )+1
             ELSE
              IDX = 1
             ENDIF
             IF ( iBuf .LE. MAX_LEN_PLOTBUF )
     &        plotBuf(iBuf:iBuf) = chList(IDX:IDX)
             IF ( val .EQ. 0. ) THEN
              IF ( iBuf .LE. MAX_LEN_PLOTBUF )
     &         plotBuf(iBuf:iBuf) = '.'
             ENDIF
            ENDDO
           ENDDO
           CALL PRINT_MESSAGE(plotBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, 1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C--   Write delimiter
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// END OF FIELD                                          ='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf = ' '
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)

      RETURN
      END
