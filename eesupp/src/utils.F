C $Header: /u/gcmpack/MITgcm/eesupp/src/utils.F,v 1.16 2014/01/19 14:33:43 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

C--   File utils.F: General purpose support routines
C--    Contents
C-- U DATE            - Returns date and time.
C--   IFNBLNK         - Returns index of first non-blank string character.
C--   ILNBLNK         - Returns index of last non-blank string character.
C--   IO_ERRCOUNT     - Reads IO error counter.
C--   LCASE           - Translates to lower case.
C--UM MACHINE         - Returns character string identifying computer.
C--   UCASE           - Translates to upper case.
C--   Routines marked "M" contain specific machine dependent code.
C--   Routines marked "U" contain UNIX OS calls.

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DATE

C     !INTERFACE:
      SUBROUTINE DATE ( string , myThreadId )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE DATE                                          |
C     | o Return current date                                    |
C     *==========================================================*

C     !USES:
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     string     :: Date returned in string
C     myThreadId :: My thread number
      CHARACTER*(*) string
      INTEGER myThreadId

C     !LOCAL VARIABLES:
C     lDate     :: Length of date string
C     msgBuffer :: Temp. for building error messages
      INTEGER lDate
      CHARACTER*(MAX_LEN_MBUF) msgBuffer
CEOP

      lDate = 24
      IF ( LEN(string) .LT. lDate ) GOTO 901
      string = ' '
#ifdef HAVE_FDATE
      CALL FDATE( string )
#endif

 1000 CONTINUE
      RETURN
  901 CONTINUE
      WRITE(msgBuffer,'(A)')
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,'(A)')
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,'(A)')
     &'procedure: "DATE".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,'(A)')
     &'Variable passed to S/R DATE is too small.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,'(A)')
     &' Argument must be at least',lDate,'characters long.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,'(A)')
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: IFNBLNK

C     !INTERFACE:
      INTEGER FUNCTION IFNBLNK( string )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION IFNBLNK                                         |
C     | o Find first non-blank in character string.              |
C     *==========================================================*

C     !INPUT PARAMETERS:
C     string :: String to find first non-blank in.
      CHARACTER*(*) string

C     !LOCAL VARIABLES:
C     L, LS :: Temps for string locations
      INTEGER L, LS
CEOP

      LS     = LEN(string)
      IFNBLNK = 0
      DO 10 L = 1, LS
       IF ( string(L:L) .EQ. ' ' ) GOTO 10
        IFNBLNK = L
        GOTO 11
   10 CONTINUE
   11 CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ILNBLNK

C     !INTERFACE:
      INTEGER FUNCTION ILNBLNK( string )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION ILNBLNK                                         |
C     | o Find last non-blank in character string.               |
C     *==========================================================*

C     !INPUT PARAMETERS:
C     string :: string to scan
      CHARACTER*(*) string

C     !LOCAL VARIABLES:
C     L, LS :: Temps. used in scanning string
      INTEGER L, LS
CEOP

      LS      = LEN(string)
c     ILNBLNK = LS
      ILNBLNK = 0
      DO 10 L = LS, 1, -1
        IF ( string(L:L) .EQ. ' ' ) GOTO 10
         ILNBLNK = L
         GOTO 11
   10 CONTINUE
   11 CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: IO_ERRCOUNT

C     !INTERFACE:
      INTEGER FUNCTION IO_ERRCOUNT(myThid)
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION IO\_ERRCOUNT                                     |
C     | o Reads IO error counter.                                |
C     *==========================================================*

C     !USES:
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: My thread number
      INTEGER myThid

CEOP

      IO_ERRCOUNT = ioErrorCount(myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: LCASE

C     !INTERFACE:
      SUBROUTINE LCASE ( string )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE LCASE                                         |
C     | o Convert character string to all lower case.            |
C     *==========================================================*

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) string

C     !LOCALVARIABLES:
      CHARACTER*26  LOWER
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/
      SAVE LOWER
      CHARACTER*26  UPPER
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      SAVE UPPER
      INTEGER   I, L
CEOP

      DO 10 I = 1, LEN(string)
        L = INDEX(UPPER,string(I:I))
        IF ( L .EQ. 0 ) GOTO 10
         string(I:I) = LOWER(L:L)
   10 CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MACHINE

C     !INTERFACE:
      SUBROUTINE MACHINE ( string )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE MACHINE                                       |
C     | o Return computer identifier in string.                  |
C     *==========================================================*

C     !USES:
#include "SIZE.h"
#include "EEPARAMS.h"
      INTEGER  IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL IFNBLNK
      EXTERNAL ILNBLNK

C     !OUTPUT PARAMETERS:
C     string :: Machine identifier
      CHARACTER*(*) string

C     !LOCAL VARIABLES:
C     iFirst, iLast,      :: String indexing temps.
C     iEnd, iFree, idSize
C     strTmp, idString    :: Temps. for strings.
      INTEGER  iFirst
      INTEGER  iLast
      INTEGER  iEnd
      INTEGER  iFree
      INTEGER  idSize
      CHARACTER*1024 strTmp
      CHARACTER*1024 idString
CEOP

      strTmp = 'UNKNOWN'
      iFree  = 1
      idSize = LEN(string)
#if (defined (TARGET_T3E) || defined (TARGET_CRAY_VECTOR) && !defined (TARGET_NEC_VECTOR))
      IFirst = 0
      CALL PXFGETENV('USER',iFirst,strTmp,ILast,Iend  )
#else
      CALL GETENV('USER',strTmp  )
#endif
      IF ( strTmp .NE. ' ' ) THEN
        iFirst = IFNBLNK(strTmp)
        iLast  = ILNBLNK(strTmp)
        iEnd   = iLast-iFirst+1
        IF (iEnd .GE. 0 ) THEN
         idString(iFree:) = strTmp(iFirst:iFirst+iEnd)
        ENDIF
        iFree = iFree+iEnd+1
        IF ( iFree .LE. idSize ) THEN
          idString(iFree:iFree) = '@'
          iFree = iFree+1
        ENDIF
      ENDIF
      strTmp = 'UNKNOWN'
#if (defined (TARGET_T3E) || defined (TARGET_CRAY_VECTOR) && !defined (TARGET_NEC_VECTOR))
      IFirst = 0
      CALL PXFGETENV('HOST',iFirst,strTmp,ILast,Iend  )
#else
      CALL GETENV('HOST',strTmp  )
#endif
      IF ( strTmp .NE. ' ' ) THEN
        iFirst = IFNBLNK(strTmp)
        iLast  = ILNBLNK(strTmp)
        iEnd   = iLast-iFirst+1
        iEnd   = MIN(iEnd,idSize-iFree)
        iEnd   = iEnd-1
        IF (iEnd .GE. 0 ) THEN
          idString(iFree:) = strTmp(iFirst:iFirst+iEnd)
        ENDIF
        iFree = iFree+iEnd+1
      ENDIF

      string = idString

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: UCASE

C     !INTERFACE:
      SUBROUTINE UCASE ( string )
      IMPLICIT NONE

C     !DESCRIPTION:
C     Translate string to upper case.

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) string

C     !LOCAL VARIABLES:
      CHARACTER*26  LOWER
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/
      SAVE LOWER
      CHARACTER*26  UPPER
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      SAVE UPPER
      INTEGER   I, L
CEOP

      DO 10 I = 1, LEN(string)
        L = INDEX(LOWER,string(I:I))
        IF ( L .EQ. 0 ) GOTO 10
          string(I:I) = UPPER(L:L)
   10 CONTINUE

      RETURN
      END
