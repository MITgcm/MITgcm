C $Header: /u/gcmpack/MITgcm/optim/utils.F,v 1.3 2003/11/11 20:38:27 edhill Exp $

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

CStartOfInterface
      SUBROUTINE DATE ( string , myThreadId )
C     /==========================================================\
C     | SUBROUTINE DATE                                          |
C     | o Return current date                                    |
C     \==========================================================/
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
C
      CHARACTER*(*) string
      INTEGER myThreadId
CEndOfInterface
C
      INTEGER lDate
      CHARACTER*(MAX_LEN_MBUF) msgBuffer
C
      lDate = 24
      IF ( LEN(string) .LT. lDate ) GOTO 901
      string = ' '
#ifdef HAVE_FDATE
      CALL FDATE( string )
#endif
C   
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

CStartOfInterface
      INTEGER FUNCTION IFNBLNK( string )
C     /==========================================================\
C     | FUNCTION IFNBLNK                                         |
C     | o Find first non-blank in character string.              |
C     \==========================================================/
      IMPLICIT NONE
C
      CHARACTER*(*) string
CEndOfInterface
C
      INTEGER L, LS
C
      LS     = LEN(string)
      IFNBLNK = 0
      DO 10 L = 1, LS
       IF ( string(L:L) .EQ. ' ' ) GOTO 10
        IFNBLNK = L
        GOTO 11
   10 CONTINUE
   11 CONTINUE
C
      RETURN
      END

CStartOfInterface
      INTEGER FUNCTION ILNBLNK( string )
C     /==========================================================\
C     | FUNCTION ILNBLNK                                         |
C     | o Find last non-blank in character string.               |
C     \==========================================================/
      IMPLICIT NONE
      CHARACTER*(*) string
CEndOfInterface
      INTEGER L, LS
C
      LS      = LEN(string)
      ILNBLNK = LS
      DO 10 L = LS, 1, -1
        IF ( string(L:L) .EQ. ' ' ) GOTO 10
         ILNBLNK = L
         GOTO 11
   10 CONTINUE
   11 CONTINUE
C
      RETURN
      END

CStartofinterface
      INTEGER FUNCTION IO_ERRCOUNT(myThid)
C     /==========================================================\
C     | FUNCTION IO_ERRCOUNT                                     |
C     | o Reads IO error counter.                                |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
      INTEGER myThid
CEndofinterface

      IO_ERRCOUNT = ioErrorCount(myThid)

      RETURN
      END

CStartOfInterface
      SUBROUTINE LCASE ( string )
C     /==========================================================\
C     | SUBROUTINE LCASE                                         |
C     | o Convert character string to all lower case.            |
C     \==========================================================/
      IMPLICIT NONE
      CHARACTER*(*) string
CEndOfInterface
      CHARACTER*26  LOWER
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/
      SAVE LOWER
      CHARACTER*26  UPPER
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      SAVE UPPER
      INTEGER   I, L
C
      DO 10 I = 1, LEN(string)
        L = INDEX(UPPER,string(I:I))
        IF ( L .EQ. 0 ) GOTO 10
         string(I:I) = LOWER(L:L)
   10 CONTINUE
C   
      RETURN
      END

CStartOfInterface
      SUBROUTINE MACHINE ( string )
C     /==========================================================\
C     | SUBROUTINE MACHINE                                       |
C     | o Return computer identifier in string.                  |
C     \==========================================================/
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      CHARACTER*(*) string
CEndOfInterface
C
      INTEGER  IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL IFNBLNK
      EXTERNAL ILNBLNK
C
      INTEGER  iFirst
      INTEGER  iLast 
      INTEGER  iEnd  
      INTEGER  iFree
      INTEGER  idSize
      CHARACTER*1024 strTmp
      CHARACTER*1024 idString

      strTmp = 'UNKNOWN'
      iFree  = 1
      idSize = LEN(string)
#if (defined (TARGET_T3E) || defined (TARGET_CRAY_VECTOR))
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
#if (defined (TARGET_T3E) || defined (TARGET_CRAY_VECTOR))
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
C
      string = idString
C
 1000 CONTINUE
      RETURN
      END
C***********************************************************************
      SUBROUTINE UCASE ( string )
      IMPLICIT NONE
C     Translate string to upper case.
      CHARACTER*(*) string
      CHARACTER*26  LOWER
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/
      SAVE LOWER
      CHARACTER*26  UPPER
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      SAVE UPPER
      INTEGER   I, L
C
      DO 10 I = 1, LEN(string)
        L = INDEX(LOWER,string(I:I))
        IF ( L .EQ. 0 ) GOTO 10
          string(I:I) = UPPER(L:L)
   10 CONTINUE
C   
      RETURN
      END
C************************************************************************
