C $Header: /u/gcmpack/MITgcm/eesupp/src/nml_change_syntax.F,v 1.2 2010/12/26 02:59:37 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: NML_CHANGE_SYNTAX

C     !INTERFACE:
      SUBROUTINE NML_CHANGE_SYNTAX(
     U                              record,
     I                              data_file, myThid )
C     !DESCRIPTION:
C     *=================================================================*
C     | SUBROUTINE NML\_CHANGE\_SYNTAX
C     | o Apply changes to namelist to fit compiler requirement
C     *=================================================================*
C     | Change trailing \& to trailing / when needed
C     | Change array specification from F95 standard
C     |        to commonly accepted F77 form (extented F77)
C     *=================================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#ifdef NML_EXTENDED_F77
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
#endif /* NML_EXTENDED_F77 */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     record    :: current line record (from parameter file) to process
C     data_file :: current parameter file which contains the current record
C     myThid    :: my Thread Id number
      CHARACTER*(MAX_LEN_PREC) record
      CHARACTER*(*) data_file
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER il
      CHARACTER*(2) nmlEnd
#ifdef NML_TERMINATOR
      PARAMETER( nmlEnd = ' /' )
#else
      PARAMETER( nmlEnd = ' &' )
#endif

#ifdef NML_EXTENDED_F77
C     i0      :: position of active "=" (end of variable name definition)
C     i1      :: position of 1rst left parenthesis
C     i2      :: position of 1rst colon
C     i3      :: position of 1rst comma after the 1rst colon
C     i4      :: position of right parenthesis after the 1rst left one
C     nWd     :: number of words following "=" found in this reccord
C     msgBuf  :: Informational/error message buffer
      INTEGER i0, i1, i2, i3, i4
      INTEGER nWd, is, ie, iUnit
      INTEGER i, n, ii
c     INTEGER iLf
      LOGICAL sngQ, dblQ, comma
      LOGICAL hasNum1, hasNum2
      LOGICAL debugPrt
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif /* NML_EXTENDED_F77 */
CEOP

      il = MAX(ILNBLNK(record),1)
      IF ( il .EQ. 2 ) THEN
       IF ( record(1:2) .EQ. ' &' ) THEN
        record(1:2) = nmlEnd
       ENDIF
      ENDIF

#ifdef NML_EXTENDED_F77
      debugPrt = .FALSE.
c     iLf = MAX(ILNBLNK(data_file),1)
      iUnit = errorMessageUnit
      i0 = 0
      i1 = 0
      i2 = 0
      i3 = 0
      i4 = 0
C--   search for end of variable spec ('=' char) and count words that follow
      nWd = 0
      sngQ  = .TRUE.
      dblQ  = .TRUE.
      comma = .FALSE.
      DO i=1,il
        IF ( record(i:i).EQ."'" .AND. dblQ ) THEN
          sngQ = .NOT.sngQ
          IF ( i0.GE.1 .AND. sngQ ) nWd = nWd + 1
        ENDIF
        IF ( record(i:i).EQ.'"' .AND. sngQ ) THEN
          dblQ = .NOT.dblQ
          IF ( i0.GE.1 .AND. dblQ ) nWd = nWd + 1
        ENDIF
        IF ( record(i:i).EQ.'='  .AND. i0.EQ.0
     &                .AND. sngQ .AND. dblQ ) i0 = i
      ENDDO
C--   find position of 1rst set of parenthesis, comma and colon
      DO i=1,i0
        IF ( record(i:i).EQ.'(' .AND. i1.EQ.0 ) i1 = -i
        IF ( record(i:i).EQ.':' .AND. i1.LT.0 ) THEN
          IF ( i2.EQ.0 ) i2 = i
          IF ( comma ) THEN
            WRITE(msgBuf,'(2A)') 'NML_CHANGE_SYNTAX: warning: ',
     &                           'no possible safe conversion of rec:'
            CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
            WRITE(iUnit,'(A)') record(1:il)
            WRITE(msgBuf,'(4A)') 'NML_CHANGE_SYNTAX: ',
     &                           'from file="', data_file, '".'
c    &                           'from file="', data_file(1:iLf), '".'
            CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
            i1 = 1
          ENDIF
        ENDIF
        IF ( record(i:i).EQ.',' .AND. i1.LT.0 ) THEN
          comma = .TRUE.
          IF ( i3.EQ.0 .AND. i2.GE.1 ) i3 = i
        ENDIF
        IF ( record(i:i).EQ.')' .AND. i1.LT.0 ) THEN
          i1 = -i1
          i4 = i
        ENDIF
      ENDDO
      IF ( debugPrt .AND. i0.GE.1 ) THEN
c       WRITE(iUnit,'(5A)') ' ', data_file(1:iLf),
c    &               ' , rec >', record(1:i0), '<'
        WRITE(iUnit,'(5A)') ' ',data_file,' , rec >',record(1:i0),'<'
        WRITE(iUnit,'(A,2I4,L5,A,4I4)')
     &  '  i0,nWd,comma =',i0,nWd,comma,' ; i1,i2,i3,i4 =',i1,i2,i3,i4
      ENDIF
      IF ( i4.EQ.0 .AND. i1.NE.0 ) THEN
        i2 = 0
        IF ( i1.NE.1 ) THEN
          WRITE(msgBuf,'(2A)') 'NML_CHANGE_SYNTAX: ',
     &                         'error in parsing record:'
          CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
          WRITE(iUnit,'(A)') record(1:il)
          WRITE(msgBuf,'(4A)') 'NML_CHANGE_SYNTAX: ',
     &                          'from file="', data_file, '".'
c    &                          'from file="', data_file(1:iLf), '".'
          CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF
C--   Only try conversion if colon found within 1rst pair of parenthesis
      IF ( i2.NE.0 ) THEN
C     check for index value between i1 and i2
       IF ( i2.GT.i1+1 ) THEN
         is = IFNBLNK(record(i1+1:i2-1))
         ie = ILNBLNK(record(i1+1:i2-1))
         i = i1+is
         IF ( record(i:i).EQ.'-' .OR.  record(i:i).EQ.'+' ) is = is+1
         hasNum1 = ( is.GE.1 .AND. is.LE.ie )
         IF ( hasNum1 ) THEN
          DO i=i1+is,i1+ie
           n = ICHAR(record(i:i))
           IF ( n.LT.ICHAR('0') .OR. n.GT.ICHAR('9') ) hasNum1 = .FALSE.
          ENDDO
         ENDIF
       ELSE
         hasNum1 = .FALSE.
       ENDIF
C     check for index value after i2 (and before i3 or i4)
       ii = i4
       IF ( i3.NE.0 ) ii = i3
       IF ( ii.GT.i2+1 ) THEN
         is = IFNBLNK(record(i2+1:ii-1))
         ie = ILNBLNK(record(i2+1:ii-1))
         i = i2+is
         IF ( record(i:i).EQ.'-' .OR.  record(i:i).EQ.'+' ) is = is+1
         hasNum2 = ( is.GE.1 .AND. is.LE.ie )
         IF ( hasNum2 ) THEN
          DO i=i2+is,i2+ie
           n = ICHAR(record(i:i))
           IF ( n.LT.ICHAR('0') .OR. n.GT.ICHAR('9') ) hasNum2 = .FALSE.
          ENDDO
         ENDIF
       ELSE
         hasNum2 = .FALSE.
       ENDIF
       IF ( i3.NE.0 ) THEN
C--   Colon applies to 1rst index of multidim array (found comma after colon)
C     Note: safe case which cannot be confused with sub-string colon
         IF ( hasNum1 .AND. hasNum2 ) THEN
           IF ( debugPrt ) WRITE(iUnit,'(3A)')
     &                           'remove: "',record(i2:i3-1),'"'
           DO i=i2,i3-1
             record(i:i) = ' '
           ENDDO
         ELSE
           WRITE(msgBuf,'(2A)') 'NML_CHANGE_SYNTAX: ',
     &                  'invalid indices for array conversion in:'
           CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
           WRITE(iUnit,'(A)') record(1:il)
           WRITE(msgBuf,'(4A)') 'NML_CHANGE_SYNTAX: ',
     &                          'from file="', data_file, '".'
c    &                          'from file="', data_file(1:iLf), '".'
           CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
       ENDIF
       IF ( i3.EQ.0 .AND. nWd.NE.1 ) THEN
C--   Colon applies to index of vector (single-dim array):
C     discard the case where colon defines sub-string of character-string variable
C     by assuming that in this case 1 and only 1 word follows the equal sign
         IF ( hasNum1 .AND. hasNum2 ) THEN
           IF ( debugPrt ) WRITE(iUnit,'(3A)')
     &                           'remove: "',record(i2:i4-1),'"'
           DO i=i2,i4-1
             record(i:i) = ' '
           ENDDO
         ELSE
           WRITE(msgBuf,'(2A)') 'NML_CHANGE_SYNTAX: ',
     &                  'invalid indices for vector conversion in:'
           CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
           WRITE(iUnit,'(A)') record(1:il)
           WRITE(msgBuf,'(4A)') 'NML_CHANGE_SYNTAX: ',
     &                          'from file="', data_file, '".'
c    &                          'from file="', data_file(1:iLf), '".'
           CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
       ENDIF
C-----
      ENDIF
#endif /* NML_EXTENDED_F77 */

      RETURN
      END
