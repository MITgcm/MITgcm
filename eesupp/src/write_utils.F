C $Header: /u/gcmpack/MITgcm/eesupp/src/write_utils.F,v 1.1 2009/05/07 18:24:40 jmc Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

C--  File write_utils.F: Routine for formatted textual I/O to Standard Output
C--   Contents
C--   o  WRITE_1D_I
C--   o  WRITE_1D_L
C--   o  WRITE_1D_RL
C--   o  WRITE_0D_I
C--   o  WRITE_0D_L
C--   o  WRITE_0D_R4
C--   o  WRITE_0D_R8
C--   o  WRITE_0D_RS
C--   o  WRITE_0D_RL
C--   o  WRITE_0D_C
C--   o  WRITE_COPY1D_R4
C--   o  WRITE_COPY1D_R8
C--   o  WRITE_COPY1D_RS
C--   o  WRITE_XY_XLINE_RS
C--   o  WRITE_XY_YLINE_RS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_1D_I
C     !INTERFACE:
      SUBROUTINE WRITE_1D_I( fld, lFld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_1D_I
C     | Controls formatted, tabular I/O for a one-dimensional
C     | INTEGER field.
C     *==========================================================*
C     | This routine produces a standard format for list
C     | one-dimensional INTEGER data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed
C     lFld       :: Number of elements (in field "fld") to print
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      INTEGER lFld
      INTEGER fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_I( fld, 1, lFld, index_type, .FALSE.,
     &                   .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_1D_L
C     !INTERFACE:
      SUBROUTINE WRITE_1D_L( fld, lFld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_1D_L
C     | Controls formatted, tabular I/O for a one-dimensional
C     | LOGICAL field.
C     *==========================================================*
C     | This routine produces a standard format for list
C     | one-dimensional LOGICAL data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed
C     lFld       :: Number of elements (in field "fld") to print
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      INTEGER lFld
      LOGICAL fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_L( fld, 1, lFld, index_type, .FALSE.,
     &                   .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_1D_RL
C     !INTERFACE:
      SUBROUTINE WRITE_1D_RL( fld, lFld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_1D_RL
C     | Controls formatted, tabular I/O for a one-dimensional
C     | "RL" type field.
C     *==========================================================*
C     | This routine produces a standard format for list
C     | one-dimensional real*8 data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
      EXTERNAL ILNBLNK
      INTEGER  ILNBLNK

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed
C     lFld       :: Number of elements (in field "fld") to print
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      INTEGER lFld
      _RL     fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
C     ILH, ILC - Index of last balnk in head and comment
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ILH, ILC
      INTEGER i, j, ic, i1, i2, nCount
      LOGICAL outpAll
      _RL     prev
CEOP

C-    when list is long, try to write just few of them
      outpAll = .TRUE.
      IF ( ( index_type.EQ.INDEX_I .OR. index_type.EQ.INDEX_J )
     &     .AND. lFld.GT.maxLengthPrt1D ) THEN
C-    Count how many would be written
        nCount = 1
        prev = fld(1)
        DO i= 2,lFld
          IF ( fld(i).NE.prev ) nCount = nCount + 1
          prev = fld(i)
        ENDDO
        IF ( nCount.GT.maxLengthPrt1D ) outpAll = .FALSE.
      ENDIF
      ILH=ILNBLNK(head)
      ILC=ILNBLNK(comment)
      WRITE(msgBuf,'(A,A)') head(1:ILH), comment(1:ILC)
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      IF ( outpAll ) THEN
C-    write all of them
        CALL PRINT_LIST_RL( fld, 1, lFld, index_type, .FALSE.,
     &                      .TRUE., standardMessageUnit )
      ELSE
C-    write just 5 sets of 5-6 consecutive values (truncate 1rst and last)
       DO j=1,4
C  e.g. lFld=33 => 1:1+2  9-2:9+2  17-2:17+2 25-2:25+2 33-2:33
C  e.g. lFld=32 => 1:1+2  8-2:9+2  16-2:17+2 24-2:25+2 32-2:32
        ic = (lFld-1)*(j-1)/4
        i1 = 1+ic-2
        i2 = 1+ic+2
        IF ( ic*4 .LT. (lFld-1)*(j-1) ) i2=i2+1
        i1 = MAX(i1,1)
        i2 = MIN(i2,lFld)
        CALL PRINT_LIST_RL( fld(i1), i1, i2, index_type, .TRUE.,
     &                      .FALSE., standardMessageUnit )
        WRITE(msgBuf,'(A)') '     . . .'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , 1)
       ENDDO
        i1 = MAX(lFld-2,1)
        i2 = lFld
        CALL PRINT_LIST_RL( fld(i1), i1, i2, index_type, .FALSE.,
     &                      .FALSE., standardMessageUnit )
      ENDIF
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_0D_I
C     !INTERFACE:
      SUBROUTINE WRITE_0D_I( fld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_0D_I
C     | Controls formatted, tabular I/O for an INTEGER variable.
C     *==========================================================*
C     | This routine produces a standard format for list
C     | one-dimensional INTEGER data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      INTEGER fld
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER idummy(1)
CEOP

      idummy(1) = fld

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_I( idummy, 1, 1, index_type, .FALSE.,
     &                   .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_0D_L
C     !INTERFACE:
      SUBROUTINE WRITE_0D_L( fld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_0D_L
C     | Controls formatted, tabular I/O for a LOGICAL variable.
C     *==========================================================*
C     | This routine produces a standard format for list
C     | one-dimensional LOGICAL data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      LOGICAL fld
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL ldummy(1)
CEOP

      ldummy(1) = fld
      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_L( ldummy, 1, 1, index_type, .FALSE.,
     &                   .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_0D_R4
C     !INTERFACE:
      SUBROUTINE WRITE_0D_R4( fld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_0D_R4
C     | Controls formatted, tabular I/O for a real*4 variable.
C     *==========================================================*
C     | This routine produces a standard format for list
C     | one-dimensional real*4 data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      Real*4 fld
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL    dummyRL(1)
CEOP

C-    convert to "RL"
      dummyRL(1) = fld

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_RL( dummyRL, 1, 1, index_type, .FALSE.,
     &                    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_0D_R8
C     !INTERFACE:
      SUBROUTINE WRITE_0D_R8( fld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_0D_R8
C     | Controls formatted, tabular I/O for a real*8 variable.
C     *==========================================================*
C     | This routine produces a standard format for list
C     | one-dimensional real*8 data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      Real*8 fld
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL    dummyRL(1)
CEOP

C-    convert to "RL"
      dummyRL(1) = fld

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_RL( dummyRL, 1, 1, index_type, .FALSE.,
     &                    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_0D_RS
C     !INTERFACE:
      SUBROUTINE WRITE_0D_RS( fld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_0D_RS
C     | Controls formatted, tabular I/O for a real "RS" variable.
C     *==========================================================*
C     | This routine produces a standard format for list
C     | one-dimensional real data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      _RS fld
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL    dummyRL(1)
CEOP

C-    convert to "RL"
      dummyRL(1) = fld

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_RL( dummyRL, 1, 1, index_type, .FALSE.,
     &                    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_0D_RL
C     !INTERFACE:
      SUBROUTINE WRITE_0D_RL( fld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_0D_RL
C     | Controls formatted, tabular I/O for a real "RL" variable.
C     *==========================================================*
C     | This routine produces a standard format for list
C     | one-dimensional real data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      _RL    fld
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL    dummyRL(1)
CEOP

C-    copy to array "RL"
      dummyRL(1) = fld

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_RL( dummyRL, 1, 1, index_type, .FALSE.,
     &                    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_0D_C
C     !INTERFACE:
      SUBROUTINE WRITE_0D_C( fld, lFld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_0D_C
C     | Controls formatted, tabular I/O for a character string
C     *==========================================================*
C     | This routine produces a standard format for list
C     | a character string data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed
C     lFld       :: Number of character (in field "fld") to print
C                   0 = all ; -1 & -2 = until the last non-blank
C                  -2 = starting at the first non-blank
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      CHARACTER*(*) fld
      INTEGER lFld
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !FUNCTIONS:
      INTEGER  IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL IFNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iS,iL
CEOP

      iS = 1
      iL = LEN(fld)
      IF ( lFld .GT. 0 ) THEN
        iL = MIN( lFld, iL )
      ELSEIF ( lFld .LT. 0 ) THEN
        iL = ILNBLNK(fld)
      ENDIF
      IF ( lFld .EQ. -2 ) iS = IFNBLNK(fld)
      iS = MAX(1,iS)

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, 1 )
      IF ( iL.GE.iS ) THEN
        iL = MIN( MAX_LEN_MBUF + iS - 17, iL )
        WRITE(msgBuf,'(14X,3A)') "'", fld(iS:iL), "'"
      ELSE
        WRITE(msgBuf,'(14X,3A)') "'","'"
      ENDIF
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, 1 )
      WRITE(msgBuf,'(A)') '    ;'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, 1 )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_COPY1D_R4
C     !INTERFACE:
      SUBROUTINE WRITE_COPY1D_R4(
     O                        tmpBufRL,
     I                        fld, lFld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_COPY1D_R4
C     | Write as formatted I/O to STDOUT a copy (in "RL" buffer)
C     |  of a one-dimensional real*4 field.
C     *==========================================================*
C     | Copy the 1.D array (fld) to "RL" type buffer (tmpBufRL)
C     |  and then call WRITE_1D_RL;
C     | Length of buffer array must be equal or larger than
C     |  input 1.D field (fld) to write.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     tmpBufRL   :: buffer ("RL" type) passed to WRITE_1D_RL
C     fld        :: Field to be printed
C     lFld       :: Number of elements (in field "fld") to print
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      INTEGER lFld
      _RL    tmpBufRL(lFld)
      Real*4 fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i
CEOP

      DO i=1,lFld
        tmpBufRL(i) = fld(i)
      ENDDO
      CALL WRITE_1D_RL( tmpBufRL, lFld, index_type, head, comment )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_COPY1D_R8
C     !INTERFACE:
      SUBROUTINE WRITE_COPY1D_R8(
     O                        tmpBufRL,
     I                        fld, lFld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_COPY1D_R8
C     | Write as formatted I/O to STDOUT a copy (in "RL" buffer)
C     |  of a one-dimensional real*8 field.
C     *==========================================================*
C     | Copy the 1.D array (fld) to "RL" type buffer (tmpBufRL)
C     |  and then call WRITE_1D_RL;
C     | Length of buffer array must be equal or larger than
C     |  input 1.D field (fld) to write.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     tmpBufRL   :: buffer ("RL" type) passed to WRITE_1D_RL
C     fld        :: Field to be printed
C     lFld       :: Number of elements (in field "fld") to print
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      INTEGER lFld
      _RL    tmpBufRL(lFld)
      Real*8 fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i
CEOP

      DO i=1,lFld
        tmpBufRL(i) = fld(i)
      ENDDO
      CALL WRITE_1D_RL( tmpBufRL, lFld, index_type, head, comment )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_COPY1D_RS
C     !INTERFACE:
      SUBROUTINE WRITE_COPY1D_RS(
     O                        tmpBufRL,
     I                        fld, lFld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_COPY1D_RS
C     | Write as formatted I/O to STDOUT a copy (in "RL" buffer)
C     |  of a one-dimensional RS field.
C     *==========================================================*
C     | Copy the 1.D array (fld) to "RL" type buffer (tmpBufRL)
C     |  and then call WRITE_1D_RL;
C     | Length of buffer array must be equal or larger than
C     |  input 1.D field (fld) to write.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     tmpBufRL   :: buffer ("RL" type) passed to WRITE_1D_RL
C     fld        :: Field to be printed
C     lFld       :: Number of elements (in field "fld") to print
C     index_type :: Type of index labelling (I=,J=,...) to use
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      INTEGER lFld
      _RL    tmpBufRL(lFld)
      _RS    fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i
CEOP

      DO i=1,lFld
        tmpBufRL(i) = fld(i)
      ENDDO
      CALL WRITE_1D_RL( tmpBufRL, lFld, index_type, head, comment )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_XY_XLINE_RS
C     !INTERFACE:
      SUBROUTINE WRITE_XY_XLINE_RS(
     I                              fld, sCoord, tCoord,
     I                              head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_XY_XLINE_RS
C     | Prints out X row of an XY RS field e.g. phi(:,n,:,m)
C     *==========================================================*
C     | This routine produces a standard format for list
C     | one-dimensional RS data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed along X.direction
C     sCoord     :: subgrid coordinate
C     tCoord     :: tile coordinate
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      _RS fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER sCoord
      INTEGER tCoord
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !FUNCTIONS:
      EXTERNAL IFNBLNK
      INTEGER  IFNBLNK
      EXTERNAL ILNBLNK
      INTEGER  ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf1
      CHARACTER*(MAX_LEN_MBUF) msgBuf2
      CHARACTER*10 num1, num2
      _RL     xcoord(sNx*nSx)
      INTEGER bi, bj, i, j
      INTEGER IFN1, ILN1, IFN2, ILN2
CEOP

      WRITE(msgBuf1,'(A,A)') head,' = '
      bj = tCoord
      J  = sCoord
      WRITE(num1,'(I10)') J
      WRITE(num2,'(I10)') bj
      IFN1 = IFNBLNK(num1)
      ILN1 = ILNBLNK(num1)
      IFN2 = IFNBLNK(num2)
      ILN2 = ILNBLNK(num2)
C     fld(:,J,:,bj)
      WRITE(msgBuf2,'(A,A,A,A,A,A,A,A,A)')
     & '  /* ', head,'(:,',
     & num1(IFN1:ILN1),',:,',
     & num2(IFN2:ILN2),') ',
     & comment,' */'
      DO bi=1,nSx
       DO I=1,sNx
        xcoord(sNx*(bi-1)+I)=fld(I,J,bi,bj)
       ENDDO
      ENDDO
      CALL WRITE_1D_RL( xcoord, sNx*nSx, INDEX_I,msgBuf1,msgBuf2)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_XY_YLINE_RS
C     !INTERFACE:
      SUBROUTINE WRITE_XY_YLINE_RS(
     I                              fld, sCoord, tCoord,
     I                              head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_XY_YLINE_RS
C     | Prints out Y row of an XY RS field e.g. phi(n,:,m,:)
C     *==========================================================*
C     | This routine produces a standard format for list
C     | one-dimensional RS data in textual form. The format
C     | is designed to be readily parsed by a post-processing
C     | utility.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        :: Field to be printed along Y.direction
C     sCoord     :: subgrid coordinate
C     tCoord     :: tile coordinate
C     head       :: Statement start (e.g. phi = )
C     comment    :: Descriptive comment for field
      _RS fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER sCoord
      INTEGER tCoord
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !FUNCTIONS:
      EXTERNAL IFNBLNK
      INTEGER  IFNBLNK
      EXTERNAL ILNBLNK
      INTEGER  ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf1
      CHARACTER*(MAX_LEN_MBUF) msgBuf2
      _RL     ycoord(sNy*nSy)
      INTEGER bi, bj, i, j
      CHARACTER*10 num1, num2
      INTEGER IFN1, ILN1, IFN2, ILN2
CEOP

      WRITE(msgBuf1,'(A,A)') head,' = '
      bi = tCoord
      I  = sCoord
      WRITE(num1,'(I10)') I
      WRITE(num2,'(I10)') bi
      IFN1 = IFNBLNK(num1)
      ILN1 = ILNBLNK(num1)
      IFN2 = IFNBLNK(num2)
      ILN2 = ILNBLNK(num2)
C     fld(I,:,bi,:)
      WRITE(msgBuf2,'(A,A,A,A,A,A,A,A,A)')
     & '  /* ',head,'(',
     & num1(IFN1:ILN1),',:,',
     & num2(IFN2:ILN2),',:) ',
     & comment,' */'
      DO bj=1,nSy
       DO J=1,sNy
        ycoord(sNy*(bj-1)+J)=fld(I,J,bi,bj)
       ENDDO
      ENDDO
      CALL WRITE_1D_RL( ycoord, sNy*nSy, INDEX_J,msgBuf1,msgBuf2)

      RETURN
      END
