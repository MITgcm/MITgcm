C $Header: /u/gcmpack/MITgcm/model/src/Attic/read_write.F,v 1.17 1999/05/05 18:32:35 adcroft Exp $
#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE WRITE_1D_I( fld, lFld, index_type, head, comment )
C     /==========================================================\
C     | o SUBROUTINE WRITE_1D_I                                  |
C     | Controls formatted, tabular I/O for a one-dimensional    |
C     | INTEGER field.                                           |
C     |==========================================================|
C     | This routine produces a standard format for list         |
C     | one-dimensional INTEGER data in textual form. The format |
C     | is designed to be readily parsed by a post-processing    |
C     | uFIELD.                                                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     fld  - Field to be printed
C     lFld - Number of elements in field fld.
C     index_type - Type of index labelling (I=,J=,...) to use
C     head - Statement start e.g. phi =
C     comment - Descriptive comment for field
      INTEGER lFld
      INTEGER fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment
CEndofinterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_I( fld, lFld, index_type, .FALSE., 
     &    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)
C
      RETURN
      END

CStartofinterface
      SUBROUTINE WRITE_1D_L( fld, lFld, index_type, head, comment )
C     /==========================================================\
C     | o SUBROUTINE WRITE_1D_L                                  |
C     | Controls formatted, tabular I/O for a one-dimensional    |
C     | LOGICAL field.                                           |
C     |==========================================================|
C     | This routine produces a standard format for list         |
C     | one-dimensional LOGICAL data in textual form. The format |
C     | is designed to be readily parsed by a post-processing    |
C     | utility.                                                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     fld  - Field to be printed
C     lFld - Number of elements in field fld.
C     index_type - Type of index labelling (I=,J=,...) to use
C     head - Statement start e.g. phi =
C     comment - Descriptive comment for field
      INTEGER lFld
      LOGICAL fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment
CEndofinterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_L( fld, lFld, index_type, .FALSE., 
     &    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)
C
      RETURN
      END

CStartofinterface
      SUBROUTINE WRITE_1D_R8( fld, lFld, index_type, head, comment )
C     /==========================================================\
C     | o SUBROUTINE WRITE_1D_R8                                 |
C     | Controls formatted, tabular I/O for a one-dimensional    |
C     | real*8 field.                                            |
C     |==========================================================|
C     | This routine produces a standard format for list         |
C     | one-dimensional real*8 data in textual form. The format  |
C     | is designed to be readilya parsed by a post-processing   |
C     | utility.                                                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     fld  - Field to be printed
C     lFld - Number of elements in field fld.
C     index_type - Type of index labelling (I=,J=,...) to use
C     head - Statement start e.g. phi =
C     comment - Descriptive comment for field
      INTEGER lFld
      Real*8 fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment
CEndofinterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_R8( fld, lFld, index_type, .FALSE., 
     &    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)
C
      RETURN
      END
