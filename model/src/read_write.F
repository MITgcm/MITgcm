C $Header: /u/gcmpack/MITgcm/model/src/Attic/read_write.F,v 1.21 2001/09/26 18:09:16 cnh Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CStartofinterface
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

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_I( fld, lFld, index_type, .FALSE., 
     &    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)

      END


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

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_L( fld, lFld, index_type, .FALSE., 
     &    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)

      END


CBOP
C     !ROUTINE: WRITE_1D_R8
C     !INTERFACE:
      SUBROUTINE WRITE_1D_R8( fld, lFld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_1D_R8                                  
C     | Controls formatted, tabular I/O for a one-dimensional     
C     | real*8 field.                                             
C     *==========================================================*
C     | This routine produces a standard format for list          
C     | one-dimensional real*8 data in textual form. The format   
C     | is designed to be readilya parsed by a post-processing    
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

C     !LOCAL VARIABLES:
C     == Local variables ==
C     ILH, ILC - Index of last balnk in head and comment
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ILH, ILC
CEOP

      ILH=ILNBLNK(head)
      ILC=ILNBLNK(comment)
      WRITE(msgBuf,'(A,A)') head(1:ILH), comment(1:ILC)
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_R8( fld, lFld, index_type, .FALSE., 
     &    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)

      END


CBOP
C     !ROUTINE: WRITE_0D_I
C     !INTERFACE:
      SUBROUTINE WRITE_0D_I( fld, index_type, head, comment )

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
C     fld  - Field to be printed
C     lFld - Number of elements in field fld.
C     index_type - Type of index labelling (I=,J=,...) to use
C     head - Statement start e.g. phi =
C     comment - Descriptive comment for field
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
     &    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_I( idummy, 1, index_type, .FALSE., 
     &    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)

      END


CBOP
C     !ROUTINE: WRITE_0D_L
C     !INTERFACE:
      SUBROUTINE WRITE_0D_L( fld, index_type, head, comment )

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
C     fld  - Field to be printed
C     lFld - Number of elements in field fld.
C     index_type - Type of index labelling (I=,J=,...) to use
C     head - Statement start e.g. phi =
C     comment - Descriptive comment for field
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
     &    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_L( ldummy, 1, index_type, .FALSE., 
     &    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)

      END


CBOP
C     !ROUTINE: WRITE_0D_R8
C     !INTERFACE:
      SUBROUTINE WRITE_0D_R8( fld, index_type, head, comment )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE WRITE_1D_R8                                  
C     | Controls formatted, tabular I/O for a one-dimensional     
C     | real*8 field.                                             
C     *==========================================================*
C     | This routine produces a standard format for list          
C     | one-dimensional real*8 data in textual form. The format   
C     | is designed to be readilya parsed by a post-processing    
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
C     fld  - Field to be printed
C     lFld - Number of elements in field fld.
C     index_type - Type of index labelling (I=,J=,...) to use
C     head - Statement start e.g. phi =
C     comment - Descriptive comment for field
      Real*8 fld
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      Real*8 r8dummy(1)
CEOP

      r8dummy(1) = fld

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_R8( r8dummy, 1, index_type, .FALSE., 
     &    .TRUE., standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT , 1)

      END


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
      EXTERNAL IFNBLNK
      INTEGER  IFNBLNK
      EXTERNAL ILNBLNK
      INTEGER  ILNBLNK

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld    - Field to be printed
C     sCoord - subgrid coordinate
C     tCoord - tile coordinate
C     head - Statement start e.g. phi =
C     comment - Descriptive comment for field
      _RS fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER sCoord
      INTEGER tCoord
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf1
      CHARACTER*(MAX_LEN_MBUF) msgBuf2
      CHARACTER*10 num1, num2
      REAL*8 xcoord(sNx*nSx)
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
      CALL WRITE_1D_R8( xcoord, sNx*nSx, INDEX_I,msgBuf1,msgBuf2)

      RETURN
      END

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
      EXTERNAL IFNBLNK
      INTEGER  IFNBLNK
      EXTERNAL ILNBLNK
      INTEGER  ILNBLNK

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld    - Field to be printed
C     sCoord - subgrid coordinate
C     tCoord - tile coordinate
C     head - Statement start e.g. phi =
C     comment - Descriptive comment for field
      _RS fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER sCoord
      INTEGER tCoord
      CHARACTER*(*) head
      CHARACTER*(*) comment

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf1
      CHARACTER*(MAX_LEN_MBUF) msgBuf2
      REAL*8 ycoord(sNy*nSy)
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
      CALL WRITE_1D_R8( ycoord, sNy*nSy, INDEX_J,msgBuf1,msgBuf2)

      RETURN
      END
