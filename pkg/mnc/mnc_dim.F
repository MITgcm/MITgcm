C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_dim.F,v 1.1 2004/01/06 23:19:27 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C==================================================================

      SUBROUTINE MNC_DIM_INIT( 
     I     myThid, 
     I     dname, 
     I     dunits, 
     I     dlen )

      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) dname
      character*(*) dunits
      integer dlen

C     Functions
      integer ILNBLNK

C     Local Variables
      integer ind, n1,n2
      character*(MAX_LEN_MBUF) msgbuf

C     Check that dname is not already used
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, dname, mnc_d_names, ind)
      IF ( ind .GT. 0 ) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: dimension ''',
     &       dname, ''' already exists--cannot declare it twice'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_DIM_INT'
      ENDIF
      
      CALL MNC_GET_NEXT_EMPTY_IND(myThid, MNC_MAX_ID, mnc_d_names, ind)

C     Check that the dimension name fits in the table
      n1 = ILNBLNK(dname)
      IF ( n1 .GT. MNC_MAX_CHAR) THEN
        n1 = MNC_MAX_CHAR
        write(msgbuf,'(a)') 'MNC WARNING: dimension name too long'
        CALL print_error(msgbuf, mythid)
      ENDIF
      n2 = ILNBLNK(dunits)
      IF ( n2 .GT. MNC_MAX_CHAR) THEN
        n2 = MNC_MAX_CHAR
        write(msgbuf,'(a)') 'MNC WARNING: dimension units too long'
        CALL print_error(msgbuf, mythid)
      ENDIF

C     Add to tables
      mnc_d_names(ind)(1:n1) = dname(1:n1)
      mnc_d_units(ind)(1:n2) = dunits(1:n2)
      mnc_d_size(ind) = dlen

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_DIM_REMOVE( 
     I     myThid, 
     I     dname )

      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) dname

C     Functions
      integer ILNBLNK

C     Local Variables
      integer ind, n
      character*(MAX_LEN_MBUF) msgbuf

C     Check that dname is not already used
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, dname, mnc_d_names, ind)
      IF ( ind .GT. 0 ) THEN
        mnc_d_names(ind)(1:MNC_MAX_CHAR) = 
     &       mnc_blank_name(1:MNC_MAX_CHAR)
      ENDIF

      RETURN
      END
