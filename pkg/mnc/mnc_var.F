C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_var.F,v 1.2 2004/01/06 23:19:27 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C==================================================================

      SUBROUTINE MNC_VAR_INIT_DBL( 
     I     myThid, 
     I     fname, 
     I     gname, 
     I     vname, 
     I     fillval )

      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname
      character*(*) gname
      character*(*) vname
      _RL fillval

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i, ind, fid
      character*(MAX_LEN_MBUF) msgbuf

C     Is the file open?
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, ind)
      IF ( ind .GT. 0 ) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''',
     &       fname, ''' must be opened first'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_FILE_ADD_ATTR_INT'
      ENDIF
      fid = mnc_f_info(ind,2)

      RETURN
      END

