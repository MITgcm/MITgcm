C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_grid.F,v 1.1 2004/01/05 06:20:07 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C==================================================================

      SUBROUTINE MNC_GRID_INIT( 
     I     myThid, 
     I     fname, 
     I     gname, 
     I     ndim, 
     I     dlens, 
     I     dnames, 
     I     dunits )

      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname
      character*(*) gname
      integer ndim
      integer dlens(ndim)
      character*(MNC_MAX_CHAR) dnames(ndim)
      character*(MNC_MAX_CHAR) dunits(ndim)

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i, n, ind, fid
      character*(MAX_LEN_MBUF) msgbuf
      character*(MNC_MAX_CHAR) app

C     Get the file ID
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, ind)
      IF ( ind .GT. 0 ) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''',
     &       fname, ''' must be opened first'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_FILE_ADD_ATTR_INT'
      ENDIF
      fid = mnc_f_info(ind,2)

      

      DO i = 1,ndim

        

      ENDDO

      END

