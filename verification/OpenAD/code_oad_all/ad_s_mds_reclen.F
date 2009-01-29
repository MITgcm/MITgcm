#include "CPP_EEOPTIONS.h"

      subroutine oad_s_MDS_RECLEN( filePrec, nnn, mythid, reclen )
C IN:
C   filePrec	integer - precision of file in bits
C   nnn		integer - number of elements in record
C OUT:
C   MDS_RECLEN  integer - appropriate length of record in bytes or words
C
C Created: 03/29/99 eckert@mit.edu + adcroft@mit.edu

      implicit none

C Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C
C Arguments
      integer filePrec
      integer nnn
      integer mythid
      integer reclen
C
C Local
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------

      if (filePrec .EQ. precFloat32) then
       reclen=nnn*WORDLENGTH
      elseif (filePrec .EQ. precFloat64) then
       reclen=nnn*WORDLENGTH*2
      else
       write(msgbuf,'(a,i2.2)')
     &   ' MDS_RECLEN: filePrec = ',filePrec
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDS_RECLEN: illegal value for filePrec'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDS_RECLEN'
      endif

C     ------------------------------------------------------------------
      return
      end
