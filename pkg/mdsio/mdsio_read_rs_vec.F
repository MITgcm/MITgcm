C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_read_rs_vec.F,v 1.3 2004/11/30 16:11:10 heimbach Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      subroutine MDS_READ_RS_VEC( dUnit, irec, narr, arr, mythid )
C IN:
C   dunit	integer - 'Opened' I/O channel
C   irec	integer - record number to write
C   narr	integer - dimension off array "arr"
C   mythid      integer - thread id
C OUT:
C   arr		_RS     - model tiled vector
C
C Created: 03/29/99 eckert@mit.edu + adcroft@mit.edu

      implicit none

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

C Arguments
      integer dUnit
      integer irec
      integer narr
      _RS arr(narr)
      integer mythid
C Local
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------
      if ( debugLevel .GE. debLevB ) then
       write(msgbuf,'(a,i9.8,2x,i9.8)')
     &  ' MDS_READ_RS_VEC: irec = ',irec,narr
       call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      endif

      read(dUnit,rec=irec) arr
C     ------------------------------------------------------------------
      return
      end
