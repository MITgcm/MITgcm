C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_read_rs_vec.F,v 1.1 2001/03/06 15:28:54 adcroft Exp $
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

C Arguments
      integer dUnit
      integer irec
      integer narr
      _RS arr(narr)
      integer mythid
C Local
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------
      write(msgbuf,'(a,i9.8,2x,i9.8)')
     &  ' MDS_READ_RS_VEC: irec = ',irec,narr
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      read(dUnit,rec=irec) arr
C     ------------------------------------------------------------------
      return
      end
