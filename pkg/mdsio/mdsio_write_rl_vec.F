C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_write_rl_vec.F,v 1.1 2001/03/06 15:28:54 adcroft Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      subroutine MDS_WRITE_RL_VEC( dUnit, irec, narr, arr, mythid )
C IN:
C   dunit       integer - 'Opened' I/O channel
C   irec        integer - record number to write
C   narr        integer - dimension off array "arr"
C   arr         _RL     - model tiled vector
C   mythid      integer - thread id
C
C Created: 03/29/99 eckert@mit.edu + adcroft@mit.edu

      implicit none

#include "EEPARAMS.h"

C Arguments
      integer dUnit
      integer irec
      integer narr
      integer mythid
      _RL arr(narr)
C Local
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------
      write(msgbuf,'(a,i9.8,2x,i9.8)')
     &  ' MDS_WRITE_RL_VEC: irec = ',irec,narr
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(dUnit,rec=irec) arr
C     ------------------------------------------------------------------
      return
      end
