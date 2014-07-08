C $Header: /u/gcmpack/MITgcm/pkg/openad/ad_s_mds_reclen.F,v 1.3 2014/07/08 19:05:13 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

      subroutine oad_s_MDS_RECLEN( filePrec, nnn, myThid, reclen )
C IN:
C   filePrec    integer :: precision of file in bits
C   nnn         integer :: number of elements in record
C OUT:
C   MDS_RECLEN  integer :: appropriate length of record in bytes or words
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
      integer myThid
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
     &                     SQUEEZE_RIGHT , myThid)
       write(msgbuf,'(a)')
     &   ' MDS_RECLEN: illegal value for filePrec'
       call print_error( msgbuf, myThid )
       stop 'ABNORMAL END: S/R MDS_RECLEN'
      endif

C     ------------------------------------------------------------------
      return
      end
