C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_findunit.F,v 1.1 2001/03/06 15:28:53 adcroft Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      subroutine MDSFINDUNIT( iounit, mythid )
C OUT:
C     iounit   integer - unit number
C
C MDSFINDUNIT returns a valid, unused unit number for f77 I/O
C The routine stops the program is an error occurs in the process
C of searching the I/O channels.
C
C Created: 03/20/99 adcroft@mit.edu

      implicit none

#include "EEPARAMS.h"

C Arguments
      integer iounit
      integer mythid
C Local
      integer ii
      logical op
      integer ios
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------

C Sweep through a valid range of unit numbers
      iounit=-1
      do ii=9,99
        if (iounit.eq.-1) then
          inquire(unit=ii,iostat=ios,opened=op)
          if (ios.ne.0) then
            write(msgbuf,'(a,i2.2)')
     &        ' MDSFINDUNIT: inquiring unit number = ',ii
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a)')
     &        ' MDSFINDUNIT: inquire statement failed!'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSFINDUNIT'
          endif
          if (.NOT. op) then
            iounit=ii
          endif
        endif
      enddo

C Was there an available unit number
      if (iounit.eq.-1) then
        write(msgbuf,'(a)')
     &    ' MDSFINDUNIT: could not find an available unit number!'
        call print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MDSFINDUNIT'
      endif

C     ------------------------------------------------------------------
      return
      end
