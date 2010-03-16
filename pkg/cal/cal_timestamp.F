C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_timestamp.F,v 1.3 2010/03/16 00:11:46 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_TimeStamp(
     I                          myiter,
     I                          mytime,
     O                          mydate,
     I                          mythid
     &                        )

c     ==================================================================
c     SUBROUTINE cal_TimeStamp
c     ==================================================================
c
c     o Get and print time step information for the numerical model.
c
c     started: Christian Eckert eckert@mit.edu 04-May-2000
c
c              - Adapted to calendar from the ECCO release routine
c                ecco_TimeStamp.
c
c     ==================================================================
c     SUBROUTINE cal_TimeStamp
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mydate(4)
      integer mythid

      character*(max_len_mbuf) msgbuf

c     == local variables ==

c     == end of interface ==

c--   Get the current date.

      call cal_GetDate( myiter, mytime, mydate, mythid )

c--   Print integration step, time, and the corresponding date
c--   if this is the master thread instance of this routine.

      _BEGIN_MASTER(mythid)
        write(msgbuf,'(a)')
     &  ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i10,a,e15.8)')
     &  ' cal_TimeStamp: iter = ',myiter,' time = ',mytime
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i10,2x,i10)')
     &  '                date = ',mydate(1),mydate(2)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)')
     &  ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER(mythid)

      return
      end

