#include "ECCO_OPTIONS.h"

      SUBROUTINE ECCO_SUMMARY( mythid )

c     ==================================================================
c     SUBROUTINE ecco_summary
c     ==================================================================
c
c     o Summarize the setup of the ECCO release.
c
c     ==================================================================
c     SUBROUTINE ecco_summary
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"

c     == routine arguments ==
c     integer mythid - thread number for this instance of the routine.
      integer mythid

c     == local variables ==
      character*(max_len_mbuf) msgbuf
#ifdef ALLOW_GENCOST_CONTRIBUTION
      integer k, k2, il, msgunit
#endif

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ECCO configuration >>> START <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

#ifdef ALLOW_GENCOST_CONTRIBUTION
      msgunit=standardmessageunit
c--
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
c--
      do k = 1, NGENCOST
       if (using_gencost(k)) then
c
        il = ilnblnk(gencost_name(k))
        write(msgbuf,'(a,i2,a,a)') 'gencost(',k,
     &        ') = ',gencost_name(k)(1:il)
        call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') '-------------'
        call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
c
        if ( gencost_datafile(k) .ne. ' ' ) then
         il = ilnblnk(gencost_datafile(k))
         write(msgbuf,'(a,a)') ' data file = ',gencost_datafile(k)(1:il)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
        endif
c
         if (gencost_nrec(k).GT.1) then
          write(msgbuf,'(a)') '  starts and ends at : '
          call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
          CALL CAL_PRINTDATE(gencost_startdate(1,k),mythid)
          CALL CAL_PRINTDATE(gencost_enddate(1,k),mythid)
          write(msgbuf,'(a,i5)') '  number of records = ',
     &                            gencost_nrec(k)
          call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif
c
         il = ilnblnk(gencost_barfile(k))
         write(msgbuf,'(a,a)') ' model file = ',gencost_barfile(k)(1:il)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         if ((gencost_barfile(k)(1:9).EQ.'m_ptracer').OR.
     &       (gencost_barfile(k)(1:17).EQ.'m_boxmean_ptracer')) then
           write(msgbuf,'(a,i2)') ' gencost_itracer = ',
     &           gencost_itracer(k)
           call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif
         il = ilnblnk(gencost_errfile(k))
         write(msgbuf,'(a,a)') ' error file = ',gencost_errfile(k)(1:il)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         if (gencost_scalefile(k).NE.' ') then
         il = ilnblnk(gencost_scalefile(k))
         write(msgbuf,'(a,a)') ' scale file = ',
     &        gencost_scalefile(k)(1:il)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif
         do k2 = 1, NGENPPROC
          if (gencost_preproc(k2,k).NE.' ') then
            il = ilnblnk(gencost_preproc(k2,k))
            write(msgbuf,'(a,a)') ' preprocess = ',
     &                            gencost_preproc(k2,k)(1:il)
            call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
          endif
          if (gencost_posproc(k2,k).NE.' ') then
            il = ilnblnk(gencost_posproc(k2,k))
            write(msgbuf,'(a,a)') ' posprocess = ',
     &                            gencost_posproc(k2,k)(1:il)
            call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
          endif
         enddo
c
         write(msgbuf,'(a,i2)') ' gencost_flag = ',gencost_flag(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         write(msgbuf,'(a,i2)') ' gencost_outputlevel = ',
     &         gencost_outputlevel(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         write(msgbuf,'(a,i2)') ' gencost_kLev_select = ',
     &         gencost_kLev_select(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
c
         if (gencost_is3d(k)) then
         write(msgbuf,'(a,i2)') ' gencost_pointer3d = ',
     &         gencost_pointer3d(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif
         if (gencost_timevaryweight(k)) then
         write(msgbuf,'(a,L2)') ' time varia. weight = ',
     &         gencost_timevaryweight(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif
         if (gencost_barskip(k)) then
         write(msgbuf,'(a,L2)') ' skip barfile write = ',
     &         gencost_barskip(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif

        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
c
       endif
      enddo
c--
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
#endif /* ALLOW_GENCOST_CONTRIBUTION */

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ECCO configuration  >>> END <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      RETURN
      END
