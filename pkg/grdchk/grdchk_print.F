C
C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_print.F,v 1.9 2003/10/27 22:32:55 heimbach Exp $
C $Name:  $

#include "AD_CONFIG.h"
#include "CPP_OPTIONS.h"


      subroutine grdchk_print(
     I                         ichknum,
     I                         ierr_grdchk,
     I                         mythid
     &                       )

c     ==================================================================
c     SUBROUTINE grdchk_print
c     ==================================================================
c
c     o Print the results of the gradient check.
c
c     started: Christian Eckert eckert@mit.edu 08-Mar-2000
c     continued: heimbach@mit.edu: 13-Jun-2001
c
c     ==================================================================
c     SUBROUTINE grdchk_print
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "grdchk.h"

c     == routine arguments ==

      integer ichknum
      integer ierr_grdchk
      integer mythid 

#ifdef ALLOW_GRDCHK
c     == local variables ==

      _RL fcref
      _RL fcpertplus, fcpertminus
      _RL xxmemo_ref
      _RL xxmemo_pert
      _RL gfd
      _RL adxxmemo
      _RL ftlxxmemo
      _RL ratio_ad
      _RL ratio_ftl

      integer i
      integer itile
      integer jtile
      integer itilepos
      integer jtilepos
      integer layer
      integer icomp
      integer ierr

      integer numchecks

      character*(max_len_mbuf) msgbuf

c     == end of interface ==

c--   Print header.
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// Gradient check results  >>> START <<<'
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
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a,e10.3)')
     &' EPS = ',grdchk_eps
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a,7(1x,a15))') 
     & 'grdchk output:  ', 'procId', 'I', 'ITILEPOS', 'JTILEPOS', 
     & 'LAYER', 'X(I)', 'X(I)+/-EPS'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#ifdef ALLOW_TANGENTLINEAR_RUN
      write(msgbuf,'(a,7(1x,a15))') 
     &     'grdchk output:  ', ' ', 'FC', 'FC1', 'FC2', 
     &     'FC1-FC2/(2*EPS)', 'TLM GRAD(FC)', '1-FDGRD/TLMGRD'
#else
      write(msgbuf,'(a,7(1x,a15))') 
     &     'grdchk output:  ', ' ', 'FC', 'FC1', 'FC2', 
     &     'FC1-FC2/(2*EPS)', 'ADJ GRAD(FC)', '1-FDGRD/ADGRD'
#endif
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c--   Individual checks.
      if ( ierr_grdchk .eq. 0 ) then
         numchecks = ichknum
      else
         numchecks = maxgrdchecks
      endif

      do i = 1, numchecks
        xxmemo_ref   = xxmemref  (i)
        xxmemo_pert  = xxmempert (i)
        adxxmemo     = adxxmem   (i)
        ftlxxmemo    = ftlxxmem  (i)
        fcref        = fcrmem    (i)
        fcpertplus   = fcppmem   (i)
        fcpertminus  = fcpmmem   (i)
        gfd          = gfdmem    (i)
        ratio_ad     = ratioadmem  (i)
        ratio_ftl    = ratioftlmem (i)
        itile        = bimem   (i)
        jtile        = bjmem   (i)
        itilepos     = ilocmem (i)
        jtilepos     = jlocmem (i)
        layer        = klocmem (i)
        icomp        = icompmem(i)
        ierr         = ierrmem (i)

        write(msgbuf,'(A,5(I16),2(1x,D15.9))')
     &       'grdchk output:  ',
     &       myprocid, i, itilepos, jtilepos, layer, 
     &       xxmemo_ref, xxmemo_pert
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        if ( ierr .eq. 0 ) then
#ifdef ALLOW_TANGENTLINEAR_RUN
          write(msgbuf,'(A,1(1x,a15),6(1x,D15.9))')
     &         'grdchk output:  ', ' ',
     &         fcref, fcpertplus, fcpertminus, 
     &         gfd, ftlxxmemo, ratio_ftl
#else
          write(msgbuf,'(A,1(1x,a15),6(1x,D15.9))')
     &          'grdchk output:  ', ' ',
     &          fcref, fcpertplus, fcpertminus, 
     &          gfd, adxxmemo, ratio_ad
#endif
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
        else
          if ( ierr .eq. -1 ) then
            write(msgbuf,'(a)')
     &      ' Component does not exist (zero)'
          else if ( ierr .eq. -2 ) then
            write(msgbuf,'(a)')
     &      ' Component does not exist (negative)'
          else if ( ierr .eq. -3 ) then
            write(msgbuf,'(a)')
     &      ' Component does not exist (too large)'
          else if ( ierr .eq. -4 ) then
            write(msgbuf,'(a)')
     &      ' Component does not exist (land point)'
          endif
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
        endif
        write(msgbuf,'(a)')
     &  ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      enddo

c--   Print final lines.
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                  SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// Gradient check results  >>> END <<<'
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

#endif /* ALLOW_GRDCHK */

      return
      end

