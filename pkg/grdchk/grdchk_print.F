C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_print.F,v 1.16 2012/07/06 23:10:28 jmc Exp $
C $Name:  $

#include "GRDCHK_OPTIONS.h"
#include "AD_CONFIG.h"

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

#include "SIZE.h"
#include "EEPARAMS.h"
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
      _RL ratio_RMS

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
     &                    SQUEEZE_RIGHT, mythid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )
      write(msgbuf,'(a)')
     &'// Gradient check results  >>> START <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid )
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )

      write(msgbuf,'(A,1PE14.6)')
     &' EPS = ',grdchk_eps
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )

      write(msgbuf,'(A,2X,4A,3(3X,A),11X,A)')
     & 'grdchk output h.p:', 'Id', ' Itile', ' Jtile',
     & ' LAYER', 'bi', 'bj', 'X(Id)', 'X(Id)+/-EPS'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid )
      write(msgbuf,'(A,2X,A,A4,1X,2A21)')
     &     'grdchk output h.c:', 'Id', 'FC', 'FC1', 'FC2'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )
#ifdef ALLOW_TANGENTLINEAR_RUN
      write(msgbuf,'(A,2X,A,2X,2A18,4X,A18)')
     &     'grdchk output h.g:', 'Id',
     &     'FC1-FC2/(2*EPS)', 'TLM GRAD(FC)', '1-FDGRD/TLMGRD'
#else
      write(msgbuf,'(A,2X,A,2X,2A18,4X,A18)')
     &     'grdchk output h.g:', 'Id',
     &     'FC1-FC2/(2*EPS)', 'ADJ GRAD(FC)', '1-FDGRD/ADGRD'
#endif
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )

c--   Individual checks.
      if ( ierr_grdchk .eq. 0 ) then
         numchecks = ichknum
      else
         numchecks = maxgrdchecks
      endif

      ratio_RMS = 0.
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

        write(msgbuf,'(a)')
     &  ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT, mythid )
        write(msgbuf,'(A,I4,3I6,2I5,1x,1P2E17.9)')
     &       'grdchk output (p):',
     &       i, itilepos, jtilepos, layer, itile, jtile,
     &       xxmemo_ref, xxmemo_pert
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT, mythid )
        if ( ierr .eq. 0 ) then
          write(msgbuf,'(A,I4,1P3E21.13)')
     &          'grdchk output (c):',
     &          i, fcref, fcpertplus, fcpertminus
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT, mythid )
#ifdef ALLOW_TANGENTLINEAR_RUN
          ratio_RMS  = ratio_RMS + ratio_ftl*ratio_ftl
          write(msgbuf,'(A,I4,3x,1P3E21.13)')
     &          'grdchk output (g):',
     &         i, gfd, ftlxxmemo, ratio_ftl
#else
          ratio_RMS  = ratio_RMS + ratio_ad*ratio_ad
          write(msgbuf,'(A,I4,3x,1P3E21.13)')
     &          'grdchk output (g):',
     &          i, gfd, adxxmemo, ratio_ad
#endif
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT, mythid )
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
     &                        SQUEEZE_RIGHT, mythid )
        endif
      enddo

c--   Print final lines.
      IF ( ichknum.GT.1 ) ratio_RMS = ratio_RMS / ichknum
      IF ( ratio_RMS.GT.0. ) ratio_RMS = SQRT( ratio_RMS )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )
      write(msgbuf,'(A,I4,A,1P1E21.13)')
     &  'grdchk  summary  :  RMS of ',ichknum,' ratios =',ratio_RMS
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )
      write(msgbuf,'(a)')
     &'// Gradient check results  >>> END <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, mythid )

#endif /* ALLOW_GRDCHK */

      return
      end
