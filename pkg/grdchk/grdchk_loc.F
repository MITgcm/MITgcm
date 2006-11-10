C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_loc.F,v 1.10 2006/11/10 04:57:09 heimbach Exp $

#include "PACKAGES_CONFIG.h"
#include "CTRL_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
#include "OBCS_OPTIONS.h"
#endif


      subroutine grdchk_loc(
     I                     icomp,
     I                     ichknum,
     O                     icvrec,
     O                     itile,
     O                     jtile,
     O                     layer,
     O                     obcspos,
     O                     itilepos,
     O                     jtilepos,
     O                     itest,
     O                     ierr,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE grdchk_loc
c     ==================================================================
c
c     o Get the location of a given component of the control vector for
c       the current process.
c
c     started: Christian Eckert eckert@mit.edu 04-Apr-2000
c     continued: heimbach@mit.edu: 13-Jun-2001
c
c     ==================================================================
c     SUBROUTINE grdchk_loc
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "ctrl.h"
#include "grdchk.h"
#ifdef ALLOW_OBCS
#include "OBCS.h"
#endif

c     == routine arguments ==

      integer       icomp
      integer       ichknum
      integer       icvrec
      integer       jtile
      integer       itile
      integer       layer
      integer       obcspos
      integer       itilepos
      integer       jtilepos
      integer       itest
      integer       iwettot
      integer       ierr
      integer       mythid

#ifdef ALLOW_GRDCHK
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itmp,jtmp
      integer iobcs
      integer biwrk,bjwrk
      integer iwrk, jwrk, kwrk
      integer iobcswrk
      integer irec, irecwrk
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer icomptest
      integer nobcsmax

c     == end of interface ==

      jtlo = 1
      jthi = nsy
      itlo = 1
      ithi = nsx
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      _BEGIN_MASTER( mythid )

c     initialise parameters
      ierr = -5

      if ( icomp .gt. 0 ) then
       if ( icomp .le. ncvarcomp ) then
c--     A valid component of the control variable has been selected.
         if ( ichknum .EQ. 1 ) then
            itest     = 0
            icomptest = 0
            irecwrk   = 1
            bjwrk     = 1
            biwrk     = 1
            kwrk      = 1
            iobcswrk  = 1
            jwrk      = 1
            iwrk      = 1
            iwettot   = 0
         else
            itest     = itestmem (ichknum-1)
            icomptest = icompmem (ichknum-1)
            irecwrk   = irecmem  (ichknum-1)
            bjwrk     = bjmem    (ichknum-1)
            biwrk     = bimem    (ichknum-1)
            kwrk      = klocmem  (ichknum-1)
            iobcswrk  = iobcsmem (ichknum-1)
            jwrk      = jlocmem  (ichknum-1)
            iwrk      = ilocmem  (ichknum-1)
            iwrk      = iwrk + 1
            iwettot   = iwetsum(kwrk)
         end if

c--   set max loop index for obcs multiplicities
        if ( ncvargrd(grdchkvarindex) .eq. 'm' ) then
           nobcsmax = nobcs
           if (grdchkvarindex.EQ.11.OR.grdchkvarindex.EQ.12) then
             jwrk = 1
           else if (grdchkvarindex.EQ.13.OR.grdchkvarindex.EQ.14) then
             iwrk = 1
           else
             STOP 'in grdchk_loc for obcs: should never get here'
           endif
        else
           nobcsmax = 1
        endif

c--   Start to loop over records.
        do irec = irecwrk, ncvarrecs(grdchkvarindex)
cph         do iobcs = iobcswrk, nobcsmax
         iobcs = MOD((irec-1),nobcsmax) + 1
          do bj = bjwrk, jthi
           do bi = biwrk, ithi
            do k = kwrk, ncvarnrmax(grdchkvarindex)
             if ( (ierr .ne. 0) .and.
     &            (icomp .gt. (iobcs-1)*iwetsum(nr)+iwetsum(k-1)) .and.
     &            (icomp .le. (iobcs-1)*iwetsum(nr)+iwetsum(k))) then
               icvrec = irec
               itile  = bi
               jtile  = bj

               do j = jwrk, ncvarymax(grdchkvarindex)
                do i = iwrk, ncvarxmax(grdchkvarindex)
                 if (ierr .ne. 0) then
                  if ( ncvargrd(grdchkvarindex) .eq. 'c' ) then
                     if ( maskC(i,j,k,bi,bj) .gt. 0.) then
                        icomptest = icomptest + 1
                        itmp = i
                        jtmp = j
                     endif
                  else if ( ncvargrd(grdchkvarindex) .eq. 's' ) then
                     if ( _maskS(i,j,k,bi,bj) .gt. 0.) then
                        icomptest = icomptest + 1
                        itmp = i
                        jtmp = j
                     endif
                  else if ( ncvargrd(grdchkvarindex) .eq. 'w' ) then
                     if ( _maskW(i,j,k,bi,bj) .gt. 0.) then
                        icomptest = icomptest + 1
                        itmp = i
                        jtmp = j
                     endif
                  else if ( ncvargrd(grdchkvarindex) .eq. 'm' ) then
                     if ( grdchkvarindex .EQ. 11 ) then
#ifdef ALLOW_OBCSN_CONTROL
                        if (grdchk_maskxz(i,k,bi,bj,iobcs) .gt. 0.) then
                           icomptest = icomptest + 1
                           itmp = i
                           jtmp = OB_Jn(I,bi,bj)
                        endif
#endif
                     else if ( grdchkvarindex .EQ. 12 ) then
#ifdef ALLOW_OBCSS_CONTROL
                        if (grdchk_maskxz(i,k,bi,bj,iobcs) .gt. 0.) then
                           icomptest = icomptest + 1
                           itmp = i
                           jtmp = OB_Js(I,bi,bj)
                        endif
#endif
                     else if ( grdchkvarindex .EQ. 13 ) then
#ifdef ALLOW_OBCSW_CONTROL
                        if (grdchk_maskyz(j,k,bi,bj,iobcs) .gt. 0.) then
                           icomptest = icomptest + 1
                           itmp = OB_Iw(J,bi,bj)
                           jtmp = j
                        endif
#endif
                     else if ( grdchkvarindex .EQ. 14 ) then
#ifdef ALLOW_OBCSE_CONTROL
                        if (grdchk_maskyz(j,k,bi,bj,iobcs) .gt. 0.) then
                           icomptest = icomptest + 1
                           itmp = OB_Ie(J,bi,bj)
                           jtmp = j
                        endif
#endif
                     endif
                  endif
                  if ( icomp .eq. icomptest ) then
                     itilepos = itmp
                     jtilepos = jtmp
                     layer    = k
                     obcspos  = iobcs
                     ierr     = 0
                     itest    = iwetsum(k)
cph(
                     print *, 'ph-grd -->hit<-- ', itmp,jtmp,k,iobcs
                     goto 1234
cph)
                  endif
                 endif
                enddo
                iwrk = 1
               enddo
               jwrk = 1
             else if (ierr .NE. 0) then
               if (icomptest .EQ. icomp-1) then
                 icomptest = icomptest
               else
                 icomptest = icomptest + nwettile(bi,bj,k,iobcs)
               endif
                iwrk      = 1
                jwrk      = 1
             else
c
             endif
c--   End of loop over k
            enddo
            kwrk = 1
c--   End of loop over bi
           enddo
           biwrk = 1
c--   End of loop over bj
          enddo
          bjwrk = 1
c--   End of loop over iobcs
cph         enddo
cph         iobcswrk = 1
c--   End of loop over irec records.
         enddo
c
        else
            if ( icomp .gt. maxncvarcomps ) then
c--           Such a component does not exist.
              ierr     = -4
              icvrec   = -1
              jtile    = -1
              itile    = -1
              layer    = -1
              obcspos  = -1
              jtilepos = -1
              itilepos = -1
            else
c--           The component is a land point.
              ierr     = -3
              icvrec   = -1
              jtile    = -1
              itile    = -1
              layer    = -1
              obcspos  = -1
              jtilepos = -1
              itilepos = -1
            endif
        endif
       else
          if ( icomp .lt. 0 ) then
c--         Such a component does not exist.
            ierr     = -2
            icvrec   = -1
            jtile    = -1
            itile    = -1
            layer    = -1
            obcspos  = -1
            jtilepos = -1
            itilepos = -1
          else
c--         Component zero.
            ierr     = -1
            icvrec   = -1
            jtile    = -1
            itile    = -1
            layer    = -1
            obcspos  = -1
            jtilepos = -1
            itilepos = -1
          endif
       endif

 1234  continue

      _END_MASTER( mythid )

      _BARRIER

#endif /* ALLOW_GRDCHK */

      end

