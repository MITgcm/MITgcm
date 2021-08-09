#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
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
     O                     icglom1,
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
#include "CTRL_OBCS.h"
#include "grdchk.h"
#ifdef ALLOW_OBCS
# include "OBCS_GRID.h"
#endif
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */

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
      integer       ierr
      integer       mythid

#ifdef ALLOW_GRDCHK
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itmp,jtmp
      integer iobcs
c     integer biwrk,bjwrk
      integer iwrk, jwrk, kwrk
      integer iobcswrk
      integer irec, irecwrk
      integer icglo, icglom1
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer icomptest
      integer icomploc
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
      ierr    = -5
      icglom1 = 0
      icomploc= 0

cph(
      print *, 'ph-test icomp, ncvarcomp, ichknum ',
     &     icomp, ncvarcomp, ichknum
cph)
      if ( icomp .gt. 0 ) then
       if ( icomp .le. ncvarcomp ) then
c--     A valid component of the control variable has been selected.
         if ( ichknum .EQ. 1 ) then
            itest     = 0
            icomptest = 0
            irecwrk   = 1
c           bjwrk     = 1
c           biwrk     = 1
            kwrk      = 1
            iobcswrk  = 1
            jwrk      = 1
            iwrk      = 1
            icglo     = 0
         else
            itest     = itestmem (ichknum-1)
            icomptest = icompmem (ichknum-1)
            irecwrk   = irecmem  (ichknum-1)
c           bjwrk     = bjmem    (ichknum-1)
c           biwrk     = bimem    (ichknum-1)
            kwrk      = klocmem  (ichknum-1)
            iobcswrk  = iobcsmem (ichknum-1)
            icglo     = icglomem (ichknum-1)
            jwrk      = jlocmem  (ichknum-1)
            iwrk      = ilocmem  (ichknum-1)
            iwrk      = iwrk + 1
         end if

c--   set max loop index for obcs multiplicities
        if ( ncvargrd(grdchkvarindex) .eq. 'm' ) then
           nobcsmax = nobcs
        else
           nobcsmax = 1
        endif

cph(
cph-print        print *, 'ph-grd _loc: icomp, ichknum ',
cph-print     &       icomp, ichknum, ncvarcomp
cpj)
c--   Start to loop over records.
        do irec = irecwrk, ncvarrecs(grdchkvarindex)
cph         do iobcs = iobcswrk, nobcsmax
         iobcs = MOD((irec-1),nobcsmax) + 1
c         do bj = bjwrk, jthi
c          do bi = biwrk, ithi
            bj = jLocTile
            bi = iLocTile
            do k = kwrk, ncvarnrmax(grdchkvarindex)
             icglo   = icglo + nwettile(bi,bj,k,iobcs)
             icglom1 = icglo - nwettile(bi,bj,k,iobcs)
cph(
        print *, 'ph-grd _loc: bi, bj, icomptest, ichknum ',
     &       bi, bj, icomptest, ichknum
cph-print        print *, 'ph-grd _loc: icglo ',
cph-print     &       k, icglo, icglom1, iwetsum(bi,bj,k)
cpj)
             if ( (ierr .ne. 0) .and.
     &            (icomp .gt. icglom1 .AND. icomp .LE. icglo) ) then
cph
cph             if ( (ierr .ne. 0) .and.
cph     &            (icomp .gt.
cph     &              (iobcs-1)*iwetsum(bi,bj,nr)+iwetsum(bi,bj,k-1))
cph     &              .and.
cph     &            (icomp .le.
cph     &              (iobcs-1)*iwetsum(bi,bj,nr)+iwetsum(bi,bj,k))) then
cph
               if ( icomptest .EQ. 0 ) then
                  icomptest = icglom1
               endif
               icomploc = icomp
               icvrec = irec
               itile  = bi
               jtile  = bj
cph(
cph-print               print *, 'ph-grd irec, bj, bi, k ', irec, bj, bi, k
cpj)
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
#ifdef ALLOW_SHEFICE
                  else if ( ncvargrd(grdchkvarindex) .eq. 'i' ) then
                     if ( maskSHI(i,j,k,bi,bj) .gt. 0.) then
                        icomptest = icomptest + 1
                        itmp = i
                        jtmp = j
                     endif
#endif /* ALLOW_SHELFICE */
                  else if ( ncvargrd(grdchkvarindex) .eq. 'm' ) then
                     if ( grdchkvarindex .EQ. 11 ) then
#ifdef ALLOW_OBCSN_CONTROL
                        if ( grdchk_maskxz(i,k,bi,bj,iobcs) .gt. 0.
     &                       .and. j.eq. OB_Jn(I,bi,bj) ) then
                           icomptest = icomptest + 1
                           itmp = i
                           jtmp = OB_Jn(I,bi,bj)
                        endif
#endif
                     else if ( grdchkvarindex .EQ. 12 ) then
#ifdef ALLOW_OBCSS_CONTROL
                        if ( grdchk_maskxz(i,k,bi,bj,iobcs) .gt. 0.
     &                       .and. j.eq. OB_Js(I,bi,bj) ) then
                           icomptest = icomptest + 1
                           itmp = i
                           jtmp = OB_Js(I,bi,bj)
                        endif
#endif
                     else if ( grdchkvarindex .EQ. 13 ) then
#ifdef ALLOW_OBCSW_CONTROL
                        if ( grdchk_maskyz(j,k,bi,bj,iobcs) .gt. 0.
     &                       .and. i.eq. OB_Iw(J,bi,bj) ) then
                           icomptest = icomptest + 1
                           itmp = OB_Iw(J,bi,bj)
                           jtmp = j
                        endif
#endif
                     else if ( grdchkvarindex .EQ. 14 ) then
#ifdef ALLOW_OBCSE_CONTROL
                        if ( grdchk_maskyz(j,k,bi,bj,iobcs) .gt. 0.
     &                       .and. i.eq. OB_Ie(J,bi,bj) ) then
                           icomptest = icomptest + 1
                           itmp = OB_Ie(J,bi,bj)
                           jtmp = j
                        endif
#endif
                     endif
                  endif
cph(
cph-print                  print *, 'ph-grd icomp, icomptest, icomploc, i, j ',
cph-print     &                 icomp, icomptest, icomploc, i, j
cpj)
                  if ( icomploc .eq. icomptest ) then
                     itilepos = itmp
                     jtilepos = jtmp
                     layer    = k
                     obcspos  = iobcs
                     ierr     = 0
cph                     itest    = iwetsum(bi,bj,k)
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
cph(
cph-print               print *, 'ph-grd after loop icomptest, icomploc, k ',
cph-print     &              icomptest, icomploc, k
cph)
                iwrk      = 1
                jwrk      = 1
             else
c
             endif
c--   End of loop over k
            enddo
            kwrk = 1
c--   End of loop over bi
c          enddo
c          biwrk = 1
c--   End of loop over bj
c         enddo
c         bjwrk = 1
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

      return
      end
