C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_loc.F,v 1.2 2001/07/13 14:50:46 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"


      subroutine grdchk_loc(
     I                     icomp,
     I                     ichknum,
     O                     icvrec,
     O                     itile,
     O                     jtile,
     O                     layer,
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

c     == routine arguments ==

      integer       icomp
      integer       ichknum
      integer       icvrec
      integer       jtile
      integer       itile
      integer       layer
      integer       itilepos
      integer       jtilepos
      integer       itest
      integer       ierr
      integer       mythid

#ifdef ALLOW_GRADIENT_CHECK
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer biwrk,bjwrk
      integer iwrk, jwrk, kwrk
      integer irec, irecwrk
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer icomptest

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
          jwrk      = 1
          iwrk      = 1
         else
          itest     = itestmem(ichknum-1)
          icomptest = icompmem(ichknum-1)
          irecwrk   = irecmem(ichknum-1)
          bjwrk     = bjmem  (ichknum-1)
          biwrk     = bimem  (ichknum-1)
          kwrk      = klocmem(ichknum-1)
          jwrk      = jlocmem(ichknum-1)
          iwrk      = ilocmem(ichknum-1)
          iwrk      = iwrk + 1
         end if

c--   Start to loop over records.
        do irec = irecwrk, ncvarrecs(grdchkvarindex)
         do bj = bjwrk, jthi
          do bi = biwrk, ithi
           do k = kwrk, ncvarnrmax(grdchkvarindex)

            if ( (ierr .ne. 0) .and.
     &           (icomp .gt. itest) .and.
     &           (icomp .le. itest + nwettile(bi,bj,k))) then
               icvrec = irec
               itile  = bi
               jtile  = bj

               do j = jwrk, sny
                do i = iwrk, snx
                 if (ierr .ne. 0) then
                  if ( ncvargrd(grdchkvarindex) .eq. 'c' ) then
                     if ( _hFacC(i,j,k,bi,bj) .ne. 0.) then
                        icomptest = icomptest + 1
                     endif
                  else if ( ncvargrd(grdchkvarindex) .eq. 's' ) then
                     if ( _maskS(i,j,k,bi,bj) .gt. 0.) then
                        icomptest = icomptest + 1
                     endif
                  else if ( ncvargrd(grdchkvarindex) .eq. 'w' ) then
                     if ( _maskW(i,j,k,bi,bj) .gt. 0.) then
                        icomptest = icomptest + 1
                     endif
                  endif
                  if ( icomp .eq. icomptest ) then
                     itilepos = i
                     jtilepos = j
                     layer    = k
                     ierr     = 0
                  endif
                 endif
                enddo
                iwrk = 1
               enddo
               jwrk = 1
              else if (ierr .NE. 0) then
                 itest     = itest + nwettile(bi,bj,k)
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
c--   End of loop over records.
          enddo
          else
            if ( icomp .gt. maxncvarcomps ) then
c--           Such a component does not exist.
              ierr     = -4
              icvrec   = -1
              jtile    = -1
              itile    = -1
              layer    = -1
              jtilepos = -1
              itilepos = -1
            else
c--           The component is a land point.
              ierr     = -3
              icvrec   = -1
              jtile    = -1
              itile    = -1
              layer    = -1
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
            jtilepos = -1
            itilepos = -1
          else
c--         Component zero.
            ierr     = -1
            icvrec   = -1
            jtile    = -1
            itile    = -1
            layer    = -1
            jtilepos = -1
            itilepos = -1
          endif
        endif

      _END_MASTER( mythid )

      _BARRIER

#endif /* ALLOW_GRADIENT_CHECK */

      end

