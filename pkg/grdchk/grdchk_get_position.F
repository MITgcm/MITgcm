#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine grdchk_get_position( mythid )

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
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif
#ifdef ALLOW_OBCS
# include "OBCS_GRID.h"
#endif
#include "ctrl.h"
#include "CTRL_OBCS.h"
#include "grdchk.h"

c     == routine arguments ==
      integer       mythid

#ifdef ALLOW_GRDCHK
c     == local variables ==
      integer       icvrec
      integer       jtile
      integer       itile
      integer       layer
      integer       obcspos
      integer       itilepos
      integer       jtilepos
      integer       itest
      integer       ierr
      integer bi,bj
      integer i,j,k
      integer iobcs
      integer iwrk, jwrk, kwrk
      integer iobcswrk
      integer irec, irecwrk
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer icomptest
      integer nobcsmax
      integer pastit
      _RL wetlocal

c     == end of interface ==

      jtlo = 1
      jthi = nsy
      itlo = 1
      ithi = nsx
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      itile = iLocTile
      jtile = jLocTile
      itilepos = iGloPos
      jtilepos = jGloPos
      layer    = kGloPos
      obcspos  = obcsglo
      icvrec   = recglo

      _BEGIN_MASTER( mythid )

c--   determine proc. number from following assumptions <= done in grdchk_readparms

      if ( myProcId .EQ. grdchkwhichproc ) then

c     initialise parameters
      ierr      = -5
      pastit    = -1
      wetlocal  = 0

      itest     = 0
      icomptest = 0
      irecwrk   = 1
c     bjwrk     = 1
c     biwrk     = 1
      kwrk      = 1
      iobcswrk  = 1
      jwrk      = 1
      iwrk      = 1

c--   set max loop index for obcs multiplicities
      if ( ncvargrd(grdchkvarindex) .eq. 'm' ) then
         nobcsmax = nobcs
      else
         nobcsmax = 1
      endif

c--   Start to loop over records.
        do irec = irecwrk, ncvarrecs(grdchkvarindex)
         iobcs = MOD((irec-1),nobcsmax) + 1
c         do bj = bjwrk, jthi
c          do bi = biwrk, ithi
            bi = itile
            bj = jtile
            do k = kwrk, ncvarnrmax(grdchkvarindex)

cph(
cph-print               print *, 'ph-grd get_pos irec, bj, bi, k ',
cph-print     &              irec, bj, bi, k
cph)
             if ( ierr .ne. 0 ) then
               do j = jwrk, ncvarymax(grdchkvarindex)
                do i = iwrk, ncvarxmax(grdchkvarindex)
                 if (ierr .ne. 0) then
                  if ( ncvargrd(grdchkvarindex) .eq. 'c' ) then
                     if ( maskC(i,j,k,bi,bj) .gt. 0.) then
                        icomptest = icomptest + 1
                     endif
                     wetlocal = maskC(i,j,k,bi,bj)
                  else if ( ncvargrd(grdchkvarindex) .eq. 's' ) then
                     if ( _maskS(i,j,k,bi,bj) .gt. 0.) then
                        icomptest = icomptest + 1
                     endif
                     wetlocal = _maskS(i,j,k,bi,bj)
                  else if ( ncvargrd(grdchkvarindex) .eq. 'w' ) then
                     if ( _maskW(i,j,k,bi,bj) .gt. 0.) then
                        icomptest = icomptest + 1
                     endif
                     wetlocal = _maskW(i,j,k,bi,bj)
#ifdef ALLOW_SHELFICE
c--             Ice shelf mask.
                  else if ( ncvargrd(grdchkvarindex) .eq. 'i' ) then
                     if ( maskSHI(i,j,k,bi,bj) .gt. 0.) then
                        icomptest = icomptest + 1
                     endif
                     wetlocal = maskSHI(i,j,k,bi,bj)
#endif /* ALLOW_SHELFICE */
                  else if ( ncvargrd(grdchkvarindex) .eq. 'm' ) then
                     if ( grdchkvarindex .EQ. 11 ) then
#ifdef ALLOW_OBCSN_CONTROL
                        if (grdchk_maskxz(i,k,bi,bj,iobcs) .gt. 0.
     &                       .and. j.eq. OB_Jn(I,bi,bj) ) then
                           icomptest = icomptest + 1
                        endif
                        wetlocal = grdchk_maskxz(i,k,bi,bj,iobcs)
#endif
                     else if ( grdchkvarindex .EQ. 12 ) then
#ifdef ALLOW_OBCSS_CONTROL
                        if (grdchk_maskxz(i,k,bi,bj,iobcs) .gt. 0.
     &                       .and. j.eq. OB_Js(I,bi,bj) ) then
                           icomptest = icomptest + 1
                        endif
                        wetlocal = grdchk_maskxz(i,k,bi,bj,iobcs)
#endif
                     else if ( grdchkvarindex .EQ. 13 ) then
#ifdef ALLOW_OBCSW_CONTROL
                        if ( grdchk_maskyz(j,k,bi,bj,iobcs) .gt. 0.
     &                      .and. i.eq. OB_Iw(J,bi,bj) ) then
                           icomptest = icomptest + 1
                        endif
                        wetlocal = grdchk_maskyz(j,k,bi,bj,iobcs)
#endif
                     else if ( grdchkvarindex .EQ. 14 ) then
#ifdef ALLOW_OBCSE_CONTROL
                        if (grdchk_maskyz(j,k,bi,bj,iobcs) .gt. 0.
     &                       .and. i.eq. OB_Ie(J,bi,bj) ) then
                           icomptest = icomptest + 1
                        endif
                        wetlocal = grdchk_maskyz(j,k,bi,bj,iobcs)
#endif
                     endif
                  endif
c
                  if ( i     .EQ. itilepos .AND.
     &                 j     .EQ. jtilepos .AND.
     &                 k     .EQ. layer .AND.
     &                 bi    .EQ. itile .AND.
     &                 bj    .EQ. jtile .AND.
     &                 iobcs .EQ. obcspos .AND.
     &                 irec  .EQ. icvrec ) then
                     pastit = 0
                     if ( wetlocal .NE.0 ) then
                        nbeg = icomptest
                        nend = nbeg + nend
                        ierr     = 0
                        print '(a,6I5)',
     &                       ' grad-res exact position met: '
                        print '(a,7I5)',
     &                       ' grad-res ', grdchkwhichproc,
     &                       nbeg, itilepos, jtilepos, layer,
     &                       itile, jtile
                        goto 1234
                     endif
                  else if ( pastit .EQ. 0 .AND.
     &                    wetlocal .NE.0 ) then
                     nbeg = icomptest
                     nend = nbeg + nend
                     ierr     = 0
                        print '(a,6I5)',
     &                       ' grad-res closest next position: '
                        print '(a,7I5)',
     &                       ' grad-res ', grdchkwhichproc,
     &                       nbeg, itilepos, jtilepos, layer,
     &                       itile, jtile
                        goto 1234
                  endif
c
                 endif
                enddo
                iwrk = 1
               enddo
               jwrk = 1
             else if (ierr .NE. 0) then
                itest     = itest + nwettile(bi,bj,k,iobcs)
                iwrk      = 1
                jwrk      = 1
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

c--   End of if myProcId statement
      endif

 1234 continue

      _END_MASTER( mythid )

      _BARRIER

#endif /* ALLOW_GRDCHK */

      return
      end
