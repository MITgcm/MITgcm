C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_weights.F,v 1.8 2014/10/20 03:20:57 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_ECCO
#include "ECCO_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
#include "CTRL_OPTIONS.h"
#endif

      subroutine seaice_cost_weights( mythid )

c     ==================================================================
c     SUBROUTINE seaice_cost_weights
c     ==================================================================
c
c     ==================================================================
c     SUBROUTINE seaice_cost_weights
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_CTRL
# include "ctrl.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif
#include "SEAICE_COST.h"

c     == routine arguments ==

      integer  mythid

#ifdef ALLOW_ECCO
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer gwunit
      integer irec,nnz
      integer ilo,ihi

      _RL dummy

c     == external ==

      integer  ifnblnk
      external ifnblnk
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

c--   Define frame.
      do j = jmin,jmax
        do i = imin,imax
c--       North/South and West/East edges set to zero.
          if ( (j .lt. 1) .or. (j .gt. sny) .or.
     &         (i .lt. 1) .or. (i .gt. snx)      ) then
            frame(i,j) = 0. _d 0
          else
            frame(i,j) = 1. _d 0
          endif
        enddo
      enddo

#ifdef ALLOW_SEAICE_COST_SMR_AREA

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              wsmrarea (i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
c--
      nnz   = 1
      irec  = 1
      k     = 1
      if ( smrarea_errfile .NE. ' ' ) then
         IF ( cost_yftype.EQ.'RL' ) THEN
           CALL READ_REC_3D_RL( smrarea_errfile, cost_iprec, nnz,
     &                          wsmrarea, irec, 0, mythid )
         ELSE
           STOP 'S/R SEAICE_COST_WEIGHTS: invalid cost_yftype'
         ENDIF
         do bj = jtlo,jthi
           do bi = itlo,ithi
             do j = jmin,jmax
               do i = imin,imax
                 wsmrarea(i,j,bi,bj) = wsmrarea(i,j,bi,bj)
     &                 *frame(i,j)*_hFacC(i,j,k,bi,bj)
               enddo
             enddo
           enddo
         enddo
      else
         do bj = jtlo,jthi
           do bi = itlo,ithi
             do j = jmin,jmax
               do i = imin,imax
                 wsmrarea(i,j,bi,bj) = wsmrarea0
     &                 *frame(i,j)*_hFacC(i,j,k,bi,bj)
               enddo
             enddo
           enddo
         enddo
      endif
c--
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              if (wsmrarea(i,j,bi,bj) .ne. 0.)
     &              wsmrarea(i,j,bi,bj) =
     &              1./wsmrarea(i,j,bi,bj)/wsmrarea(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif /* ALLOW_SEAICE_COST_SMR_AREA */

#endif

      end
