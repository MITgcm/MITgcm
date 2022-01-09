#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_ECCO
#include "ECCO_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
#include "CTRL_OPTIONS.h"
#endif

      subroutine seaice_cost_weights( myThid )

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
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif
#include "SEAICE_COST.h"

c     == routine arguments ==
      integer  myThid

#ifdef ALLOW_ECCO
#ifdef ALLOW_SEAICE_COST_SMR_AREA
c     == local variables ==
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec,nnz

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1-OLy
      jmax = sNy+OLy
      imin = 1-OLx
      imax = sNx+OLx

c--   Define frame.
      do j = jmin,jmax
        do i = imin,imax
c--       North/South and West/East edges set to zero.
          if ( (j .lt. 1) .or. (j .gt. sNy) .or.
     &         (i .lt. 1) .or. (i .gt. sNx)      ) then
            frame(i,j) = 0. _d 0
          else
            frame(i,j) = 1. _d 0
          endif
        enddo
      enddo

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
         CALL READ_REC_3D_RL( smrarea_errfile, cost_iprec, nnz,
     &                        wsmrarea, irec, 0, myThid )
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

#endif /* ALLOW_ECCO */

      RETURN
      END
