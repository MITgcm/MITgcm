C
C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_set_globfld_xz.F,v 1.16 2012/08/10 19:38:58 jmc Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"

      subroutine ctrl_set_globfld_xz(
     I     fname, ivartype, mythid )

c     ==================================================================
c     SUBROUTINE ctrl_set_globfld_xz
c     ==================================================================
c
c     o initialise field
c
c     started: heimbach@mit.edu, 16-Aug-2001
c
c     changed: heimbach@mit.edu 17-Jun-2003
c              merged changes from Armin to replace write of
c              nr * globfld2d by 1 * globfld3d
c              (ad hoc fix to speed up global I/O)
c
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ctrl.h"
#include "optim.h"

c     == routine arguments ==

      character*( 80)   fname
      integer ivartype
      integer mythid

c     == local variables ==

      integer bi,bj
      integer ip,jp
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec,nrec_nl

      _RL globfld3d(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nr,nSx,nSy)
      _RL globfldxz(1-oLx:sNx+oLx,nr,nSx,nSy)

c     == external ==

c     == end of interface ==

      jtlo = 1
      jthi = nsy
      itlo = 1
      ithi = nsx
      jmin = 1-oLy
      jmax = sny+oLy
      imin = 1-oLx
      imax = snx+oLx

c     Initialise temporary file
      do bj = jtlo,jthi
         do bi = itlo,ithi
            do k = 1,nr
               do i = imin,imax
                  globfldxz(i,k,bi,bj) = 0. _d 0
               enddo
            enddo
         enddo
      enddo
c     Initialise temporary file
      do bj = jtlo,jthi
         do bi = itlo,ithi
            do k = 1,nr
               do j = jmin,jmax
                  do i = imin,imax
                     globfld3d(i,j,k,bi,bj) = 0. _d 0
                  enddo
               enddo
            enddo
         enddo
      enddo

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

      if ( useSingleCPUio ) then
C     MDSWRITEFIELDXZ does not know about useSingleCPUio, so the faster
C     method that works for .not.useSingleCPUio cannot be used
       nrec_nl = 0
      else
       nrec_nl = int(ncvarrecs(ivartype)/Ny)
      endif

      do irec = 1, nrec_nl
       call MDSWRITEFIELD( fname, ctrlprec,.FALSE., 'RL',
     &      Nr, globfld3d,
     &      irec,   optimcycle,  mythid)
      enddo

      do irec = nrec_nl*Ny+1, ncvarrecs(ivartype)
       call MDSWRITEFIELDXZ( fname, ctrlprec,.FALSE., 'RL',
     &      Nr, globfldxz,
     &      irec,   optimcycle,  mythid)
      enddo

      _END_MASTER( mythid )

      end
