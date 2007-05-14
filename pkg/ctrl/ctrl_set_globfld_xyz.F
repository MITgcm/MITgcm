C
C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_set_globfld_xyz.F,v 1.9 2007/05/14 23:06:13 heimbach Exp $
C $Name:  $

#include "CTRL_CPPOPTIONS.h"

      subroutine ctrl_set_globfld_xyz( 
     I     fname, ivartype, mythid )

c     ==================================================================
c     SUBROUTINE ctrl_set_globfld_xyz
c     ==================================================================
c
c     o initialise field
c
c     started: heimbach@mit.edu, 16-Aug-2001
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
      integer irec

      _RL     globfld3d( snx,sny,nr,nsx,nsy )

c     == external ==

c     == end of interface ==

      jtlo = 1
      jthi = nsy
      itlo = 1
      ithi = nsx
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c     Initialise temporary file
      do k = 1,nr
            do bj = jtlo,jthi
               do j = jmin,jmax
                     do bi = itlo,ithi
                        do i = imin,imax
                           globfld3d(i,j,k,bi,bj) = 0. _d 0
                        enddo
                     enddo
               enddo
            enddo
      enddo

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

CMM(add one for good measure
CMM      do irec = 1, ncvarrecs(ivartype)
      do irec = 1, ncvarrecs(ivartype)+1
CMM)
         call MDSWRITEFIELD( fname, ctrlprec,.FALSE., 'RL',
     &        Nr, globfld3d,
     &        irec,   optimcycle,  mythid)
      enddo

      _END_MASTER( mythid )

      end
