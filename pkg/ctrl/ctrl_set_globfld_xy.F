#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_set_globfld_xy( 
     I     fname, ivartype, mythid )

c     ==================================================================
c     SUBROUTINE ctrl_set_globfld_xy
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
#include "cost.h"

#ifdef ALLOW_ECCO_OPTIMIZATION
#include "optim.h"
#endif

c     == routine arguments ==

      character*( 80)   fname
      integer ivartype
      integer mythid

c     == local variables ==

#ifndef ALLOW_ECCO_OPTIMIZATION
      integer optimcycle
#endif

      integer bi,bj
      integer ip,jp
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec

      _RL     globfld2d( snx,nsx,npx,sny,nsy,npy )

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
      do jp = 1,nPy
         do bj = jtlo,jthi
            do j = jmin,jmax
               do ip = 1,nPx
                  do bi = itlo,ithi
                     do i = imin,imax
#ifdef ALLOW_TANGENTLINEAR_RUN
                        globfld2d(i,bi,ip,j,bj,jp) = 0. _d 0
#else
                        globfld2d(i,bi,ip,j,bj,jp) = 0. _d 0
#endif
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo

#ifndef ALLOW_ECCO_OPTIMIZATION
      optimcycle = 0
#endif

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

      do irec = 1, ncvarrecs(ivartype)
         call MDSWRITEFIELD_2D_GL( fname, ctrlprec, 'RL',
     &        1, globfld2d,
     &        irec,   optimcycle,  mythid)
      enddo

      _END_MASTER( mythid )

      end
