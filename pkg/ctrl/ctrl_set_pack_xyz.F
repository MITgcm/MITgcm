
#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_set_pack_xyz(
     &     cunit, ivartype, fname, masktype,
     &     weightfld, lxxadxx, mythid)

c     ==================================================================
c     SUBROUTINE ctrl_set_pack_xyz
c     ==================================================================
c
c     o Compress the control vector such that only ocean points are
c       written to file.
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

      integer cunit
      integer ivartype
      character*( 80) fname
      character*  (5) masktype
      _RL     weightfld( nr,nsx,nsy )
      logical lxxadxx
      integer mythid

c     == local variables ==

#ifndef ALLOW_ECCO_OPTIMIZATION
      integer optimcycle
#endif

      integer bi,bj
      integer ip,jp
      integer i,j,k
      integer ii
      integer il
      integer irec
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer cbuffindex

      _RL     cbuff    ( snx*nsx*npx*sny*nsy*npy )
      _RL     globmsk  ( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globfld3d( snx,nsx,npx,sny,nsy,npy,nr )

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

#ifndef ALLOW_ECCO_OPTIMIZATION
      optimcycle = 0
#endif

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
         do jp = 1,nPy
            do bj = jtlo,jthi
               do j = jmin,jmax
                  do ip = 1,nPx
                     do bi = itlo,ithi
                        do i = imin,imax
                           globfld3d(i,bi,ip,j,bj,jp,k) = 0. _d 0
                           globmsk  (i,bi,ip,j,bj,jp,k) = 0. _d 0
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

      call MDSREADFIELD_3D_GL( 
     &     masktype, ctrlprec, 'RL',
     &     Nr, globmsk, 1, mythid)

      do irec = 1, ncvarrecs(ivartype)

         call MDSREADFIELD_3D_GL( fname, ctrlprec, 'RL',
     &        Nr, globfld3d, irec, mythid)

         write(cunit) ncvarindex(ivartype)
         write(cunit) 1
         write(cunit) 1
         do k = 1, nr
            cbuffindex = 0
            do jp = 1,nPy
             do bj = jtlo,jthi
              do j = jmin,jmax
               do ip = 1,nPx
                do bi = itlo,ithi
                 do i = imin,imax
                  if (globmsk(i,bi,ip,j,bj,jp,k)  .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     if (lxxadxx) then
                        cbuff(cbuffindex) = 
     &                       globfld3d(i,bi,ip,j,bj,jp,k) *
     &                       sqrt(weightfld(k,bi,bj))
                     else
                        cbuff(cbuffindex) = 
     &                       globfld3d(i,bi,ip,j,bj,jp,k) /
     &                       sqrt(weightfld(k,bi,bj))
                     endif
#else
                     cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
#endif
                  endif
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
c           --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo
c
c     -- end of irec loop --
      enddo

      _END_MASTER( mythid )

      return
      end

