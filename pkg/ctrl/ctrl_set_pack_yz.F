
#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_set_pack_yz(
     &     cunit, ivartype, fname, masktype, 
     &     weightfld, lxxadxx, mythid)

c     ==================================================================
c     SUBROUTINE ctrl_set_pack_yz
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
      character*(  9) masktype
      _RL     weightfld( nr,nobcs )
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
      integer irec,iobcs
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer cbuffindex
cgg(
      integer igg
      _RL     gg
cgg)
      _RL     cbuff      ( nsx*npx*sny*nsy*npy )
      _RL     globmskyz  ( nsx,npx,sny,nsy,npy,nr )
      _RL     globfldyz  ( nsx,npx,sny,nsy,npy,nr )

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
                        globfldyz(bi,ip,j,bj,jp,k) = 0. _d 0
                        globmskyz(bi,ip,j,bj,jp,k) = 0. _d 0
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

      do irec = 1, ncvarrecs(ivartype)

cgg       do iobcs = 1, nobcs
cgg    Need to solve for what iobcs would have been.
          gg   = (irec-1)/nobcs
          igg  = int(gg)
          iobcs= irec - igg*nobcs

         call MDSREADFIELD_YZ_GL( 
     &        masktype, ctrlprec, 'RL',
     &        Nr, globmskyz, iobcs, mythid)

         call MDSREADFIELD_YZ_GL( fname, ctrlprec, 'RL',
     &        nr, globfldyz, irec, mythid)

         write(cunit) ncvarindex(ivartype)
         write(cunit) 1
         write(cunit) 1
         do k = 1,nr
            cbuffindex = 0
            do jp = 1,nPy
             do bj = jtlo,jthi
              do ip = 1,nPx
               do bi = itlo,ithi
                do j = jmin,jmax
                 if (globmskyz(bi,ip,j,bj,jp,k)  .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     if (lxxadxx) then
                        cbuff(cbuffindex) = 
     &                       globfldyz(bi,ip,j,bj,jp,k) *
     &                       sqrt(weightfld(k,iobcs))
                     else
                        cbuff(cbuffindex) = 
     &                       globfldyz(bi,ip,j,bj,jp,k) /
     &                       sqrt(weightfld(k,iobcs))
                     endif
#else
                     cbuff(cbuffindex) = globfldyz(bi,ip,j,bj,jp,k)
#endif
                 endif
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
c     -- end of iobcs loop --
cgg       enddo
c     -- end of irec loop --
      enddo

      _END_MASTER( mythid )

      return
      end

