
#include "CTRL_CPPOPTIONS.h"

      subroutine ctrl_set_pack_xy(
     &     cunit, ivartype, fname, masktype, weighttype,
     &     lxxadxx, mythid)

c     ==================================================================
c     SUBROUTINE ctrl_set_pack_xy
c     ==================================================================
c
c     o Compress the control vector such that only ocean points are
c       written to file.
c
c     changed: heimbach@mit.edu 17-Jun-2003
c              merged Armin's changes to replace write of
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

      integer cunit
      integer ivartype
      character*( 80) fname, fnameGlobal
      character*(  5) masktype
      character*( 80) weighttype
      logical lxxadxx
      integer mythid

c     == local variables ==

      integer bi,bj
      integer ip,jp
      integer i,j,k
      integer ii
      integer il
      integer irec,nrec_nl
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer cbuffindex

      real*4     cbuff    ( snx*nsx*npx*sny*nsy*npy )
      _RL     globmsk  ( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globfld3d( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globfld2d( snx,nsx,npx,sny,nsy,npy )

      character*( 80) weightname

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = 1
      jthi = nsy
      itlo = 1
      ithi = nsx
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      nbuffglobal = nbuffglobal + 1

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

#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
      il=ilnblnk( weighttype)
      write(weightname(1:80),'(80a)') ' '
      write(weightname(1:80),'(a)') weighttype(1:il)
      call MDSREADFIELD_2D_GL( 
     &     weightname, ctrlprec, 'RL',
     &     1, globfld2d, 1, mythid)
#endif
      
      call MDSREADFIELD_3D_GL( 
     &     masktype, ctrlprec, 'RL',
     &     Nr, globmsk, 1, mythid)

      nrec_nl=int(ncvarrecs(ivartype)/Nr)
      do irec = 1, nrec_nl

         call MDSREADFIELD_3D_GL( fname, ctrlprec, 'RL',
     &        Nr, globfld3d, irec, mythid)
         do k = 1, Nr
         write(cunit) ncvarindex(ivartype)
         write(cunit) 1
         write(cunit) 1
            cbuffindex = 0
            do jp = 1,nPy
             do bj = jtlo,jthi
              do j = jmin,jmax
               do ip = 1,nPx
                do bi = itlo,ithi
                 do i = imin,imax
                  if (globmsk(i,bi,ip,j,bj,jp,1)  .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     if (lxxadxx) then
                        cbuff(cbuffindex) =
     &                       globfld3d(i,bi,ip,j,bj,jp,k) *
     &                       sqrt(globfld2d(i,bi,ip,j,bj,jp))
                     else
                        cbuff(cbuffindex) =
     &                       globfld3d(i,bi,ip,j,bj,jp,k) /
     &                       sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
               write(cunit) 1
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo
c
c     -- end of irec loop --
      enddo

      do irec = nrec_nl*Nr+1, ncvarrecs(ivartype)

         call MDSREADFIELD_2D_GL( fname, ctrlprec, 'RL',
     &        1, globfld3d(1,1,1,1,1,1,1), irec, mythid)

         write(cunit) ncvarindex(ivartype)
         write(cunit) 1
         write(cunit) 1
         do k = 1, 1
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
     &                       sqrt(globfld2d(i,bi,ip,j,bj,jp))
                     else
                        cbuff(cbuffindex) = 
     &                       globfld3d(i,bi,ip,j,bj,jp,k) /
     &                       sqrt(globfld2d(i,bi,ip,j,bj,jp))
                     endif
#else
                     cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
#endif
#ifdef ALLOW_ADMTLM
                     cbuffGlobal(cbuffindex) = cbuff(cbuffindex)
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
#ifdef ALLOW_ADMTLM
            write(fnameGlobal(1:80),'(a)') ' '
            write(fnameGlobal,'(a,i4.4)') 
     &           'admtlm_vector.it', optimcycle
            call mdswritevector( fnameGlobal, 64, .false., 'RL',
     &           cbuffindex, cbuffGlobal, 1, 1, nbuffGlobal, 0, mythid )
#endif
         enddo
c
c     -- end of irec loop --
      enddo

      _END_MASTER( mythid )

      return
      end

