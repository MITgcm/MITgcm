
#include "CTRL_CPPOPTIONS.h"

      subroutine ctrl_set_pack_xz(
     &     cunit, ivartype, fname, masktype,weighttype,
     &     weightfld, lxxadxx, mythid)

c     ==================================================================
c     SUBROUTINE ctrl_set_pack_xz
c     ==================================================================
c
c     o Compress the control vector such that only ocean points are
c       written to file.
c
c     o Open boundary packing finalized :
c          gebbie@mit.edu, 18-Mar-2003
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
      character*( 80) fname
      character*(  9) masktype
      character*( 80) weighttype
      _RL     weightfld( nr,nobcs )
      logical lxxadxx
      integer mythid

#ifndef EXCLUDE_CTRL_PACK
c     == local variables ==

      integer bi,bj
      integer ip,jp
      integer i,j,k
      integer ii,jj,kk
      integer il
      integer irec,iobcs,nrec_nl
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer cbuffindex
cgg(
      integer igg
      _RL     gg
      character*(80) weightname
cgg)

      real*4     cbuff      ( snx*nsx*npx*nsy*npy )
      _RL     globfldxz  ( snx,nsx,npx,nsy,npy,nr )
      _RL     globfld3d  ( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globmskxz  ( snx,nsx,npx,nsy,npy,nr,nobcs )
#ifdef CTRL_PACK_PRECISE
      _RL     weightfldxz( snx,nsx,npx,nsy,npy,nr,nobcs )
#endif

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

c     Initialise temporary file
      do k = 1,nr
         do jp = 1,nPy
            do bj = jtlo,jthi
               do ip = 1,nPx
                  do bi = itlo,ithi
                     do i = imin,imax
                        globfldxz(i,bi,ip,bj,jp,k) = 0. _d 0
                        do iobcs=1,nobcs
                           globmskxz(i,bi,ip,bj,jp,k,iobcs) = 0. _d 0
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo
c     Initialise temporary file
      do k = 1,nr
         do jp = 1,nPy
            do bj = jtlo,jthi
               do j = jmin,jmax
                  do ip = 1,nPx
                     do bi = itlo,ithi
                        do i = imin,imax
                           globfld3d(i,bi,ip,j,bj,jp,k) = 0. _d 0
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

      do iobcs = 1, nobcs
         call MDSREADFIELD_XZ_GL(
     &        masktype, ctrlprec, 'RL',
     &        Nr, globmskxz(1,1,1,1,1,1,iobcs), iobcs, mythid)
#ifdef CTRL_PACK_PRECISE
         il=ilnblnk( weighttype)
         write(weightname(1:80),'(80a)') ' '
         write(weightname(1:80),'(a)') weighttype(1:il)
         call MDSREADFIELD_XZ_GL(
     &        weightname, ctrlprec, 'RL',
     &        Nr, weightfldxz(1,1,1,1,1,1,iobcs), iobcs, mythid)
CGG   One special exception: barotropic velocity should be nondimensionalized
cgg   differently. Probably introduce new variable.
         if (iobcs .eq. 3 .or. iobcs .eq. 4) then
            k = 1
            do jp = 1,nPy
               do bj = jtlo,jthi
                  do ip = 1,nPx
                     do bi = itlo,ithi
                        do i = imin,imax
cph                           weightfldxz(i,bi,ip,bj,jp,k,iobcs) = wbaro
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         endif
#endif
      enddo

      nrec_nl=int(ncvarrecs(ivartype)/sny)
      do irec = 1, nrec_nl
         call MDSREADFIELD_3D_GL( fname, ctrlprec, 'RL',
     &        nr, globfld3d, irec, mythid)
         do j=1,sny
            iobcs= mod((irec-1)*sny+j-1,nobcs)+1

CGG   One special exception: barotropic velocity should be nondimensionalized
cgg   differently. Probably introduce new variable.
            if (iobcs .eq. 3 .or. iobcs .eq. 4) then
               k = 1
               do jp = 1,nPy
                  do bj = jtlo,jthi
                     do ip = 1,nPx
                        do bi = itlo,ithi
                           do i = imin,imax
#ifdef NO_CONTROL_BAROTROPIC_VELOCITY
                              if (.not. lxxadxx) then
cgg    Get rid of any sensitivity to barotropic velocity.
                                 globfld3d(i,bi,ip,j,bj,jp,k) = 0.
                              endif
#endif
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            endif

            write(cunit) ncvarindex(ivartype)
            write(cunit) 1
            write(cunit) 1
            do k = 1,nr
             cbuffindex = 0
             do jp = 1,nPy
              do bj = jtlo,jthi
               do ip = 1,nPx
                do bi = itlo,ithi
                 do i = imin,imax
                  jj=mod((j-1)*nr+k-1,sny)+1
                  kk=int((j-1)*nr+K-1)/sny+1
                  if (globmskxz(i,bi,ip,bj,jp,k,iobcs)  .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     if (lxxadxx) then
                        cbuff(cbuffindex) = 
     &                       globfld3d(i,bi,ip,jj,bj,jp,kk) *
# ifdef CTRL_PACK_PRECISE
     &                       sqrt(weightfldxz(i,bi,ip,bj,jp,k,iobcs))
# else
     &                       sqrt(weightfld(k,iobcs))
# endif
                     else
                        cbuff(cbuffindex) = 
     &                       globfld3d(i,bi,ip,jj,bj,jp,kk) /
# ifdef CTRL_PACK_PRECISE
     &                       sqrt(weightfldxz(i,bi,ip,bj,jp,k,iobcs))
# else
     &                       sqrt(weightfld(k,iobcs))
# endif
                     endif
#else /* ALLOW_NONDIMENSIONAL_CONTROL_IO undef */
                     cbuff(cbuffindex) = globfld3d(i,bi,ip,jj,bj,jp,kk)
#endif /* ALLOW_NONDIMENSIONAL_CONTROL_IO */
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
c     -- end of k loop --
            enddo
c     -- end of j loop --
         enddo
c     -- end of irec loop --
      enddo

      do irec = nrec_nl*sny+1, ncvarrecs(ivartype)
cgg       do iobcs = 1, nobcs
cgg    Need to solve for what iobcs would have been.
         iobcs= mod(irec-1,nobcs)+1

         call MDSREADFIELD_XZ_GL( fname, ctrlprec, 'RL',
     &        nr, globfldxz, irec, mythid)

CGG   One special exception: barotropic velocity should be nondimensionalized
cgg   differently. Probably introduce new variable.
         if (iobcs .eq. 3 .or. iobcs .eq. 4) then
            k = 1
            do jp = 1,nPy
               do bj = jtlo,jthi
                  do ip = 1,nPx
                     do bi = itlo,ithi
                        do i = imin,imax
#ifdef NO_CONTROL_BAROTROPIC_VELOCITY
                           if (.not. lxxadxx) then
cgg    Get rid of any sensitivity to barotropic velocity.
                              globfldxz(i,bi,ip,bj,jp,k) = 0.
                           endif
#endif
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         endif

         write(cunit) ncvarindex(ivartype)
         write(cunit) 1
         write(cunit) 1
         do k = 1,nr
            cbuffindex = 0
            do jp = 1,nPy
             do bj = jtlo,jthi
              do ip = 1,nPx
               do bi = itlo,ithi
                do i = imin,imax
                 if (globmskxz(i,bi,ip,bj,jp,k,iobcs)  .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     if (lxxadxx) then
                        cbuff(cbuffindex) = 
     &                       globfldxz(i,bi,ip,bj,jp,k) *
# ifdef CTRL_PACK_PRECISE
     &                       sqrt(weightfldxz(i,bi,ip,bj,jp,k,iobcs))
# else
     &                       sqrt(weightfld(k,iobcs))
# endif
                     else
                        cbuff(cbuffindex) = 
     &                       globfldxz(i,bi,ip,bj,jp,k) /
# ifdef CTRL_PACK_PRECISE
     &                       sqrt(weightfldxz(i,bi,ip,bj,jp,k,iobcs))
# else
     &                       sqrt(weightfld(k,iobcs))
# endif
                     endif
#else
                     cbuff(cbuffindex) = globfldxz(i,bi,ip,bj,jp,k)
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
c
c     -- end of k loop --
         enddo
c     -- end of iobcs loop --
cgg       enddo
c     -- end of irec loop --
      enddo

      _END_MASTER( mythid )

#endif

      return
      end





