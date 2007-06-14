
#include "CTRL_CPPOPTIONS.h"

      subroutine ctrl_set_unpack_xz(
     &     cunit, ivartype, fname, masktype, weighttype,
     &     weightfld, nwetglobal, mythid)

c     ==================================================================
c     SUBROUTINE ctrl_set_unpack_xz
c     ==================================================================
c
c     o Unpack the control vector such that land points are filled in.
c
c     o Open boundary packing added :
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
      character*( 80)   fname
      character*  (9) masktype
      character*( 80) weighttype
      _RL     weightfld( nr,nobcs )
      integer nwetglobal(nr,nobcs)
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

      real*4     cbuff    ( snx*nsx*npx*nsy*npy )
      _RL     globfldxz( snx,nsx,npx,nsy,npy,nr )
      _RL     globfld3d( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globmskxz( snx,nsx,npx,nsy,npy,nr,nobcs )
#ifdef CTRL_UNPACK_PRECISE
      _RL   weightfldxz( snx,nsx,npx,nsy,npy,nr,nobcs )
#endif

cgg(
      integer igg
      _RL     gg
      character*(80) weightname
cgg)

c     == external ==

      integer  ilnblnk
      external ilnblnk

cc     == end of interface ==

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

      do iobcs=1,nobcs
         call MDSREADFIELD_XZ_GL( 
     &        masktype, ctrlprec, 'RL',
     &        Nr, globmskxz(1,1,1,1,1,1,iobcs), iobcs,mythid)
#ifdef CTRL_UNPACK_PRECISE
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
#endif /* CTRL_UNPACK_PRECISE */
      enddo

      nrec_nl=int(ncvarrecs(ivartype)/sny)
      do irec = 1, nrec_nl
cgg       do iobcs = 1, nobcs
cgg       And now back-calculate what iobcs should be.
         do j=1,sny
            iobcs= mod((irec-1)*sny+j-1,nobcs)+1
     
            read(cunit) filencvarindex(ivartype)
            if (filencvarindex(ivartype) .NE. ncvarindex(ivartype))
     &           then
               print *, 'ctrl-set_unpack:xz:WARNING: wrong ncvarindex ',
     &              filencvarindex(ivartype), ncvarindex(ivartype)
               STOP 'in S/R ctrl_unpack'
            endif
            read(cunit) filej
            read(cunit) filei
            do k = 1, Nr
               cbuffindex = nwetglobal(k,iobcs)
               if ( cbuffindex .gt. 0 ) then
                  read(cunit) filencbuffindex
                  if (filencbuffindex .NE. cbuffindex) then
                     print *, 'WARNING: wrong cbuffindex ',
     &                    filencbuffindex, cbuffindex
                     STOP 'in S/R ctrl_unpack'
                  endif
                  read(cunit) filek
                  if (filek .NE. k) then
                     print *, 'WARNING: wrong k ',
     &                    filek, k
                     STOP 'in S/R ctrl_unpack'
                  endif
                  read(cunit) (cbuff(ii), ii=1,cbuffindex)
               endif
               cbuffindex = 0
               jj=mod((j-1)*nr+k-1,sny)+1
               kk=int((j-1)*nr+k-1)/sny+1
               do jp = 1,nPy
                do bj = jtlo,jthi
                 do ip = 1,nPx
                  do bi = itlo,ithi
                   do i = imin,imax
                    if ( globmskxz(i,bi,ip,bj,jp,k,iobcs) .ne. 0. ) then
                       cbuffindex = cbuffindex + 1
                       globfld3d(i,bi,ip,jj,bj,jp,kk) = 
     &                      cbuff(cbuffindex)
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                       globfld3d(i,bi,ip,jj,bj,jp,kk) = 
     &                      globfld3d(i,bi,ip,jj,bj,jp,kk)/
# ifdef CTRL_UNPACK_PRECISE
     &                      sqrt(weightfldxz(i,bi,ip,bj,jp,k,iobcs))
# else
     &                      sqrt(weightfld(k,iobcs))
# endif
#endif /* ALLOW_NONDIMENSIONAL_CONTROL_IO */
                    else
                       globfld3d(i,bi,ip,jj,bj,jp,kk) = 0. _d 0
                    endif
                   enddo
                  enddo
                 enddo
                enddo
               enddo
c
c     -- end of k loop --
         enddo
c     -- end of j loop --
         enddo
             
         call MDSWRITEFIELD_3D_GL( fname, ctrlprec, 'RL',
     &                             Nr, globfld3d, irec,
     &                             optimcycle, mythid)

c     -- end of iobcs loop -- This loop removed. 3-28-02.
cgg       enddo
c     -- end of irec loop --
      enddo

      do irec = nrec_nl*sny+1, ncvarrecs(ivartype)
cgg       do iobcs = 1, nobcs
cgg       And now back-calculate what iobcs should be.
         iobcs= mod(irec-1,nobcs)+1

         read(cunit) filencvarindex(ivartype)
         if (filencvarindex(ivartype) .NE. ncvarindex(ivartype))
     &        then
            print *, 'ctrl-set_unpack:xz:WARNING: wrong ncvarindex ',
     &           filencvarindex(ivartype), ncvarindex(ivartype)
            STOP 'in S/R ctrl_unpack'
         endif
         read(cunit) filej
         read(cunit) filei
         do k = 1, Nr
            cbuffindex = nwetglobal(k,iobcs)
            if ( cbuffindex .gt. 0 ) then
               read(cunit) filencbuffindex
               if (filencbuffindex .NE. cbuffindex) then
                  print *, 'WARNING: wrong cbuffindex ',
     &                 filencbuffindex, cbuffindex
                  STOP 'in S/R ctrl_unpack'
               endif
               read(cunit) filek
               if (filek .NE. k) then
                  print *, 'WARNING: wrong k ',
     &                 filek, k
                  STOP 'in S/R ctrl_unpack'
               endif
               read(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
            cbuffindex = 0
            do jp = 1,nPy
             do bj = jtlo,jthi
              do ip = 1,nPx
               do bi = itlo,ithi
                do i = imin,imax
                  if ( globmskxz(i,bi,ip,bj,jp,k,iobcs) .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
                     globfldxz(i,bi,ip,bj,jp,k) = cbuff(cbuffindex)
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     globfldxz(i,bi,ip,bj,jp,k) = 
     &                    globfldxz(i,bi,ip,bj,jp,k)/
# ifdef CTRL_UNPACK_PRECISE
     &                    sqrt(weightfldxz(i,bi,ip,bj,jp,k,iobcs))
# else
     &                    sqrt(weightfld(k,iobcs))
# endif
#endif /* ALLOW_NONDIMENSIONAL_CONTROL_IO */
                  else
                     globfldxz(i,bi,ip,bj,jp,k) = 0. _d 0
                  endif
                enddo
               enddo
              enddo
             enddo
            enddo
c
c     -- end of k loop --
         enddo
             
         call MDSWRITEFIELD_XZ_GL( fname, ctrlprec, 'RL',
     &                             Nr, globfldxz, irec,
     &                             optimcycle, mythid)

c     -- end of iobcs loop -- This loop removed. 3-28-02.
cgg       enddo
c     -- end of irec loop --
      enddo

      _END_MASTER( mythid )

#endif

      return
      end





