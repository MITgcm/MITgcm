
#include "CTRL_CPPOPTIONS.h"

      subroutine ctrl_set_unpack_xyz(
     &     cunit, ivartype, fname, masktype, weighttype,
     &     weightfld, nwetglobal, mythid)

c     ==================================================================
c     SUBROUTINE ctrl_set_unpack_xyz
c     ==================================================================
c
c     o Unpack the control vector such that land points are filled in.
c
c     o Use a more precise nondimensionalization that depends on (x,y)
c       Added weighttype to the argument list so that I can geographically
c       vary the nondimensionalization.
c       gebbie@mit.edu, 18-Mar-2003
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
      character*  (5) masktype
      character*( 80) weighttype
      _RL     weightfld( nr,nsx,nsy )
      integer nwetglobal(nr)
      integer mythid

c     == local variables ==

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

      real*4     cbuff    ( snx*nsx*npx*sny*nsy*npy )
      _RL     globmsk  ( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globfld3d( snx,nsx,npx,sny,nsy,npy,nr )
#ifdef CTRL_UNPACK_PRECISE
      _RL   weightfld3d( snx,nsx,npx,sny,nsy,npy,nr )
#endif

      character*(128)   cfile
      character*(80) weightname

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

#ifdef CTRL_UNPACK_PRECISE
      il=ilnblnk( weighttype)
      write(weightname(1:80),'(80a)') ' '
      write(weightname(1:80),'(a)') weighttype(1:il)

      call MDSREADFIELD_3D_GL(
     &     weightname, ctrlprec, 'RL',
     &     Nr, weightfld3d, 1, mythid)
#endif

      call MDSREADFIELD_3D_GL( 
     &     masktype, ctrlprec, 'RL',
     &     Nr, globmsk, 1, mythid)

      do irec = 1, ncvarrecs(ivartype)
         read(cunit) filencvarindex(ivartype)
         if (filencvarindex(ivartype) .NE. ncvarindex(ivartype))
     &        then
            print *, 'ctrl_set_unpack_xyz:WARNING: wrong ncvarindex ',
     &           filencvarindex(ivartype), ncvarindex(ivartype)
            STOP 'in S/R ctrl_unpack'
         endif
         read(cunit) filej
         read(cunit) filei
         do k = 1, Nr
            cbuffindex = nwetglobal(k)
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
              do j = jmin,jmax
               do ip = 1,nPx
                do bi = itlo,ithi
                 do i = imin,imax
                  if ( globmsk(i,bi,ip,j,bj,jp,k) .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
                     globfld3d(i,bi,ip,j,bj,jp,k) = cbuff(cbuffindex)
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     globfld3d(i,bi,ip,j,bj,jp,k) = 
     &                    globfld3d(i,bi,ip,j,bj,jp,k)/
# ifdef CTRL_UNPACK_PRECISE
     &                    sqrt(weightfld3d(i,bi,ip,j,bj,jp,k))
# else
     &                    sqrt(weightfld(k,bi,bj))
# endif
#endif /* ALLOW_NONDIMENSIONAL_CONTROL_IO */
                  else
                     globfld3d(i,bi,ip,j,bj,jp,k) = 0. _d 0
                  endif
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
c
         enddo
             
         call MDSWRITEFIELD_3D_GL( fname, ctrlprec, 'RL',
     &                             Nr, globfld3d,
     &                             irec,  optimcycle, mythid)

      enddo

      _END_MASTER( mythid )

      return
      end

