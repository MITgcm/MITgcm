
#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_set_unpack_xz(
     &     cunit, ivartype, fname, masktype,
     &     weightfld, nwetglobal, mythid)

c     ==================================================================
c     SUBROUTINE ctrl_set_unpack_xz
c     ==================================================================
c
c     o Unpack the control vector such that the land points are filled
c       in.
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
      character*( 80)   fname
      character*  (9) masktype
      _RL     weightfld( nr,nobcs )
      integer nwetglobal(nr,nobcs)
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

      _RL     cbuff    ( snx*nsx*npx*nsy*npy )
      _RL     globmskxz( snx,nsx,npx,nsy,npy,nr )
      _RL     globfldxz( snx,nsx,npx,nsy,npy,nr )

      integer        filenvartype
      integer        filenvarlength
      character*(10) fileExpId
      integer        fileOptimCycle
      integer        filencbuffindex
      _RL            fileDummy
      integer        fileIg
      integer        fileJg
      integer        fileI
      integer        fileJ
      integer        filensx
      integer        filensy
      integer        filek
      integer        filencvarindex(maxcvars)
      integer        filencvarrecs(maxcvars)
      integer        filencvarxmax(maxcvars)
      integer        filencvarymax(maxcvars)
      integer        filencvarnrmax(maxcvars)
      character*( 1) filencvargrd(maxcvars)
cgg(
      integer igg
      _RL     gg
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
                        globmskxz(i,bi,ip,bj,jp,k) = 0. _d 0
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
cgg       do iobcs = 1, nobcs
cgg      Iobcs has already been included in the calculation
cgg      of ncvarrecs.
cgg      And now back-calculate what iobcs should be.
          gg   = (irec-1)/nobcs
          igg  = int(gg)
          iobcs = irec - igg*nobcs

         call MDSREADFIELD_XZ_GL( 
     &        masktype, ctrlprec, 'RL',
     &        Nr, globmskxz, iobcs, mythid)

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
                  if ( globmskxz(i,bi,ip,bj,jp,k) .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
                     globfldxz(i,bi,ip,bj,jp,k) = cbuff(cbuffindex)
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     globfldxz(i,bi,ip,bj,jp,k) = 
     &                    globfldxz(i,bi,ip,bj,jp,k)/
     &                    sqrt(weightfld(k,iobcs))
#endif
                  else
                     globfldxz(i,bi,ip,bj,jp,k) = 0. _d 0
                  endif
                enddo
               enddo
              enddo
             enddo
            enddo
c
         enddo
             
         call MDSWRITEFIELD_XZ_GL( fname, ctrlprec, 'RL',
     &                             Nr, globfldxz, irec,
     &                             optimcycle, mythid)
cgg     &                             Nr, globfldxz, (irec-1)*nobcs+iobcs,
cgg     &                             optimcycle, mythid)

c     -- end of iobcs loop -- This loop removed. 3-28-02.
cgg       enddo
c     -- end of irec loop --
      enddo

      _END_MASTER( mythid )

      return
      end





