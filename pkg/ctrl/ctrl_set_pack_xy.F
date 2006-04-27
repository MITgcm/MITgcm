
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
      character*(  9) masktype
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

      _RL     globmsk  ( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globfld3d( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globfld2d( snx,nsx,npx,sny,nsy,npy )
      real*4 cbuff      ( snx*nsx*npx*sny*nsy*npy )

      character*( 80) weightname

      integer reclen, irectrue
      integer cunit2, cunit3
      character*(80) cfile2, cfile3
      real*4 globfldtmp2( snx,nsx,npx,sny,nsy,npy )
      real*4 globfldtmp3( snx,nsx,npx,sny,nsy,npy )

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
                           globfld3d  (i,bi,ip,j,bj,jp,k) = 0. _d 0
                           globmsk    (i,bi,ip,j,bj,jp,k) = 0. _d 0
                           globfldtmp2(i,bi,ip,j,bj,jp)   = 0.
                           globfldtmp3(i,bi,ip,j,bj,jp)   = 0.
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

      if ( doPackDiag ) then
         write(cfile2(1:80),'(80a)') ' '
         write(cfile3(1:80),'(80a)') ' '
         if ( lxxadxx ) then
            write(cfile2(1:80),'(a,I2.2,a,I4.4,a)')
     &           'diag_pack_nonout_ctrl_', 
     &           ivartype, '_', optimcycle, '.bin'
            write(cfile3(1:80),'(a,I2.2,a,I4.4,a)')
     &           'diag_pack_dimout_ctrl_', 
     &           ivartype, '_', optimcycle, '.bin'
         else
            write(cfile2(1:80),'(a,I2.2,a,I4.4,a)')
     &           'diag_pack_nonout_grad_', 
     &           ivartype, '_', optimcycle, '.bin'
            write(cfile3(1:80),'(a,I2.2,a,I4.4,a)')
     &           'diag_pack_dimout_grad_', 
     &           ivartype, '_', optimcycle, '.bin'
         endif

         reclen = FLOAT(snx*nsx*npx*sny*nsy*npy*4)
         call mdsfindunit( cunit2, mythid )
         open( cunit2, file=cfile2, status='unknown',
     &        access='direct', recl=reclen )
         call mdsfindunit( cunit3, mythid )
         open( cunit3, file=cfile3, status='unknown',
     &        access='direct', recl=reclen )
      endif

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
         irectrue = (irec-1)*nr + k
#ifndef ALLOW_ADMTLM
         write(cunit) ncvarindex(ivartype)
         write(cunit) 1
         write(cunit) 1
#endif
            cbuffindex = 0
            do jp = 1,nPy
             do bj = jtlo,jthi
              do j = jmin,jmax
               do ip = 1,nPx
                do bi = itlo,ithi
                 do i = imin,imax
                  if (globmsk(i,bi,ip,j,bj,jp,1)  .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
cph(
                     globfldtmp3(i,bi,ip,j,bj,jp) =
     &                    globfld3d(i,bi,ip,j,bj,jp,k)
cph)
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
cph(
                     globfldtmp2(i,bi,ip,j,bj,jp) = cbuff(cbuffindex)
cph)
#ifdef ALLOW_ADMTLM
                     nveccount = nveccount + 1
                     phtmpadmtlm(nveccount) = cbuff(cbuffindex)
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
#ifndef ALLOW_ADMTLM
               write(cunit) cbuffindex
               write(cunit) 1
cph#endif
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
#endif
            endif
c
            if ( doPackDiag ) then
               write(cunit2,rec=irectrue) globfldtmp2
               write(cunit3,rec=irectrue) globfldtmp3
            endif
c
         enddo
c
c     -- end of irec loop --
      enddo

      do irec = nrec_nl*Nr+1, ncvarrecs(ivartype)

         call MDSREADFIELD_2D_GL( fname, ctrlprec, 'RL',
     &        1, globfld3d(1,1,1,1,1,1,1), irec, mythid)

#ifndef ALLOW_ADMTLM
         write(cunit) ncvarindex(ivartype)
         write(cunit) 1
         write(cunit) 1
#endif
         do k = 1, 1
            irectrue = irec
            cbuffindex = 0
            do jp = 1,nPy
             do bj = jtlo,jthi
              do j = jmin,jmax
               do ip = 1,nPx
                do bi = itlo,ithi
                 do i = imin,imax
                  if (globmsk(i,bi,ip,j,bj,jp,k)  .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
cph(
                     globfldtmp3(i,bi,ip,j,bj,jp) =
     &                    globfld3d(i,bi,ip,j,bj,jp,k)
cph)
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
cph(
                     globfldtmp2(i,bi,ip,j,bj,jp) = cbuff(cbuffindex)
cph)
#ifdef ALLOW_ADMTLM
                     nveccount = nveccount + 1
                     phtmpadmtlm(nveccount) = cbuff(cbuffindex)
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
#ifndef ALLOW_ADMTLM
               write(cunit) cbuffindex
               write(cunit) k
cph#endif
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
#endif
            endif
c
            if ( doPackDiag ) then
               write(cunit2,rec=irectrue) globfldtmp2
               write(cunit3,rec=irectrue) globfldtmp3
            endif
c
         enddo
c
c     -- end of irec loop --
      enddo

      if ( doPackDiag ) then
         close ( cunit2 )
         close ( cunit3 )
      endif
 
      _END_MASTER( mythid )

      return
      end

