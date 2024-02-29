#include "CTRL_OPTIONS.h"

      subroutine ctrl_set_pack_yz(
     &     cunit, ivartype, fname, masktype, weighttype,
     &     weightfld, lxxadxx, mythid)

c     ==================================================================
c     SUBROUTINE ctrl_set_pack_yz
c     ==================================================================
c
c     o Compress the control vector such that only ocean points are
c       written to file.
c
c     o Open boundary packing added :
c          gebbie@mit.edu, 18-Mar-2003
c
c     changed: heimbach@mit.edu 17-Jun-2003
c              merged changes from Armin to replace write of
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

#include "CTRL.h"
#include "CTRL_OBCS.h"
#include "OPTIMCYCLE.h"

c     == routine arguments ==

      integer cunit
      integer ivartype
      character*(MAX_LEN_FNAM) fname
      character*(  9) masktype
      character*(MAX_LEN_FNAM) weighttype
      _RL     weightfld( Nr,nobcs )
      logical lxxadxx
      integer mythid

#ifndef EXCLUDE_CTRL_PACK
c     == local variables ==

      integer bi,bj
      integer ip,jp
      integer i,j,k
      integer ii,jj,kk
      integer irec,iobcs,nrec_nl
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer cbuffindex
      integer reclen, irectrue
      integer cunit2, cunit3
      character*(MAX_LEN_FNAM) cfile2, cfile3

      real*4  cbuff      ( nSx*nPx*sNy*nSy*nPy )
      real*4  globfldtmp2( nSx,nPx,sNy,nSy,nPy )
      real*4  globfldtmp3( nSx,nPx,sNy,nSy,nPy )
      _RL     globfldyz  ( nSx,nPx,sNy,nSy,nPy,Nr )
      _RL     globfld3d  ( sNx,nSx,nPx,sNy,nSy,nPy,Nr )
      _RL     globmskyz  ( nSx,nPx,sNy,nSy,nPy,Nr,nobcs )
#ifdef CTRL_PACK_PRECISE
      integer il
      character*(MAX_LEN_FNAM) weightname
      _RL     weightfldyz( nSx,nPx,sNy,nSy,nPy,Nr,nobcs )

c     == external ==

      integer  ilnblnk
      external ilnblnk
#endif

c     == end of interface ==

      jtlo = 1
      jthi = nSy
      itlo = 1
      ithi = nSx
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c     Initialise temporary file
      do k = 1,Nr
         do jp = 1,nPy
            do bj = jtlo,jthi
               do j = jmin,jmax
                  do ip = 1,nPx
                     do bi = itlo,ithi
                        globfldyz  (bi,ip,j,bj,jp,k) = 0. _d 0
                        globfldtmp2(bi,ip,j,bj,jp)   = 0.
                        globfldtmp3(bi,ip,j,bj,jp)   = 0.
                        do iobcs=1,nobcs
                           globmskyz(bi,ip,j,bj,jp,k,iobcs) = 0. _d 0
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo
c     Initialise temporary file
      do k = 1,Nr
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

      if ( doPackDiag ) then
         if ( lxxadxx ) then
            write(cfile2,'(a,I2.2,a,I4.4,a)')
     &           'diag_pack_nonout_ctrl_',
     &           ivartype, '_', optimcycle, '.bin'
            write(cfile3,'(a,I2.2,a,I4.4,a)')
     &           'diag_pack_dimout_ctrl_',
     &           ivartype, '_', optimcycle, '.bin'
         else
            write(cfile2,'(a,I2.2,a,I4.4,a)')
     &           'diag_pack_nonout_grad_',
     &           ivartype, '_', optimcycle, '.bin'
            write(cfile3,'(a,I2.2,a,I4.4,a)')
     &           'diag_pack_dimout_grad_',
     &           ivartype, '_', optimcycle, '.bin'
         endif

         reclen = nSx*nPx*sNy*nSy*nPy*4
         call mdsfindunit( cunit2, mythid )
         open( cunit2, file=cfile2, status='unknown',
     &        access='direct', recl=reclen )
         call mdsfindunit( cunit3, mythid )
         open( cunit3, file=cfile3, status='unknown',
     &        access='direct', recl=reclen )
      endif

      do iobcs=1,nobcs
         call MDSREADFIELD_YZ_GL(
     &        masktype, ctrlprec, 'RL',
     &        Nr, globmskyz(1,1,1,1,1,1,iobcs), iobcs,mythid)
#ifdef CTRL_PACK_PRECISE
         il=ilnblnk( weighttype)
         write(weightname,'(a)') weighttype(1:il)
         call MDSREADFIELD_YZ_GL(
     &       weightname, ctrlprec, 'RL',
     &       Nr, weightfldyz(1,1,1,1,1,1,iobcs), iobcs, mythid)
#endif
      enddo

      if ( useSingleCPUio ) then
C     MDSREADFIELD_YZ_GL does not know about useSingleCPUio, so the faster
C     method that works for .not.useSingleCPUio cannot be used
       nrec_nl = 0
      else
       nrec_nl = int(ncvarrecs(ivartype)/Nx)
      endif
      do irec = 1, nrec_nl
c     Need to solve for what iobcs would have been.

         call MDSREADFIELD_3D_GL( fname, ctrlprec, 'RL',
     &        Nr, globfld3D, irec, mythid)

         do i=1,sNx
            iobcs= mod((irec-1)*sNx+i-1,nobcs)+1

            write(cunit) ncvarindex(ivartype)
            write(cunit) 1
            write(cunit) 1
            do k = 1,Nr
             irectrue = (irec-1)*nobcs*Nr + (iobcs-1)*Nr + k
             cbuffindex = 0
             do jp = 1,nPy
              do bj = jtlo,jthi
               do ip = 1,nPx
                do bi = itlo,ithi
                 do j = jmin,jmax
                  ii=mod ( (i-1)*Nr*sNy+(k-1)*sNy+j-1      , sNx ) + 1
                  jj=mod( ((i-1)*Nr*sNy+(k-1)*sNy+j-1)/sNx , sNy ) + 1
                  kk=int((i-1)*Nr*sNy+(k-1)*sNy+j-1)/(sNx*sNy) + 1
                  if (globmskyz(bi,ip,j,bj,jp,k,iobcs)  .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
cph(
                     globfldtmp3(bi,ip,j,bj,jp) =
     &                    globfld3d(ii,bi,ip,jj,bj,jp,kk)
cph)
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     if (lxxadxx) then
                        cbuff(cbuffindex) =
     &                       globfld3d(ii,bi,ip,jj,bj,jp,kk) *
#ifdef CTRL_PACK_PRECISE
     &                       sqrt(weightfldyz(bi,ip,j,bj,jp,k,iobcs))
#else
     &                       sqrt(weightfld(k,iobcs))
#endif
                     else
                        cbuff(cbuffindex) =
     &                       globfld3d(ii,bi,ip,jj,bj,jp,kk) /
#ifdef CTRL_PACK_PRECISE
     &                       sqrt(weightfldyz(bi,ip,j,bj,jp,k,iobcs))
#else
     &                       sqrt(weightfld(k,iobcs))
#endif
                     endif
cph(
                     globfldtmp2(bi,ip,j,bj,jp) = cbuff(cbuffindex)
cph)
#else /* ALLOW_NONDIMENSIONAL_CONTROL_IO undef */
                     cbuff(cbuffindex) = globfld3d(ii,bi,ip,jj,bj,jp,kk)
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
c
            if ( doPackDiag ) then
               write(cunit2,rec=irectrue) globfldtmp2
               write(cunit3,rec=irectrue) globfldtmp3
            endif
c
c     -- end of k loop --
            enddo
c     -- end of i loop --
         enddo
c     -- end of irec loop --
      enddo

      do irec = nrec_nl*nx+1, ncvarrecs(ivartype)
c     Need to solve for what iobcs would have been.
         iobcs= mod(irec-1,nobcs)+1

         call MDSREADFIELD_YZ_GL( fname, ctrlprec, 'RL',
     &        Nr, globfldyz, irec, mythid)

         write(cunit) ncvarindex(ivartype)
         write(cunit) 1
         write(cunit) 1
         do k = 1,Nr
            irectrue = (irec-1)*nobcs*Nr + (iobcs-1)*Nr + k
            cbuffindex = 0
            do jp = 1,nPy
             do bj = jtlo,jthi
              do ip = 1,nPx
               do bi = itlo,ithi
                do j = jmin,jmax
                 if (globmskyz(bi,ip,j,bj,jp,k,iobcs)  .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
cph(
                     globfldtmp3(bi,ip,j,bj,jp) =
     &                    globfldyz(bi,ip,j,bj,jp,k)
cph)
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     if (lxxadxx) then
                        cbuff(cbuffindex) =
     &                       globfldyz(bi,ip,j,bj,jp,k) *
#ifdef CTRL_PACK_PRECISE
     &                       sqrt(weightfldyz(bi,ip,j,bj,jp,k,iobcs))
#else
     &                       sqrt(weightfld(k,iobcs))
#endif
                     else
                        cbuff(cbuffindex) =
     &                       globfldyz(bi,ip,j,bj,jp,k) /
#ifdef CTRL_PACK_PRECISE
     &                       sqrt(weightfldyz(bi,ip,j,bj,jp,k,iobcs))
#else
     &                       sqrt(weightfld(k,iobcs))
#endif
                     endif
cph(
                     globfldtmp2(bi,ip,j,bj,jp) = cbuff(cbuffindex)
cph)
#else /* ALLOW_NONDIMENSIONAL_CONTROL_IO undef */
                     cbuff(cbuffindex) = globfldyz(bi,ip,j,bj,jp,k)
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
c
            if ( doPackDiag ) then
               write(cunit2,rec=irectrue) globfldtmp2
               write(cunit3,rec=irectrue) globfldtmp3
            endif
c
c     -- end of k loop --
         enddo
c     -- end of irec loop --
      enddo

      _END_MASTER( mythid )

#endif

      return
      end

