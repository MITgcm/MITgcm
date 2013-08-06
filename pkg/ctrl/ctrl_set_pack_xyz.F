C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_set_pack_xyz.F,v 1.26 2013/08/06 20:57:00 gforget Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"

      subroutine ctrl_set_pack_xyz(
     &     cunit, ivartype, fname, masktype, weighttype,
     &     weightfld, lxxadxx, mythid)

c     ==================================================================
c     SUBROUTINE ctrl_set_pack_xyz
c     ==================================================================
c
c     o Compress the control vector such that only ocean points are
c       written to file.
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
      character*( 80) fname
      character*(  9) masktype
      character*( 80) weighttype
      _RL     weightfld( nr,nsx,nsy )
      logical lxxadxx
      integer mythid

#ifndef EXCLUDE_CTRL_PACK
# ifndef ALLOW_PACKUNPACK_METHOD2
c     == local variables ==

      integer bi,bj
      integer ip,jp
      integer i,j,k
      integer ii
      integer irec
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer cbuffindex

      _RL     globmsk  ( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globfld3d( snx,nsx,npx,sny,nsy,npy,nr )
#ifdef CTRL_PACK_PRECISE
      integer il
      character*(80) weightname
      _RL   weightfld3d( snx,nsx,npx,sny,nsy,npy,nr )
#endif
      real*4 cbuff      ( snx*nsx*npx*sny*nsy*npy )
      real*4 globfldtmp2( snx,nsx,npx,sny,nsy,npy )
      real*4 globfldtmp3( snx,nsx,npx,sny,nsy,npy )

      _RL delZnorm
      integer reclen, irectrue
      integer cunit2, cunit3
      character*(80) cfile2, cfile3

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

#ifdef CTRL_DELZNORM
      delZnorm = 0.
      do k = 1, Nr
         delZnorm = delZnorm + delR(k)/FLOAT(Nr)
      enddo
#endif

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
             globfldtmp2(i,bi,ip,j,bj,jp)   = 0. _d 0
             globfldtmp3(i,bi,ip,j,bj,jp)   = 0. _d 0
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
            write(cfile2(1:80),'(a,I3.3,a,I4.4,a)')
     &           'diag_pack_nonout_ctrl_',
     &           ivartype, '_', optimcycle, '.bin'
            write(cfile3(1:80),'(a,I3.3,a,I4.4,a)')
     &           'diag_pack_dimout_ctrl_',
     &           ivartype, '_', optimcycle, '.bin'
         else
            write(cfile2(1:80),'(a,I3.3,a,I4.4,a)')
     &           'diag_pack_nonout_grad_',
     &           ivartype, '_', optimcycle, '.bin'
            write(cfile3(1:80),'(a,I3.3,a,I4.4,a)')
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

#ifdef CTRL_PACK_PRECISE
      if (weighttype.NE.' ') then
       il=ilnblnk( weighttype)
       write(weightname(1:80),'(80a)') ' '
       write(weightname(1:80),'(a)') weighttype(1:il)
       call MDSREADFIELD_3D_GL(
     &     weightname, ctrlprec, 'RL',
     &     Nr, weightfld3d, 1, mythid)
      else
       do k = 1,nr
        do jp = 1,nPy
         do bj = jtlo,jthi
          do j = jmin,jmax
           do ip = 1,nPx
            do bi = itlo,ithi
             do i = imin,imax
              weightfld3d(i,bi,ip,j,bj,jp,k) = 1. _d 0
             enddo
            enddo
           enddo
          enddo
         enddo
        enddo
       enddo
      endif
#endif

      call MDSREADFIELD_3D_GL(
     &     masktype, ctrlprec, 'RL',
     &     Nr, globmsk, 1, mythid)

      do irec = 1, ncvarrecs(ivartype)

         call MDSREADFIELD_3D_GL( fname, ctrlprec, 'RL',
     &        Nr, globfld3d, irec, mythid)

#ifndef ALLOW_ADMTLM
         write(cunit) ncvarindex(ivartype)
         write(cunit) 1
         write(cunit) 1
#endif
         do k = 1, nr
         irectrue = (irec-1)*nr + k
            if ( doZscalePack ) then
               delZnorm = (delR(1)/delR(k))**delZexp
            else
               delZnorm = 1. _d 0
            endif
            cbuffindex = 0
            do jp = 1,nPy
             do bj = jtlo,jthi
              do j = jmin,jmax
               do ip = 1,nPx
                do bi = itlo,ithi
                 do i = imin,imax
                  if (globmsk(i,bi,ip,j,bj,jp,k) .ne. 0. ) then
                     cbuffindex = cbuffindex + 1
cph(
                     globfldtmp3(i,bi,ip,j,bj,jp) =
     &                    globfld3d(i,bi,ip,j,bj,jp,k)
cph)
                  IF ( .NOT.ctrlSmoothCorrel3D ) THEN
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     if (lxxadxx) then
                        cbuff(cbuffindex) = 1/delZnorm
     &                       * globfld3d(i,bi,ip,j,bj,jp,k)
# ifdef CTRL_PACK_PRECISE
     &                       * sqrt(weightfld3d(i,bi,ip,j,bj,jp,k))
# else
     &                       * sqrt(weightfld(k,bi,bj))
# endif
                     else
                        cbuff(cbuffindex) = delZnorm
     &                       * globfld3d(i,bi,ip,j,bj,jp,k)
# ifdef CTRL_PACK_PRECISE
     &                       / sqrt(weightfld3d(i,bi,ip,j,bj,jp,k))
# else
     &                       / sqrt(weightfld(k,bi,bj))
# endif
                     endif
cph(
                     globfldtmp2(i,bi,ip,j,bj,jp) = cbuff(cbuffindex)
cph)
#else /* ALLOW_NONDIMENSIONAL_CONTROL_IO undef */
                     cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
#endif /* ALLOW_NONDIMENSIONAL_CONTROL_IO */
                     ELSE !IF ( .NOT.ctrlSmoothCorrel3D ) THEN
                     cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
                     ENDIF !IF ( .NOT.ctrlSmoothCorrel3D ) THEN
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

# else
c     == local variables ==

      integer bi,bj
      integer ip,jp
      integer i,j,k
      integer ii
      integer il
      integer irec
      integer itlo,ithi
      integer jtlo,jthi

      integer cbuffindex

      _RL msk3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      real*8 msk2d_buf(sNx,sNy,nSx,nSy)
      real*8 msk2d_buf_glo(Nx,Ny)

      _RL fld3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      real*8 fld2d_buf(sNx,sNy,nSx,nSy)
      real*8 fld2d_buf_glo(Nx,Ny)

      _RL fld3dDim(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL fld3dNodim(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)

#ifdef CTRL_PACK_PRECISE
      _RL wei3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
#endif

      real*4 cbuff      ( snx*nsx*npx*sny*nsy*npy )

      character*(80) weightname
      _RL delZnorm
      character*(80) cfile2, cfile3
      _RL dummy

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c-- part 1: preliminary reads and definitions

#ifdef CTRL_PACK_PRECISE
      call active_read_xyz(weighttype, wei3d, 1,
     &    .FALSE., .FALSE., 0 , mythid, dummy)
#endif

      call active_read_xyz(masktype, msk3d, 1,
     &    .FALSE., .FALSE., 0 , mythid, dummy)

      if ( doPackDiag ) then
         write(cfile2(1:80),'(80a)') ' '
         write(cfile3(1:80),'(80a)') ' '
         il = ilnblnk( fname )
         if ( lxxadxx ) then
            write(cfile2(1:80),'(2a)') fname(1:il),'.pack_ctrl_adim'
            write(cfile3(1:80),'(2a)') fname(1:il),'.pack_ctrl_dim'
         else
            write(cfile2(1:80),'(2a)') fname(1:il),'.pack_grad_adim'
            write(cfile3(1:80),'(2a)') fname(1:il),'.pack_grad_dim'
         endif
      endif

c-- part 2: loop over records

      do irec = 1, ncvarrecs(ivartype)

c-- 2.1:
      call READ_REC_3D_RL( fname, ctrlprec,
     &        Nr, fld3dDim, irec, 0, mythid)

c-- 2.2: normalize field if needed
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         if ( doZscalePack ) then
            delZnorm = (delR(1)/delR(k))**delZexp
         else
            delZnorm = 1. _d 0
         endif
         DO j=1,sNy
          DO i=1,sNx
           if (msk3d(i,j,k,bi,bj).EQ.0. _d 0) then
            fld3dDim(i,j,k,bi,bj)=0. _d 0
            fld3dNodim(i,j,k,bi,bj)=0. _d 0
           else
           IF ( ctrlSmoothCorrel3D ) THEN
            fld3dNodim(i,j,k,bi,bj)=fld3dDim(i,j,k,bi,bj)
           ELSE !IF ( ctrlSmoothCorrel3D ) THEN
# ifndef ALLOW_NONDIMENSIONAL_CONTROL_IO
            fld3dNodim(i,j,k,bi,bj) = fld3dDim(i,j,k,bi,bj)
# else
            if (lxxadxx) then
               fld3dNodim(i,j,k,bi,bj) =
     &              fld3dDim(i,j,k,bi,bj) / delZnorm
#  ifdef CTRL_PACK_PRECISE
     &              * sqrt(wei3d(i,j,k,bi,bj))
#  else
     &              * sqrt(weightfld(k,bi,bj))
#  endif
            else
               fld3dNodim(i,j,k,bi,bj) =
     &              fld3dDim(i,j,k,bi,bj) * delZnorm
#  ifdef CTRL_PACK_PRECISE
     &              / sqrt(wei3d(i,j,k,bi,bj))
#  else
     &              / sqrt(weightfld(k,bi,bj))
#  endif
            endif
# endif /* ALLOW_NONDIMENSIONAL_CONTROL_IO */
           ENDIF !IF ( ctrlSmoothCorrel3D ) THEN
           endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

c-- 2.3:
      if ( doPackDiag ) then
c error: twice the same one
      call WRITE_REC_3D_RL( cfile2, ctrlprec,
     &        Nr, fld3dNodim, irec, 0, mythid)
      call WRITE_REC_3D_RL( cfile3, ctrlprec,
     &        Nr, fld3dDim, irec, 0, mythid)
      endif

c-- 2.4: array -> buffer -> global buffer -> global file

#ifndef ALLOW_ADMTLM
      _BEGIN_MASTER( mythid )
      IF ( myProcId .eq. 0 ) THEN
         write(cunit) ncvarindex(ivartype)
         write(cunit) 1
         write(cunit) 1
      ENDIF
      _END_MASTER( mythid )
      _BARRIER
#endif

      do k = 1, nr

        CALL MDS_PASS_R8toRL( fld2d_buf, fld3dNodim,
     &                 0, 0, 1, k, Nr, 0, 0, .FALSE., myThid )
        CALL BAR2( myThid )
        CALL GATHER_2D_R8( fld2d_buf_glo, fld2d_buf,
     &                       Nx,Ny,.FALSE.,.TRUE.,myThid)
        CALL BAR2( myThid )

        CALL MDS_PASS_R8toRL( msk2d_buf, msk3d,
     &                 0, 0, 1, k, Nr, 0, 0, .FALSE., myThid )
        CALL BAR2( myThid )
        CALL GATHER_2D_R8( msk2d_buf_glo, msk2d_buf,
     &                       Nx,Ny,.FALSE.,.TRUE.,myThid)
        CALL BAR2( myThid )

        _BEGIN_MASTER( mythid )
        cbuffindex = 0
        IF ( myProcId .eq. 0 ) THEN

        DO j=1,Ny
          DO i=1,Nx
            if (msk2d_buf_glo(i,j) .ne. 0. ) then
               cbuffindex = cbuffindex + 1
               cbuff(cbuffindex) = fld2d_buf_glo(i,j)
#ifdef ALLOW_ADMTLM
               nveccount = nveccount + 1
               phtmpadmtlm(nveccount) = cbuff(cbuffindex)
#endif
            endif
          ENDDO
        ENDDO

#ifndef ALLOW_ADMTLM
        if ( cbuffindex .gt. 0) then
          write(cunit) cbuffindex
          write(cunit) k
          write(cunit) (cbuff(ii), ii=1,cbuffindex)
        endif
#endif

        ENDIF
        _END_MASTER( mythid )
        _BARRIER

      enddo
      enddo

# endif /* ALLOW_PACKUNPACK_METHOD2 */
# endif /* EXCLUDE_CTRL_PACK */

      return
      end
