#include "CTRL_OPTIONS.h"

      subroutine ctrl_set_unpack_xyz(
     &     lxxadxx, cunit, ivartype, fname, masktype, weighttype,
     &     weightfld, nwetglobal, mythid )

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

      logical lxxadxx
      integer cunit
      integer ivartype
      character*( 80)   fname
      character*(  9) masktype
      character*( 80) weighttype
      _RL     weightfld( Nr,nSx,nSy )
      integer nwetglobal(Nr)
      integer mythid

#ifndef EXCLUDE_CTRL_PACK
c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
      LOGICAL doPackOld
      integer bi,bj
      integer i,j,k
      integer ii, irec
      integer cbuffindex
      real*4 cbuff( sNx*nSx*nPx*sNy*nSy*nPy )
      character*(80) cfile2, cfile3
C========================================================================
# ifndef ALLOW_PACKUNPACK_METHOD2
      integer ip,jp
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      _RL     globmsk  ( sNx,nSx,nPx,sNy,nSy,nPy,Nr )
      _RL     globfld3d( sNx,nSx,nPx,sNy,nSy,nPy,Nr )
#ifdef CTRL_UNPACK_PRECISE
      integer il
      character*(80) weightname
      _RL   weightfld3d( sNx,nSx,nPx,sNy,nSy,nPy,Nr )
#endif
      real*4 globfldtmp2( sNx,nSx,nPx,sNy,nSy,nPy )
      real*4 globfldtmp3( sNx,nSx,nPx,sNy,nSy,nPy )
      _RL delZnorm
      integer reclen, irectrue
      integer cunit2, cunit3
# else /* ALLOW_PACKUNPACK_METHOD2 */
      integer il
      _RL msk3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      real*8 msk2d_buf(sNx,sNy,nSx,nSy)
      real*8 msk2d_buf_glo(Nx,Ny)
      real*8 fld2d_buf(sNx,sNy,nSx,nSy)
      real*8 fld2d_buf_glo(Nx,Ny)
      _RL fld3dDim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fld3dNodim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#ifdef CTRL_UNPACK_PRECISE
      _RL wei3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
      _RL delZnorm
      _RL dummy
# endif /* ALLOW_PACKUNPACK_METHOD2 */
c     == end of interface ==

# ifndef ALLOW_PACKUNPACK_METHOD2

      jtlo = 1
      jthi = nSy
      itlo = 1
      ithi = nSx
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

#ifdef CTRL_DELZNORM
      delZnorm = 0.
      do k = 1, Nr
         delZnorm = delZnorm + delR(k)/FLOAT(Nr)
      enddo
#endif

      doPackOld = (.NOT.ctrlSmoothCorrel3D).AND.(.NOT.ctrlUseGen)

c     Initialise temporary file
      do k = 1,Nr
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

#ifdef CTRL_DELZNORM
      do k = 1, Nr
         print *, 'ph-delznorm ', k, delZnorm, delR(k)
         print *, 'ph-weight   ', weightfld(k,1,1)
      enddo
#endif

      if ( doPackDiag ) then
         write(cfile2(1:80),'(80a)') ' '
         write(cfile3(1:80),'(80a)') ' '
         if ( lxxadxx ) then
            write(cfile2(1:80),'(a,I3.3,a,I4.4,a)')
     &           'diag_unpack_nondim_ctrl_',
     &           ivartype, '_', optimcycle, '.bin'
            write(cfile3(1:80),'(a,I3.3,a,I4.4,a)')
     &           'diag_unpack_dimens_ctrl_',
     &           ivartype, '_', optimcycle, '.bin'
         else
            write(cfile2(1:80),'(a,I3.3,a,I4.4,a)')
     &           'diag_unpack_nondim_grad_',
     &           ivartype, '_', optimcycle, '.bin'
            write(cfile3(1:80),'(a,I3.3,a,I4.4,a)')
     &           'diag_unpack_dimens_grad_',
     &           ivartype, '_', optimcycle, '.bin'
         endif

         reclen = FLOAT(sNx*nSx*nPx*sNy*nSy*nPy*4)
         call mdsfindunit( cunit2, mythid )
         open( cunit2, file=cfile2, status='unknown',
     &        access='direct', recl=reclen )
         call mdsfindunit( cunit3, mythid )
         open( cunit3, file=cfile3, status='unknown',
     &        access='direct', recl=reclen )
      endif

#ifdef CTRL_UNPACK_PRECISE
      if (weighttype.NE.' ') then
       il=ilnblnk( weighttype)
       write(weightname(1:80),'(80a)') ' '
       write(weightname(1:80),'(a)') weighttype(1:il)
       call MDSREADFIELD_3D_GL(
     &     weightname, ctrlprec, 'RL',
     &     Nr, weightfld3d, 1, mythid)
      else
       do k = 1,Nr
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
#ifndef ALLOW_ADMTLM
         read(cunit) filencvarindex(ivartype)
         if (filencvarindex(ivartype) .NE. ncvarindex(ivartype))
     &        then
            print *, 'ctrl_set_unpack_xyz:WARNING: wrong ncvarindex ',
     &           filencvarindex(ivartype), ncvarindex(ivartype)
            STOP 'in S/R ctrl_set_unpack_xyz'
         endif
         read(cunit) filej
         read(cunit) filei
#endif /* ALLOW_ADMTLM */
         do k = 1, Nr
         irectrue = (irec-1)*Nr + k
            if ( doZscaleUnpack ) then
               delZnorm = (delR(1)/delR(k))**delZexp
            else
               delZnorm = 1. _d 0
            endif
            cbuffindex = nwetglobal(k)
            if ( cbuffindex .gt. 0 ) then
#ifndef ALLOW_ADMTLM
               read(cunit) filencbuffindex
               if (filencbuffindex .NE. cbuffindex) then
                  print *, 'WARNING: wrong cbuffindex ',
     &                 filencbuffindex, cbuffindex
                  STOP 'in S/R ctrl_set_unpack_xyz'
               endif
               read(cunit) filek
               if (filek .NE. k) then
                  print *, 'WARNING: wrong k ',
     &                 filek, k
                  STOP 'in S/R ctrl_set_unpack_xyz'
               endif
cph#endif /* ALLOW_ADMTLM */
               read(cunit) (cbuff(ii), ii=1,cbuffindex)
#endif /* ALLOW_ADMTLM */
            endif
c
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
cph(
                     globfldtmp2(i,bi,ip,j,bj,jp) = cbuff(cbuffindex)
cph)
#ifdef ALLOW_ADMTLM
                     nveccount = nveccount + 1
                     globfld3d(i,bi,ip,j,bj,jp,k) =
     &                 phtmpadmtlm(nveccount)
cph(
                     globfldtmp2(i,bi,ip,j,bj,jp) =
     &                 phtmpadmtlm(nveccount)
cph)
#endif
                     IF ( doPackOld ) THEN
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
                     if ( lxxadxx ) then
                        globfld3d(i,bi,ip,j,bj,jp,k) = delZnorm
     &                       * globfld3d(i,bi,ip,j,bj,jp,k)
# ifdef CTRL_UNPACK_PRECISE
     &                       / sqrt(weightfld3d(i,bi,ip,j,bj,jp,k))
# else
     &                       / sqrt(weightfld(k,bi,bj))
# endif
                     else
                        globfld3d(i,bi,ip,j,bj,jp,k) = 1/delZnorm
     &                       * globfld3d(i,bi,ip,j,bj,jp,k)
# ifdef CTRL_UNPACK_PRECISE
     &                       * sqrt(weightfld3d(i,bi,ip,j,bj,jp,k))
# else
     &                       * sqrt(weightfld(k,bi,bj))
# endif
                     endif
#endif /* ALLOW_NONDIMENSIONAL_CONTROL_IO */
                     ENDIF !IF ( doPackOld ) THEN
                  else
                     globfld3d(i,bi,ip,j,bj,jp,k) = 0. _d 0
                  endif
cph(
                  globfldtmp3(i,bi,ip,j,bj,jp) =
     &                 globfld3d(i,bi,ip,j,bj,jp,k)
cph)
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
c
            if ( doPackDiag ) then
               write(cunit2,rec=irectrue) globfldtmp2
               write(cunit3,rec=irectrue) globfldtmp3
            endif
c
         enddo

         call MDSWRITEFIELD_3D_GL( fname, ctrlprec, 'RL',
     &                             Nr, globfld3d,
     &                             irec,  optimcycle, mythid)

      enddo

      if ( doPackDiag ) then
         close ( cunit2 )
         close ( cunit3 )
      endif

      _END_MASTER( mythid )

# else /* ALLOW_PACKUNPACK_METHOD2 */

c-- part 1: preliminary reads and definitions

      doPackOld = (.NOT.ctrlSmoothCorrel3D).AND.(.NOT.ctrlUseGen)

#ifdef CTRL_UNPACK_PRECISE
#ifdef ALLOW_AUTODIFF
      call active_read_xyz(weighttype, wei3d, 1,
     &    .FALSE., .FALSE., 0 , mythid, dummy)
#else
      CALL READ_REC_XYZ_RL( weighttype, wei3d, 1, 1, myThid )
#endif
#endif

#ifdef ALLOW_AUTODIFF
      call active_read_xyz(masktype, msk3d, 1,
     &    .FALSE., .FALSE., 0 , mythid, dummy)
#else
      CALL READ_REC_XYZ_RL( masktype, msk3d, 1, 1, myThid )
#endif

      if ( doPackDiag ) then
         write(cfile2(1:80),'(80a)') ' '
         write(cfile3(1:80),'(80a)') ' '
         il = ilnblnk( fname )
         if ( lxxadxx ) then
            write(cfile2(1:80),'(2a)') fname(1:il),'.unpack_ctrl_adim'
            write(cfile3(1:80),'(2a)') fname(1:il),'.unpack_ctrl_dim'
         else
            write(cfile2(1:80),'(2a)') fname(1:il),'.unpack_grad_adim'
            write(cfile3(1:80),'(2a)') fname(1:il),'.unpack_grad_dim'
         endif
      endif

c-- part 2: loop over records

      do irec = 1, ncvarrecs(ivartype)

c-- 2.1: array <- buffer <- global buffer <- global file

#ifndef ALLOW_ADMTLM
      _BEGIN_MASTER( mythid )
        IF ( myProcId .eq. 0 ) THEN
         read(cunit) filencvarindex(ivartype)
         if (filencvarindex(ivartype) .NE. ncvarindex(ivartype))
     &        then
            print *, 'ctrl_set_unpack_xyz:WARNING: wrong ncvarindex ',
     &           filencvarindex(ivartype), ncvarindex(ivartype)
            STOP 'in S/R ctrl_set_unpack_xyz'
         endif
         read(cunit) filej
         read(cunit) filei
        ENDIF
      _END_MASTER( mythid )
      _BARRIER
#endif /* ALLOW_ADMTLM */

      do k = 1, Nr

        CALL MDS_PASS_R8toRL( msk2d_buf, msk3d,
     &                 0, 0, 1, k, Nr, 0, 0, .FALSE., myThid )
        CALL BAR2( myThid )
        CALL GATHER_2D_R8( msk2d_buf_glo, msk2d_buf,
     &                       Nx,Ny,.FALSE.,.TRUE.,myThid)
        CALL BAR2( myThid )

        _BEGIN_MASTER( mythid )
         cbuffindex = nwetglobal(k)
        IF ( myProcId .eq. 0 ) THEN

#ifndef ALLOW_ADMTLM
        if ( cbuffindex .gt. 0) then
          read(cunit) filencbuffindex
          read(cunit) filek
          if (filencbuffindex .NE. cbuffindex) then
            print *, 'WARNING: wrong cbuffindex ',
     &                 filencbuffindex, cbuffindex
            STOP 'in S/R ctrl_set_unpack_xyz'
          endif
          if (filek .NE. k) then
            print *, 'WARNING: wrong k ',
     &                 filek, k
            STOP 'in S/R ctrl_set_unpack_xyz'
          endif
          read(cunit) (cbuff(ii), ii=1,cbuffindex)
        endif
#endif

        cbuffindex = 0
        DO j=1,Ny
          DO i=1,Nx
            if (msk2d_buf_glo(i,j) .ne. 0. ) then
               cbuffindex = cbuffindex + 1
               fld2d_buf_glo(i,j) = cbuff(cbuffindex)
            endif
          ENDDO
        ENDDO

        ENDIF
        _END_MASTER( mythid )
        _BARRIER

        CALL BAR2( myThid )
        CALL SCATTER_2D_R8( fld2d_buf_glo, fld2d_buf,
     &                       Nx,Ny,.FALSE.,.TRUE.,myThid)
        CALL BAR2( myThid )
        CALL MDS_PASS_R8toRL( fld2d_buf, fld3dNodim,
     &                 0, 0, 1, k, Nr, 0, 0, .TRUE., myThid )

      enddo !do k = 1, Nr

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
#ifdef ALLOW_ADMTLM
            nveccount = nveccount + 1
            fld3dNodim(i,j,k,bi,bj)=phtmpadmtlm(nveccount)
#endif
            IF ( .NOT.doPackOld ) THEN
            fld3dDim(i,j,k,bi,bj)=fld3dNodim(i,j,k,bi,bj)
            ELSE !IF ( .NOT.doPackOld ) THEN
# ifndef ALLOW_NONDIMENSIONAL_CONTROL_IO
            fld3dDim(i,j,k,bi,bj) = fld3dNodim(i,j,k,bi,bj)
# else
            if (lxxadxx) then
               fld3dDim(i,j,k,bi,bj) =
     &              fld3dNodim(i,j,k,bi,bj) * delZnorm
#  ifdef CTRL_UNPACK_PRECISE
     &              / sqrt(wei3d(i,j,k,bi,bj))
#  else
     &              / sqrt(weightfld(k,bi,bj))
#  endif
            else
               fld3dDim(i,j,k,bi,bj) =
     &              fld3dNodim(i,j,k,bi,bj) / delZnorm
#  ifdef CTRL_UNPACK_PRECISE
     &              * sqrt(wei3d(i,j,k,bi,bj))
#  else
     &              * sqrt(weightfld(k,bi,bj))
#  endif
            endif
# endif /* ALLOW_NONDIMENSIONAL_CONTROL_IO */
            ENDIF !IF ( .NOT.doPackOld ) THEN
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

c-- 2.4:
      call WRITE_REC_3D_RL( fname, ctrlprec,
     &        Nr, fld3dDim, irec, 0, mythid)

      enddo !do irec = 1, ncvarrecs(ivartype)

# endif /* ALLOW_PACKUNPACK_METHOD2 */
# endif /* EXCLUDE_CTRL_PACK */

      return
      end
