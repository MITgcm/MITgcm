C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_pack.F,v 1.4 2001/09/28 15:15:55 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"

CBOP
C     !ROUTINE: ctrl_pack
C     !INTERFACE:
      subroutine ctrl_pack( myiter, mytime, mythid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE ctrl_pack
c     | Pack the control vector
c     | * All control variable and adjoint variable fields are
c     |   read from disk.
c     | * Wet points are extracted, and elements are
c     |   normalized (optional)
c     | * A single control vector containing only (normalized
c     |   wet points is written to file.
c     *=================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ctrl.h"
#include "cost.h"
#include "optim.h"

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer myiter
      _RL     mytime
      integer mythid

C     !LOCAL VARIABLES:
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
      integer cunit
      integer prec

      logical doglobalread
      logical ladinit

      _RL     cbuff( snx*nsx*npx*sny*nsy*npy )
      _RL     globfld3d( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globfld2d( snx,nsx,npx,sny,nsy,npy )
      _RL     globmsk( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     tmpvar

      character*(128) cfile
      character*( 80) fname

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP

      prec           = precFloat64
      tmpvar         = -9999. _d 0

      jtlo = 1
      jthi = nsy
      itlo = 1
      ithi = nsx
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c--   Tiled files are used.
      doglobalread = .false.

c--   Initialise adjoint variables on active files.
      ladinit = .false.

c
c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

c--   read global mask file
          call MDSREADFIELD_3D_GL( "hFacC", 
     &                           prec, 'RL', Nr, globmsk,
     &                           1,  mythid)


c     >>> Write control vector <<<

          call mdsfindunit( cunit, mythid )
          write(cfile(1:128),'(2a,i4.4)')
     &      ctrlname(1:9),'.opt',
     &      optimcycle

          open( cunit, file   = cfile,
     &                 status = 'unknown',
     &                 form   = 'unformatted',
     &                 access = 'sequential'   )

c--       Header information.
          
          write(cunit) nvartype
          write(cunit) nvarlength
          write(cunit) expId
          write(cunit) optimCycle
          write(cunit) tmpvar
          write(cunit) 1
          write(cunit) 1
          write(cunit) 1
          write(cunit) 1
          write(cunit) (nWetcTile(1,1,k), k=1,nr)
          write(cunit) (nWetsTile(1,1,k), k=1,nr)
          write(cunit) (nWetwTile(1,1,k), k=1,nr)
          write(cunit) (ncvarindex(i), i=1,maxcvars)
          write(cunit) (ncvarrecs(i),  i=1,maxcvars)
          write(cunit) (nx,  i=1,maxcvars)
          write(cunit) (ny,  i=1,maxcvars)
          write(cunit) (ncvarnrmax(i), i=1,maxcvars)
          write(cunit) (ncvargrd(i),   i=1,maxcvars)
          write(cunit)

#ifdef ALLOW_THETA0_CONTROL

          il=ilnblnk( xx_theta_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(2a,i10.10)')
     &         xx_theta_file(1:il),'.',optimcycle
          call MDSREADFIELD_3D_GL( fname, 
     &                          prec, 'RL', Nr, globfld3d, 
     &                          1,  mythid)

          write(cunit) ncvarindex(1)
          write(cunit) 1
          write(cunit) 1
          do k = 1,nr
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(wtheta(k,bi,bj))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo

#endif

#ifdef ALLOW_SALT0_CONTROL

          il=ilnblnk( xx_salt_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(2a,i10.10)')
     &         xx_salt_file(1:il),'.',optimcycle
          call MDSREADFIELD_3D_GL( fname, 
     &                          prec, 'RL', Nr, globfld3d, 
     &                          1,  mythid)

          write(cunit) ncvarindex(2)
          write(cunit) 1
          write(cunit) 1
          do k = 1,nr
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(wsalt(k,bi,bj))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo
            
#endif

#ifdef ALLOW_TR10_CONTROL

          il=ilnblnk( xx_tr1_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(2a,i10.10)')
     &         xx_tr1_file(1:il),'.',optimcycle
          call MDSREADFIELD_3D_GL( fname, 
     &                          prec, 'RL', Nr, globfld3d, 
     &                          1,  mythid)

          write(cunit) ncvarindex(9)
          write(cunit) 1
          write(cunit) 1
          do k = 1,nr
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
cph     &                      * sqrt(wtr1(k,bi,bj))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo
            
#endif

#ifdef ALLOW_HFLUX0_CONTROL

          il=ilnblnk( xx_hflux_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(2a,i10.10)')
     &         xx_hflux_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "whflux", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(3)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_SFLUX0_CONTROL

          il=ilnblnk( xx_sflux_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(2a,i10.10)')
     &         xx_sflux_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "wsflux", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(4)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_TAUU0_CONTROL

          il=ilnblnk( xx_tauu_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(2a,i10.10)')
     &         xx_tauu_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "wtauu", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(5)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_TAUV0_CONTROL

          il=ilnblnk( xx_tauv_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(2a,i10.10)')
     &         xx_tauv_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "wtauv", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(6)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_SST0_CONTROL

          il=ilnblnk( xx_sst_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(2a,i10.10)')
     &         xx_sst_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "wsst", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(7)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_SSS0_CONTROL

          il=ilnblnk( xx_sss_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(2a,i10.10)')
     &         xx_sss_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "wsss", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(8)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_DIFFKR_CONTROL

          il=ilnblnk( xx_diffkr_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(2a,i10.10)')
     &         xx_diffkr_file(1:il),'.',optimcycle
          call MDSREADFIELD_3D_GL( fname, 
     &                          prec, 'RL', Nr, globfld3d, 
     &                          1,  mythid)

          write(cunit) ncvarindex(15)
          write(cunit) 1
          write(cunit) 1
          do k = 1,nr
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
cph     &                      * sqrt(wdiffkr(k,bi,bj))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo
            
#endif

#ifdef ALLOW_KAPGM_CONTROL

          il=ilnblnk( xx_kapgm_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(2a,i10.10)')
     &         xx_kapgm_file(1:il),'.',optimcycle
          call MDSREADFIELD_3D_GL( fname, 
     &                          prec, 'RL', Nr, globfld3d, 
     &                          1,  mythid)

          write(cunit) ncvarindex(16)
          write(cunit) 1
          write(cunit) 1
          do k = 1,nr
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
cph     &                      * sqrt(wkapgm(k,bi,bj))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo
            
#endif

          close ( cunit )

        _END_MASTER( mythid )

c======================================================================

c--   read global mask file
          call MDSREADFIELD_3D_GL( "hFacC", 
     &                           prec, 'RL', Nr, globmsk,
     &                           1,  mythid)

c     >>> Write gradient vector <<<

          call mdsfindunit( cunit, mythid )
          write(cfile(1:128),'(2a,i4.4)')
     &    costname(1:9),'.opt',
     &    optimcycle

          open( cunit, file   = cfile,
     &               status = 'unknown',
     &               form   = 'unformatted',
     &               access  = 'sequential'   )

c--       Header information.
          write(cunit) nvartype
          write(cunit) nvarlength
          write(cunit) expId
          write(cunit) optimCycle
          write(cunit) fc
          write(cunit) 1
          write(cunit) 1
          write(cunit) 1
          write(cunit) 1
          write(cunit) (nWetcTile(1,1,k), k=1,nr)
          write(cunit) (nWetsTile(1,1,k), k=1,nr)
          write(cunit) (nWetwTile(1,1,k), k=1,nr)
          write(cunit) (ncvarindex(i), i=1,maxcvars)
          write(cunit) (ncvarrecs(i),  i=1,maxcvars)
          write(cunit) (nx,  i=1,maxcvars)
          write(cunit) (ny,  i=1,maxcvars)
          write(cunit) (ncvarnrmax(i), i=1,maxcvars)
          write(cunit) (ncvargrd(i),   i=1,maxcvars)
          write(cunit)

#ifdef ALLOW_THETA0_CONTROL

          il=ilnblnk( xx_theta_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(3a,i10.10)')
     &         yadmark,xx_theta_file(1:il),'.',optimcycle

          call MDSREADFIELD_3D_GL( fname, 
     &                          prec, 'RL', Nr, 
     &                          globfld3d, 
     &                          1,  mythid)
          
          write(cunit) ncvarindex(1)
          write(cunit) 1
          write(cunit) 1
          do k = 1,nr
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(wtheta(k,bi,bj))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo

#endif

#ifdef ALLOW_SALT0_CONTROL

          il=ilnblnk( xx_salt_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(3a,i10.10)')
     &         yadmark,xx_salt_file(1:il),'.',optimcycle

          call MDSREADFIELD_3D_GL( fname, 
     &                          prec, 'RL', Nr, 
     &                          globfld3d, 
     &                          1,  mythid)
          
          write(cunit) ncvarindex(2)
          write(cunit) 1
          write(cunit) 1
          do k = 1,nr
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(wsalt(k,bi,bj))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo

#endif

#ifdef ALLOW_TR10_CONTROL

          il=ilnblnk( xx_tr1_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(3a,i10.10)')
     &         yadmark,xx_tr1_file(1:il),'.',optimcycle

          call MDSREADFIELD_3D_GL( fname, 
     &                          prec, 'RL', Nr, 
     &                          globfld3d, 
     &                          1,  mythid)
          
          write(cunit) ncvarindex(9)
          write(cunit) 1
          write(cunit) 1
          do k = 1,nr
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
cph     &                      * sqrt(wtr1(k,bi,bj))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo

#endif

#ifdef ALLOW_HFLUX0_CONTROL

          il=ilnblnk( xx_hflux_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(3a,i10.10)')
     &         yadmark,xx_hflux_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "whflux", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(3)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_SFLUX0_CONTROL

          il=ilnblnk( xx_sflux_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(3a,i10.10)')
     &         yadmark,xx_sflux_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "wsflux", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(4)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_TAUU0_CONTROL

          il=ilnblnk( xx_tauu_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(3a,i10.10)')
     &         yadmark,xx_tauu_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "wtauu", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(5)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_TAUV0_CONTROL

          il=ilnblnk( xx_tauv_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(3a,i10.10)')
     &         yadmark,xx_tauv_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "wtauv", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(6)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_SST0_CONTROL

          il=ilnblnk( xx_sst_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(3a,i10.10)')
     &         yadmark,xx_sst_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "wsst", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(7)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_SSS0_CONTROL

          il=ilnblnk( xx_sss_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(3a,i10.10)')
     &         yadmark,xx_sss_file(1:il),'.',optimcycle
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
          call MDSREADFIELD_2D_GL( "wsss", 
     &                          prec, 'RL', 1, 
     &                          globfld2d,
     &                          1,  mythid)
#endif
          call MDSREADFIELD_2D_GL( fname, 
     &                          prec, 'RL', 1, 
     &                          globfld3d(1,1,1,1,1,1,1), 
     &                          1,  mythid)

          write(cunit) ncvarindex(8)
          write(cunit) 1
          write(cunit) 1
          k = 1
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
     &                      * sqrt(globfld2d(i,bi,ip,j,bj,jp))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif

#endif

#ifdef ALLOW_DIFFKR_CONTROL

          il=ilnblnk( xx_diffkr_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(3a,i10.10)')
     &         yadmark,xx_diffkr_file(1:il),'.',optimcycle

          call MDSREADFIELD_3D_GL( fname, 
     &                          prec, 'RL', Nr, 
     &                          globfld3d, 
     &                          1,  mythid)
          
          write(cunit) ncvarindex(9)
          write(cunit) 1
          write(cunit) 1
          do k = 1,nr
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
cph     &                      * sqrt(wdiffkr(k,bi,bj))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo

#endif

#ifdef ALLOW_KAPGM_CONTROL

          il=ilnblnk( xx_kapgm_file)
          write(fname(1:80),'(80a)') ' '
          write(fname(1:80),'(3a,i10.10)')
     &         yadmark,xx_kapgm_file(1:il),'.',optimcycle

          call MDSREADFIELD_3D_GL( fname, 
     &                          prec, 'RL', Nr, 
     &                          globfld3d, 
     &                          1,  mythid)
          
          write(cunit) ncvarindex(9)
          write(cunit) 1
          write(cunit) 1
          do k = 1,nr
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
                       cbuff(cbuffindex) = globfld3d(i,bi,ip,j,bj,jp,k)
cph     &                      * sqrt(wkapgm(k,bi,bj))
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
c     --> check cbuffindex.
            if ( cbuffindex .gt. 0) then
               write(cunit) cbuffindex
               write(cunit) k
               write(cunit) (cbuff(ii), ii=1,cbuffindex)
            endif
         enddo

#endif


          close ( cunit )

      return
      end

