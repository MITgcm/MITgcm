C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_unpack.F,v 1.1 2001/03/25 22:33:55 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_Unpack(
     I                        myiter,
     I                        mytime,
     I                        mythid
     &                      )

c     ==================================================================
c     SUBROUTINE ctrl_Unpack
c     ==================================================================
c
c     o Unpack the control vector such that the land points are filled
c       in.
c
c     started: Christian Eckert eckert@mit.edu  10-Mar-2000
c
c     changed: Patrick Heimbach heimbach@mit.edu 06-Jun-2000
c              - Transferred some filename declarations
c                from here to namelist in ctrl_init
c
c              Patrick Heimbach heimbach@mit.edu 16-Jun-2000
c              - single file name convention with or without
c                ALLOW_ECCO_OPTIMIZATION
C
c              Armin Koehl akoehl@ucsd.edu 05-Dec-2000
c              - single processor reads global parameter file
c               and writes multiple xx* and adxx* files
c
c     ==================================================================
c     SUBROUTINE ctrl_Unpack
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ctrl.h"
#include "cost.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

c     == local variables ==

      integer bi,bj
      integer ip,jp
      integer i,j,k
      integer ii
      integer il
      integer irec
      integer ig,jg
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer nrec_hfl
      integer nrec_sfl
      integer nrec_ust
      integer nrec_vst

      integer cbuffindex
      integer cunit
      _RL     cbuff( snx*nsx*npx*sny*nsy*npy )
      _RL     globfld3d( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globfld2d( snx,nsx,npx,sny,nsy,npy )
      _RL     globmsk( snx,nsx,npx,sny,nsy,npy,nr )
      character*(128) cfile

      logical first
      
      integer        filenvartype
      integer        filenvarlength
      character*(10) fileExpId
      integer        fileOptimCycle
      integer        filecindex
      _RL            fileDummy
      _RL            dummy
      integer        fileIg
      integer        fileJg
      integer        fileI
      integer        fileJ
      integer        filensx
      integer        filensy
      integer        filek
      integer        filenWetcTile(nsx,nsy,nr)
      integer        filenWetsTile(nsx,nsy,nr)
      integer        filenWetwTile(nsx,nsy,nr)
      integer        filencvarindex(maxcvars)
      integer        filencvarrecs(maxcvars)
      integer        filencvarxmax(maxcvars)
      integer        filencvarymax(maxcvars)
      integer        filencvarnrmax(maxcvars)
      character*( 1) filencvargrd(maxcvars)

      character*( 80)   fname
      character*( 80) adfname

      integer prec

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      prec = precFloat64
      dummy = 0.

      jtlo = 1
      jthi = nsy
      itlo = 1
      ithi = nsx
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c     Initialise temporary files
      do k = 1,nr
         do jp = 1,nPy
            do bj = jtlo,jthi
               do j = jmin,jmax
                  do ip = 1,nPx
                     do bi = itlo,ithi
                        do i = imin,imax
                           globfld3d(i,bi,ip,j,bj,jp,k) = 0.0
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo
      do jp = 1,nPy
         do bj = jtlo,jthi
            do j = jmin,jmax
               do ip = 1,nPx
                  do bi = itlo,ithi
                     do i = imin,imax
                        globfld2d(i,bi,ip,j,bj,jp) = 0.0
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo

c--     Only the master thread will do I/O.
        _BEGIN_MASTER( mythid )

        jG = 1 + (myygloballo - 1)/sny
        iG = 1 + (myxgloballo - 1)/snx

        call active_write_xyz( 'hFacC' , hFacC,  1, 0, mythid, dummy)
        call active_write_xyz( 'maskW' , maskW,  1, 0, mythid, dummy)
        call active_write_xyz( 'maskS' , maskS,  1, 0, mythid, dummy)

#ifdef ALLOW_THETA0_CONTROL
        il=ilnblnk( xx_theta_file )
        write(  fname(1:80),'(80a)') ' '
        write(adfname(1:80),'(80a)') ' '
        write(fname(1:80),'(2a,i10.10)')
     &       xx_theta_file(1:il),'.',optimcycle
        write(adfname(1:80),'(3a,i10.10)')
     &       yadmark,xx_theta_file(1:il),'.',optimcycle
        call MDSWRITEFIELD_3D_GL( adfname, 
     &                         prec, 'RL', Nr, globfld3d, 
     &                         1, optimcycle, mythid)
        call MDSWRITEFIELD_3D_GL( fname, 
     &                         prec, 'RL', Nr, globfld3d, 
     &                         1, optimcycle, mythid)
#endif /* ALLOW_THETA0_CONTROL */

#ifdef ALLOW_SALT0_CONTROL
        il=ilnblnk( xx_salt_file )
        write(  fname(1:80),'(80a)') ' '
        write(adfname(1:80),'(80a)') ' '
        write(fname(1:80),'(2a,i10.10)')
     &       xx_salt_file(1:il),'.',optimcycle
        write(adfname(1:80),'(3a,i10.10)')
     &       yadmark,xx_salt_file(1:il),'.',optimcycle
        call MDSWRITEFIELD_3D_GL( adfname, 
     &                         prec, 'RL', Nr, globfld3d, 
     &                         1, optimcycle, mythid)
        call MDSWRITEFIELD_3D_GL( fname, 
     &                         prec, 'RL', Nr, globfld3d, 
     &                         1,optimcycle,  mythid)
#endif /* ALLOW_SALT0_CONTROL */

#ifdef ALLOW_HFLUX0_CONTROL
        il=ilnblnk( xx_hflux_file )
        write(  fname(1:80),'(80a)') ' '
        write(adfname(1:80),'(80a)') ' '
        write(fname(1:80),'(2a,i10.10)')
     &       xx_hflux_file(1:il),'.',optimcycle
        write(adfname(1:80),'(3a,i10.10)')
     &       yadmark,xx_hflux_file(1:il),'.',optimcycle
        call MDSWRITEFIELD_2D_GL( adfname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1, optimcycle, mythid)
        call MDSWRITEFIELD_2D_GL( fname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1,optimcycle,  mythid)
#endif /* ALLOW_HFLUX0_CONTROL */

#ifdef ALLOW_SFLUX0_CONTROL
        il=ilnblnk( xx_sflux_file )
        write(  fname(1:80),'(80a)') ' '
        write(adfname(1:80),'(80a)') ' '
        write(fname(1:80),'(2a,i10.10)')
     &       xx_sflux_file(1:il),'.',optimcycle
        write(adfname(1:80),'(3a,i10.10)')
     &       yadmark,xx_sflux_file(1:il),'.',optimcycle
        call MDSWRITEFIELD_2D_GL( adfname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1, optimcycle, mythid)
        call MDSWRITEFIELD_2D_GL( fname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1,optimcycle,  mythid)
#endif /* ALLOW_SFLUX0_CONTROL */

#ifdef ALLOW_TAUU0_CONTROL
        il=ilnblnk( xx_tauu_file )
        write(  fname(1:80),'(80a)') ' '
        write(adfname(1:80),'(80a)') ' '
        write(fname(1:80),'(2a,i10.10)')
     &       xx_tauu_file(1:il),'.',optimcycle
        write(adfname(1:80),'(3a,i10.10)')
     &       yadmark,xx_tauu_file(1:il),'.',optimcycle
        call MDSWRITEFIELD_2D_GL( adfname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1, optimcycle, mythid)
        call MDSWRITEFIELD_2D_GL( fname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1,optimcycle,  mythid)
#endif /* ALLOW_TAUU0_CONTROL */

#ifdef ALLOW_TAUV0_CONTROL
        il=ilnblnk( xx_tauv_file )
        write(  fname(1:80),'(80a)') ' '
        write(adfname(1:80),'(80a)') ' '
        write(fname(1:80),'(2a,i10.10)')
     &       xx_tauv_file(1:il),'.',optimcycle
        write(adfname(1:80),'(3a,i10.10)')
     &       yadmark,xx_tauv_file(1:il),'.',optimcycle
        call MDSWRITEFIELD_2D_GL( adfname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1, optimcycle, mythid)
        call MDSWRITEFIELD_2D_GL( fname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1,optimcycle,  mythid)
#endif /* ALLOW_TAUV0_CONTROL */

#ifdef ALLOW_SSS0_CONTROL
        il=ilnblnk( xx_sss_file )
        write(  fname(1:80),'(80a)') ' '
        write(adfname(1:80),'(80a)') ' '
        write(fname(1:80),'(2a,i10.10)')
     &       xx_sss_file(1:il),'.',optimcycle
        write(adfname(1:80),'(3a,i10.10)')
     &       yadmark,xx_sss_file(1:il),'.',optimcycle
        call MDSWRITEFIELD_2D_GL( adfname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1, optimcycle, mythid)
        call MDSWRITEFIELD_2D_GL( fname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1,optimcycle,  mythid)
#endif /* ALLOW_SSS0_CONTROL */

#ifdef ALLOW_SST0_CONTROL
        il=ilnblnk( xx_sst_file )
        write(  fname(1:80),'(80a)') ' '
        write(adfname(1:80),'(80a)') ' '
        write(fname(1:80),'(2a,i10.10)')
     &       xx_sst_file(1:il),'.',optimcycle
        write(adfname(1:80),'(3a,i10.10)')
     &       yadmark,xx_sst_file(1:il),'.',optimcycle
        call MDSWRITEFIELD_2D_GL( adfname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1, optimcycle, mythid)
        call MDSWRITEFIELD_2D_GL( fname, 
     &                         prec, 'RL', 1, globfld2d, 
     &                         1,optimcycle,  mythid)
#endif /* ALLOW_SST0_CONTROL */

#ifdef ALLOW_DIFFKR_CONTROL
        il=ilnblnk( xx_diffkr_file )
        write(  fname(1:80),'(80a)') ' '
        write(adfname(1:80),'(80a)') ' '
        write(fname(1:80),'(2a,i10.10)')
     &       xx_diffkr_file(1:il),'.',optimcycle
        write(adfname(1:80),'(3a,i10.10)')
     &       yadmark,xx_diffkr_file(1:il),'.',optimcycle
        call MDSWRITEFIELD_3D_GL( adfname, 
     &                         prec, 'RL', Nr, globfld3d, 
     &                         1, optimcycle, mythid)
        call MDSWRITEFIELD_3D_GL( fname, 
     &                         prec, 'RL', Nr, globfld3d, 
     &                         1,optimcycle,  mythid)
#endif /* ALLOW_DIFFKR_CONTROL */

#ifdef ALLOW_KAPGM_CONTROL
        il=ilnblnk( xx_kapgm_file )
        write(  fname(1:80),'(80a)') ' '
        write(adfname(1:80),'(80a)') ' '
        write(fname(1:80),'(2a,i10.10)')
     &       xx_kapgm_file(1:il),'.',optimcycle
        write(adfname(1:80),'(3a,i10.10)')
     &       yadmark,xx_kapgm_file(1:il),'.',optimcycle
        call MDSWRITEFIELD_3D_GL( adfname, 
     &                         prec, 'RL', Nr, globfld3d, 
     &                         1, optimcycle, mythid)
        call MDSWRITEFIELD_3D_GL( fname, 
     &                         prec, 'RL', Nr, globfld3d, 
     &                         1,optimcycle,  mythid)
#endif /* ALLOW_KAPGM_CONTROL */


        _END_MASTER( mythid )

      end

