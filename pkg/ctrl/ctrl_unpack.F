C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_unpack.F,v 1.4 2001/09/28 15:56:14 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"

CBOI
C
C !TITLE: CONTROL VECTOR
C !AUTHORS: mitgcm developers ( support@mitgcm.org )
C !AFFILIATION: Massachussetts Institute of Technology
C !DATE:
C !INTRODUCTION: control vector handling
C \bv
c The control vector package is connected to the differntiability
c of the code. Differentiability refers to computing the derivative
c of a cost function with respect to a set of control variables
c (initial state, boundary values, model parameters).
c To reduce memory, the control variables are kept on file and
c are only read/written, when needed.
c To achieve this, adjoint support routines are available which
c handle I/O of active variables.
c In addition, a single control vector is created, which contains
c all wet points of all control variables.
c
C     !CALLING SEQUENCE:
c    ...
c     |-- the_model_main
c         |
c         |-- initialise_fixed
c         |   |
c         |   |-- packages_readparms
c         |       |
c         |       |-- ctrl_init                - initialise control
c         |                                      package
c         |-- ctrl_unpack                      - unpack control vector
c         |
c         |-- adthe_main_loop                  - forward/adjoint run
c         |   |
c         |   |-- initialise_variables
c         |   |   |
c         |   |   |-- packages_init_variables
c         |   |       |
c         |   |       |-- ctrl_map_ini         - link init. state to
c         |   |                                  control variables
c         |   |-- ctrl_map_forcing             - link forcing fields to
c         |  ...                                 control variables
c         |
c         |-- ctrl_pack                        - pack control vector
C \ev
CEOI

CBOP
C     !ROUTINE: ctrl_unpack
C     !INTERFACE:
      subroutine ctrl_unpack( myiter, mytime, mythid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE ctrl_unpack
c     | Unpack the control vector
c     | * If in sensitivity study mode,
c     |   control variable and adjoint variable arrays and files
c     |   are initialised.
c     | * If in optimization mode,
c     |   wet points of updated control variables (i.e. anomalies 
c     |   to corresponding fields) from previous optimization
c     |   iteration are extracted from single control vector,
c     |   mapped onto full (2D or 3D) fields and distributed 
c     |   to control variable files.
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
      integer prec

      _RL     cbuff( snx*nsx*npx*sny*nsy*npy )
      _RL     globfld3d( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     globfld2d( snx,nsx,npx,sny,nsy,npy )
      _RL     globmsk( snx,nsx,npx,sny,nsy,npy,nr )
      _RL     fileDummy
      _RL     dummy

      logical first
      
      integer        filenvartype
      integer        filenvarlength
      character*(10) fileExpId
      integer        fileOptimCycle
      integer        filecindex
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
      character*(128) cfile

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP

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

#ifdef ALLOW_TR10_CONTROL
        il=ilnblnk( xx_tr1_file )
        write(  fname(1:80),'(80a)') ' '
        write(adfname(1:80),'(80a)') ' '
        write(fname(1:80),'(2a,i10.10)')
     &       xx_tr1_file(1:il),'.',optimcycle
        write(adfname(1:80),'(3a,i10.10)')
     &       yadmark,xx_tr1_file(1:il),'.',optimcycle
        call MDSWRITEFIELD_3D_GL( adfname, 
     &                         prec, 'RL', Nr, globfld3d, 
     &                         1, optimcycle, mythid)
        call MDSWRITEFIELD_3D_GL( fname, 
     &                         prec, 'RL', Nr, globfld3d, 
     &                         1,optimcycle,  mythid)
#endif /* ALLOW_TR10_CONTROL */

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

