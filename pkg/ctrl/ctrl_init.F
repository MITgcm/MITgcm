#include "CTRL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      subroutine ctrl_init( myThid )

c     ==================================================================
c     SUBROUTINE ctrl_init
c     ==================================================================
c
c     o The vector of control variables is defined here.
c
c     ==================================================================
c     SUBROUTINE ctrl_init
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "CTRL_GENARR.h"
# include "CTRL_OBCS.h"
#endif
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_DIC_CONTROL
# include "DIC_CTRL.h"
#endif

c     == routine arguments ==

      integer myThid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer ivar

      _RL dummy
      _RL loctmp3d (1-olx:snx+olx,1-oly:sny+oly,Nr,nsx,nsy)

#ifdef ALLOW_OBCS_CONTROL_MODES
      INTEGER  length_of_rec,dUnit
      INTEGER  MDS_RECLEN
      EXTERNAL MDS_RECLEN
#endif

#if ( defined ALLOW_GENTIM2D_CONTROL || \
      defined ALLOW_GENARR2D_CONTROL || \
      defined ALLOW_GENARR3D_CONTROL )
      INTEGER iarr
      CHARACTER*(1) ncvargrdtmp
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      CHARACTER*(MAX_LEN_FNAM) fnamegen
      INTEGER ilgen, k2, diffrecFull, endrecFull
      INTEGER diffrec, startrec, endrec
#elif ( defined ALLOW_OBCS_CONTROL )
      INTEGER diffrec, startrec, endrec
#endif

c     == external ==
#ifdef ALLOW_GENTIM2D_CONTROL
      integer  ilnblnk
      external ilnblnk
#endif

c     == end of interface ==

c--     Set default values.
      do ivar = 1,maxcvars
       ncvarindex(ivar) = -1
       ncvarrecs(ivar)  =  0
       ncvarxmax(ivar)  =  0
       ncvarymax(ivar)  =  0
       ncvarnrmax(ivar) =  0
       ncvargrd(ivar)   = '?'
      enddo

c     Set unit weight to 1
c

      do bj=1,nSy
         do bi=1,nSx
            do k=1,Nr
             wunit(k,bi,bj) = 1. _d 0
             do j=1-oly,sNy+oly
              do i=1-olx,sNx+olx
               loctmp3d(i,j,k,bi,bj) = 1. _d 0
              enddo
             enddo
            enddo
         enddo
      enddo

#ifdef ALLOW_AUTODIFF
      call active_write_xyz( 'wunit', loctmp3d, 1, 0, mythid, dummy)
#else
      CALL WRITE_REC_XYZ_RL( 'wunit', loctmp3d, 1, 1, myThid )
#endif

      _BARRIER

c--   ===========================
c--   Open boundary contributions.
c--   ===========================

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSN_CONTROL
c--   Northern obc.
      call ctrl_init_rec ( xx_obcsn_file,
     I     xx_obcsnstartdate1, xx_obcsnstartdate2, xx_obcsnperiod, 4,
     O     xx_obcsnstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_obcsn_file, 11, 111, diffrec, startrec, endrec,
     &     snx, 1, nr, 'm', 'xz', myThid )
#endif /* ALLOW_OBCSN_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSS_CONTROL
c--   Southern obc.
      call ctrl_init_rec ( xx_obcss_file,
     I     xx_obcssstartdate1, xx_obcssstartdate2, xx_obcssperiod, 4,
     O     xx_obcssstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_obcss_file, 12, 112, diffrec, startrec, endrec,
     &     snx, 1, nr, 'm', 'xz', myThid )
#endif /* ALLOW_OBCSS_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSW_CONTROL
c--   Western obc.
      call ctrl_init_rec ( xx_obcsw_file,
     I     xx_obcswstartdate1, xx_obcswstartdate2, xx_obcswperiod, 4,
     O     xx_obcswstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_obcsw_file, 13, 113, diffrec, startrec, endrec,
     &     1, sny, nr, 'm', 'yz', myThid )
#endif  /* ALLOW_OBCSW_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSE_CONTROL
c--   Eastern obc.
      call ctrl_init_rec ( xx_obcse_file,
     I     xx_obcsestartdate1, xx_obcsestartdate2, xx_obcseperiod, 4,
     O     xx_obcsestartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_obcse_file, 14, 114, diffrec, startrec, endrec,
     &     1, sny, nr, 'm', 'yz', myThid )
#endif /* ALLOW_OBCSE_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCS_CONTROL_MODES
cih  Get matrices for reconstruction from barotropic-barclinic modes
CMM  To use modes now hardcoded with ECCO_CPPOPTION.  Would be good to have
c     run-time option and also define filename=baro_invmodes.bin
        CALL MDSFINDUNIT( dUnit, myThid )
        length_of_rec = MDS_RECLEN( 64, NR*NR, myThid )
        open(dUnit, file='baro_invmodes.bin', status='old',
     &         access='direct', recl=length_of_rec )
        do j = 1,Nr
           read(dUnit,rec=j) ((modesv(k,i,j), k=1,Nr), i=1,Nr)
        end do
        CLOSE( dUnit )
CMM  double precision modesv is size [NR,NR,NR]
c     dim one is z-space
c     dim two is mode space
c     dim three is the total depth for which this set of modes applies
c     so for example modesv(:,2,nr) will be the second mode
c     in z-space for the full model depth
c    The modes are to be orthogonal when weighted by dz.
c     i.e. if f_i(z) = mode i, sum_j(f_i(z_j)*f_j(z_j)*dz_j = delta_ij
c    first mode should also be constant in depth...barotropic
c    For a matlab code example how to construct the orthonormal modes,
c     which are ideally the solution of planetary vertical mode equation
c     using model mean dRho/dz, see
c     MITgcm/verification/obcs_ctrl/input/gendata.m
c    This code is compatible with partial cells
#endif

c----------------------------------------------------------------------
c--
#ifdef ALLOW_GENARR2D_CONTROL
       do iarr = 1, maxCtrlArr2D
        ncvargrdtmp='c'
# ifdef ALLOW_SHELFICE
C       Under iceshelf, use maskSHI for these
        if ((xx_genarr2d_file(iarr)(1:11).eq.'xx_shicoeff').or.
     &      (xx_genarr2d_file(iarr)(1:11).eq.'xx_shicdrag')) then
            ncvargrdtmp='i'
        endif
# endif

# ifndef ALLOW_OPENAD
        if (xx_genarr2d_weight(iarr).NE.' ')
     &  call ctrl_init_ctrlvar (
# else
        call ctrl_init_ctrlvar (
# endif
     &       xx_genarr2d_file(iarr)(1:MAX_LEN_FNAM),
     &       100+iarr, 200+iarr, 1, 1, 1,
     &       snx, sny, 1, ncvargrdtmp, 'xy', myThid )

       enddo
#endif /* ALLOW_GENARR2D_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_GENARR3D_CONTROL
       do iarr = 1, maxCtrlArr3D
        ncvargrdtmp='c'
#ifndef ALLOW_OPENAD
        if (xx_genarr3d_weight(iarr).NE.' ')
     &  call ctrl_init_ctrlvar (
#else
        call ctrl_init_ctrlvar (
#endif
     &       xx_genarr3d_file(iarr)(1:MAX_LEN_FNAM),
     &       200+iarr, 300+iarr, 1, 1, 1,
     &       snx, sny, nr, ncvargrdtmp, '3d', myThid )
       enddo
#endif /* ALLOW_GENARR3D_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_GENTIM2D_CONTROL
       do iarr = 1, maxCtrlTim2D

#ifdef ALLOW_CAL
        if (xx_gentim2d_startdate1(iarr).EQ.0) then
          xx_gentim2d_startdate1(iarr)=startdate_1
          xx_gentim2d_startdate2(iarr)=startdate_2
        endif
#endif
        ncvargrdtmp='c'
# ifdef ALLOW_SHELFICE
C       Under iceshelf, use maskSHI for these
        if (xx_gentim2d_file(iarr)(1:11).eq.'xx_shifwflx')
     &       ncvargrdtmp='i'
# endif
        if (xx_gentim2d_file(iarr)(1:5).eq.'xx_fu')
     &       ncvargrdtmp='w'
        if (xx_gentim2d_file(iarr)(1:5).eq.'xx_fv')
     &       ncvargrdtmp='s'

        call ctrl_init_rec ( xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
     I       xx_gentim2d_startdate1(iarr),
     I       xx_gentim2d_startdate2(iarr),
     I       xx_gentim2d_period(iarr),
     I       1,
     O       xx_gentim2d_startdate(1,iarr),
     O       diffrec, startrec, endrec,
     I       myThid )
C

#ifndef ALLOW_OPENAD
        if (xx_gentim2d_weight(iarr).NE.' ') then
#endif
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'replicate')
     &   xx_gentim2d_preproc(k2,iarr)='docycle'
         if (xx_gentim2d_preproc(k2,iarr).EQ.'doglomean')
     &   xx_gentim2d_glosum(iarr)     = .TRUE.
         if (xx_gentim2d_preproc(k2,iarr).EQ.'documul')
     &   xx_gentim2d_cumsum(iarr)     = .TRUE.
        enddo
C
        diffrecFull=diffrec
        endrecFull=endrec
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'docycle') then
           if (xx_gentim2d_preproc_i(k2,iarr).NE.0) then
            diffrec=min(diffrec,xx_gentim2d_preproc_i(k2,iarr))
            endrec=min(endrec,xx_gentim2d_preproc_i(k2,iarr))
           endif
         endif
        enddo
C
        ilgen=ilnblnk( xx_gentim2d_file(iarr) )
        write(fnamegen(1:MAX_LEN_FNAM),'(2a)')
     &       xx_gentim2d_file(iarr)(1:ilgen),'.effective'
        call ctrl_init_ctrlvar (
     &       fnamegen(1:MAX_LEN_FNAM),
     &       300+iarr, 400+iarr,
     &       diffrecFull, startrec, endrecFull,
     &       snx, sny, 1, ncvargrdtmp, 'xy', myThid )
C
        ilgen=ilnblnk( xx_gentim2d_file(iarr) )
        write(fnamegen(1:MAX_LEN_FNAM),'(2a)')
     &       xx_gentim2d_file(iarr)(1:ilgen),'.tmp'
        call ctrl_init_ctrlvar (
     &       fnamegen(1:MAX_LEN_FNAM),
     &       300+iarr, 400+iarr,
     &       diffrecFull, startrec, endrecFull,
     &       snx, sny, 1, ncvargrdtmp, 'xy', myThid )
C
        call ctrl_init_ctrlvar (
     &       xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
     &       300+iarr, 400+iarr,
     &       diffrec, startrec, endrec,
     &       snx, sny, 1, ncvargrdtmp, 'xy', myThid )
C
#ifndef ALLOW_OPENAD
       endif
#endif
C
       enddo
#endif /* ALLOW_GENTIM2D_CONTROL */

c----------------------------------------------------------------------

      call ctrl_init_wet( myThid )

c----------------------------------------------------------------------

#ifdef ALLOW_DIC_CONTROL
      do i = 1, dic_n_control
       xx_dic(i) = 0. _d 0
      enddo
#endif

c----------------------------------------------------------------------

      _BARRIER

c--   Summarize the cost function setup.
      _BEGIN_MASTER( myThid )
      call ctrl_summary( myThid )
      _END_MASTER( myThid )

      return
      end
