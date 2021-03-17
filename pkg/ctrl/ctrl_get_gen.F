#include "CTRL_OPTIONS.h"

      subroutine ctrl_get_gen(
     I          xx_gen_file, xx_genstartdate, xx_genperiod,
     I          genmask, genfld, xx_gen0, xx_gen1, xx_gen_dummy,
     I          xx_gen_remo_intercept, xx_gen_remo_slope,
     I          genweight,
     I          mytime, myiter, mythid
     &                     )

c     ==================================================================
c     SUBROUTINE ctrl_get_gen
c     ==================================================================
c
c     o new generic routine for reading time dependent control variables
c       heimbach@mit.edu 12-Jun-2003
c
c     ==================================================================
c     SUBROUTINE ctrl_get_gen
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
#endif

c     == routine arguments ==

      character*(80) fnamegeneric
      character*(MAX_LEN_FNAM) xx_gen_file
      integer xx_genstartdate(4)
      _RL     xx_genperiod
      _RS     genmask(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL     genfld(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     xx_gen0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     xx_gen1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     xx_gen_dummy
      _RL     xx_gen_remo_intercept
      _RL     xx_gen_remo_slope
      _RL     genweight(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      _RL     mytime
      integer myiter
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer ilgen

      _RL     gensign
      _RL     genfac
      logical doCtrlUpdate
      logical genfirst
      logical genchanged
      integer gencount0
      integer gencount1

      logical doglobalread
      logical ladinit

      character*(80) fnamegen
      _RS mask2D(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#ifdef ALLOW_SMOOTH
#ifdef ALLOW_SMOOTH_CTRL2D
      _RS dummyRS(1)

#endif
#endif
#ifndef ECCO_CTRL_DEPRECATED
      character*(MAX_LEN_FNAM) xx_tauu_file
      character*(MAX_LEN_FNAM) xx_tauv_file
      character*(MAX_LEN_FNAM) xx_aqh_file
      character*(MAX_LEN_FNAM) xx_atemp_file
      character*(MAX_LEN_FNAM) xx_precip_file
      character*(MAX_LEN_FNAM) xx_lwdown_file
      character*(MAX_LEN_FNAM) xx_swdown_file
#endif

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

c--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

C --- Get the appropriate mask for this field
      call ctrl_get_mask2d( xx_gen_file, mask2D, myThid)

      if ( (optimcycle .ge. 0).AND.(.NOT.ctrlUseGen) ) then
        ilgen=ilnblnk( xx_gen_file )
        write(fnamegen(1:80),'(2a,i10.10)')
     &       xx_gen_file(1:ilgen), '.', optimcycle
      endif

      if ( (optimcycle .ge. 0).AND.ctrlUseGen ) then
        ilgen=ilnblnk( xx_gen_file )
        write(fnamegen(1:80),'(2a,i10.10)')
     &    xx_gen_file(1:ilgen),'.effective.',optimcycle
      endif

c--   Get the counters, flags, and the interpolation factor.
      call ctrl_get_gen_rec(
     I                       xx_genstartdate, xx_genperiod,
     O                       genfac, genfirst, genchanged,
     O                       gencount0,gencount1,
     I                       mytime, myiter, mythid )

      if ( genfirst ) then
cc#ifdef ALLOW_OPENAD
cc        call oad_active_read_xy( fnamegen, xx_gen1, gencount0,
cc     &                       doglobalread, ladinit, optimcycle,
cc     &                       mythid, xx_gen_dummy )
cc#else
#ifdef ALLOW_AUTODIFF
        call active_read_xy( fnamegen, xx_gen1, gencount0,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gen_dummy )
       if (.false.) then
        call active_read_xy( fnamegen, xx_gen0, gencount0,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gen_dummy )
       endif
#else
        CALL READ_REC_XY_RL( fnamegen, xx_gen1, gencount0, 1, myThid )
#endif
cc#endif /* ALLOW_OPENAD */

#ifdef ECCO_CTRL_DEPRECATED
#ifdef ALLOW_CTRL_SMOOTH
        if ( xx_gen_file .EQ. xx_tauu_file .OR.
     &       xx_gen_file .EQ. xx_tauv_file )
     &     call ctrl_smooth(xx_gen1,genmask,myThid)
#endif
#endif

#ifdef ALLOW_SMOOTH
#ifdef ALLOW_SMOOTH_CTRL2D
        if (useSMOOTH) call smooth2D(xx_gen1,mask2D,1,mythid)
        write(fnamegeneric(1:80),'(2a,i10.10)')
     &    xx_gen_file(1:ilgen),'.smooth.',optimcycle
        CALL MDS_WRITE_FIELD(fnamegeneric,ctrlprec,.FALSE.,.FALSE.,
     &   'RL',1,1,1,xx_gen1,dummyRS,gencount1,optimcycle,mythid)
#endif /* ALLOW_SMOOTH_CTRL2D */
#endif /* ALLOW_SMOOTH */

      endif

      if (( genfirst ) .or. ( genchanged )) then
        call CTRL_SWAPFFIELDS( xx_gen0, xx_gen1, mythid )

cc#ifdef ALLOW_OPENAD
cc        call oad_active_read_xy( fnamegen, xx_gen1 , gencount1,
cc     &                       doglobalread, ladinit, optimcycle,
cc     &                       mythid, xx_gen_dummy )
cc#else
#ifdef ALLOW_AUTODIFF
        call active_read_xy( fnamegen, xx_gen1 , gencount1,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gen_dummy )
#else
        CALL READ_REC_XY_RL( fnamegen, xx_gen1, gencount1, 1, myThid )
#endif
cc#endif /* ALLOW_OPENAD */
#ifdef ECCO_CTRL_DEPRECATED
#ifdef ALLOW_CTRL_SMOOTH
        if ( xx_gen_file .EQ. xx_tauu_file .OR.
     &       xx_gen_file .EQ. xx_tauv_file )
     &     call ctrl_smooth(xx_gen1,genmask,myThid)
#endif
#endif

#ifdef ALLOW_SMOOTH
#ifdef ALLOW_SMOOTH_CTRL2D
        if (useSMOOTH) call smooth2D(xx_gen1,mask2D,1,mythid)
        write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_gen_file(1:ilgen),'.smooth.',optimcycle
        CALL MDS_WRITE_FIELD(fnamegeneric,ctrlprec,.FALSE.,.FALSE.,
     &   'RL',1,1,1,xx_gen1,dummyRS,gencount0,optimcycle,mythid)
#endif /* ALLOW_SMOOTH_CTRL2D */
#endif /* ALLOW_SMOOTH */

      endif

c--   Add control to model variable.
cph(
cph this flag ported from the SIO code
cph Initial wind stress adjustments are too vigorous.

#ifndef ECCO_CTRL_DEPRECATED
      xx_tauu_file       = 'xx_tauu'
      xx_tauv_file       = 'xx_tauv'
      xx_aqh_file        = 'xx_aqh'
      xx_atemp_file      = 'xx_atemp'
      xx_precip_file     = 'xx_precip'
      xx_lwdown_file     = 'xx_lwdown'
      xx_swdown_file     = 'xx_swdown'
#endif

      if ( gencount0 .LE. 2 .AND. (
#ifdef CTRL_SKIP_FIRST_TWO_ATM_REC_ALL
     &       xx_gen_file(1:6) .EQ. xx_aqh_file  .OR.
     &       xx_gen_file(1:8) .EQ. xx_atemp_file .OR.
     &       xx_gen_file(1:9) .EQ. xx_precip_file .OR.
     &       xx_gen_file(1:9) .EQ. xx_lwdown_file .OR.
     &       xx_gen_file(1:9) .EQ. xx_swdown_file .OR.
#endif
     &       xx_gen_file(1:7) .EQ. xx_tauu_file .OR.
     &       xx_gen_file(1:7) .EQ. xx_tauv_file ) .AND.
     &     ( xx_genperiod .NE. zeroRL ) ) then
         doCtrlUpdate = .FALSE.
      else
         doCtrlUpdate = .TRUE.
      endif
      if ( xx_gen_file(1:7) .EQ. xx_tauu_file .OR.
     &     xx_gen_file(1:7) .EQ. xx_tauv_file ) then
         gensign = -1.
      else
         gensign = 1.
      endif

cph since the above is ECCO specific, we undo it here:
cph      doCtrlUpdate = .TRUE.
      if ( doCtrlUpdate ) then
cph)
CMLCML(
CMLCML   hack until if find something better for the masks
CML      if ( xx_gen_file(1:11) .eq. 'xx_shifwflx' ) then
CML      do bj = jtlo,jthi
CML        do bi = itlo,ithi
CMLc--       Calculate mask for tracer cells (0 => land, 1 => water).
CML          do j = 1,sny
CML            do i = 1,snx
CML              genfld(i,j,bi,bj) = genfld (i,j,bi,bj)
CML     &              + gensign*genfac            *xx_gen0(i,j,bi,bj)
CML     &              + gensign*(1. _d 0 - genfac)*xx_gen1(i,j,bi,bj)
CML              if ( ksurfc(i,j,bi,bj) .ne. nr+1 ) then
CML               genfld(i,j,bi,bj) =
CML     &             ( genfld (i,j,bi,bj) -
CML     &             ( xx_gen_remo_intercept +
CML     &               xx_gen_remo_slope*(mytime-starttime) ) )
CML              else
CML               genfld(i,j,bi,bj) = 0. _d 0
CML              endif
CML            enddo
CML          enddo
CML        enddo
CML      enddo
CML      else
CMLCML)
      do bj = jtlo,jthi
        do bi = itlo,ithi
c--       Calculate mask for tracer cells (0 => land, 1 => water).
          do j = 1,sny
            do i = 1,snx
              genfld(i,j,bi,bj) = genfld (i,j,bi,bj)
     &              + gensign*genfac            *xx_gen0(i,j,bi,bj)
     &              + gensign*(1. _d 0 - genfac)*xx_gen1(i,j,bi,bj)
              genfld(i,j,bi,bj) =
     &             mask2D(i,j,bi,bj)*( genfld (i,j,bi,bj) -
     &             ( xx_gen_remo_intercept +
     &               xx_gen_remo_slope*(mytime-starttime) ) )
            enddo
          enddo
        enddo
      enddo
CMLCML)
CML      endif
CMLCML)
cph(
      endif
cph)

      RETURN
      END
