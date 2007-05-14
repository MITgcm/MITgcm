C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_get_gen.F,v 1.12 2007/05/14 22:02:33 heimbach Exp $
C $Name:  $

#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_get_gen(
     I          xx_gen_file, xx_genstartdate, xx_genperiod,
     I          genmask, genfld, xx_gen0, xx_gen1, xx_gen_dummy, 
     I          xx_gen_remo_intercept, xx_gen_remo_slope,
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

#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
#endif

c     == routine arguments ==

      character*(MAX_LEN_FNAM) xx_gen_file
      integer xx_genstartdate(4)
      _RL     xx_genperiod
      _RL     genmask(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL     genfld(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     xx_gen0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     xx_gen1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     xx_gen_dummy
      _RL     xx_gen_remo_intercept
      _RL     xx_gen_remo_slope

      _RL     mytime
      integer myiter
      integer mythid

c     == local variables ==

#ifdef ALLOW_EXF

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

      if (optimcycle .ge. 0) then
        ilgen=ilnblnk( xx_gen_file )
        write(fnamegen(1:80),'(2a,i10.10)') 
     &       xx_gen_file(1:ilgen), '.', optimcycle
      endif

# ifdef ALLOW_CAL
         if ( xx_genperiod .EQ. 0 ) then
c     record numbers are assumed 1 to 12 corresponding to
c     Jan. through Dec.
            call cal_GetMonthsRec(
     O           genfac, genfirst, genchanged,
     O           gencount0, gencount1,
     I           mytime, myiter, mythid
     &           )
         else
c--   Get the counters, flags, and the interpolation factor.
      call ctrl_get_gen_rec(
     I                   xx_genstartdate, xx_genperiod,
     O                   genfac, genfirst, genchanged,
     O                   gencount0,gencount1,
     I                   mytime, myiter, mythid )
        endif
# else
c--   Get the counters, flags, and the interpolation factor.
      call ctrl_get_gen_rec(
     I                   xx_genstartdate, xx_genperiod,
     O                   genfac, genfirst, genchanged,
     O                   gencount0,gencount1,
     I                   mytime, myiter, mythid )
# endif

      if ( genfirst ) then
        call active_read_xy( fnamegen, xx_gen1, gencount0,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gen_dummy )
#ifdef ALLOW_CTRL_SMOOTH
	if ( xx_gen_file .EQ. xx_tauu_file .OR.
     &       xx_gen_file .EQ. xx_tauv_file )
     &     call ctrl_smooth(xx_gen1,genmask)
#endif
      endif

      if (( genfirst ) .or. ( genchanged )) then
        call exf_SwapFFields( xx_gen0, xx_gen1, mythid )

        call active_read_xy( fnamegen, xx_gen1 , gencount1,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gen_dummy )
#ifdef ALLOW_CTRL_SMOOTH
	if ( xx_gen_file .EQ. xx_tauu_file .OR.
     &       xx_gen_file .EQ. xx_tauv_file )
     &     call ctrl_smooth(xx_gen1,genmask)
#endif
      endif

c--   Add control to model variable.
cph(
cph this flag ported from the SIO code
cph Initial wind stress adjustments are too vigorous.
      if ( gencount0 .LE. 2 .AND. 
     &     ( xx_gen_file .EQ. xx_tauu_file .OR.
     &       xx_gen_file .EQ. xx_tauv_file ) .AND.
     &     ( xx_genperiod .NE. 0 ) ) then
         doCtrlUpdate = .FALSE.
      else
         doCtrlUpdate = .TRUE.
      endif
      if ( xx_gen_file .EQ. xx_tauu_file .OR.
     &     xx_gen_file .EQ. xx_tauv_file ) then
         gensign = -1.
      else
         gensign = 1.
      endif
c
cph since the above is ECCO specific, we undo it here:
cph      doCtrlUpdate = .TRUE.
c
      if ( doCtrlUpdate ) then
cph)
      do bj = jtlo,jthi
        do bi = itlo,ithi
c--       Calculate mask for tracer cells (0 => land, 1 => water).
          k = 1
          do j = 1,sny
            do i = 1,snx
              genfld(i,j,bi,bj) = genfld (i,j,bi,bj)
     &              + gensign*genfac            *xx_gen0(i,j,bi,bj)
     &              + gensign*(1. _d 0 - genfac)*xx_gen1(i,j,bi,bj)
              genfld(i,j,bi,bj) = 
     &             genmask(i,j,k,bi,bj)*( genfld (i,j,bi,bj) -
     &             ( xx_gen_remo_intercept +
     &               xx_gen_remo_slope*(mytime-starttime) ) )
            enddo
          enddo
        enddo
      enddo
cph(
      endif
cph)

#endif /* ALLOW_EXF */

      end

