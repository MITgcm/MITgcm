
#include "CTRL_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine ctrl_obcsbal(
     I                       mytime,
     I                       myiter,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE ctrl_obcsbal
c     ==================================================================
c
c     o volumetrically balance the control vector contribution. 
c     o Assume the calendar is identical
c       for all open boundaries. Need to save the barotropic adjustment
c       velocity so it can be used in all ctrl_getobcs files. 
c     o WARNING: eastern boundary (not defined) filenames have been a 
c                problem in the past.
c
c     - started G. Gebbie, MIT-WHOI, 15-June-2002
c     ==================================================================
c     SUBROUTINE ctrl_obcsvol
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_OBCS
# include "OBCS.h"
#endif

#include "ctrl.h"
#include "ctrl_dummy.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

#ifdef BALANCE_CONTROL_VOLFLUX_GLOBAL
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer il
      integer iobcs
      integer ip1
      integer jp1
      integer nrec
      integer ilfld
      integer igg

      _RL volflux
      _RL area
      _RL tmpflux
      _RL tmparea
      _RL dummy
      _RL gg
      _RL tmpx
      _RL tmpy
      _RL obcsnfac
      character*(80) fnamefldn
      character*(80) fnameflds
      character*(80) fnamefldw
      character*(80) fnameflde

      logical doglobalread
      logical ladinit
      logical obcsnfirst, obcsnchanged
      integer obcsncount0, obcsncount1

#ifdef ECCO_VERBOSE
      character*(MAX_LEN_MBUF) msgbuf
#endif

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

cgg   Assume the number of records is the same for
cgg   all boundaries. Needs to be improved someday.

#if   (defined (ALLOW_OBCS_CONTROL) || \
       defined (ALLOW_OBCS_COST_CONTRIBUTION))

      tmpflux= 0. d 0
      tmparea= 0. d 0
      area= 0. d 0
      volflux = 0. d 0

#ifdef ECCO_VERBOSE
      _BEGIN_MASTER( mythid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8)')
     &  ' ctrl_obcsvol: number of records to process: ',nrec
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

c--   Get the counters, flags, and the interpolation factor.
      call ctrl_GetRec( 'xx_obcsn',
     O                   obcsnfac, obcsnfirst, obcsnchanged,
     O                   obcsncount0,obcsncount1,
     I                   mytime, myiter, mythid )

c--   Loop over records. For north boundary, we only need V velocity.

      if ( obcsnfirst ) then

        shiftvel(1) = 0. d0
        shiftvel(2) = 0. d0

        call ctrl_volflux( obcsncount0, area, volflux, mythid)

c--   Do the global summation.
        _GLOBAL_SUM_R8( volflux, mythid )
        _GLOBAL_SUM_R8( area,mythid )

        shiftvel(2) = volflux / area
        print*,'volflux,area',volflux,area
      endif  
cgg    End of the obcsnfirst loop. 

      if ( ( obcsnfirst) .or. (obcsnchanged)) then

cgg     Swap the value.
        shiftvel(1) = shiftvel(2)
 
        volflux = 0. d0
        area= 0. d0

        call ctrl_volflux( obcsncount1, area, volflux, mythid)

c--   Do the global summation.
        _GLOBAL_SUM_R8( volflux, mythid )
        _GLOBAL_SUM_R8( area,mythid )

        shiftvel(2) = volflux /area
        print*,'volflux,area',volflux,area

      endif  
cgg    End of the obcsnfirst, obcsnchanged loop. 

#endif

#endif /* BALANCE_CONTROL_VOLFLUX_GLOBAL */

      return
      end







