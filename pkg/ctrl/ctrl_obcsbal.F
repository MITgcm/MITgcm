C $Header: /u/gcmpack/MITgcm/pkg/ctrl/Attic/ctrl_obcsbal.F,v 1.7 2011/03/10 09:14:20 mlosch Exp $
C $Name:  $

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

      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      _RL volflux
      _RL area
      _RL obcsnfac

      logical obcsnfirst, obcsnchanged
      integer obcsncount0, obcsncount1

#ifdef ECCO_VERBOSE
      integer nrec
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

cgg   Assume the number of records is the same for
cgg   all boundaries. Needs to be improved someday.

#if   (defined (ALLOW_OBCS_CONTROL) || \
       defined (ALLOW_OBCS_COST_CONTRIBUTION))

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

CML   I think that this call of ctrl_getrec was forgotten. I also think
CML   that this code has not been use in ages, so we do not know what it
CML   actually does, and if it does it correcly.
CMLc--   Get the counters, flags, and the interpolation factor.
CML      call ctrl_GetRec( 'xx_obcsn',
CML     O                   obcsnfac, obcsnfirst, obcsnchanged,
CML     O                   obcsncount0,obcsncount1,
CML     I                   mytime, myiter, mythid )
c--   Get the counters, flags, and the interpolation factor.
      call ctrl_get_gen_rec(
     I                   xx_obcsnstartdate, xx_obcsnperiod,
     O                   obcsnfac, obcsnfirst, obcsnchanged,
     O                   obcsncount0,obcsncount1,
     I                   mytime, myiter, mythid )

c--   Loop over records. For north boundary, we only need V velocity.

      if ( obcsnfirst ) then

        shiftvel(1) = 0. d0
        shiftvel(2) = 0. d0

        call ctrl_volflux( obcsncount0, area, volflux, mythid)

c--   Do the global summation.
        _GLOBAL_SUM_RL( volflux, mythid )
        _GLOBAL_SUM_RL( area,mythid )

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
        _GLOBAL_SUM_RL( volflux, mythid )
        _GLOBAL_SUM_RL( area,mythid )

        shiftvel(2) = volflux /area
        print*,'volflux,area',volflux,area

      endif
cgg    End of the obcsnfirst, obcsnchanged loop.

#endif

#endif /* BALANCE_CONTROL_VOLFLUX_GLOBAL */

      return
      end







