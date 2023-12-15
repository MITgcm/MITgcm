#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: COST_OBCS
C     !INTERFACE:
      subroutine cost_obcs(
     I                     myiter,
     I                     mytime,
     I                     mythid
     &                   )
C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE cost_obcs
c     ==================================================================
c
c     o Evaluate cost function contributions for obcs
c
c     ==================================================================
c     SUBROUTINE cost_obcs
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"

#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL.h"
#endif
#ifdef ALLOW_OBCSE_CONTROL
# include "CTRL_OBCS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

#if ( defined ALLOW_CTRL && defined ALLOW_OBCS )

C     !LOCAL VARIABLES:
c     == local variables ==
      integer startrec
      integer endrec
      integer ivar
      integer iobcsn, iobcss, iobcse, iobcsw

c     == end of interface ==
CEOP

C     Find ctrl-indices
      iobcsn = 0
      iobcss = 0
      iobcse = 0
      iobcsw = 0
      DO ivar = 1, maxcvars
       IF ( ncvarfname(ivar) .EQ. xx_obcsn_file ) iobcsn = ivar
       IF ( ncvarfname(ivar) .EQ. xx_obcss_file ) iobcss = ivar
       IF ( ncvarfname(ivar) .EQ. xx_obcse_file ) iobcse = ivar
       IF ( ncvarfname(ivar) .EQ. xx_obcsw_file ) iobcsw = ivar
      ENDDO
#if ( defined ALLOW_OBCSN_CONTROL && defined ALLOW_OBCSN_COST_CONTRIBUTION )
cgg   North boundary contribution to cost function.
      startrec = ncvarrecstart(iobcsn)
      endrec   = ncvarrecsend(iobcsn)
      call cost_obcsn ( myiter, mytime,
     &                  startrec,endrec, mythid )
#endif
#if ( defined ALLOW_OBCSS_CONTROL && defined ALLOW_OBCSS_COST_CONTRIBUTION )
cgg   South boundary contribution to cost function.
      startrec = ncvarrecstart(iobcss)
      endrec   = ncvarrecsend(iobcss)
      call cost_obcss ( myiter, mytime,
     &                  startrec,endrec, mythid )
#endif
#if ( defined ALLOW_OBCSW_CONTROL && defined ALLOW_OBCSW_COST_CONTRIBUTION )
cgg   West boundary contribution to cost function.
      startrec = ncvarrecstart(iobcsw)
      endrec   = ncvarrecsend(iobcsw)
      call cost_obcsw ( myiter, mytime,
     &                  startrec,endrec, mythid )
#endif
#if ( defined ALLOW_OBCSE_CONTROL && defined ALLOW_OBCSE_COST_CONTRIBUTION )
cgg   East boundary contribution to cost function.
      startrec = ncvarrecstart(iobcse)
      endrec   = ncvarrecsend(iobcse)
      call cost_obcse ( myiter, mytime,
     &                  startrec,endrec, mythid )
#endif

#ifdef OBCS_VOLFLUX_COST_CONTRIBUTION
      call cost_obcsvol ( myiter,mytime,
     &                    startrec, endrec, mythid)
#endif

#endif /* ALLOW_CTRL and ALLOW_OBCS */

      end
