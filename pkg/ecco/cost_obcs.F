C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_obcs.F,v 1.9 2014/10/20 03:16:12 gforget Exp $
C $Name:  $

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
# include "ctrl.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))

C     !LOCAL VARIABLES:
c     == local variables ==
      integer startrec
      integer endrec

c     == end of interface ==
CEOP

#ifdef  ALLOW_OBCSN_COST_CONTRIBUTION
#ifdef  ALLOW_OBCSN_CONTROL
cgg   North boundary contribution to cost function.
      startrec = ncvarrecstart(11 )
      endrec   = ncvarrecsend(11 )
      call cost_obcsn ( myiter, mytime,
     &                   startrec,endrec, mythid )
#endif
#endif
#ifdef  ALLOW_OBCSS_COST_CONTRIBUTION
#ifdef  ALLOW_OBCSS_CONTROL
cgg   South boundary contribution to cost function.
      startrec = ncvarrecstart(12 )
      endrec   = ncvarrecsend(12 )
      call cost_obcss ( myiter, mytime,
     &                   startrec,endrec, mythid )
#endif
#endif
#ifdef ALLOW_OBCSW_COST_CONTRIBUTION
#ifdef ALLOW_OBCSW_CONTROL
cgg   West boundary contribution to cost function.
      startrec = ncvarrecstart(13 )
      endrec   = ncvarrecsend(13 )
      call cost_obcsw ( myiter, mytime,
     &                   startrec,endrec, mythid )
#endif
#endif
#ifdef     ALLOW_OBCSE_COST_CONTRIBUTION
#ifdef     ALLOW_OBCSE_CONTROL
cgg   East boundary contribution to cost function.
      startrec = ncvarrecstart(14 )
      endrec   = ncvarrecsend(14 )
      call cost_obcse ( myiter, mytime,
     &                  startrec,endrec, mythid )
#endif
#endif

#ifdef OBCS_VOLFLUX_COST_CONTRIBUTION
      call cost_obcsvol ( myiter,mytime,
     &                    startrec, endrec, mythid)
#endif

#endif /* ALLOW_CTRL and ALLOW_OBCS */

      end
