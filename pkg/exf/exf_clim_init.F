c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_clim_init.F,v 1.7 2006/01/13 17:19:32 heimbach Exp $

#include "EXF_OPTIONS.h"


      subroutine exf_clim_init( mythid )

c     ==================================================================
c     SUBROUTINE exf_clim_init
c     ==================================================================
c
c     o This routine initialises the climatologic forcing
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-20000
c
c     ==================================================================
c     SUBROUTINE exf_clim_init
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "cal.h"
#include "exf.h"
#include "exf_param.h"
#include "exf_clim_param.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

c     == end of interface ==

c     initialize climatology fields

#ifdef ALLOW_CLIMSST_RELAXATION
      call exf_init_climsst(  mythid )
#endif
#ifdef ALLOW_CLIMSSS_RELAXATION
      call exf_init_climsss(  mythid )
#endif

      end
