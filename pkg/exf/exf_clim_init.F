c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_clim_init.F,v 1.5 2003/08/04 22:53:41 dimitri Exp $

#include "EXF_CPPOPTIONS.h"


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
#ifdef ALLOW_CLIMTEMP_RELAXATION
      call exf_init_climtemp( mythid )
#endif
#ifdef ALLOW_CLIMSALT_RELAXATION
      call exf_init_climsalt( mythid )
#endif
#ifdef ALLOW_CLIMSST_RELAXATION
      call exf_init_climsst(  mythid )
#endif
#ifdef ALLOW_CLIMSSS_RELAXATION
      call exf_init_climsss(  mythid )
#endif

      end
