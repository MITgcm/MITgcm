C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_clim_init.F,v 1.8 2007/04/16 23:27:21 jmc Exp $
C $Name:  $

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
#include "EXF.h"
#include "EXF_PARAM.h"
#include "EXF_CLIM_PARAM.h"

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
