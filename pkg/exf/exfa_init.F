c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exfa_init.F,v 1.1 2001/05/14 22:08:42 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exfa_init(
     I                      mythid
     &                    )

c     ==================================================================
c     SUBROUTINE exfa_init
c     ==================================================================
c
c     o This routine initialises the package that calculates external
c       forcing field anomalies for a given timestep of the MITgcmUV. Parameters
c       for this package are set in "data.exfa". Some additional
c       precompiler switches have to be specified in "EXF_CPPOPTIONS.h" .
c
c     started: Ralf.Giering@FastOpt.de 24-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exfa_init
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "cal.h"
#include "exfa.h"
#include "exfa_param.h"

#include "exf_constants.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

c     == end of interface ==

      call exf_init_hfluxa  ( mythid )
      call exf_init_sfluxa  ( mythid )

      call exf_init_ustressa( mythid )
      call exf_init_vstressa( mythid )

      end
