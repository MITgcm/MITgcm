C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/ecco_init.F,v 1.3 2002/05/30 22:49:56 heimbach Exp $

#include "CPP_OPTIONS.h"


      subroutine ecco_init( mythid )

c     ==================================================================
c     SUBROUTINE ecco_init
c     ==================================================================
c
c     o Initialize the ECCO release of the MITgcmUV.
c
c     started: Christian Eckert eckert@mit.edu 29-Feb-1999
c
c              - Introduced to create a package for the MITgcmUV.
c
c     changed: Christian Eckert eckert@mit.edu
c
c     ==================================================================
c     SUBROUTINE ecco_init
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

c     == external ==

c     == end of interface ==

#ifdef INCLUDE_ECCO_PACKAGE

#ifndef ALLOW_ECCO_FORWARD_RUN
c--   Map the control variables onto the model state. 
      call ctrl_Map( mythid )
      _BARRIER
#endif

#ifdef ALLOW_COST
c--   Initialise the cost function.
      call cost_InitVaria( mythid )

c--   Initialise the time averages.
      call cost_AveragesInit( mythid )
#endif

#endif

c--   Everyone else must wait for the parameters to be loaded and
c--   the setup to be completed.
      _BARRIER

      return
      end

