C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_bulkcdn.F,v 1.7 2007/04/16 23:27:20 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


      _RL function exf_BulkCdn(
     I                          umps
     &                        )

c     ==================================================================
c     FUNCTION exf_BulkCdn
c     ==================================================================
c
c     o Compute the neutral drag coefficient as a function of the wind
c       speed.
c
c     started: Christian Eckert eckert@mit.edu  27-Aug-1999
c
c     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Patrick Heimbach heimbach@mit.edu  05-May-2000
c
c              - Included appropriate CPP options
c                ALLOW_BULKFORMULAE, ALLOW_ATM_TEMP
c
c     ==================================================================
c     FUNCTION exf_BulkCdn
c     ==================================================================

      implicit none

#if (defined (ALLOW_BULKFORMULAE))

c     == global variables ==

#include "SIZE.h"
#include "EXF_CONSTANTS.h"
#include "EXF_PARAM.h"

c     == function arguments ==
      _RL umps

c     == end of interface ==

      exf_BulkCdn = exf_scal_BulkCdn *
     &     ( cdrag_1/umps + cdrag_2 + cdrag_3*umps )

#else

c     == function arguments ==
      _RL umps

      exf_BulkCdn = 0.0
#endif

      end
