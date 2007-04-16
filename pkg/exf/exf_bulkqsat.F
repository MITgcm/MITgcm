C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_bulkqsat.F,v 1.6 2007/04/16 23:27:20 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


      _RL function exf_BulkqSat(
     I                           tkelvin
     &                         )

c     ==================================================================
c     FUNCTION exf_BulkqSat
c     ==================================================================
c
c     o Compute the saturation specific humidity.
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
c     FUNCTION exf_BulkqSat
c     ==================================================================

      implicit none

#if (defined (ALLOW_BULKFORMULAE))

c     == global variables ==

#include "SIZE.h"
#include "EXF_CONSTANTS.h"

c     == function arguments ==
      _RL tkelvin

c     == end of interface ==

      exf_BulkqSat = cvapor_fac/exp(cvapor_exp/tkelvin)

#else

c     == function arguments ==
      _RL tkelvin

      exf_BulkqSat = 0.0
#endif

      end



