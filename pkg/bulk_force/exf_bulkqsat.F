c $Header: /u/gcmpack/MITgcm/pkg/bulk_force/Attic/exf_bulkqsat.F,v 1.1 2002/11/21 19:11:42 cheisey Exp $

#include "CPP_OPTIONS.h"

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
c                ALLOW_BULK_FORCEE, ALLOW_ATM_TEMP
c
c     swd, April 2002: changed to be used in bulkf package
c
c     ==================================================================
c     FUNCTION exf_BulkqSat
c     ==================================================================

      implicit none

#ifdef ALLOW_BULK_FORCE

c     == global variables ==

#include "SIZE.h"
#include "BULKF_ICE_CONSTANTS.h"

c     == function arguments ==
      _RL tkelvin
      _RL cvapor_fac, cvapor_exp
      cvapor_fac  =  640380.000
      cvapor_exp  =    5107.400
c
      exf_BulkqSat = cvapor_fac/exp(cvapor_exp/tkelvin)

#else

c     == function arguments ==
      _RL tkelvin

      exf_BulkqSat = 0.0
#endif

      end



