c $Header: /u/gcmpack/MITgcm/pkg/bulk_force/Attic/exf_bulkcdn.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

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
c                ALLOW_BULK_FORCEE, ALLOW_ATM_TEMP
c     swd, Apr 2002: modified to be used in bulkf package
c
c     ==================================================================
c     FUNCTION exf_BulkCdn
c     ==================================================================

      implicit none

#ifdef ALLOW_BULK_FORCE

c     == global variables ==

#include "SIZE.h"
#include "BULKF_ICE_CONSTANTS.h"

c     == function arguments ==
      _RL umps

c     == end of interface ==

      exf_BulkCdn = cdrag_1/umps + cdrag_2 + cdrag_3*umps

#else

c     == function arguments ==
      _RL umps

      exf_BulkCdn = 0.0
#endif

      end
