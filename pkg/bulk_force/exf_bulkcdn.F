c $Header: /u/gcmpack/MITgcm/pkg/bulk_force/Attic/exf_bulkcdn.F,v 1.3 2003/11/23 01:36:55 jmc Exp $
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

c     == global variables ==

#include "BULKF_PARAMS.h"

c     == function arguments ==
      _RL umps

c     == end of interface ==
#ifdef ALLOW_BULK_FORCE

      exf_BulkCdn = cdrag_1/umps + cdrag_2 + cdrag_3*umps

#endif

      end
