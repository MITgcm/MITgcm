c $Header: /u/gcmpack/MITgcm/pkg/bulk_force/Attic/exf_bulkrhn.F,v 1.1 2002/11/21 19:11:42 cheisey Exp $

#include "CPP_OPTIONS.h"

      _RL function exf_BulkRhn(
     I                          stab
     &                        )

c     ==================================================================
c     FUNCTION exf_BulkRhn
c     ==================================================================
c
c     o Compute the Dalton number as a function of stability.
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
c     FUNCTION exf_BulkRhn
c     ==================================================================

      implicit none

#ifdef ALLOW_BULK_FORCE

c     == global variables ==

#include "SIZE.h"
#include "BULKF_ICE_CONSTANTS.h"


c     == function arguments ==
      _RL stab
      _RL cstanton_1, cstanton_2
       cstanton_1  =       0.0327000
       cstanton_2  =       0.0180000

      exf_BulkRhn = (1.d0    - stab)*cstanton_1 + stab*cstanton_2

#else

c     == function arguments ==
      _RL stab

       exf_BulkRhn = 0.0

#endif

      end
