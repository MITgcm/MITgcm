c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_bulkrhn.F,v 1.1 2001/05/14 22:08:40 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


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
c                ALLOW_BULKFORMULAE, ALLOW_ATM_TEMP
c
c     ==================================================================
c     FUNCTION exf_BulkRhn
c     ==================================================================

      implicit none

#if (defined (ALLOW_BULKFORMULAE) && defined (ALLOW_ATM_TEMP))

c     == global variables ==

#include "SIZE.h"
#include "exf_constants.h"

c     == function arguments ==
      _RL stab

c     == end of interface ==

      exf_BulkRhn = (exf_one - stab)*cstanton_1 + stab*cstanton_2

#else

c     == function arguments ==
      _RL stab

       exf_BulkRhn = 0.0

#endif

      end
