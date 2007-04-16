C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_bulkrhn.F,v 1.6 2007/04/16 23:27:20 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


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

#if (defined (ALLOW_BULKFORMULAE))

c     == global variables ==

#include "SIZE.h"
#include "EXF_CONSTANTS.h"

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
