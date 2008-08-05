c $Header: /u/gcmpack/MITgcm/pkg/cheapaml/Attic/cheapaml_bulkcdn.F,v 1.1 2008/08/05 21:49:31 jmc Exp $


#include "CPP_OPTIONS.h"

c      real*8 function cheapaml_BulkCdn(
      _RL   function cheapaml_BulkCdn(
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
c     FUNCTION cheapaml_BulkCdn
c     ==================================================================

      implicit none

#if (defined (ALLOW_CHEAPAML))

c     == global variables ==

#include "SIZE.h"
#include "CHEAPAML.h"

c     == function arguments ==
c      real*8 umps
      _RL umps

c     == end of interface ==

	write(*,*)'in bulkcdn ',cdrag_1,cdrag_2,cdrag_3
      cheapaml_BulkCdn = cdrag_1/umps + cdrag_2 + cdrag_3*umps

#else

c     == function arguments ==
c      real*8 umps
      _RL  umps
      cheapaml_BulkCdn = 0.0
#endif

      end
