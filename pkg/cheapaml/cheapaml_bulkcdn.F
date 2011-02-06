C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/Attic/cheapaml_bulkcdn.F,v 1.4 2011/02/06 00:18:05 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

c      real*8 function cheapaml_BulkCdn(
      _RL   function cheapaml_BulkCdn(
     I                          umps
     &                        )

c     ==================================================================
c     FUNCTION cheapaml_BulkCdn
c     ==================================================================
c
c     o Compute the neutral drag coefficient as a function of the wind
c       speed.
c
c
c
c              - Restructured the code  from exf_BulkCdn in order to
c                create the CHEAPaml package for the MITgcm.
c
c
c
c     ==================================================================
c     FUNCTION cheapaml_BulkCdn
c     ==================================================================

      implicit none

#if (defined (ALLOW_CHEAPAML))

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "CHEAPAML.h"

c     == function arguments ==
      _RL umps

c     == end of interface ==

      write(*,*)'in bulkcdn ',cdrag_1,cdrag_2,cdrag_3
      cheapaml_BulkCdn = cdrag_1/umps + cdrag_2 + cdrag_3*umps

#else

c     == function arguments ==
      _RL  umps
      cheapaml_BulkCdn = 0.0
#endif

      end
