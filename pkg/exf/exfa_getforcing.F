c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exfa_getforcing.F,v 1.1 2001/05/14 22:08:41 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exfa_GetForcing(
     I                            mytime,
     I                            myiter,
     I                            mythid
     &                          )

c     ==================================================================
c     SUBROUTINE exfa_GetForcing
c     ==================================================================
c
c     o Get the forcing anomaly fields for the current time step. The switches
c       for the inclusion of the individual forcing components have to
c       be set in EXF_CPPOPTIONS.h .
c
c       A note on surface fluxes:
c
c       The MITgcmUV's vertical coordinate z is positive upward.
c       This implies that a positive flux is out of the ocean
c       model. However, the wind stress forcing is not treated
c       this way. A positive zonal wind stress accelerates the
c       model ocean towards the east.
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exfa_GetForcing
c     ==================================================================

      implicit none

c     == global variables ==

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

c     == end of interface ==

c     Get the surface forcing anomaly fields.
      call exfa_GetFFields(
     I                      mytime, myiter, mythid
     &                    )

c     Map the forcing anomaly fields onto the corresponding model fields.
      call exfa_MapFields(
     I                    mythid
     &                  )

      end

