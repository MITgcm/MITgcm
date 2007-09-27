C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_radiation.F,v 1.4 2007/09/27 09:41:13 mlosch Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_radiation(mytime, myiter, mythid)

c     ==================================================================
c     SUBROUTINE exf_radiation
c     ==================================================================
c
c     o Set radiative fluxes at the surface.
c
c     ==================================================================
c     SUBROUTINE exf_radiation
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"

c     == routine arguments ==

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

      integer bi,bj
      integer i,j,k

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Use atmospheric state to compute surface fluxes.

      k = 1

#ifdef ALLOW_DOWNWARD_RADIATION
c--   Compute net from downward and downward from net longwave and
c     shortwave radiation, if needed.
c     lwflux = Stefan-Boltzmann constant * emissivity * SST - lwdown
c     swflux = - ( 1 - albedo ) * swdown

#ifdef ALLOW_ATM_TEMP
      if ( lwfluxfile .EQ. ' ' .AND. lwdownfile .NE. ' ' ) then
c     Loop over tiles.
       do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
         do j = 1,sny
          do i = 1,snx
           lwflux(i,j,bi,bj) = 
     &          ocean_emissivity*stefanBoltzmann*
     &          ((theta(i,j,k,bi,bj)+cen2kel)**4)
     &          - lwdown(i,j,bi,bj)
          enddo
         enddo
        enddo
       enddo
      endif

c     Loop over tiles.
      if ( lwfluxfile .NE. ' ' .AND. lwdownfile .EQ. ' ' ) then
       do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
         do j = 1,sny
          do i = 1,snx
           lwdown(i,j,bi,bj) = 
     &          ocean_emissivity*stefanBoltzmann*
     &          ((theta(i,j,k,bi,bj)+cen2kel)**4)
     &          - lwflux(i,j,bi,bj)
          enddo
         enddo
        enddo
       enddo
      endif
#endif

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      if ( swfluxfile .EQ. ' ' .AND. swdownfile .NE. ' ' ) then
       do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
         do j = 1,sny
          do i = 1,snx
           swflux(i,j,bi,bj) = -(1.0-exf_albedo) * swdown(i,j,bi,bj)
          enddo
         enddo
        enddo
       enddo
      endif
      if ( swfluxfile .NE. ' ' .AND. swdownfile .EQ. ' ' ) then
       do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
         do j = 1,sny
          do i = 1,snx
           swdown(i,j,bi,bj) = -swflux(i,j,bi,bj) / (1.0-exf_albedo)
          enddo
         enddo
        enddo
       enddo
      endif
#endif

#endif /* ALLOW_DOWNWARD_RADIATION */

      end
