c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exfa_mapfields.F,v 1.1 2001/05/14 22:08:42 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exfa_MapFields(
     I                          mythid
     &                        )

c     ==================================================================
c     SUBROUTINE exfa_MapFields
c     ==================================================================
c
c     o Map the external forcing anomaly fields on the ocean model arrays.
c
c       The units have to be such that the individual forcing record has
c       units equal to [quantity/s]. See the header file *FFIELDS.h* of
c       the MITgcmuv.
c
c       Required units such that no scaling has to be applied:
c
c       heat flux:          input file W/m^2
c       salt flux:          input file m/s
c       zonal wind stress:  input file N/m^2
c       merid. wind stress: input file N/m^2
c
c       To allow for such unit conversions this routine contains scaling
c       factors scal_quantity.
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exfa_MapFields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "FFIELDS.h"
#include "exfa_fields.h"

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j
      integer jtlo
      integer jthi
      integer itlo
      integer ithi
      integer jmin
      integer jmax
      integer imin
      integer imax
      _RL     scal_hfl
      _RL     scal_sfl
      _RL     scal_ust
      _RL     scal_vst

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

      scal_hfl = 1. _d 0
      scal_sfl = 1. _d 0
      scal_ust = 1. _d 0
      scal_vst = 1. _d 0

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax

c             Heat flux.
              qnet(i,j,bi,bj)  =
     &        qnet(i,j,bi,bj)  + scal_hfl * hfluxa(i,j,bi,bj)

c             Salt flux.
              empmr(i,j,bi,bj) =
     &        empmr(i,j,bi,bj) + scal_sfl * sfluxa(i,j,bi,bj)

c             Zonal wind stress.
              fu(i,j,bi,bj)    = 
     &        fu(i,j,bi,bj)    + scal_ust * ustressa(i,j,bi,bj)

c             Meridional wind stress.
              fv(i,j,bi,bj)    =
     &        fv(i,j,bi,bj)    + scal_vst * vstressa(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

c     Update the tile edges.

      _EXCH_XY_R4(  qnet, mythid )
      _EXCH_XY_R4( empmr, mythid )
      _EXCH_XY_R4(    fu, mythid )
      _EXCH_XY_R4(    fv, mythid )

      end
