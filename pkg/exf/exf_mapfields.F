c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_mapfields.F,v 1.4 2002/12/28 10:11:11 dimitri Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exf_mapfields( mythid )

c     ==================================================================
c     SUBROUTINE exf_mapfields
c     ==================================================================
c
c     o Map the external forcing fields on the ocean model arrays. This
c       routine is included to separate the ocean state estimation tool
c       as much as possible from the ocean model. Unit conversion factors
c       are to be set by the user.
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
c     started: Christian Eckert eckert@mit.edu  09-Aug-1999
c
c     changed: Christian Eckert eckert@mit.edu  11-Jan-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c
c              - Changed Routine names (package prefix: exf_)
c
c              Patrick Heimbach, heimbach@mit.edu  06-May-2000
c
c              - added and changed CPP flag structure for
c                ALLOW_BULKFORMULAE, ALLOW_ATM_TEMP
c
c              Patrick Heimbach, heimbach@mit.edu  23-May-2000
c
c              - sign change of ustress/vstress incorporated into
c                scaling factors scal_ust, scal_vst
c
c              Dimitris Menemenlis, menemenlis@jpl.nasa.gov 20-Dec-2002
c
c              - removed: empmr(i,j,bi,bj) = scal_prc*precip(i,j,bi,bj)
c
c     ==================================================================
c     SUBROUTINE exf_mapfields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "FFIELDS.h"
#include "exf_param.h"
#include "exf_constants.h"
#include "exf_fields.h"
#include "exf_clim_fields.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif
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

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

      do bj = jtlo,jthi
        do bi = itlo,ithi

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

          do j = jmin,jmax
            do i = imin,imax
c             Heat flux.
              qnet(i,j,bi,bj)  = scal_hfl*hflux(i,j,bi,bj)
            enddo
          enddo


          do j = jmin,jmax
            do i = imin,imax
c             Salt flux.
              empmr(i,j,bi,bj) = scal_sfl*sflux(i,j,bi,bj)
            enddo
          enddo

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          do j = jmin,jmax
            do i = imin,imax
c             Zonal wind stress.
              if (ustress(i,j,bi,bj).gt.2.0D0) then
                ustress(i,j,bi,bj)=2.0D0
              endif
            enddo
          enddo
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          do j = jmin,jmax
            do i = imin,imax
              if (ustress(i,j,bi,bj).lt.-2.0D0) then
                ustress(i,j,bi,bj)=-2.0D0
              endif
            enddo
          enddo
          do j = jmin,jmax
            do i = imin,imax
              fu(i,j,bi,bj)    = scal_ust*ustress(i,j,bi,bj)
            enddo
          enddo

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE vstress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          do j = jmin,jmax
            do i = imin,imax
c             Meridional wind stress.
              if (vstress(i,j,bi,bj).gt.2.0D0) then
                vstress(i,j,bi,bj)=2.0D0
              endif
            enddo
          enddo
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE vstress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          do j = jmin,jmax
            do i = imin,imax
              if (vstress(i,j,bi,bj).lt.-2.0D0) then
                vstress(i,j,bi,bj)=-2.0D0
              endif
            enddo
          enddo
          do j = jmin,jmax
            do i = imin,imax
              fv(i,j,bi,bj)    = scal_vst*vstress(i,j,bi,bj)
            enddo
          enddo

#ifdef ALLOW_KPP || \
 (defined (ALLOW_BULKFORMULAE) && defined (ALLOW_ATM_TEMP)))
c             Short wave radiative flux.
          do j = jmin,jmax
            do i = imin,imax
              qsw(i,j,bi,bj)   = scal_swf*swflux(i,j,bi,bj)
            enddo
          enddo
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
          do j = jmin,jmax
            do i = imin,imax
              sst(i,j,bi,bj)   = scal_sst*climsst(i,j,bi,bj)
            enddo
          enddo
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
          do j = jmin,jmax
            do i = imin,imax
              sss(i,j,bi,bj)   = scal_sss*climsss(i,j,bi,bj)
            enddo
          enddo
#endif

#ifdef ATMOSPHERIC_LOADING
          do j = jmin,jmax
            do i = imin,imax
              pload(i,j,bi,bj) = scal_apressure*apressure(i,j,bi,bj)
            enddo
          enddo
#endif


        enddo
      enddo

c     Update the tile edges.

      _EXCH_XY_R4(  qnet, mythid )
      _EXCH_XY_R4( empmr, mythid )
c      _EXCH_XY_R4(    fu, mythid )
c      _EXCH_XY_R4(    fv, mythid )
       CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)
#ifdef ALLOW_KPP
      _EXCH_XY_R4(   qsw, mythid )
#endif
#ifdef ALLOW_CLIMSST_RELAXATION
      _EXCH_XY_R4(   sst, mythid )
#endif
#ifdef ALLOW_CLIMSSS_RELAXATION
      _EXCH_XY_R4(   sss, mythid )
#endif
#ifdef ATMOSPHERIC_LOADING
      _EXCH_XY_R4( pload, mythid )
#endif

      end
