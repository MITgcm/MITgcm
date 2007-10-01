C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_mapfields.F,v 1.22 2007/10/01 13:36:05 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_mapfields( mytime, myiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_mapfields
c     ==================================================================
c
c     o Map external forcing fields (ustress, vstress, hflux, sflux,
c       swflux, apressure, climsss, climsst, etc.) onto ocean model
c       arrays (fu, fv, Qnet, EmPmR, Qsw, pload, sss, sst, etc.).
c       This routine is included to separate the ocean state estimation
c       tool as much as possible from the ocean model.  Unit and sign
c       conventions can be customized using variables exf_outscal_*,
c       which are set in exf_readparms.F.  See the header files
c       EXF_FIELDS.h and FFIELDS.h for definitions of the various input
c       and output fields and for default unit and sign convetions.
c
c     started: Christian Eckert eckert@mit.edu  09-Aug-1999
c
c     changed: Christian Eckert eckert@mit.edu  11-Jan-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c              - Changed Routine names (package prefix: exf_)
c
c              Patrick Heimbach, heimbach@mit.edu  06-May-2000
c              - added and changed CPP flag structure for
c                ALLOW_BULKFORMULAE, ALLOW_ATM_TEMP
c
c              Patrick Heimbach, heimbach@mit.edu  23-May-2000
c              - sign change of ustress/vstress incorporated into
c                scaling factors exf_outscal_ust, exf_outscal_vst
c
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     SUBROUTINE exf_mapfields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_FIELDS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif
c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      _RL     mytime
      integer myiter
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      INTEGER imin, imax
      INTEGER jmin, jmax
      PARAMETER ( imin = 1-OLx , imax = sNx+OLx )
      PARAMETER ( jmin = 1-OLy , jmax = sNy+OLy )

c     == end of interface ==

      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)

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

c     Heat flux.
          do j = jmin,jmax
            do i = imin,imax
             qnet(i,j,bi,bj) = exf_outscal_hflux*hflux(i,j,bi,bj)
            enddo
          enddo
          if ( hfluxfile .EQ. ' ' ) then
           do j = jmin,jmax
            do i = imin,imax
                  qnet(i,j,bi,bj) = qnet(i,j,bi,bj) -
     &            exf_outscal_hflux * ( hflux_exfremo_intercept +
     &            hflux_exfremo_slope*(mytime-starttime) )
            enddo
           enddo
          endif

c     Salt flux.
          do j = jmin,jmax
            do i = imin,imax
             EmPmR(i,j,bi,bj)= exf_outscal_sflux*sflux(i,j,bi,bj)
     &                                          *rhoConstFresh
            enddo
          enddo
          if ( sfluxfile .EQ. ' ' ) then
           do j = jmin,jmax
            do i = imin,imax
                 EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj) - rhoConstFresh*
     &            exf_outscal_sflux * ( sflux_exfremo_intercept +
     &            sflux_exfremo_slope*(mytime-starttime) )
            enddo
           enddo
          endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          do j = jmin,jmax
            do i = imin,imax
c             Zonal wind stress.
              if (ustress(i,j,bi,bj).gt.windstressmax) then
                ustress(i,j,bi,bj)=windstressmax
              endif
            enddo
          enddo
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          do j = jmin,jmax
            do i = imin,imax
              if (ustress(i,j,bi,bj).lt.-windstressmax) then
                ustress(i,j,bi,bj)=-windstressmax
              endif
            enddo
          enddo
          IF ( stressIsOnCgrid ) THEN
           do j = jmin,jmax
            do i = imin+1,imax
              fu(i,j,bi,bj) = exf_outscal_ustress*ustress(i,j,bi,bj)
            enddo
           enddo
          ELSE
           do j = jmin,jmax
            do i = imin+1,imax
c     Shift wind stresses calculated at Grid-center to W/S points
              fu(i,j,bi,bj) = exf_outscal_ustress*
     &              (ustress(i,j,bi,bj)+ustress(i-1,j,bi,bj))
     &              *exf_half*maskW(i,j,1,bi,bj)
            enddo
           enddo
          ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE vstress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          do j = jmin,jmax
            do i = imin,imax
c             Meridional wind stress.
              if (vstress(i,j,bi,bj).gt.windstressmax) then
                vstress(i,j,bi,bj)=windstressmax
              endif
            enddo
          enddo
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE vstress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          do j = jmin,jmax
            do i = imin,imax
              if (vstress(i,j,bi,bj).lt.-windstressmax) then
                vstress(i,j,bi,bj)=-windstressmax
              endif
            enddo
          enddo
          IF ( stressIsOnCgrid ) THEN
           do j = jmin+1,jmax
            do i = imin,imax
              fv(i,j,bi,bj) = exf_outscal_vstress*vstress(i,j,bi,bj)
            enddo
           enddo
          ELSE
           do j = jmin+1,jmax
            do i = imin,imax
c     Shift wind stresses calculated at C-points to W/S points
              fv(i,j,bi,bj) = exf_outscal_vstress*
     &              (vstress(i,j,bi,bj)+vstress(i,j-1,bi,bj))
     &              *exf_half*maskS(i,j,1,bi,bj)
            enddo
           enddo
          ENDIF

#ifdef SHORTWAVE_HEATING
c             Short wave radiative flux.
          do j = jmin,jmax
            do i = imin,imax
             qsw(i,j,bi,bj)  = exf_outscal_swflux*swflux(i,j,bi,bj)
            enddo
          enddo
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
          do j = jmin,jmax
            do i = imin,imax
             sst(i,j,bi,bj)  = exf_outscal_sst*climsst(i,j,bi,bj)
            enddo
          enddo
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
          do j = jmin,jmax
            do i = imin,imax
             sss(i,j,bi,bj)  = exf_outscal_sss*climsss(i,j,bi,bj)
            enddo
          enddo
#endif

#ifdef ATMOSPHERIC_LOADING
          do j = jmin,jmax
            do i = imin,imax
             pload(i,j,bi,bj)=exf_outscal_apressure*apressure(i,j,bi,bj)
            enddo
          enddo
#endif

        ENDDO
      ENDDO

c     Update the tile edges.

      _EXCH_XY_R4(  qnet, mythid )
      _EXCH_XY_R4( empmr, mythid )
       CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)
#ifdef SHORTWAVE_HEATING
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

      RETURN
      END
