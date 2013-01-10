C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_mapfields.F,v 1.26 2013/01/10 22:59:24 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

CBOP 0
C     !ROUTINE: EXF_MAPFIELDS

C     !INTERFACE:
      SUBROUTINE EXF_MAPFIELDS( myTime, myIter, myThid )

C     !DESCRIPTION:
C     ==================================================================
C     SUBROUTINE EXF_MAPFIELDS
C     ==================================================================
C
C     o Map external forcing fields (ustress, vstress, hflux, sflux,
C       swflux, apressure, climsss, climsst, etc.) onto ocean model
C       arrays (fu, fv, Qnet, EmPmR, Qsw, pLoad, SSS, SST, etc.).
C       This routine is included to separate the ocean state estimation
C       tool as much as possible from the ocean model.  Unit and sign
C       conventions can be customized using variables exf_outscal_*,
C       which are set in exf_readparms.F.  See the header files
C       EXF_FIELDS.h and FFIELDS.h for definitions of the various input
C       and output fields and for default unit and sign convetions.
C
C     started: Christian Eckert eckert@mit.edu  09-Aug-1999
C
C     changed: Christian Eckert eckert@mit.edu  11-Jan-2000
C              - Restructured the code in order to create a package
C                for the MITgcmUV.
C
C              Christian Eckert eckert@mit.edu  12-Feb-2000
C              - Changed Routine names (package prefix: exf_)
C
C              Patrick Heimbach, heimbach@mit.edu  06-May-2000
C              - added and changed CPP flag structure for
C                ALLOW_BULKFORMULAE, ALLOW_ATM_TEMP
C
C              Patrick Heimbach, heimbach@mit.edu  23-May-2000
C              - sign change of ustress/vstress incorporated into
C                scaling factors exf_outscal_ust, exf_outscal_vst
C
C     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
C
C     ==================================================================
C     SUBROUTINE EXF_MAPFIELDS
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
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

C     !INPUT/OUTPUT PARAMETERS:
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number
C     myThid  :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,ks
      INTEGER imin, imax
      INTEGER jmin, jmax
      PARAMETER ( imin = 1-OLx , imax = sNx+OLx )
      PARAMETER ( jmin = 1-OLy , jmax = sNy+OLy )
CEOP

C--   set surface level index:
      ks = 1

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

C     Heat flux.
          DO j = jmin,jmax
            DO i = imin,imax
             Qnet(i,j,bi,bj) = exf_outscal_hflux*hflux(i,j,bi,bj)
            ENDDO
          ENDDO
          IF ( hfluxfile .EQ. ' ' ) THEN
           DO j = jmin,jmax
            DO i = imin,imax
             Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj) -
     &            exf_outscal_hflux * ( hflux_exfremo_intercept +
     &            hflux_exfremo_slope*(myTime-startTime) )
            ENDDO
           ENDDO
          ENDIF

C     Salt flux.
          DO j = jmin,jmax
            DO i = imin,imax
             EmPmR(i,j,bi,bj)= exf_outscal_sflux*sflux(i,j,bi,bj)
     &                                          *rhoConstFresh
            ENDDO
          ENDDO
          IF ( sfluxfile .EQ. ' ' ) THEN
           DO j = jmin,jmax
            DO i = imin,imax
             EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj) - rhoConstFresh*
     &            exf_outscal_sflux * ( sflux_exfremo_intercept +
     &            sflux_exfremo_slope*(myTime-startTime) )
            ENDDO
           ENDDO
          ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          DO j = jmin,jmax
            DO i = imin,imax
C             Zonal wind stress.
              IF (ustress(i,j,bi,bj).GT.windstressmax) THEN
                ustress(i,j,bi,bj)=windstressmax
              ENDIF
            ENDDO
          ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          DO j = jmin,jmax
            DO i = imin,imax
              IF (ustress(i,j,bi,bj).LT.-windstressmax) THEN
                ustress(i,j,bi,bj)=-windstressmax
              ENDIF
            ENDDO
          ENDDO
          IF ( stressIsOnCgrid ) THEN
           DO j = jmin,jmax
            DO i = imin+1,imax
              fu(i,j,bi,bj) = exf_outscal_ustress*ustress(i,j,bi,bj)
            ENDDO
           ENDDO
          ELSE
           DO j = jmin,jmax
            DO i = imin+1,imax
C     Shift wind stresses calculated at Grid-center to W/S points
              fu(i,j,bi,bj) = exf_outscal_ustress*
     &              (ustress(i,j,bi,bj)+ustress(i-1,j,bi,bj))
     &              *exf_half*maskW(i,j,ks,bi,bj)
            ENDDO
           ENDDO
          ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE vstress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          DO j = jmin,jmax
            DO i = imin,imax
C             Meridional wind stress.
              IF (vstress(i,j,bi,bj).GT.windstressmax) THEN
                vstress(i,j,bi,bj)=windstressmax
              ENDIF
            ENDDO
          ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE vstress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          DO j = jmin,jmax
            DO i = imin,imax
              IF (vstress(i,j,bi,bj).LT.-windstressmax) THEN
                vstress(i,j,bi,bj)=-windstressmax
              ENDIF
            ENDDO
          ENDDO
          IF ( stressIsOnCgrid ) THEN
           DO j = jmin+1,jmax
            DO i = imin,imax
              fv(i,j,bi,bj) = exf_outscal_vstress*vstress(i,j,bi,bj)
            ENDDO
           ENDDO
          ELSE
           DO j = jmin+1,jmax
            DO i = imin,imax
C     Shift wind stresses calculated at C-points to W/S points
              fv(i,j,bi,bj) = exf_outscal_vstress*
     &              (vstress(i,j,bi,bj)+vstress(i,j-1,bi,bj))
     &              *exf_half*maskS(i,j,ks,bi,bj)
            ENDDO
           ENDDO
          ENDIF

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
C             Short wave radiative flux.
          DO j = jmin,jmax
            DO i = imin,imax
             Qsw(i,j,bi,bj)  = exf_outscal_swflux*swflux(i,j,bi,bj)
            ENDDO
          ENDDO
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
          DO j = jmin,jmax
            DO i = imin,imax
             SST(i,j,bi,bj)  = exf_outscal_sst*climsst(i,j,bi,bj)
            ENDDO
          ENDDO
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
          DO j = jmin,jmax
            DO i = imin,imax
             SSS(i,j,bi,bj)  = exf_outscal_sss*climsss(i,j,bi,bj)
            ENDDO
          ENDDO
#endif

#ifdef ATMOSPHERIC_LOADING
          DO j = jmin,jmax
            DO i = imin,imax
             pLoad(i,j,bi,bj)=exf_outscal_apressure*apressure(i,j,bi,bj)
            ENDDO
          ENDDO
#endif

#ifdef EXF_ALLOW_SEAICE_RELAX
          DO j = jmin,jmax
            DO i = imin,imax
              obsSIce(i,j,bi,bj) =
     &           exf_outscal_areamask*areamask(i,j,bi,bj)
              obsSIce(I,J,bi,bj) =
     &           MIN(MAX(obsSIce(I,J,bi,bj), 0.d0 ), 1.d0)
            ENDDO
          ENDDO
#endif

       ENDDO
      ENDDO

C--   Update the tile edges.
      _EXCH_XY_RS(  Qnet, myThid )
      _EXCH_XY_RS( EmPmR, myThid )
       CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)
c#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
#ifdef SHORTWAVE_HEATING
C     Qsw used in SHORTWAVE_HEATING code & for diagnostics (<- EXCH not needed)
      _EXCH_XY_RS(   Qsw, myThid )
#endif
#ifdef ALLOW_CLIMSST_RELAXATION
      _EXCH_XY_RS(   SST, myThid )
#endif
#ifdef ALLOW_CLIMSSS_RELAXATION
      _EXCH_XY_RS(   SSS, myThid )
#endif
#ifdef ATMOSPHERIC_LOADING
      _EXCH_XY_RS( pLoad, myThid )
#endif
#ifdef EXF_ALLOW_SEAICE_RELAX
      _EXCH_XY_RS( obsSIce, myThid )
#endif

      RETURN
      END
