#include "EXF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

      SUBROUTINE EXF_GETCLIM(
     I                        myTime, myIter, myThid )

c     ==================================================================
c     SUBROUTINE exf_getclim
c     ==================================================================
c
c     o Get the climatogy fields for the current time step. The switches
c       for the inclusion of the individual forcing components have to
c       be set in EXF_OPTIONS.h .
c
c       A note on surface fluxes:
c
c       The MITgcm-UV vertical coordinate z is positive upward.
c       This implies that a positive flux is out of the ocean
c       model. However, the wind stress forcing is not treated
c       this way. A positive zonal wind stress accelerates the
c       model ocean towards the east.
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_getclim
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
#ifdef ALLOW_BULK_OFFLINE
# include "DYNVARS.h"
#endif
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_INTERP_SIZE.h"
#include "EXF_INTERP_PARAM.h"
#include "EXF_FIELDS.h"
#if (defined (ALLOW_CTRL) && defined (ECCO_CTRL_DEPRECATED))
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# ifdef ALLOW_ECCO
#  include "ecco_cost.h"
# endif
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

c     == routine arguments ==

c     myThid - thread number for this instance of the routine.

      _RL     myTime
      INTEGER myIter
      INTEGER myThid

c     == local variables ==

      INTEGER ks
#if (defined ALLOW_CLIMSST_RELAXATION || defined ALLOW_BULK_OFFLINE )
      INTEGER i, j, bi, bj
#endif
#if (defined (ALLOW_CTRL) && defined (ECCO_CTRL_DEPRECATED))
      _RS mask2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

c     == end of interface ==

      ks = 1
      IF ( usingPCoords ) ks = Nr

#ifdef ALLOW_CLIMSST_RELAXATION
c     Get values of climatological sst fields.
      CALL EXF_SET_FLD(
     I     'climsst', climsstfile, climsstmask,
     I     climsstStartTime, climsstperiod, climsstRepCycle,
     I     exf_inscal_climsst,
     I     climsst_exfremo_intercept, climsst_exfremo_slope,
     U     climsst, climsst0, climsst1,
#ifdef USE_EXF_INTERPOLATION
     I     climsst_lon0, climsst_lon_inc, climsst_lat0, climsst_lat_inc,
     I     climsst_nlon, climsst_nlat, xC, yC, climsst_interpMethod,
#endif
     I     myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
C     this avoids storing climsst0/1
CADJ STORE climsst      = comlev1, key=ikey_dynamics, kind=isbyte
#endif
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx
            IF (climsst(i,j,bi,bj) .LT. climtempfreeze) THEN
               climsst(i,j,bi,bj) = climtempfreeze
            ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
c--   Update the tile edges.
      _EXCH_XY_RL(climsst, myThid)
#endif /* ALLOW_CLIMSST_RELAXATION */

#if (defined (ALLOW_CTRL) && defined (ECCO_CTRL_DEPRECATED))
# ifdef ALLOW_SST_CONTROL
      CALL CTRL_GET_MASK2D( xx_sst_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_sst_file, xx_sststartdate, xx_sstperiod,
     &     mask2D, climsst, xx_sst0, xx_sst1, xx_sst_dummy,
     &     xx_sst_remo_intercept, xx_sst_remo_slope,
     &     wsst, myTime, myIter, myThid )
# endif
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
c     Get values of climatological sss fields.
      CALL EXF_SET_FLD(
     I     'climsss', climsssfile, climsssmask,
     I     climsssStartTime, climsssperiod, climsssRepCycle,
     I     exf_inscal_climsss,
     I     climsss_exfremo_intercept, climsss_exfremo_slope,
     U     climsss, climsss0, climsss1,
#ifdef USE_EXF_INTERPOLATION
     I     climsss_lon0, climsss_lon_inc, climsss_lat0, climsss_lat_inc,
     I     climsss_nlon, climsss_nlat, xC, yC, climsss_interpMethod,
#endif
     I     myTime, myIter, myThid )
c--   Update the tile edges.
      _EXCH_XY_RL(climsss, myThid)
#endif

#if (defined (ALLOW_CTRL) && defined (ECCO_CTRL_DEPRECATED))
# ifdef ALLOW_SSS_CONTROL
      CALL CTRL_GET_MASK2D( xx_sss_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_sss_file, xx_sssstartdate, xx_sssperiod,
     &     mask2D, climsss, xx_sss0, xx_sss1, xx_sss_dummy,
     &     xx_sss_remo_intercept, xx_sss_remo_slope,
     &     wsss, myTime, myIter, myThid )
# endif
#endif

#ifdef ALLOW_CLIMSTRESS_RELAXATION
c     Get values of climatological ustr fields.
      CALL EXF_SET_UV (
     I     'climustr', climustrfile, climustrmask,
     I     climustrStartTime, climustrperiod, climustrRepCycle,
     I     exf_inscal_climustr,
     I     climustr_exfremo_intercept, climustr_exfremo_slope,
     U     climustr, climustr0, climustr1,
     I     'climvstr', climvstrfile, climvstrmask,
     I     climvstrStartTime, climvstrperiod, climvstrRepCycle,
     I     exf_inscal_climvstr,
     I     climvstr_exfremo_intercept, climvstr_exfremo_slope,
     U     climvstr, climvstr0, climvstr1,
#ifdef USE_EXF_INTERPOLATION
     I     climustr_lon0, climustr_lon_inc,
     I     climustr_lat0, climustr_lat_inc,
     I     climustr_nlon, climustr_nlat, climustr_interpMethod,
     I     climvstr_lon0, climvstr_lon_inc,
     I     climvstr_lat0, climvstr_lat_inc,
     I     climvstr_nlon, climvstr_nlat, climvstr_interpMethod,
     I     uvInterp_climstr,
#endif
     I     myTime, myIter, myThid )

      CALL EXCH_UV_XY_RL( climustr, climvstr, .TRUE., myThid )
#endif /* CLIMSTRESS_RELAXATION */

#ifdef ALLOW_BULK_OFFLINE
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
# ifdef ALLOW_CLIMSST_RELAXATION
           IF ( climsstfile .NE. ' ' .AND.
     &          climsstperiod .NE. 0. )
     &          theta(i,j,ks,bi,bj) = climsst(i,j,bi,bj)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
           IF ( climsssfile .NE. ' ' .AND.
     &          climsssperiod .NE. 0. )
     &          salt(i,j,ks,bi,bj) = climsss(i,j,bi,bj)
# endif
# ifdef ALLOW_CLIMSTRESS_RELAXATION
           IF ( climustrfile .NE. ' ' .AND.
     &          climustrperiod .NE. 0. )
     &          uVel(i,j,ks,bi,bj) = climustr(i,j,bi,bj)
           IF ( climvstrfile .NE. ' ' .AND.
     &          climvstrperiod .NE. 0. )
     &          vVel(i,j,ks,bi,bj) = climvstr(i,j,bi,bj)
# endif
           IF ( myIter .EQ. nIter0 ) THEN
              IF ( maskC(i,j,ks,bi,bj) .NE. 0. .AND.
     &             theta(i,j,ks,bi,bj) .EQ. 0. ) THEN
                print *, 'ph-warn-exf-clim ', i, j, theta(i,j,ks,bi,bj)
cph                STOP 'in exf_getclim'
              ENDIF
           ENDIF
          ENDDO
         ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_BULK_OFFLINE */

      RETURN
      END
