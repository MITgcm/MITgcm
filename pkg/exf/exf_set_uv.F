C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_set_uv.F,v 1.22 2011/12/21 17:19:08 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      SUBROUTINE EXF_SET_UV(
     I     uvecfile, uvecstartdate, uvecperiod,
     I     exf_inscal_uvec, uvec_remove_intercept, uvec_remove_slope,
     U     uvec, uvec0, uvec1, uvecmask,
     I     vvecfile, vvecstartdate, vvecperiod,
     I     exf_inscal_vvec, vvec_remove_intercept, vvec_remove_slope,
     U     vvec, vvec0, vvec1, vvecmask,
#ifdef USE_EXF_INTERPOLATION
     I     uvec_lon0, uvec_lon_inc, uvec_lat0, uvec_lat_inc,
     I     uvec_nlon, uvec_nlat, u_interp_method,
     I     vvec_lon0, vvec_lon_inc, vvec_lat0, vvec_lat_inc,
     I     vvec_nlon, vvec_nlat, v_interp_method,
#endif /* USE_EXF_INTERPOLATION */
     I     myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE EXF_SET_UV
C     ==================================================================
C
C     o Read-in, interpolate, and rotate wind or wind stress vectors
C       from a spherical-polar input grid to an arbitrary output grid.
C
C       menemenlis@jpl.nasa.gov, 8-Dec-2003
C
C     ==================================================================
C     SUBROUTINE EXF_SET_UV
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"

#ifdef ALLOW_AUTODIFF
# include "ctrl.h"
# include "ctrl_dummy.h"
#endif

C     == routine arguments ==

C     *vec_lon_0,          :: longitude and latitude of SouthWest
C     *vec_lat_0              corner of global input grid for *vec
C     *vec_nlon, *vec_nlat :: input x-grid and y-grid size for *vec
C     *vec_lon_inc         :: scalar x-grid increment for *vec
C     *vec_lat_inc         :: vector y-grid increments for *vec

      CHARACTER*(128) uvecfile
      _RL     uvecstartdate, uvecperiod
      _RL     exf_inscal_uvec
      _RL     uvec_remove_intercept, uvec_remove_slope
      _RL     uvec  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     uvec0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     uvec1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*1 uvecmask
      CHARACTER*(128) vvecfile
      _RL     vvecstartdate, vvecperiod
      _RL     exf_inscal_vvec
      _RL     vvec_remove_intercept, vvec_remove_slope
      _RL     vvec  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     vvec0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     vvec1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*1 vvecmask
#ifdef USE_EXF_INTERPOLATION
      _RL uvec_lon0, uvec_lon_inc
      _RL uvec_lat0, uvec_lat_inc(MAX_LAT_INC)
      INTEGER uvec_nlon, uvec_nlat, u_interp_method
      _RL vvec_lon0, vvec_lon_inc
      _RL vvec_lat0, vvec_lat_inc(MAX_LAT_INC)
      INTEGER vvec_nlon, vvec_nlat, v_interp_method
#endif /* USE_EXF_INTERPOLATION */
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     == local variables ==

#ifdef USE_EXF_INTERPOLATION
      CHARACTER*(128) uvecfile0, uvecfile1
      CHARACTER*(128) vvecfile0, vvecfile1
      LOGICAL first, changed
      _RL     fac, x1, x2, x3, x4, y1, y2, y3, y4, dx, dy
      _RL     tmp_u (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     tmp_v (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER count0, count1
      INTEGER i, j, bi, bj
      INTEGER year0, year1
#endif /* USE_EXF_INTERPOLATION */

C     == end of interface ==

#ifdef USE_EXF_INTERPOLATION
      IF ( u_interp_method.GE.1 .AND. v_interp_method.GE.1
     &   .AND.  uvecfile.NE.' ' .AND. vvecfile.NE.' '
     &   .AND. ( usingCurvilinearGrid .OR. rotateGrid ) ) THEN

C     get record numbers and interpolation factor
        CALL exf_GetFFieldRec(
     I           uvecstartdate, uvecperiod,
     I           useExfYearlyFields,
     O           fac, first, changed,
     O           count0, count1, year0, year1,
     I           myTime, myIter, myThid )

        IF ( first ) THEN

          CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, uvecperiod, year0,
     I         uvecfile,
     O         uvecfile0,
     I         myTime, myIter, myThid )
          CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, vvecperiod, year0,
     I         vvecfile,
     O         vvecfile0,
     I         myTime, myIter, myThid )

C     scalar interpolation to (xC,yC) locations
          CALL EXF_INTERP(
     I             uvecfile0, exf_iprec,
     O             tmp_u,
     I             count0, xC, yC,
     I             uvec_lon0, uvec_lon_inc, uvec_lat0, uvec_lat_inc,
     I             uvec_nlon, uvec_nlat, u_interp_method, myThid )
          CALL EXF_INTERP(
     I             vvecfile0, exf_iprec,
     O             tmp_v,
     I             count0, xC, yC,
     I             vvec_lon0, vvec_lon_inc, vvec_lat0, vvec_lat_inc,
     I             vvec_nlon, vvec_nlat, v_interp_method, myThid )
C     vector rotation
          DO bj = myByLo(myThid),myByHi(myThid)
            DO bi = myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
                DO i = 1,sNx
                    x1=xG(i,j,bi,bj)
                    x2=xG(i+1,j,bi,bj)
                    x3=xG(i,j+1,bi,bj)
                    x4=xG(i+1,j+1,bi,bj)
                    IF ((x2-x1).GT.180) x2=x2-360
                    IF ((x1-x2).GT.180) x2=x2+360
                    IF ((x3-x1).GT.180) x3=x3-360
                    IF ((x1-x3).GT.180) x3=x3+360
                    IF ((x4-x1).GT.180) x4=x4-360
                    IF ((x1-x4).GT.180) x4=x4+360
                    y1=yG(i,j,bi,bj)
                    y2=yG(i+1,j,bi,bj)
                    y3=yG(i,j+1,bi,bj)
                    y4=yG(i+1,j+1,bi,bj)
                    dx=0.5*(x3+x4-x1-x2)
                    dx=dx*
     &                  cos(deg2rad*yC(i,j,bi,bj))
                    dy=0.5*(y3+y4-y1-y2)
                    vvec1(i,j,bi,bj)=
     &                  (tmp_u(i,j,bi,bj)*dx+
     &                  tmp_v(i,j,bi,bj)*dy)/
     &                  SQRT(dx*dx+dy*dy)
                    dx=0.5*(x2+x4-x1-x3)
                    dx=dx*
     &                  cos(deg2rad*yC(i,j,bi,bj))
                    dy=0.5*(y2+y4-y1-y3)
                    uvec1(i,j,bi,bj)=
     &                  (tmp_u(i,j,bi,bj)*dx+
     &                  tmp_v(i,j,bi,bj)*dy)/
     &                  SQRT(dx*dx+dy*dy)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
C     apply mask
          CALL EXF_FILTER_RL( uvec1, uvecmask, myThid )
          CALL EXF_FILTER_RL( vvec1, vvecmask, myThid )
        ENDIF

        IF (  first .OR. changed ) THEN
          CALL exf_SwapFFields( uvec0, uvec1, myThid )
          CALL exf_SwapFFields( vvec0, vvec1, myThid )

          CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, uvecperiod, year1,
     I         uvecfile,
     O         uvecfile1,
     I         myTime, myIter, myThid )
          CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, vvecperiod, year1,
     I         vvecfile,
     O         vvecfile1,
     I         myTime, myIter, myThid )

C     scalar interpolation to (xC,yC) locations
          CALL EXF_INTERP(
     I             uvecfile1, exf_iprec,
     O             tmp_u,
     I             count1, xC, yC,
     I             uvec_lon0, uvec_lon_inc, uvec_lat0, uvec_lat_inc,
     I             uvec_nlon, uvec_nlat, u_interp_method, myThid )
          CALL EXF_INTERP(
     I             vvecfile1, exf_iprec,
     O             tmp_v,
     I             count1, xC, yC,
     I             vvec_lon0, vvec_lon_inc, vvec_lat0, vvec_lat_inc,
     I             vvec_nlon, vvec_nlat, v_interp_method, myThid )
C     vector rotation
          DO bj = myByLo(myThid),myByHi(myThid)
            DO bi = myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
                DO i = 1,sNx
                    x1=xG(i,j,bi,bj)
                    x2=xG(i+1,j,bi,bj)
                    x3=xG(i,j+1,bi,bj)
                    x4=xG(i+1,j+1,bi,bj)
                    IF ((x2-x1).GT.180) x2=x2-360
                    IF ((x1-x2).GT.180) x2=x2+360
                    IF ((x3-x1).GT.180) x3=x3-360
                    IF ((x1-x3).GT.180) x3=x3+360
                    IF ((x4-x1).GT.180) x4=x4-360
                    IF ((x1-x4).GT.180) x4=x4+360
                    y1=yG(i,j,bi,bj)
                    y2=yG(i+1,j,bi,bj)
                    y3=yG(i,j+1,bi,bj)
                    y4=yG(i+1,j+1,bi,bj)
                    dx=0.5*(x3+x4-x1-x2)
                    dx=dx*
     &                  cos(deg2rad*yC(i,j,bi,bj))
                    dy=0.5*(y3+y4-y1-y2)
                    vvec1(i,j,bi,bj)=
     &                  (tmp_u(i,j,bi,bj)*dx+
     &                  tmp_v(i,j,bi,bj)*dy)/
     &                  SQRT(dx*dx+dy*dy)
                    dx=0.5*(x2+x4-x1-x3)
                    dx=dx*
     &                  cos(deg2rad*yC(i,j,bi,bj))
                    dy=0.5*(y2+y4-y1-y3)
                    uvec1(i,j,bi,bj)=
     &                  (tmp_u(i,j,bi,bj)*dx+
     &                  tmp_v(i,j,bi,bj)*dy)/
     &                  SQRT(dx*dx+dy*dy)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
C     apply mask
          CALL EXF_FILTER_RL( uvec1, uvecmask, myThid )
          CALL EXF_FILTER_RL( vvec1, vvecmask, myThid )
        ENDIF

C     Interpolate linearly onto the current time.
        DO bj = myByLo(myThid),myByHi(myThid)
          DO bi = myBxLo(myThid),myBxHi(myThid)
            DO j = 1,sNy
              DO i = 1,sNx
                uvec(i,j,bi,bj) = exf_inscal_uvec * (
     &                fac * uvec0(i,j,bi,bj) +
     &                (exf_one - fac) * uvec1(i,j,bi,bj) )
                vvec(i,j,bi,bj) = exf_inscal_vvec * (
     &                fac * vvec0(i,j,bi,bj) +
     &                (exf_one - fac) * vvec1(i,j,bi,bj) )
              ENDDO
            ENDDO
          ENDDO
        ENDDO

      ELSE
C     case no-interpolation or .NOT.(usingCurvilinearGrid .OR. rotateGrid)
#else  /* USE_EXF_INTERPOLATION */
      IF ( .TRUE. ) THEN
#endif /* USE_EXF_INTERPOLATION */

        CALL EXF_SET_GEN(
     &      uvecfile, uvecstartdate, uvecperiod,
     &      exf_inscal_uvec,
     &      uvec_remove_intercept, uvec_remove_slope,
     &      uvec, uvec0, uvec1, uvecmask,
#ifdef USE_EXF_INTERPOLATION
     &      uvec_lon0, uvec_lon_inc, uvec_lat0, uvec_lat_inc,
     &      uvec_nlon, uvec_nlat, xC, yC, u_interp_method,
#endif /* USE_EXF_INTERPOLATION */
     &      myTime, myIter, myThid )

        CALL EXF_SET_GEN(
     &      vvecfile, vvecstartdate, vvecperiod,
     &      exf_inscal_vvec,
     &      vvec_remove_intercept, vvec_remove_slope,
     &      vvec, vvec0, vvec1, vvecmask,
#ifdef USE_EXF_INTERPOLATION
     &      vvec_lon0, vvec_lon_inc, vvec_lat0, vvec_lat_inc,
     &      vvec_nlon, vvec_nlat, xC, yC, v_interp_method,
#endif /* USE_EXF_INTERPOLATION */
     &      myTime, myIter, myThid )

      ENDIF

      RETURN
      END
