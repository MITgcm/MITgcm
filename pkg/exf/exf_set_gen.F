#include "EXF_OPTIONS.h"

C--  File exf_set_gen.F: General routines to load-in an input field
C--   Contents
C--   o EXF_SET_GEN
C--   o EXF_INIT_GEN

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXF_SET_GEN(
     &     genfile, genStartTime, genperiod,
     &     exf_inscal_gen, genremove_intercept, genremove_slope,
     &     genfld, gen0, gen1, genmask,
#ifdef USE_EXF_INTERPOLATION
     &     gen_lon0, gen_lon_inc, gen_lat0, gen_lat_inc,
     &     gen_nlon, gen_nlat, gen_xout, gen_yout, interp_method,
#endif
     &     myTime, myIter, myThid )

C  *=================================================================*
C  | SUBROUTINE EXF_SET_GEN
C  *=================================================================*
C  | o old version of current S/R EXF_SET_FLD, with fewer arguments
C  | o kept to allow to compile and use old piece of code
C  *=================================================================*

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "EXF_PARAM.h"
#include "EXF_INTERP_SIZE.h"

C     == routine arguments ==
      _RL genStartTime, genperiod
      _RL exf_inscal_gen
      _RL genremove_intercept, genremove_slope
      _RL genfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gen0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gen1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*1 genmask
      CHARACTER*(128) genfile
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef USE_EXF_INTERPOLATION
C     gen_lon_0 ,gen_lat_0 :: longitude and latitude of SouthWest
C                             corner of global input grid
C     gen_nlon, gen_nlat   :: input x-grid and y-grid size
C     gen_lon_inc          :: scalar x-grid increment
C     gen_lat_inc          :: vector y-grid increments
C     gen_xout, gen_yout   :: coordinates for output grid
      _RL gen_lon0, gen_lon_inc
      _RL gen_lat0, gen_lat_inc(MAX_LAT_INC)
      INTEGER gen_nlon, gen_nlat
      _RS gen_xout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS gen_yout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER interp_method
#endif /* USE_EXF_INTERPOLATION */

C     == local variables ==
C     msgBuf     :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(7) gen_name
C     == end of interface ==

      gen_name   = 'exf_gen'
      CALL EXF_SET_FLD(
     I     gen_name, genfile, genmask,
     I     genStartTime, genperiod, repeatPeriod,
     I     exf_inscal_gen, genremove_intercept, genremove_slope,
     U     genfld, gen0, gen1,
#ifdef USE_EXF_INTERPOLATION
     I     gen_lon0, gen_lon_inc, gen_lat0, gen_lat_inc,
     I     gen_nlon, gen_nlat, gen_xout, gen_yout, interp_method,
#endif
     I     myTime, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXF_INIT_GEN (
     &     genfile, genperiod, exf_inscal_gen, genmask,
     &     genconst, genfld, gen0, gen1,
#ifdef USE_EXF_INTERPOLATION
     &     gen_lon0, gen_lon_inc, gen_lat0, gen_lat_inc,
     &     gen_nlon, gen_nlat, gen_xout, gen_yout, interp_method,
#endif
     &     myThid )

C  *=================================================================*
C  | SUBROUTINE EXF_INIT_GEN
C  *=================================================================*
C  | o old version of current S/R EXF_INIT_FLD, with fewer arguments
C  | o kept to allow to compile and use old piece of code
C  *=================================================================*

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "EXF_INTERP_SIZE.h"

C     == routine arguments ==
      _RL genperiod, exf_inscal_gen, genconst
      _RL genfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gen0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gen1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*1 genmask
      CHARACTER*(128) genfile
      INTEGER myThid

#ifdef USE_EXF_INTERPOLATION
C     gen_lon_0 ,gen_lat_0 :: longitude and latitude of SouthWest
C                             corner of global input grid
C     gen_nlon, gen_nlat   :: input x-grid and y-grid size
C     gen_lon_inc          :: scalar x-grid increment
C     gen_lat_inc          :: vector y-grid increments
C     gen_xout, gen_yout   :: coordinates for output grid
      _RL gen_lon0, gen_lon_inc
      _RL gen_lat0, gen_lat_inc(MAX_LAT_INC)
      INTEGER gen_nlon, gen_nlat
      _RS gen_xout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS gen_yout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER interp_method
#endif /* USE_EXF_INTERPOLATION */

C     == local variables ==
C     msgBuf     :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(7) gen_name
C     == end of interface ==

      gen_name   = 'exf_gen'
      CALL EXF_INIT_FLD (
     I     gen_name, genfile, genmask,
     I     genperiod, exf_inscal_gen, genconst,
     U     genfld, gen0, gen1,
#ifdef USE_EXF_INTERPOLATION
     I     gen_lon0, gen_lon_inc, gen_lat0, gen_lat_inc,
     I     gen_nlon, gen_nlat, gen_xout, gen_yout, interp_method,
#endif
     I     myThid )

      RETURN
      END
