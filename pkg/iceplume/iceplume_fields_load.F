#include "ICEPLUME_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif /* ALLOW_EXF */

C     !ROUTINE: ICEPLUME_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE ICEPLUME_FIELDS_LOAD( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE ICEPLUME_FIELDS_LOAD
C     | o Control reading of fields from external source.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "ICEPLUME_PARAMS.h"
#include "ICEPLUME_FIELDS.h"
#ifdef ALLOW_EXF
# include "EXF_INTERP_SIZE.h"
# include "ICEPLUME_EXF_INTERP.h"
#endif /* ALLOW_EXF */

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: Simulation timestep number
C     myTime :: Simulation time
C     myThid :: Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     aWght,bWght :: Interpolation weights
      _RL aWght, bWght

      INTEGER bi, bj
      INTEGER i, j, iG, jG
      INTEGER intimeP, intime0, intime1

#ifdef ALLOW_ICEPLUME

      IF (useICEPLUME) THEN

C First call requires that we initialize everything to zero for safety
       IF ( myIter .EQ. nIter0 ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            runoffQsg0  (i,j,bi,bj) = 0.
            runoffQsg1  (i,j,bi,bj) = 0.
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

#ifdef ALLOW_EXF
C-    RunoffQsg subglacial discharge 
C The field in the file is provided in unit m3/s
C and the scaling factor will be 1. , then scaled later
C inside iceplume by rhoconst to convert to unit kg/s
       CALL EXF_SET_FLD(
     I      'runoffQsg', runoffQsgfile, runoffQsgmask,
     I      runoffQsgStartTime, runoffQsgperiod, runoffQsgRepCycle,
     I      runoffQsg_inscal,
     I      runoffQsg_remov_intercept, runoffQsg_remov_slope,
     U      runoffQsg, runoffQsg0, runoffQsg1,
#ifdef USE_EXF_INTERPOLATION
     I      runoffQsg_lon0, runoffQsg_lon_inc, 
     I      runoffQsg_lat0, runoffQsg_lat_inc,
     I      runoffQsg_nlon, runoffQsg_nlat, xC, yC, 
     I      runoffQsg_interpMethod,
#endif
     I      myTime, myIter, myThid )

#else /* ALLOW_EXF */
       IF ( periodicExternalForcing_iceplume ) THEN

C--   Now calculate whether it is time to update the forcing arrays
        CALL GET_PERIODIC_INTERVAL(
     O       intimeP, intime0, intime1, bWght, aWght,
     I       externForcingCycle_iceplume, externForcingPeriod_iceplume,
     I       deltaTClock, myTime, myThid )

        IF ( runoffQsgfile .NE. ' ') THEN
         CALL READ_REC_XY_RL(
     &       runoffQsgFile,runoffQsg0,inTime0,myIter,myThid)
         CALL READ_REC_XY_RL(
     &       runoffQsgFile,runoffQsg1,inTime1,myIter,myThid)
         _EXCH_XY_RL( runoffQsg0 , myThid )
         _EXCH_XY_RL( runoffQsg1 , myThid )

        ENDIF

C--   Interpolate
        IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             runoffQsg(i,j,bi,bj) = bWght*runoffQsg0(i,j,bi,bj)
     &                            + aWght*runoffQsg1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
C end of periodic forcing options, on to steady option
       ELSE
        IF ( myIter .EQ. nIter0 ) THEN
         IF ( runoffQsgfile .NE. ' ' ) THEN
          CALL READ_FLD_XY_RL(runoffQsgFile,' ',runoffQsg,0,myThid )
         ENDIF
         _EXCH_XY_RL( runoffQsg, myThid )
C--     endif myIter = nIter0
        ENDIF
C endif for Steady Option
       ENDIF
#endif /* ALLOW_EXF */
      ENDIF

#endif /* ALLOW_ICEPLUME */

      RETURN
      END
