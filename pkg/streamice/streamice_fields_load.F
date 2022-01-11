#include "STREAMICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE STREAMICE_FIELDS_LOAD(
     I                            myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE AIM_FIELDS_LOAD
C     | o Control reading of AIM fields from external source.
C     *==========================================================*
C     | Loads surface boundary condition datasets for AIM.
C     | The routine is called every timetep and periodically
C     | loads a set of external fields.
C     | Monthly climatology files are read either for
C     | a) a direct use (useMMsurfFc):
C     |   Albedo, Soil moisture, Surface Temperature
C     | b) time interpolation (useFMsurfBC):
C     |   Sea & Land surf. Temp, snow, sea-ice, soil-water (2-lev)
C     | + (1rst.iter) ground albedo, vegetation, land-sea fraction
C     | Most of the work is done by the master thread while
C     | the other threads spin (but all inside MDSIO S/R).
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "STREAMICE.h"

C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

C     === Functions ===

#ifdef ALLOW_STREAMICE
#ifdef ALLOW_STREAMICE_TIMEDEP_FORCING
C     === Local variables ===
C     bi,bj, i,j  :: Loop counters
C     tYear       :: Fraction within year of myTime
C     mnthIndex   :: Current time in whole months
C     prevMnthIndex
C     fNam        :: Strings used in constructing file names
C     mnthNam
C     pfact       :: used to convert Pot.Temp. to in-situ Temp.
C     loadNewData :: true when need to load new data from file
      INTEGER bi,bj,i,j
c     _RL pfact
      LOGICAL first, changed
C--   for use with useMMsurfFc:
      CHARACTER*(MAX_LEN_FNAM) fNam

      INTEGER nm0, nm1, nmP
      _RL myRelTime, fac, tmpFac

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   find which month to use for surface BC
C     aim_surfForc_TimePeriod :: Length of forcing time period (e.g. 1 month)
C     aim_surfForc_NppCycle   :: Number of time period per Cycle (e.g. 12)

      myRelTime = myTime - startTime
      first = (myRelTime .lt. 0.5*deltaTClock)
      if ( streamice_forcing_period .eq. 0.D0 ) THEN
!     &     .or. externForcingCycle .eq. 0.D0 ) then
C     control parameter is constant in time and only needs to be updated
C     once in the beginning
       changed = .false.
       nm0  = 1
       nm1  = 1
       fac     = 1.D0
      else

C--   Now calculate whether it is time to update the forcing arrays
       if (externForcingCycle .eq. 0.0 ) THEN
        CALL GET_PERIODIC_INTERVAL(
     O                   nmP, nm0, nm1, tmpFac, fac,
     I                   externForcingCycle, streamice_forcing_period,
     I                   deltaTClock,
     I                   myTime+0.5*streamice_forcing_period,
     I                   myThid )
        fac = 1.D0 - fac
       else
        CALL GET_PERIODIC_INTERVAL(
     O                   nmP, nm0, nm1, tmpFac, fac,
     I                   externForcingCycle, streamice_forcing_period,
     I                   deltaTClock, myTime,
     I                   myThid )
       endif

       IF ( nm0.NE.nmP ) THEN
        changed = .true.
       ELSE
        changed = .false.
       ENDIF
       IF ( first ) changed = .false.
      endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-     Load new data:

C-    Only one thread updates parameter in common block
C-    Wait for everyone to set loadNewData before Master updates prevMnthIndex
        _BARRIER

        IF ( first ) THEN

#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL

!         PRINT *, "GOT HERE FIELDS LOAD 1", first, changed,
!     &     nm0,nm1,myTime,fac

         IF ( STREAMICEuNormalTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEuNormalTimeDepFile,
     &                        streamice_u_normal_stress1,
     &                        nm0,myIter,myThid)
         ENDIF

         IF ( STREAMICEvNormalTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEvNormalTimeDepFile,
     &                        streamice_v_normal_stress1,
     &                        nm0,myIter,myThid)
         ENDIF

         IF ( STREAMICEuShearTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEuShearTimeDepFile,
     &                        streamice_u_shear_stress1,
     &                        nm0,myIter,myThid)
         ENDIF

         IF ( STREAMICEvShearTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEvShearTimeDepFile,
     &                        streamice_v_shear_stress1,
     &                        nm0,myIter,myThid)
         ENDIF
!         PRINT *, "GOT HERE FIELDS LOAD 2"

#endif
#ifdef ALLOW_STREAMICE_FLUX_CONTROL
         IF ( STREAMICEuFluxTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEuFluxTimeDepFile,
     &                        u_flux_bdry_SI_1,
     &                        nm0,myIter,myThid)
         ENDIF
         IF ( STREAMICEvFluxTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEvFluxTimeDepFile,
     &                        u_flux_bdry_SI_1,
     &                        nm0,myIter,myThid)
         ENDIF
#endif

         IF ( STREAMICEBdotTimeDepFile .NE. ' '  ) THEN
!         PRINT *, "GOT HERE FIELDS LOAD 3",  STREAMICEBdotTimeDepFile
          CALL READ_REC_XY_RL(STREAMICEBdotTimeDepFile,
     &                        bdot_streamice1,
     &                        nm0,myIter,myThid)
         ENDIF

C-      endif 1rst iter.
        ENDIF

        IF ( first .OR. changed) THEN

#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL

         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx

             streamice_u_normal_stress0(i,j,bi,bj) =
     &        streamice_u_normal_stress1(i,j,bi,bj)
             streamice_v_normal_stress0(i,j,bi,bj) =
     &        streamice_v_normal_stress1(i,j,bi,bj)
             streamice_u_shear_stress0(i,j,bi,bj) =
     &        streamice_u_shear_stress1(i,j,bi,bj)
             streamice_v_shear_stress0(i,j,bi,bj) =
     &        streamice_v_shear_stress1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
!         PRINT *, "GOT HERE FIELDS LOAD 4"

         IF ( STREAMICEuNormalTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEuNormalTimeDepFile,
     &                        streamice_u_normal_stress1,
     &                        nm1,myIter,myThid)
         ENDIF

         IF ( STREAMICEvNormalTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEvNormalTimeDepFile,
     &                        streamice_v_normal_stress1,
     &                        nm1,myIter,myThid)
         ENDIF

         IF ( STREAMICEuShearTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEuShearTimeDepFile,
     &                        streamice_u_shear_stress1,
     &                        nm1,myIter,myThid)
         ENDIF

         IF ( STREAMICEvShearTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEvShearTimeDepFile,
     &                        streamice_v_shear_stress1,
     &                        nm1,myIter,myThid)
         ENDIF

#endif
#ifdef ALLOW_STREAMICE_FLUX_CONTROL

         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx

             u_flux_bdry_SI_0(i,j,bi,bj) =
     &        u_flux_bdry_SI_1(i,j,bi,bj)
             v_flux_bdry_SI_0(i,j,bi,bj) =
     &        v_flux_bdry_SI_1(i,j,bi,bj)

            ENDDO
           ENDDO
          ENDDO
         ENDDO
!         PRINT *, "GOT HERE FIELDS LOAD 4"

         IF ( STREAMICEuFluxTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEuFluxTimeDepFile,
     &                        u_flux_bdry_SI_1,
     &                        nm1,myIter,myThid)
         ENDIF

         IF ( STREAMICEuFluxTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEvFluxTimeDepFile,
     &                        v_flux_bdry_SI_1,
     &                        nm1,myIter,myThid)
         ENDIF

#endif

         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             bdot_streamice0(i,j,bi,bj) =
     &        bdot_streamice1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO

         IF ( STREAMICEBdotTimeDepFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RL(STREAMICEBdotTimeDepFile,
     &                        bdot_streamice1,
     &                        nm1,myIter,myThid)
         ENDIF

C-      endif 1rst iter.
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL
             streamice_u_normal_stress(i,j,bi,bj) =
     &        fac    * streamice_u_normal_stress0(i,j,bi,bj)
     &     + (1-fac) * streamice_u_normal_stress1(i,j,bi,bj)
             streamice_v_normal_stress(i,j,bi,bj) =
     &        fac    * streamice_v_normal_stress0(i,j,bi,bj)
     &     + (1-fac) * streamice_v_normal_stress1(i,j,bi,bj)
             streamice_u_shear_stress(i,j,bi,bj) =
     &        fac    * streamice_u_shear_stress0(i,j,bi,bj)
     &     + (1-fac) * streamice_u_shear_stress1(i,j,bi,bj)
             streamice_v_shear_stress(i,j,bi,bj) =
     &        fac    * streamice_v_shear_stress0(i,j,bi,bj)
     &     + (1-fac) * streamice_v_shear_stress1(i,j,bi,bj)
#endif
#ifdef ALLOW_STREAMICE_FLUX_CONTROL
             u_flux_bdry_pert(i,j,bi,bj) =
     &        fac    * u_flux_bdry_SI_0(i,j,bi,bj)
     &     + (1-fac) * u_flux_bdry_SI_1(i,j,bi,bj)
             v_flux_bdry_pert(i,j,bi,bj) =
     &        fac    * v_flux_bdry_SI_0(i,j,bi,bj)
     &     + (1-fac) * v_flux_bdry_SI_1(i,j,bi,bj)
#endif

             bdot_streamice (i,j,bi,bj) =
     &        fac    * bdot_streamice0(i,j,bi,bj)
     &     + (1-fac) * bdot_streamice1(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL
      CALL EXCH_XY_RL
     & (streamice_v_shear_stress, myThid)
      CALL EXCH_XY_RL
     & (streamice_u_shear_stress, myThid)
      CALL EXCH_XY_RL
     & (streamice_v_normal_stress, myThid)
      CALL EXCH_XY_RL
     & (streamice_u_normal_stress, myThid)
#endif

       _EXCH_XY_RL(bdot_streamice, myThid )
#ifdef ALLOW_STREAMICE_FLUX_CONTROL
      CALL EXCH_XY_RL
     & (u_flux_bdry_pert, myThid)
      CALL EXCH_XY_RL
     & (v_flux_bdry_pert, myThid)
#endif

!      print *, "GOT HERE STREAMICE FIELDS LOAD",
!     &  first,changed,nm0,nm1,fac
#endif
#endif

      RETURN
      END
