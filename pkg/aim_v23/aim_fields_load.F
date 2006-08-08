C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_fields_load.F,v 1.5 2006/08/08 03:16:11 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_FIELDS_LOAD(
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
C     | Most of the work is done by the master the thread while
C     | the other threads spin.
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "AIM_PARAMS.h"
c #include "AIM_GRID.h"
#include "AIM_FFIELDS.h"

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation time
C     myIter - Simulation timestep number
      INTEGER myThid
      _RL     myTime
      INTEGER myIter
CEndOfInterface

C     === Functions ===

#ifdef ALLOW_AIM
C     === Local variables ===
C     bi,bj, i,j  - Loop counters
C     tYear       - Fraction within year of myTime
C     mnthIndex   - Current time in whole months
C     prevMnthIndex
C     fNam        - Strings used in constructing file names
C     mnthNam
C     pfact       - used to convert Pot.Temp. to in-situ Temp.
      INTEGER bi,bj, i, j
c     _RL pfact
      INTEGER prevMnthIndex
      COMMON / LOCAL_AIM_FIELDS_LOAD / prevMnthIndex
c     DATA    prevMnthIndex / 0 /
c     SAVE    prevMnthIndex
C--   for use with useMMsurfFc:
      _RL tYear, yearLength
      INTEGER mnthIndex
      CHARACTER*(MAX_LEN_FNAM) fNam
      CHARACTER*3 mnthNam(12)
      DATA mnthNam /
     & 'jan', 'feb', 'mar', 'apr', 'may', 'jun',
     & 'jul', 'aug', 'sep', 'oct', 'nov', 'dec' /
      SAVE mnthNam
C--   for use with useFMsurfBC:
      INTEGER aim_surfForc_NppCycle, nm0, nm1, nm2, nm3
      _RL aim_surfForc_TimePeriod, aim_surfForc_TransRatio
      _RL t0prd, tNcyc, tmprd, dTprd

C-    Initialise local common block
      IF ( myIter.EQ.nIter0 ) prevMnthIndex = 0

      IF (aim_useMMsurfFc) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Use Monthly Mean surface forcing fields:

C-    Calculate offset into a year
c      tYear = myTime/(86400.*360.) -
c    &  FLOAT(INT(myTime/(86400.*360.)))
       yearLength = 86400.*360.
       tYear = MOD(myTime/yearLength, 1. _d 0)
       mnthIndex = INT(tYear*12.)+1

       IF ( mnthIndex .NE. prevMnthIndex .OR.
     &      myIter.EQ.nIter0 ) THEN
C-    New month so load in data

C     Prevent loading of new data before everyone has finished with it
         CALL BAR2(myThid)

         _BEGIN_MASTER( myThid )
         prevMnthIndex = mnthIndex
         _END_MASTER( myThid   )

C     Master thread coordinates loading of AIM datasets
C  note: with some I/O option (e.g.: SingleCpuIO) READ S/R contains
C        multi-threaded tasks => let all threads call the READ S/R ;
C        ( MASTER directives are inside S/R READ to do master-thread I/O)

C        o Albedo ( convert % to fraction )
         WRITE(fNam,'(A,A,A)' ) 'salb.',
     &         mnthNam(mnthIndex), aim_MMsufx(1:aim_MMsufxLength)
         CALL READ_REC_XY_RS( fNam, aim_albedo,   1, myIter, myThid )

C        o Surface temperature ( in kelvin )
         IF (aim_surfPotTemp) THEN
          WRITE(fNam,'(A,A,A)' )'stheta.',
     &         mnthNam(mnthIndex), aim_MMsufx(1:aim_MMsufxLength)
         ELSE
          WRITE(fNam,'(A,A,A)' )'sTemp.',
     &         mnthNam(mnthIndex), aim_MMsufx(1:aim_MMsufxLength)
         ENDIF
         CALL READ_REC_XY_RS( fNam, aim_surftemp, 1, myIter, myThid )

C        o Soil moisture
         WRITE(fNam,'(A,A,A)' ) 'smoist.',
     &         mnthNam(mnthIndex), aim_MMsufx(1:aim_MMsufxLength)
         CALL READ_REC_XY_RS( fNam, aim_soilWater,1, myIter, myThid )

C     Stop anyone leaving until all data is read
         CALL BAR2(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--  Converts fields for direct use in Atmos. Physics routine.
C     better here rather than in "aim_do_atmos" since:
C     a) change together conversion factor and input file name.
C     b) conversion applied only 1 time / month ;
C     c) easy to check here (variable in common).

         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)

C-  Converts surface albedo : input data is in % 0-100
C     and Franco s package needs a fraction between 0-1
           DO j=1,sNy
            DO i=1,sNx
             aim_albedo(I,J,bi,bj) = aim_albedo(I,J,bi,bj)/100.
            ENDDO
           ENDDO

C-  Converts soil moisture (case input is in cm in bucket of depth 20cm.)
c          DO j=1,sNy
c           DO i=1,sNx
c            aim_soilWater(I,J,bi,bj) = aim_soilWater(I,J,bi,bj)
c    &                                   /20.
c           ENDDO
c          ENDDO

C--   Correct for truncation (because of hFacMin) of surface reference
C      pressure Ro_surf that affects Surf.Temp. :
           DO j=1,sNy
            DO i=1,sNx
c            pfact = (Ro_surf(i,j,bi,bj)/atm_Po)**atm_kappa
             aim_surftemp(i,j,bi,bj) = aim_surftemp(i,j,bi,bj)
     &                               * truncSurfP(i,j,bi,bj)
            ENDDO
           ENDDO

C-- end bi,bj loops
          ENDDO
         ENDDO

         IF (myIter.EQ.nIter0) THEN
          CALL BAR2(myThid)
          CALL WRITE_FLD_XY_RL('aim_Tsurf',' ',aim_surfTemp,0,myThid)
         ENDIF


C-     endif New month - load in data
       ENDIF

      ELSEIF (aim_useFMsurfBC) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Use Franco Molteni surface BC:
C    take part of S/R INFORC + part of S/R FORDATE (albedo)
C    of the F.Molteni SPEEDY code (ver23)

       IF ( myIter.EQ.nIter0 ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
c           aim_landFr(i,j,bi,bj)= 0.
            aim_albedo(i,j,bi,bj)= 0.
            aim_veget(i,j,bi,bj) = 0.
            aim_sst0(i,j,bi,bj) =300.
            aim_lst0(i,j,bi,bj) =300.
            aim_oic0(i,j,bi,bj) = 0.
            aim_snw0(i,j,bi,bj) = 0.
            aim_sw10(i,j,bi,bj) = 0.
            aim_sw20(i,j,bi,bj) = 0.
            aim_sst1(i,j,bi,bj) =300.
            aim_lst1(i,j,bi,bj) =300.
            aim_oic1(i,j,bi,bj) = 0.
            aim_snw1(i,j,bi,bj) = 0.
            aim_sw11(i,j,bi,bj) = 0.
            aim_sw21(i,j,bi,bj) = 0.
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

C-     Length (s) of one time period:
       aim_surfForc_TimePeriod = 30.*86400.
C-     Number of time period per Cycle:
       aim_surfForc_NppCycle   = 12
C-     define how fast the (linear) transition is from one month to the next
C       = 1                 -> linear between 2 midle month
C       > TimePeriod/deltaT -> jump from one month to the next one
       aim_surfForc_TransRatio = 1.

       t0prd = myTime / aim_surfForc_TimePeriod
       tNcyc = aim_surfForc_NppCycle
       tmprd = t0prd - 0.5 _d 0 + tNcyc
       tmprd = MOD(tmprd,tNcyc)
C-     indices of previous month and next month:
       nm0 = 1 + INT(tmprd)
       nm1 = 1 + MOD(nm0,aim_surfForc_NppCycle)
C-     interpolation weight:
       dTprd = tmprd - (nm0 - 1)
       aim_sWght1 = 0.5 _d 0+(dTprd-0.5 _d 0)*aim_surfForc_TransRatio
       aim_sWght1 = MAX( 0. _d 0, MIN(1. _d 0, aim_sWght1) )
       aim_sWght0 = 1. _d 0 - aim_sWght1


       IF (myIter.EQ.nIter0 .OR. nm0.NE.prevMnthIndex) THEN
C-     Load new data:

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Prevent loading of new data before everyone has finished with it
        CALL BAR2(myThid)

        _BEGIN_MASTER( myThid )
C-      only one thread updates parameter in common block
        prevMnthIndex = nm0
        _END_MASTER( myThid )

C     Master thread coordinates loading of AIM datasets
C     note: with some I/O option (e.g.: SingleCpuIO) READ S/R contains
C        multi-threaded tasks => let all threads call the READ S/R ;
C        ( MASTER directives are inside S/R READ to do master-thread I/O)

        IF ( myIter.EQ.nIter0 ) THEN
C-      Load Fixed Forcing only at the 1rst iter:

c        IF ( aim_LandFile .NE. ' '  ) THEN
c         CALL READ_REC_XY_RS(aim_LandFile,aim_landFr,1,myIter,myThid)
c        ENDIF
         IF ( aim_albFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS(aim_albFile,aim_albedo, 1,myIter,myThid)
         ENDIF
c           alb0(i,j) = 0.01*r4inp(i,j)
         IF ( aim_vegFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS(aim_vegFile,aim_veget,1,myIter,myThid)
         ENDIF

C-      endif 1rst iter.
        ENDIF

        IF ( aim_sstFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS(aim_sstFile,aim_sst0,nm0,myIter,myThid)
          CALL READ_REC_XY_RS(aim_sstFile,aim_sst1,nm1,myIter,myThid)
        ENDIF
        IF ( aim_lstFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS(aim_lstFile,aim_lst0,nm0,myIter,myThid)
          CALL READ_REC_XY_RS(aim_lstFile,aim_lst1,nm1,myIter,myThid)
        ENDIF
        IF ( aim_oiceFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS(aim_oiceFile,aim_oic0,nm0,myIter,myThid)
          CALL READ_REC_XY_RS(aim_oiceFile,aim_oic1,nm1,myIter,myThid)
        ENDIF
        IF ( aim_snowFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS(aim_snowFile,aim_snw0,nm0,myIter,myThid)
          CALL READ_REC_XY_RS(aim_snowFile,aim_snw1,nm1,myIter,myThid)
        ENDIF
        IF ( aim_swcFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS(aim_swcFile,aim_sw10,nm0,myIter,myThid)
          CALL READ_REC_XY_RS(aim_swcFile,aim_sw11,nm1,myIter,myThid)
          nm2 = nm0 + aim_surfForc_NppCycle
          nm3 = nm1 + aim_surfForc_NppCycle
          CALL READ_REC_XY_RS(aim_swcFile,aim_sw20,nm2,myIter,myThid)
          CALL READ_REC_XY_RS(aim_swcFile,aim_sw21,nm3,myIter,myThid)
        ENDIF

C     Stop anyone leaving until all data is read
        CALL BAR2(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)

          IF ( myIter.EQ.nIter0 ) THEN
C-  Converts surface albedo : from % (input data) to a fraction [0-1}
c          alb0(i,j) = 0.01*r4inp(i,j)
           DO j=1,sNy
            DO i=1,sNx
             aim_albedo(i,j,bi,bj) = aim_albedo(I,J,bi,bj)/100. _d 0
            ENDDO
           ENDDO

C-  Converts vegetation fraction: from % (input data) to a fraction [0-1]
c          veg(i,j)=max(0.,0.01*veg(i,j))
           DO j=1,sNy
            DO i=1,sNx
             aim_veget(i,j,bi,bj) =
     &                 MAX(0. _d 0, aim_veget(i,j,bi,bj)/100. _d 0)
            ENDDO
           ENDDO
C-      endif 1rst iter.
          ENDIF

C--   Correct for truncation (because of hFacMin) of surface reference
C      pressure Ro_surf that affects Surf.Temp. :
          DO j=1,sNy
           DO i=1,sNx
c            pfact = (Ro_surf(i,j,bi,bj)/atm_Po)**atm_kappa
             aim_lst0(i,j,bi,bj) = aim_lst0(i,j,bi,bj)
     &                               * truncSurfP(i,j,bi,bj)
             aim_lst1(i,j,bi,bj) = aim_lst1(i,j,bi,bj)
     &                               * truncSurfP(i,j,bi,bj)
           ENDDO
          ENDDO

C-     end bi,bj loops
         ENDDO
        ENDDO

C--    endif load new data.
       ENDIF

      ENDIF

#endif /* ALLOW_AIM */

      RETURN
      END
