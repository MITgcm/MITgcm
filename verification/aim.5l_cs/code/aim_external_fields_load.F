C $Header: /u/gcmpack/MITgcm/verification/aim.5l_cs/code/Attic/aim_external_fields_load.F,v 1.1 2002/01/09 00:30:44 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE AIM_EXTERNAL_FIELDS_LOAD( 
     I            myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE AIM_EXTERNAL_FIELDS_LOAD                      |
C     | o Control reading of AIM fields from external source.    |
C     |==========================================================|
C     | Loads bottom boundary condition datasets for AIM.        |
C     | The routine is called every timetep and periodically     |
C     | loads a set of external fields.                          |
C     | Monthly climatology files are read for                   |
C     |  Albedo                                                  |
C     |  Soil moisture                                           |
C     |  Land and sea surface temps.                             |
C     | Most of the work is done by the master the thread while  |
C     | the other threads spin.                                  |
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
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
      LOGICAL DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

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
      _RL pfact
      REAL tYear
      INTEGER mnthIndex
      INTEGER prevMnthIndex
      DATA    prevMnthIndex / 0 /
      SAVE    prevMnthIndex
      CHARACTER*17 fNam
      CHARACTER*3 mnthNam(12)
      DATA mnthNam /
     & 'jan', 'feb', 'mar', 'apr', 'may', 'jun',
     & 'jul', 'aug', 'sep', 'oct', 'nov', 'dec' /
      SAVE mnthNam
      LOGICAL firstCall
      DATA    FirstCall /.TRUE./
      SAVE    FirstCall

C     Prevent loading of new data before everyone has finished with it
      CALL BAR2(myThid) 

C     Master thread coordinates loading of AIM datasets
      _BEGIN_MASTER( myThid )

C       Calculate offset into a year
        tYear = myTime/(86400.*360.) -
     &  FLOAT(INT(myTime/(86400.*360.)))
        mnthIndex = INT(tYear*12.)+1
        IF ( mnthIndex .NE. prevMnthIndex .OR.
     &     FirstCall ) THEN
C        New month so load in data
         prevMnthIndex = mnthIndex
C        o Albedo ( convert % to fraction )
         WRITE(fNam,'(A,A,A)' ) 'salb.',mnthNam(mnthIndex),'.cs.bin'
         CALL MDSREADFIELD(fNam(1:15),readBinaryPrec,'RS',1,
     O    aim_albedo,  
     I    1,myThid)

C        o Surface temperature ( in kelvin )
         WRITE(fNam,'(A,A,A)' ) 'stheta.',mnthNam(mnthIndex),'.cs.bin'
c        WRITE(fNam,'(A,A,A)' ) 'tsurf.',mnthNam(mnthIndex),'.cs.bin'
         CALL MDSREADFIELD(fNam(1:17),readBinaryPrec,'RS',1,
     O    aim_surftemp,  
     I    1,myThid)

C        o Soil moisture 
         WRITE(fNam,'(A,A,A)' ) 'smoist.',mnthNam(mnthIndex),'.cs.bin'
         CALL MDSREADFIELD(fNam(1:17),readBinaryPrec,'RS',1,
     O    aim_soilMoisture,  
     I    1,myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--  Converts fields for direct use in Atmos. Physics routine.
C     better here rather than in "aim_do_atmos" since:
C     a) change together conversion factor and input file name.
C     b) conversion applied only 1 time / month ; 
C     c) easy to check here (variable in common).

         DO bj=1,nSy
          DO bi=1,nSx

C-  Converts surface albedo : input data is in % 0-100
C     and Francos package needs a fraction between 0-1
           DO j=1,sNy
            DO i=1,sNx
             aim_albedo(I,J,bi,bj) = aim_albedo(I,J,bi,bj)/100.
            ENDDO
           ENDDO

C-  Converts soil moisture (case input is in cm in bucket of depth 20cm.)
c          DO j=1,sNy
c           DO i=1,sNx
c            aim_soilMoisture(I,J,bi,bj) = aim_soilMoisture(I,J,bi,bj)
c    &                                   /20.
c           ENDDO
c          ENDDO
           
C-  Converts surface potential temp. to in-situ temperature :
           DO j=1,sNy
            DO i=1,sNx
             pfact = (Ro_surf(i,j,bi,bj)/atm_po)**atm_kappa
             aim_surftemp(i,j,bi,bj) = aim_surftemp(i,j,bi,bj) 
     &                               * pfact
            ENDDO
           ENDDO

C-- end bi,bj loops
          ENDDO
         ENDDO

         IF (FirstCall) 
     &     CALL WRITE_FLD_XY_RL('aim_Tsurf',' ',aim_surftemp,0,myThid)     

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        ENDIF

        FirstCall = .FALSE.
      _END_MASTER( myThid   )

C     Stop anyone leaving until all data is read
      CALL BAR2(myThid) 

#endif /* ALLOW_AIM */

      RETURN 
      END
