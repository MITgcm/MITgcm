C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_external_fields_load.F,v 1.2 2001/06/18 17:39:58 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
 
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
C     I, J  - Loop counters
C     tYear - Fraction within year of myTime
C     mnthIndex     - Current time in whole months
C     prevMnthIndex
C     fNam          - Strings used in constructing file names
C     mnthNam
      INTEGER I, J
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
         WRITE(fNam,'(A,A,A)' ) 'tsurf.',mnthNam(mnthIndex),'.cs.bin'
         CALL MDSREADFIELD(fNam(1:16),readBinaryPrec,'RS',1,
     O    aim_surftemp,  
     I    1,myThid)

C        o Soil moisture ( convert to 20cm bucket fraction )
         WRITE(fNam,'(A,A,A)' ) 'smoist.',mnthNam(mnthIndex),'.cs.bin'
         CALL MDSREADFIELD(fNam(1:17),readBinaryPrec,'RS',1,
     O    aim_soilMoisture,  
     I    1,myThid)

        ENDIF

        FirstCall = .FALSE.
      _END_MASTER( myThid   )

C     Stop anyone leaving until all data is read
      CALL BAR2(myThid) 

#endif /* ALLOW_AIM */

      RETURN 
      END
