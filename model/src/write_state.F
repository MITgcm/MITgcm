C $Header: /u/gcmpack/MITgcm/model/src/write_state.F,v 1.39 2004/05/12 17:12:53 adcroft Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#undef  MULTIPLE_RECORD_STATE_FILES

CBOP
C     !ROUTINE: WRITE_STATE 
C     !INTERFACE:
      SUBROUTINE WRITE_STATE ( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE WRITE_STATE                                    
C     | o Controlling routine for IO to dump model state.         
C     *==========================================================*
C     | Write model state files for post-processing. This file   
C     | includes code for diagnosing W and RHO for output.       
C     | File write_state.F: Routines to handle mid-level I/O
C     | interface.
C     | o WRITE_STATE - Write out model state.
C     | The following CPP flag (MULTIPLE_RECORD_STATE_FILES) is
C     | #define/#undefed here since it is specific to this routine
C     | and very user-preference specific.
C     |
C     | If #undefed (default) the state files are written as in all versions
C     | prior to checkpoint32, where a file is created per variable, per time
C     | and per tile. This *has* to be the default because most users use this
C     | mode and all utilities and scripts (diagnostic) assume this form.
C     | It is also robust, as explained below.
C     |
C     | If #defined, subsequent snap-shots are written as records in the
C     | same file (no iteration number in filenames).
C     | Advantages: - fewer files
C     |       - for small problems, is easy to copy the output around
C     | Disadvantages:
C     |       - breaks a lot of diagnostic scripts
C     |       - for large or long problems this creates huge files
C     |       - is an unexpected, unsolicited change in behaviour which came
C     |         as a surprise (in c32) and inconvenience to several users
C     |       - can not accomodate changing the frequency of output
C     |         after a pickup (this is trivial in previous method
C     |         but needs new code and parameters in this new method)
C     | Known Bugs:
C     |       - if the length of integration is not exactly an integer
C     |         times the output frequency then the last record written
C     |         (at end of integration) overwrites a previously written
C     |         record corresponding to an earier time. *BE WARNED*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h" 
#include "EOS.h"
#ifdef ALLOW_PASSIVE_TRACER
#include "TR1.h"
#endif
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      _RL     myTime
      INTEGER myThid
      INTEGER myIter

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) suff
      INTEGER iRec
#ifdef ALLOW_MNC
      _RL mnc_iter
#endif
CEOP

      IF ( 
     &     DIFFERENT_MULTIPLE(dumpFreq,myTime,
     &     myTime-deltaTClock) .OR. 
     &     myTime.EQ.endTime .OR.
     &     myTime.EQ.startTime
     &     ) THEN
         IF ( dumpFreq .EQ. 0.0 ) THEN
            iRec = 1
         ELSE
            iRec = int ( (myTime-startTime) / dumpFreq +1.5 )
         ENDIF
         
C--   Going to really do some IO. Make everyone except master thread wait.
         _BARRIER
         _BEGIN_MASTER( myThid )

C--     Write model fields

#ifdef MULTIPLE_RECORD_STATE_FILES

C       Write each snap-shot as a new record in one file per variable
C       - creates relatively few files but these files can become huge
        CALL WRITE_REC_XYZ_RL( 'U',  uVel,iRec,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'V',  vVel,iRec,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'T', theta,iRec,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'S',  salt,iRec,myIter,myThid)
        CALL WRITE_REC_XY_RL('Eta',etaN,iRec,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'W',wVel,iRec,myIter,myThid)
#ifdef ALLOW_PASSIVE_TRACER
ceh3 needs an IF ( usePASSIVE_TRACER ) THEN
        CALL WRITE_REC_XYZ_RL( 'TR1',tr1,iRec,myIter,myThid)
#endif /* ALLOW_PASSIVE_TRACER */
#ifdef ALLOW_NONHYDROSTATIC
        IF (nonHydroStatic) THEN
         CALL WRITE_REC_XYZ_RL( 'PNH',phi_nh,iRec,myIter,myThid)
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef NONLIN_FRSURF
c       CALL WRITE_REC_XYZ_RS('hFacC.',hFacC,iRec,myIter,myThid)
c       CALL WRITE_REC_XYZ_RS('hFacW.',hFacW,iRec,myIter,myThid)
c       CALL WRITE_REC_XYZ_RS('hFacS.',hFacS,iRec,myIter,myThid)
#endif /* NONLIN_FRSURF */

#else /* MULTIPLE_RECORD_STATE_FILES */

C       Write each snap-shot as a new file (original and default method)
C       - creates many files but for large configurations is easier to
C         transfer analyse a particular snap-shots
        WRITE(suff,'(I10.10)') myIter
        CALL WRITE_FLD_XYZ_RL( 'U.',suff,uVel,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL( 'V.',suff,vVel,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL( 'T.',suff,theta,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL( 'S.',suff,salt,myIter,myThid)
        CALL WRITE_FLD_XY_RL('Eta.',suff,etaN,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL( 'W.',suff,wVel,myIter,myThid)
        IF ( useDynP_inEos_Zc .OR. myIter.NE.nIter0 )
     &  CALL WRITE_FLD_XYZ_RL( 'PH.',suff,totPhiHyd,myIter,myThid)
        IF (buoyancyRelation.NE.'ATMOSPHERIC' .AND. myIter.NE.nIter0)
     &   CALL WRITE_FLD_XY_RL('PHL.',suff,phiHydLow,myIter,myThid)
#ifdef ALLOW_PASSIVE_TRACER
ceh3 needs an IF ( usePASSIVE_TRACER ) THEN
        CALL WRITE_FLD_XYZ_RL( 'TR1.',suff,tr1,myIter,myThid)
#endif
#ifdef ALLOW_NONHYDROSTATIC
        IF (nonHydroStatic) THEN
         CALL WRITE_FLD_XYZ_RL( 'PNH.',suff,phi_nh,myIter,myThid)
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef NONLIN_FRSURF
c       CALL WRITE_FLD_XYZ_RS('hFacC.',suff,hFacC,myIter,myThid)
c       CALL WRITE_FLD_XYZ_RS('hFacW.',suff,hFacW,myIter,myThid)
c       CALL WRITE_FLD_XYZ_RS('hFacS.',suff,hFacS,myIter,myThid)
#endif /* NONLIN_FRSURF */

#endif /* MULTIPLE_RECORD_STATE_FILES */

#ifdef ALLOW_PTRACERS
       IF (usePTRACERS) CALL PTRACERS_WRITE_STATE(myIter,myTime,myThid)
#endif /* ALLOW_PTRACERS */
         _END_MASTER( myThid )
         _BARRIER

#ifdef ALLOW_MNC
      IF (useMNC) THEN
C       Write dynvars using the MNC package
        mnc_iter = myIter
        CALL MNC_CW_SET_UDIM('state', -1, myThid)
        CALL MNC_CW_RL_W('D','state',0,0,'iter',mnc_iter, myThid)
        CALL MNC_CW_SET_UDIM('state', 0, myThid)
        CALL MNC_CW_RL_W('D','state',0,0,'model_time',myTime, myThid)
        CALL MNC_CW_RL_W('D','state',0,0,'U', uVel, myThid)
        CALL MNC_CW_RL_W('D','state',0,0,'V', vVel, myThid)
        CALL MNC_CW_RL_W('D','state',0,0,'T', theta, myThid)
        CALL MNC_CW_RL_W('D','state',0,0,'S', salt, myThid)
        CALL MNC_CW_RL_W('D','state',0,0,'Eta', etaN, myThid)
        CALL MNC_CW_RL_W('D','state',0,0,'W', wVel, myThid)
        IF ( useDynP_inEos_Zc .OR. myIter.NE.nIter0 ) THEN
          CALL MNC_CW_SET_UDIM('totPhiHyd', -1, myThid)
          CALL MNC_CW_RL_W('R','totPhiHyd',0,0,'iter',mnc_iter,myThid)
          CALL MNC_CW_SET_UDIM('totPhiHyd', 0, myThid)
          CALL MNC_CW_RL_W('D','totPhiHyd',0,0,'totPhiHyd',
     &         totPhiHyd, myThid)
        ENDIF
        IF ((buoyancyRelation .NE. 'ATMOSPHERIC')
     &       .AND. (myIter .NE. nIter0)) THEN
          CALL MNC_CW_SET_UDIM('phiHydLow', -1, myThid)
          CALL MNC_CW_RL_W('D','phiHydLow',0,0,'iter',mnc_iter,myThid)
          CALL MNC_CW_SET_UDIM('phiHydLow', 0, myThid)
          CALL MNC_CW_RL_W('D','phiHydLow',0,0,'phiHydLow',
     &         phiHydLow, myThid)
        ENDIF
#ifdef ALLOW_PASSIVE_TRACER
        CALL MNC_CW_RL_W('D','state',0,0,'tr1', tr1, myThid)
#endif /*  ALLOW_PASSIVE_TRACER  */
#ifdef ALLOW_NONHYDROSTATIC
        IF (nonHydroStatic) THEN
          CALL MNC_CW_RL_W('D','state',0,0,'phi_nh', phi_nh, myThid)
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
      ENDIF
#endif /*  ALLOW_MNC  */

      ENDIF

      RETURN
      END
