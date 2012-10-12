C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_check.F,v 1.13 2012/10/12 12:54:56 heimbach Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

      SUBROUTINE ECCO_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE ECCO_CHECK                                    |
C     | o Check runtime activated packages have been built in.   |
C     |==========================================================|
C     | All packages can be selected/deselected at build time    |
C     | ( when code is compiled ) and activated/deactivated at   |
C     | runtime. This routine does a quick check to trap packages|
C     | that were activated at runtime but that were not compiled|
C     | in at build time.                                        |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "ecco_cost.h"
#include "ctrl.h"
#include "cal.h"
#include "optim.h"
#ifdef ALLOW_PROFILES
#include "profiles.h"
#endif

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_GENCOST_CONTRIBUTION
      INTEGER k
#endif

      _BEGIN_MASTER(myThid)

#if (defined (ALLOW_TRANSPORT_COST_CONTRIBUTION) || \
     defined (ALLOW_NEW_SSH_COST))
      IF ( ndaysrec .GT. maxNumDays ) THEN
       WRITE(msgBuf,'(2A,2I10)')
     &        'ECCO_CHECK: for ALLOW_TRANSPORT_COST_CONTRIBUTION: ',
     &        'ndaysrec > maxNumDays in ecco_cost.h ',
     &        ndaysrec, maxNumDays
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       CALL PRINT_ERROR( msgBuf , myThid )
       STOP 'ABNORMAL END: S/R ECCO_CHECK'
      ENDIF
#endif

#ifdef ALLOW_PROFILES_CONTRIBUTION
      IF ( .NOT.usePROFILES ) THEN
       WRITE(msgBuf,'(2A)')
     &        'ECCO_CHECK: for ALLOW_PROFILES_CONTRIBUTION ',
     &        'requires usePROFILES to be .true.'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       CALL PRINT_ERROR( msgBuf , myThid )
       STOP 'ABNORMAL END: S/R ECCO_CHECK'
      ENDIF
#endif

      _END_MASTER(myThid)

c check for missing data files
#ifdef ALLOW_BP_COST_CONTRIBUTION    
      CALL ECCO_CHECK_FILES( using_cost_bp,'bp',
     & bpdatfile, bpstartdate(1), myThid )
#endif
#ifdef ALLOW_SST_COST_CONTRIBUTION    
      CALL ECCO_CHECK_FILES( using_cost_sst,'sst',
     & sstdatfile, sststartdate(1), myThid )
#endif
#ifdef ALLOW_TMI_COST_CONTRIBUTION
      CALL ECCO_CHECK_FILES( using_cost_sst,'sst',
     & tmidatfile, tmistartdate(1), myThid )
#endif
#if (defined (ALLOW_SCAT_COST_CONTRIBUTION) || \
     defined (ALLOW_DAILYSCAT_COST_CONTRIBUTION) )
      CALL ECCO_CHECK_FILES( using_cost_scat,'scat',
     & scatxdatfile, scatxstartdate(1), myThid )
      CALL ECCO_CHECK_FILES( using_cost_scat,'scat',
     & scatydatfile, scatystartdate(1), myThid )
#endif
#ifdef ALLOW_SSH_COST_CONTRIBUTION
      IF ( using_topex ) CALL ECCO_CHECK_FILES( using_topex,
     & 'altim', topexfile, topexstartdate(1), myThid )
      IF ( using_ers ) CALL ECCO_CHECK_FILES( using_ers,
     & 'altim', ersfile, ersstartdate(1), myThid )
      IF ( using_gfo ) CALL ECCO_CHECK_FILES( using_gfo,
     & 'altim', gfofile, gfostartdate(1), myThid )
c
      IF ( (.NOT.using_topex ).AND.(.NOT.using_ers)
     &     .AND.(.NOT.using_gfo) ) THEN
c warn user as we override using_cost_altim
             WRITE(msgBuf,'(2A)')
     &       '** WARNING ** S/R ECCO_CHECK: missing file: ',
     &       ' for altimeter data so cost gets switched off'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
c switch off cost function term
             using_cost_altim = .FALSE.
      ENDIF
#endif
#ifdef ALLOW_SSH_MEAN_COST_CONTRIBUTION
      CALL ECCO_CHECK_FILES( using_cost_altim,
     & 'mdt', topexmeanfile, modelstartdate(1), myThid ) 
#endif

c left for later : slightly different treatment would apply to profiles and gencost

#ifdef ALLOW_GENCOST_CONTRIBUTION
      do k=1,NGENCOST
         if ( gencost_datafile(k) .ne. ' ' ) then
           CALL ECCO_CHECK_FILES( using_gencost(k), 'gencost',
     &       gencost_datafile(k), modelstartdate(1), myThid )
         endif
      enddo
#endif

      RETURN
      END


      SUBROUTINE ECCO_CHECK_FILES( 
     O using_cost_local, 
     I localname, localobsfile, localstartdate1, 
     I myThid )
     
C     /==========================================================\
C     | SUBROUTINE ECCO_CHECK_FILES                              |
C     | o Check that obs files are present for specified years.  |
C     |   If not then set using_cost_local to false.             |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ecco_cost.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid
      LOGICAL using_cost_local
      character*(*) localname
      character*(MAX_LEN_FNAM) localobsfile
      integer localstartdate1
      
C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER irec, mody, modm, yday, locy, il
      LOGICAL exst, singleFileTest, yearlyFileTest
      character*(128) fname

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c left for later : refine test accounting for localstartdate1

#ifdef ALLOW_CAL

      _BEGIN_MASTER(myThid)

      IF ( (using_cost_local).AND.(localobsfile.EQ.' ') ) THEN
c warn user as we override using_cost_local
             WRITE(msgBuf,'(4A)')
     &       '** WARNING ** ECCO_CHECK_FILES: missing file',
     &       ' definition so ',localname,' gets switched off'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
c switch off cost function term
             using_cost_local = .FALSE.
      ENDIF

      singleFileTest = .FALSE.
      IF (using_cost_local) THEN
           inquire( file=localobsfile, exist=exst )
           IF ( exst ) singleFileTest=.TRUE.
      ENDIF

      yearlyFileTest = .FALSE.
      IF ( (using_cost_local).AND.(.NOT.singleFileTest) ) THEN 
        DO irec = 1, nmonsrec
           mody   = modelstartdate(1)/10000
           modm   = modelstartdate(1)/100 - mody*100
           yday   = mody + INT((modm-1+irec-1)/12)

           locy   = localstartdate1/10000

           il=ilnblnk(localobsfile)
           write(fname(1:128),'(2a,i4)')
     &       localobsfile(1:il), '_', yday
           inquire( file=fname, exist=exst )

           IF ( (.NOT.exst).AND.(yday.GE.locy) ) THEN
c warn user as we override using_cost_local
             WRITE(msgBuf,'(5A)')
     &       '** WARNING ** ECCO_CHECK_FILES: missing',fname,
     &       ' so ',localname,' gets switched off'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
c switch off cost function term
             using_cost_local = .FALSE.
           ELSEIF ( (exst).AND.(yday.GE.locy) ) THEN
             yearlyFileTest = .TRUE.
           ENDIF
        ENDDO
      ENDIF

      IF (using_cost_local) THEN
      IF ( (.NOT.yearlyFileTest).AND.(.NOT.singleFileTest) ) THEN
c warn user as we override using_cost_local
             WRITE(msgBuf,'(4A)')
     &       '** WARNING ** ECCO_CHECK_FILES: no data ',
     &       ' so ',localname,' gets switched off'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
c switch off cost function term
             using_cost_local = .FALSE.
      ENDIF
      ENDIF
      
      _END_MASTER(myThid)

#endif /* ALLOW_CAL */

      RETURN
      END



