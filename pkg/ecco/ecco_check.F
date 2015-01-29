C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_check.F,v 1.22 2015/01/29 17:07:37 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

C--  File ecco_check.F:
C--   Contents
C--   o ECCO_CHECK
C--   o ECCO_CHECK_FILES

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE ECCO_CHECK( myThid )
C     *==========================================================*
C     | SUBROUTINE ECCO_CHECK                                    |
C     | o Check runtime activated packages have been built in.   |
C     *==========================================================*
C     | All packages can be selected/deselected at build time    |
C     | ( when code is compiled ) and activated/deactivated at   |
C     | runtime. This routine does a quick check to trap packages|
C     | that were activated at runtime but that were not compiled|
C     | in at build time.                                        |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco_cost.h"
#endif
#ifdef ALLOW_PROFILES
# include "profiles.h"
#endif

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_GENCOST_CONTRIBUTION
      INTEGER k, igen_etaday
#endif

      _BEGIN_MASTER(myThid)

#if (defined (ALLOW_TRANSPORT_COST_CONTRIBUTION) || \
     defined (ALLOW_NEW_SSH_COST))
      IF ( ndaysrec .GT. maxNumDays ) THEN
       WRITE(msgBuf,'(2A,2I10)')
     &        'ECCO_CHECK: for ALLOW_TRANSPORT_COST_CONTRIBUTION: ',
     &        'ndaysrec > maxNumDays in ecco_cost.h ',
     &        ndaysrec, maxNumDays
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       CALL PRINT_ERROR( msgBuf , myThid )
       STOP 'ABNORMAL END: S/R ECCO_CHECK'
      ENDIF
#endif

#ifdef ALLOW_PROFILES_CONTRIBUTION
      IF ( .NOT.usePROFILES ) THEN
       WRITE(msgBuf,'(3A)') '** WARNING ** S/R ECCO_CHECK: ', 
     &        'ALLOW_PROFILES_CONTRIBUTION requires ',
     &        'netcdf and usePROFILES to be .true.'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
      ENDIF
#endif

cgf next line is only for backward compatibility
      IF ( (using_topex).AND.(.NOT.using_tpj) ) using_tpj=.TRUE.

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
#ifdef ALLOW_TMI_SST_COST_CONTRIBUTION
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

c left for later : slightly different treatment would apply to profiles and gencost

#ifdef ALLOW_GENCOST_CONTRIBUTION
      do k=1,NGENCOST
         if (gencost_pointer3d(k).GT.NGENCOST3D) then
           WRITE(msgBuf,'(2A)')
     &      'ECCO_CHECK: too many 3D cost terms; please',
     &      'increase NGENCOST3D and recompile.'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
           CALL PRINT_ERROR( msgBuf , myThid )
           STOP 'ABNORMAL END: S/R ECCO_CHECK'
         endif

         if ( gencost_datafile(k) .ne. ' ' ) then

           CALL ECCO_CHECK_FILES( using_gencost(k), 'gencost',
     &       gencost_datafile(k), gencost_startdate1(k), myThid )

c altimetry related checks
         if (gencost_name(k).EQ.'sshv4-tp') using_tpj=using_gencost(k)
         if (gencost_name(k).EQ.'sshv4-ers') using_ers=using_gencost(k)
         if (gencost_name(k).EQ.'sshv4-gfo') using_gfo=using_gencost(k)
         if (gencost_name(k).EQ.'sshv4-mdt') using_mdt=using_gencost(k)

c seaice related checks
         if (gencost_name(k).EQ.'siv4-conc')
     &       using_cost_seaice=using_gencost(k)
         if (gencost_name(k).EQ.'siv4-deconc')
     &       using_cost_seaice=using_gencost(k)
         if (gencost_name(k).EQ.'siv4-exconc')
     &       using_cost_seaice=using_gencost(k)
catn-- put stop statement if use old siv4 names:
         if (gencost_name(k).EQ.'siv4-sst') then
           WRITE(msgBuf,'(2A)')
     &      'ECCO_CHECK: OLD seaice gencost_name siv4-sst is retired,',
     &      ' NEW name is siv4-deconc'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
           CALL PRINT_ERROR( msgBuf , myThid )
           STOP 'ABNORMAL END: S/R ECCO_CHECK'
         endif
         if (gencost_name(k).EQ.'siv4-vol') then
           WRITE(msgBuf,'(2A)')
     &      'ECCO_CHECK: OLD seaice gencost_name siv4-vol is retired,',
     &      ' NEW name is siv4-exconc'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
           CALL PRINT_ERROR( msgBuf , myThid )
           STOP 'ABNORMAL END: S/R ECCO_CHECK'
         endif

         endif !if ( gencost_datafile(k) .ne. ' ' ) then
      enddo

c check that one of the used gencost term defines etaday (needed in sshv4)
      IF ( (using_tpj ).OR.(using_ers).OR.(using_gfo)
     &     .OR.(using_mdt) ) using_cost_altim = .TRUE.

      igen_etaday=0
      do k=1,NGENCOST
        if ( (gencost_barfile(k).EQ.'etaday').AND.
     &       (using_gencost(k)) )  igen_etaday=k
      enddo

      if (igen_etaday.EQ.0) then
c warn user as we override using_cost_altim
        using_cost_altim = .FALSE.
        WRITE(msgBuf,'(2A)')
     &    '** WARNING ** S/R ECCO_CHECK: missing file: ',
     &    ' for altimeter data so cost gets switched off'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      else
c print result to screen
        write(msgbuf,'(a,i3)')
     &  'etaday defined by gencost ',igen_etaday
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      endif

#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE ECCO_CHECK_FILES(
     O using_cost_local,
     I localname, localobsfile, localstartdate1,
     I myThid )

C     *==========================================================*
C     | SUBROUTINE ECCO_CHECK_FILES                              |
C     | o Check that obs files are present for specified years.  |
C     |   If not then set using_cost_local to false.             |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ecco.h"
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
