#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

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
#include "GRID.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco_cost.h"
#endif

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_GENCOST_CONTRIBUTION
      INTEGER igen_etaday, il, k2, ioUnit
      LOGICAL exst
      CHARACTER*(128) tempfile,tempfile1
      INTEGER icount_transp
#endif

      INTEGER bi, bj, i, j, k
      integer nRetired

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          do k = 1,nr
            do j = 1-OLy,sNy+OLy
              do i = 1-OLx,sNx+OLx
                eccoVol_0(i,j,k,bi,bj)=
     &          hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
              enddo
            enddo
          enddo
        enddo
      enddo

      _BEGIN_MASTER(myThid)

c ============ retired compile option checks

        nRetired              =      0

#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_MEAN_HFLUX_COST_CONTRIBUTION has no',
     &      'effect since cost_mean_heatflux has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_MEAN_HFLUX_COST_CONTRIBUTION has no',
     &      'effect since cost_mean_saltflux has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_NEW_SSH_COST
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_NEW_SSH_COST has no',
     &      'effect since cost_ssh_new has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_CURMTR_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_CURMTR_COST_CONTRIBUTION has no',
     &      'effect since cost_curmtr has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_DRIFTER_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_DRIFTER_COST_CONTRIBUTION has no',
     &      'effect since cost_drifter has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_SCAT_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_SCAT_COST_CONTRIBUTION has no',
     &      'effect since cost_scat etc has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_DAILYSCAT_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_DAILYSCAT_COST_CONTRIBUTION has no',
     &      'effect since cost_scat etc has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_DRIFT_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_DRIFT_COST_CONTRIBUTION has no',
     &      'effect since cost_drift has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_DRIFTW_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_DRIFTW_COST_CONTRIBUTION has no',
     &      'effect since cost_driftw has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_COST_INI_FIN
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_COST_INI_FIN has no',
     &      'effect since cost_theta_ini_fin etc has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_COST_TRANSPORT
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_COST_TRANSPORT has no',
     &      'effect since cost_trans_merid etc has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_COST_ATLANTIC
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_COST_ATLANTIC has no',
     &      'effect since cost_atlantic has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_TRANSPORT_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_TRANSPORT_COST_CONTRIBUTION has no',
     &      'effect since cost_gen_transport has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_EGM96_ERROR_COV
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_EGM96_ERROR_COV has no',
     &      'effect since cost_geoid etc has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_IESTAU_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_IESTAU_COST_CONTRIBUTION has no',
     &      'effect since cost_ies etc has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_SIGMAR_COST_CONTRIBUTION has no',
     &      'effect since cost_sigmar has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif
#ifdef ALLOW_EDDYPSI_COST_CONTRIBUTION
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'ALLOW_EDDYPSI_COST_CONTRIBUTION has no',
     &      'effect since cost_tau_eddy has been retired'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       nRetired = nRetired + 1
#endif

      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(2A)') 'S/R ECCO_CHECK: ',
     &  '  retired compile-time options need to be undefined'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R ECCO_CHECK'
      ENDIF

c ============ retired run-time parameter checks

        nRetired              =      0

#ifdef ALLOW_GENCOST_CONTRIBUTION
      do k=1,NGENCOST
       IF (gencost_scalefile(k).NE.' ') THEN
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'gencost_scalefile has been retired; ',
     &      'gencost_posproc_c should now be used instead'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
           nRetired = nRetired + 1
       ENDIF
       IF (gencost_smooth2Ddiffnbt(k).NE.0) THEN
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'gencost_smooth2Ddiffnbt has been retired; ',
     &      'gencost_posproc_i should now be used instead'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
           nRetired = nRetired + 1
       ENDIF
       IF (gencost_timevaryweight(k)) THEN
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'gencost_timevaryweight has been retired; ',
     &      'gencost_posproc should now be used instead'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
           nRetired = nRetired + 1
       ENDIF
       IF (gencost_nrecperiod(k).NE.0) THEN
           WRITE(msgBuf,'(3A)') '** WARNING ** ECCO_CHECK:',
     &      'gencost_nrecperiod has been retired; ',
     &      'gencost_posproc clim should now be used instead'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
           nRetired = nRetired + 1
       ENDIF

      ENDDO
#endif

      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(2A)') 'S/R ECCO_CHECK: ',
     &  '  retired run-time options were found in data.ecco'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R ECCO_CHECK'
      ENDIF

c ============ backward compatibility checks
      IF ( (using_topex).AND.(.NOT.using_tpj) ) using_tpj=.TRUE.

      _END_MASTER(myThid)

c ============ check for missing data files

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

c ============ deprecated codes related checks

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

c ============ generic cost function related checks

#ifdef ALLOW_GENCOST_CONTRIBUTION
      icount_transp=0
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

         if (gencost_msk_pointer3d(k).GT.NGENCOST3D) then
           WRITE(msgBuf,'(2A)')
     &      'ECCO_CHECK: too many 3D msk terms; please',
     &      'increase NGENCOST3D and recompile.'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
           CALL PRINT_ERROR( msgBuf , myThid )
           STOP 'ABNORMAL END: S/R ECCO_CHECK'
         endif

         if ( gencost_datafile(k) .ne. ' ' ) then

           CALL ECCO_CHECK_FILES( using_gencost(k), 'gencost',
     &       gencost_datafile(k), gencost_startdate1(k), myThid )

c--
           if ( (gencost_preproc(1,k).EQ.'variaweight').AND.
     &          ( gencost_errfile(k) .NE. ' ' ) ) then
             CALL ECCO_CHECK_FILES( using_gencost(k), 'gencost',
     &         gencost_errfile(k), gencost_startdate1(k), myThid )
           elseif ( gencost_errfile(k) .NE. ' ' ) then
             il = ilnblnk(gencost_errfile(k))
             inquire( file=gencost_errfile(k)(1:il), exist=exst )
             if (.NOT.exst) then
               using_gencost(k)=.FALSE.
               il = ilnblnk(gencost_name(k))
               WRITE(msgBuf,'(4A)')
     &           '** WARNING ** ECCO_CHECK_FILES: missing error file',
     &           ' so ',gencost_name(k)(1:il),' gets switched off'
               CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
             endif
           endif

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

c-atn add another block for cost that do not need datafile but
c should have checks for other things:
c here, separate into different gencost_flag block
         else  !if ( gencost_datafile(k) .ne. ' ' ) then
c---------------- block -1 ----------------------------
c block -1: cost ssh-[mdt,lsc,amsre-lsc] do not need datafile
c but need errfile. at the moment do not accomodate variaweight.
          if(gencost_flag(k).eq. -1) then
            if(gencost_errfile(k) .ne. ' ') then
             il = ilnblnk(gencost_errfile(k))
             inquire( file=gencost_errfile(k)(1:il), exist=exst )
             if (.NOT.exst) then
              using_gencost(k)=.FALSE.
              il = ilnblnk(gencost_name(k))
              WRITE(msgBuf,'(4A)')
     &          '** WARNING ** ECCO_CHECK_FILES: missing error file',
     &          ' so ',gencost_name(k)(1:il),' gets switched off'
              CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                   SQUEEZE_RIGHT , myThid)
             endif
            elseif(.not.(gencost_name(k).eq.'sshv4-gmsl')) then
             using_gencost(k)=.FALSE.
             il = ilnblnk(gencost_name(k))
             WRITE(msgBuf,'(4A)')
     &        '** WARNING ** ECCO_CHECK_FILES: error file not defined',
     &        ' so ',gencost_name(k)(1:il),' gets switched off'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                  SQUEEZE_RIGHT , myThid)
            endif

c---------------- block -3 ----------------------------
c-- boxmean: require [err,bar]file, can have datafile, not checked here
c-- also not checked for variwei at the moment
          elseif(gencost_flag(k) .eq. -3 ) then
           WRITE(msgBuf,'(A,i3,L5)')
     &     'entering boxmean/horflux check,k,using_gencost(k): ,',
     &     k,using_gencost(k)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)

            if(gencost_errfile(k) .ne. ' ') then
              WRITE(msgBuf,'(3A)') 'S/R ECCO_CHECK: boxmean now ',
     &          ' uses gencost_mask instead of gencost_errfile --',
     &          ' please update data.ecco accordingly'
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R ECCO_CHECK'
            endif

            if(gencost_mask(k) .eq. ' ') then
              using_gencost(k)=.FALSE.
              il = ilnblnk(gencost_name(k))
              WRITE(msgBuf,'(4A)')
     &          '** WARNING ** ECCO_CHECK_FILES: gencost_mask is',
     &          ' undefined so ',gencost_name(k)(1:il),
     &          ' gets switched off'
              CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                   SQUEEZE_RIGHT , myThid)
            endif

            if ((gencost_mask(k) .ne. ' ').AND.
     &          (gencost_barfile(k)(1:9).EQ.'m_boxmean')) then
             il = ilnblnk(gencost_mask(k))
             write(tempfile(1:128),'(2A)') gencost_mask(k)(1:il),'C'
             inquire( file=tempfile(1:il+1), exist=exst )
             if (.NOT.exst) then
               using_gencost(k)=.FALSE.
               il = ilnblnk(gencost_name(k))
               WRITE(msgBuf,'(4A)')
     &           '** WARNING ** ECCO_CHECK_FILES: missing mask C file',
     &           ' so ',gencost_name(k)(1:il),' gets switched off'
               CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
             endif
            endif

            if ((gencost_mask(k) .ne. ' ').AND.
     &          (gencost_barfile(k)(1:9).EQ.'m_horflux')) then
             il = ilnblnk(gencost_mask(k))
             write(tempfile(1:128),'(2A)') gencost_mask(k)(1:il),'W'
             inquire( file=tempfile(1:il+1), exist=exst )
             if (.NOT.exst) then
               using_gencost(k)=.FALSE.
               il = ilnblnk(gencost_name(k))
               WRITE(msgBuf,'(4A)')
     &           '** WARNING ** ECCO_CHECK_FILES: missing mask W file',
     &           ' so ',gencost_name(k)(1:il),' gets switched off'
               CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
             endif
c
             il = ilnblnk(gencost_mask(k))
             write(tempfile(1:128),'(2A)') gencost_mask(k)(1:il),'S'
             inquire( file=tempfile(1:il+1), exist=exst )
             if (.NOT.exst) then
               using_gencost(k)=.FALSE.
               il = ilnblnk(gencost_name(k))
               WRITE(msgBuf,'(4A)')
     &           '** WARNING ** ECCO_CHECK_FILES: missing mask S file',
     &           ' so ',gencost_name(k)(1:il),' gets switched off'
               CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
             endif
            endif

c-- check barfile, make sure character m_[theta,salt] match exactly
c-- (upper/lower-case matters) in cost_gencost_customize
           if (.not.(
     &          (gencost_barfile(k)(1:15).EQ.'m_boxmean_theta').OR.
     &          (gencost_barfile(k)(1:13).EQ.'m_boxmean_eta').OR.
     &          (gencost_barfile(k)(1:14).EQ.'m_boxmean_salt').OR.
     &          (gencost_barfile(k)(1:17).EQ.'m_boxmean_ptracer').OR.
     &          (gencost_barfile(k)(1:18).EQ.'m_boxmean_shelfice').OR.
     &          (gencost_barfile(k)(1:14).EQ.'m_horflux_vol')
     &        )) then
            using_gencost(k)=.FALSE.
            il=ilnblnk(gencost_barfile(k))
            WRITE(msgBuf,'(3A)')
     &      '** WARNING ** S/R ECCO_CHECK: barfile ',
     &      gencost_barfile(k)(1:il),': has no matched model var.'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(A)') 'Edit cost_gencost_customize to fix. '
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            il = ilnblnk(gencost_name(k))
          WRITE(msgBuf,'(2A)') gencost_name(k)(1:il),' is switched off'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
           endif !barfile

c---------------- block -4 ----------------------------
c-- transs: require [mask,bar][W,S]file, can have datafile, but
c-- not checked here. also not checked for variwei at the moment
         elseif ( (gencost_flag(k).eq. -4) .or.
     &            (gencost_flag(k).eq. -5) ) then
          WRITE(msgBuf,'(A,i3,L5)')
     &     'ecco_check for gencost transp & moc; k,using_gencost(k): ,',
     &     k,using_gencost(k)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)

C ---
C Warn the user that this is a less tested objective function
C TBD: Write a more consistent transport objective function
C ---

          if ((gencost_barfile(k)(1:7).eq.'m_trVol').and.
     &        (gencost_flag(k).eq.-4)) then
           WRITE(msgBuf,'(A)')
     &  '** WARNING ** ECCO_CHECK: cost_gencost_transp.F not tested'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(A)')
     &   ' See m_horflux_vol via cost_gencost_boxmean.F.'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          elseif( (gencost_barfile(k)(1:8).eq.'m_trHeat').or.
     &            (gencost_barfile(k)(1:8).eq.'m_trSalt') ) then
           WRITE(msgBuf,'(2A)')
     &   '** WARNING ** ECCO_CHECK: m_tr[Heat,Salt] to be used with',
     &   ' caution because:'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(A)')
     &  '  (1) advection inconsistent unless ENUM_CENTERED_2ND is used'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(A)')
     &  '  (2) bolus velocities not included'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(A)')
     &  '  (3) diffusion components not included'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
         endif

C ---
C Note to user: use gencost_mask not errfile
C ---
          if(gencost_errfile(k) .ne. ' ') then
            WRITE(msgBuf,'(3A)') 'S/R ECCO_CHECK: transp & moc now ',
     &        ' use gencost_mask instead of gencost_errfile --',
     &        ' please update data.ecco accordingly'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R ECCO_CHECK'
          endif

          if(gencost_mask(k) .eq. ' ') then
            using_gencost(k)=.FALSE.
            il = ilnblnk(gencost_name(k))
            WRITE(msgBuf,'(4A)')
     &        '** WARNING ** ECCO_CHECK_FILES: gencost_mask is',
     &        ' undefined so ',gencost_name(k)(1:il),
     &        ' gets switched off'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                 SQUEEZE_RIGHT , myThid)

          else
c-- West ----------------------
           il = ilnblnk(gencost_mask(k))
           write(tempfile(1:128),'(2A)') gencost_mask(k)(1:il),'W'
           inquire( file=tempfile(1:il+1), exist=exst )
           write(msgBuf,'(2A,L5)') 'ecco_check file, exst: ',
     &       tempfile(1:il+1),exst
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , myThid)
           if (.NOT.exst) then
             using_gencost(k)=.FALSE.
             WRITE(msgBuf,'(2A)')
     &         '** WARNING ** ECCO_CHECK_FILES: missing mask file: ',
     &         tempfile(1:il+1)
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                  SQUEEZE_RIGHT , myThid)
             il = ilnblnk(gencost_name(k))
             WRITE(msgBuf,'(3A)')
     &         ' so ',gencost_name(k)(1:il),' gets switched off'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                  SQUEEZE_RIGHT , myThid)
           endif
c-- South --------------------
           il = ilnblnk(gencost_mask(k))
           write(tempfile(1:128),'(2A)') gencost_mask(k)(1:il),'S'
           inquire( file=tempfile(1:il+1), exist=exst )
           write(msgBuf,'(2A,L5)') 'ecco_check file, exst: ',
     &       tempfile(1:il+1),exst
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , myThid)
           if (.NOT.exst) then
             using_gencost(k)=.FALSE.
             WRITE(msgBuf,'(2A)')
     &         '** WARNING ** ECCO_CHECK_FILES: missing mask file: ',
     &         tempfile(1:il+1)
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                  SQUEEZE_RIGHT , myThid)
             il = ilnblnk(gencost_name(k))
             WRITE(msgBuf,'(3A)')
     &         ' so ',gencost_name(k)(1:il),' gets switched off'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                  SQUEEZE_RIGHT , myThid)
           endif
         endif ! gencost_mask.eq.' '

c-- check barfile, make sure character m_tr[Vol,Heat,Salt] match exactly
c-- (upper/lower-case matters) in cost_gencost_customize
          if(.not.( (gencost_barfile(k)(1:7).EQ.'m_trVol') .OR.
     &              (gencost_barfile(k)(1:8).EQ.'m_trHeat').OR.
     &              (gencost_barfile(k)(1:8).EQ.'m_trSalt') )) then
           using_gencost(k)=.FALSE.
           il=ilnblnk(gencost_barfile(k))
           WRITE(msgBuf,'(3A)')
     &     '** WARNING ** S/R ECCO_CHECK: barfile ',
     &     gencost_barfile(k)(1:il),': has no matched model var.'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(A)') 'Edit cost_gencost_customize to fix. '
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           il = ilnblnk(gencost_name(k))
           WRITE(msgBuf,'(2A)') gencost_name(k)(1:il),' is switched off'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          endif !barfile
c-- set using_cost_transp
          if((using_gencost(k)).and.(gencost_flag(k).eq.-4))
     &      icount_transp=icount_transp+1
          if(icount_transp.gt.0) using_cost_transp = .TRUE.
c-- final report to make sure using_cost_transp is set correctly
          WRITE(msgBuf,'(2A,i3,L5)')
     &     'ecco_check: gencost transp; icount_transp,',
     &     'using_cost_transp: ',icount_transp,using_cost_transp
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)

         endif  !gencost_flag

         endif !if ( gencost_datafile(k) .ne. ' ' ) then
      enddo

c check that one of the used gencost term defines etaday (needed in sshv4)
      IF ( (using_tpj ).OR.(using_ers).OR.(using_gfo)
     &     .OR.(using_mdt) ) using_cost_altim = .TRUE.

      igen_etaday=0
      do k=1,NGENCOST
        if ( (gencost_barfile(k)(1:5).EQ.'m_eta').AND.
     &       (using_gencost(k)) )  igen_etaday=k
      enddo

      if ((igen_etaday.EQ.0).AND.(using_cost_altim)) then
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
     &                      SQUEEZE_RIGHT , myThid)
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
C     msgBuf      - Informational/error message buffer
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
