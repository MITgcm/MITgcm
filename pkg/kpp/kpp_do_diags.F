C $Header: /u/gcmpack/MITgcm/pkg/kpp/Attic/kpp_do_diags.F,v 1.22 2005/09/29 12:19:52 edhill Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

#undef  MULTIPLE_RECORD_KPP_STATE_FILES
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: KPP_DO_DIAGS

C     !INTERFACE:
      SUBROUTINE KPP_DO_DIAGS( myTime, myIter, myThid )

C     !DESCRIPTION: 
C     Create the KPP diagnostic output.                              
C
C     The following CPP flag (MULTIPLE_RECORD_KPP_STATE_FILES) is
C     #define/#undefed here since it is specific to this routine and
C     very user-preference specific.
C     
C     If #undefed (default) the state files are written as in all
C     versions prior to checkpoint32, where a file is created per
C     variable, per time and per tile. This *has* to be the default
C     because most users use this mode and all utilities and scripts
C     (diagnostic) assume this form.  It is also robust, as explained
C     below.
C     
C     If #defined, subsequent snap-shots are written as records in the
C     same file (no iteration number in filenames).
C
C     Advantages:
C     - fewer files
C     - for small problems, is easy to copy the output around
C     Disadvantages:
C     - breaks a lot of diagnostic scripts
C     - for large or long problems this creates huge files
C     - is an unexpected, unsolicited change in behaviour which came
C     as a surprise (in c32) and inconvenience to several users
C     - can not accomodate changing the frequency of output
C     after a pickup (this is trivial in previous method
C     but needs new code and parameters in this new method)

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "KPP.h"
#include "KPP_PARAMS.h"
#include "KPP_DIAGS.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_KPP

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) suff
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj, K
      _RL DDTT
      INTEGER thisdate(4), prevdate(4)
      LOGICAL dumpFiles
      CHARACTER*(1) pf
CEOP

      IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
        pf(1:1) = 'D'
      ELSE
        pf(1:1) = 'R'
      ENDIF

C----------------------------------------------------------------
C     Dump snapshot of KPP variables.

      IF ( myIter.NE.nIter0 .AND. 
     &     DIFFERENT_MULTIPLE(kpp_dumpFreq,myTime,deltaTClock)
     &     ) THEN
        
        IF (KPPmixingMaps) THEN
          CALL PLOT_FIELD_XYRL  ( KPPhbl    , 'KPPhbl'    ,    
     &         myIter, myThid )
          CALL PLOT_FIELD_XYZRL ( KPPviscAz , 'KPPviscAz' ,
     &         Nr, myIter, myThid )
          CALL PLOT_FIELD_XYZRL ( KPPdiffKzT, 'KPPdiffKzT',
     &         Nr, myIter, myThid )
          CALL PLOT_FIELD_XYZRL ( KPPghat   , 'KPPghat'   ,
     &         Nr, myIter, myThid )
        ENDIF
        
        IF (KPPwriteState .AND. snapshot_mdsio) THEN
#ifdef MULTIPLE_RECORD_KPP_STATE_FILES
C         Write each snap-shot as a new record in one file per variable
C         - creates relatively few files but these files can become huge
C         NOTE: file size and number problems are *SOLVED* by MNC
          CALL WRITE_REC_XYZ_RL('KPPviscAz',KPPviscAz,kpp_drctrec,
     &         myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPdiffKzT',KPPdiffKzT,kpp_drctrec,
     &         myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPdiffKzS',KPPdiffKzS,kpp_drctrec,
     &         myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPghat',KPPghat,kpp_drctrec,
     &         myIter,myThid)
          CALL WRITE_REC_XY_RL('KPPhbl',KPPhbl,kpp_drctrec,
     &         myIter,myThid)
#else /* MULTIPLE_RECORD_KPP_STATE_FILES */
C         Write each snap-shot as a new file - creates many files but
C         for large configurations is easier to transfer
C         NOTE: file size and number problems are *SOLVED* by MNC
          WRITE(suff,'(I10.10)') myIter
          CALL WRITE_FLD_XYZ_RL('KPPviscAz.',suff,KPPviscAz,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPdiffKzT.',suff,KPPdiffKzT,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPdiffKzS.',suff,KPPdiffKzS,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPghat.',suff,KPPghat,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('KPPhbl.',suff,KPPhbl,
     &         myIter,myThid)
#endif /* MULTIPLE_RECORD_KPP_STATE_FILES */
        ENDIF

#ifdef ALLOW_MNC
        IF (KPPwriteState .AND. useMNC .AND. snapshot_mnc) THEN
          CALL MNC_CW_SET_UDIM('kpp_state', -1, myThid)
          CALL MNC_CW_RL_W_S('D','kpp_state',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('kpp_state', 0, myThid)
          CALL MNC_CW_I_W_S('I','kpp_state',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W(pf,'kpp_state',0,0,
     &         'KPPviscAz', KPPviscAz, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_state',0,0,
     &         'KPPdiffKzT', KPPdiffKzT, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_state',0,0,
     &         'KPPdiffKzS', KPPdiffKzS, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_state',0,0,
     &         'KPPghat', KPPghat, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_state',0,0,
     &         'KPPhbl', KPPhbl, myThid)
        ENDIF
#endif /*  ALLOW_MNC  */

C--     Increment record counter
        kpp_drctrec = kpp_drctrec + 1
        
      ENDIF
      
C----------------------------------------------------------------
C     Do KPP time averaging.

#ifdef ALLOW_TIMEAVE

C     Initialize averages to zero
      IF ( myIter.EQ.nIter0 ) THEN

        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_RESET(KPPviscAztave, Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPdiffKzTtave,Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPghattave,   Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPdiffKzStave,Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPhbltave,    1, bi,bj,myThid)
            DO k=1,Nr
              kpp_TimeAve(k,bi,bj)=0.
            ENDDO
          ENDDO
        ENDDO

      ELSE
      
C     Time Average KPP fields
       DDTT=deltaTclock
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_CUMULATE(
     &         KPPviscAztave, KPPviscAz, Nr,DDTT,bi,bj,myThid)
          CALL TIMEAVE_CUMULATE(
     &         KPPdiffKzTtave,KPPdiffKzT,Nr,DDTT,bi,bj,myThid)
          CALL TIMEAVE_CUMULATE(
     &         KPPghattave,   KPPghat,   Nr,DDTT,bi,bj,myThid)
          CALL TIMEAVE_CUMULATE(
     &         KPPdiffKzStave,KPPdiffKzS,Nr,DDTT,bi,bj,myThid)
          CALL TIMEAVE_CUMULATE(
     &         KPPhbltave,    KPPhbl,    1, DDTT,bi,bj,myThid)
C         Keep record of how much time has been integrated over
          DO k=1,Nr
            kpp_TimeAve(k,bi,bj)=kpp_TimeAve(k,bi,bj)+DDTT
          ENDDO
        ENDDO
       ENDDO

      ENDIF
      
C     Dump files and restart average computation if needed
      dumpFiles = .FALSE.
      IF ( myIter .NE. nIter0 ) THEN
      IF (DIFFERENT_MULTIPLE(KPP_taveFreq,myTime,deltaTClock))
     &     dumpFiles = .TRUE.
#ifdef ALLOW_CAL
      IF ( calendarDumps .AND. (
     & (KPP_taveFreq.GE. 2592000.AND.KPP_taveFreq.LE. 2678400).OR.
     & (KPP_taveFreq.GE.31104000.AND.KPP_taveFreq.LE.31968000)))
     & THEN
C--   Convert approximate months (30-31 days) and years (360-372 days)
C     to exact calendar months and years.
C-    First determine calendar dates for this and previous time step.
       call cal_GetDate( myiter  ,mytime            ,thisdate,mythid )
       call cal_GetDate( myiter-1,mytime-deltaTClock,prevdate,mythid )
       dumpFiles = .FALSE.
C-    Monthly KPP_taveFreq:
       IF( KPP_taveFreq.GE. 2592000 .AND. KPP_taveFreq.LE. 2678400
     &      .AND. (thisdate(1)-prevdate(1)).GT.50   ) dumpFiles = .TRUE.
C-    Yearly  KPP_taveFreq:
       IF( KPP_taveFreq.GE.31104000 .AND. KPP_taveFreq.LE.31968000
     &      .AND. (thisdate(1)-prevdate(1)).GT.5000 ) dumpFiles = .TRUE.
      ENDIF
#endif
      ENDIF

      IF (dumpFiles) THEN
C       Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_NORMALIZ(KPPviscAztave,kpp_timeave,
     &           Nr, bi,bj,myThid)
            CALL TIMEAVE_NORMALIZ(KPPdiffKzTtave,kpp_timeave,
     &           Nr, bi,bj,myThid)
            CALL TIMEAVE_NORMALIZ(KPPghattave,kpp_timeave,
     &           Nr, bi,bj,myThid)
            CALL TIMEAVE_NORMALIZ(KPPdiffKzStave,kpp_timeave,
     &           Nr, bi,bj,myThid)
            CALL TIMEAVE_NORMALIZ(KPPhbltave,kpp_timeave,
     &           1,  bi,bj,myThid)
          ENDDO
        ENDDO
        
        IF (KPPwriteState .AND. timeave_mdsio) THEN
#ifdef MULTIPLE_RECORD_KPP_STATE_FILES
C         Write each tave output as a new record in one file per variable
C         - creates relatively few files but these files can become huge
C         NOTE: file size and number problems are *SOLVED* by MNC
          CALL WRITE_REC_XYZ_RL('KPPviscAz-T',KPPviscAzTave,
     &         kpp_drctrecTave,myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPdiffKzT-T',KPPdiffKzTTave,
     &         kpp_drctrecTave,myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPdiffKzS-T',KPPdiffKzSTave,
     &         kpp_drctrecTave,myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPghat-T',KPPghatTave,
     &         kpp_drctrecTave,myIter,myThid)
          CALL WRITE_REC_XY_RL('KPPhbl-T',KPPhblTave,
     &         kpp_drctrecTave,myIter,myThid)
C--       Increment record counter
          kpp_drctrecTave = kpp_drctrecTave + 1
#else /* MULTIPLE_RECORD_KPP_STATE_FILES */
C         Write each tave output as a new file - creates many files but for
C         large configurations is easier to transfer
C         NOTE: file size and number problems are *SOLVED* by MNC
          WRITE(suff,'(I10.10)') myIter
          CALL WRITE_FLD_XYZ_RL('KPPviscAz-T.',suff,KPPviscAzTave,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPdiffKzT-T.',suff,KPPdiffKzTTave,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPdiffKzS-T.',suff,KPPdiffKzSTave,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPghat-T.',suff,KPPghatTave,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('KPPhbl-T.',suff,KPPhblTave,
     &         myIter,myThid)
#endif /* MULTIPLE_RECORD_KPP_STATE_FILES */          
        ENDIF
        
#ifdef ALLOW_MNC
        IF (KPPwriteState .AND. useMNC .AND. timeave_mnc) THEN
          CALL MNC_CW_SET_UDIM('kpp_timeave', -1, myThid)
          CALL MNC_CW_RL_W_S('D','kpp_timeave',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('kpp_timeave', 0, myThid)
          CALL MNC_CW_I_W_S('I','kpp_timeave',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W(pf,'kpp_timeave',0,0,
     &         'KPPviscAz', KPPviscAzTave, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_timeave',0,0,
     &         'KPPdiffKzT', KPPdiffKzTTave, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_timeave',0,0,
     &         'KPPdiffKzS', KPPdiffKzSTave, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_timeave',0,0,
     &         'KPPghat', KPPghatTave, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_timeave',0,0,
     &         'KPPhbl', KPPhblTave, myThid)
        ENDIF
#endif /*  ALLOW_MNC  */

C       Reset averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_RESET(KPPviscAztave, Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPdiffKzTtave,Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPghattave,   Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPdiffKzStave,Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPhbltave,    1, bi,bj,myThid)
            DO k=1,Nr
              kpp_TimeAve(k,bi,bj)=0.
            ENDDO
          ENDDO
        ENDDO
        
      ENDIF
      
#endif /* ALLOW_TIMEAVE */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(KPPviscAz ,'KPPviscA',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(KPPdiffKzS,'KPPdiffS',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(KPPdiffKzT,'KPPdiffT',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(KPPghat   ,'KPPghat ',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(KPPhbl    ,'KPPhbl  ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(KPPfrac   ,'KPPfrac ',0,1 ,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
     
#endif /* ALLOW_KPP */
      
      RETURN
      END
