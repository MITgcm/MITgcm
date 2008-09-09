C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/cpl_write_diags.F,v 1.11 2008/09/09 19:58:26 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CPL_WRITE_DIAGS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE CPL_WRITE_DIAGS                               |
C     | o Calculate CPL diagnostics                              |
C     | - Oceanic version -                                      |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "OCNIDS.h"
#include "OCNCPL.h"
#include "CPL_DIAGS.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid - Number of this instance of the routine
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE

C     == Local variables ==
C     bi,bj  - Tile index
      INTEGER bi,bj
      CHARACTER*(MAX_LEN_MBUF) fn
      LOGICAL gf
      CHARACTER*(1) pf


      IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
        pf(1:1) = 'D'
      ELSE
        pf(1:1) = 'R'
      ENDIF

#ifdef ALLOW_TIMEAVE

C     Dump files and restart average computation if needed
      IF ( myIter.NE.nIter0 .AND.
     &     DIFFERENT_MULTIPLE(cpl_taveFreq,myTime,deltaTClock)
     &     ) THEN

C       Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           CALL TIMEAVE_NORMALIZ(SLPtave,CPL_TimeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZ(HFtave ,CPL_TimeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZ(QSWtave,CPL_TimeAve,1,bi,bj,myThid)
c          CALL TIMEAVE_NORMALIZ(QLTtave,CPL_TimeAve,1,bi,bj,myThid)
c          CALL TIMEAVE_NORMALIZ(QSNtave,CPL_TimeAve,1,bi,bj,myThid)
c          CALL TIMEAVE_NORMALIZ(QLWtave,CPL_TimeAve,1,bi,bj,myThid)
c          CALL TIMEAVE_NORMALIZ(UGtave ,CPL_TimeAve,1,bi,bj,myThid)
c          CALL TIMEAVE_NORMALIZ(VGtave ,CPL_TimeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZ(TXtave ,CPL_TimeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZ(TYtave ,CPL_TimeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZ(FWtave ,CPL_TimeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZ(SFxtave,CPL_TimeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZ(SICtave,CPL_TimeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZ(MXLtave,CPL_TimeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZ(SSTtave,CPL_TimeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZ(SSStave,CPL_TimeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZ(vSqtave,CPL_TimeAve,1,bi,bj,myThid)
           IF ( ocnCpl_exchange_DIC ) THEN
            CALL TIMEAVE_NORMALIZ(aCO2tave, CPL_TimeAve,1,bi,bj,myThid)
            CALL TIMEAVE_NORMALIZ(sWSpdtave,CPL_TimeAve,1,bi,bj,myThid)
            CALL TIMEAVE_NORMALIZ(iceftave, CPL_TimeAve,1,bi,bj,myThid)
            CALL TIMEAVE_NORMALIZ(fCO2tave, CPL_TimeAve,1,bi,bj,myThid)
           ENDIF
         ENDDO
        ENDDO

        IF (cpl_timeave_mdsio) THEN

          _BARRIER
          gf = globalFiles
          WRITE(fn,'(A,I10.10)') 'cplFldtave.',myIter
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, SLPtave ,  1, myIter, myThid )
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, HFtave  ,  2, myIter, myThid )
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, QSWtave ,  3, myIter, myThid )
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, TXtave  ,  4, myIter, myThid )
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, TYtave  ,  5, myIter, myThid )
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, FWtave  ,  6, myIter, myThid )
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, SFxtave ,  7, myIter, myThid )
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, SICtave ,  8, myIter, myThid )
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, MXLtave ,  9, myIter, myThid )
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, SSTtave , 10, myIter, myThid )
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, SSStave , 11, myIter, myThid )
          CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, vSqtave , 12, myIter, myThid )
          IF ( ocnCpl_exchange_DIC ) THEN
            CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, aCO2tave , 13, myIter, myThid )
            CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, sWSpdtave ,14, myIter, myThid )
            CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, iceftave , 15, myIter, myThid )
            CALL MDSWRITEFIELD(fn, writeBinaryPrec, gf, 'RL',
     &         1, fCO2tave , 16, myIter, myThid )
          ENDIF
          _BARRIER

        ENDIF

#ifdef ALLOW_MNC
        IF (useMNC .AND. cpl_timeave_mnc) THEN
          CALL MNC_CW_SET_UDIM('cpl_tave', -1, myThid)
          CALL MNC_CW_RL_W_S('D','cpl_tave',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('cpl_tave', 0, myThid)
          CALL MNC_CW_I_W_S('I','cpl_tave',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'SLPtave',SLPtave,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'HFtave',HFtave,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'QSWtave',QSWtave,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'TXtave',TXtave,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'TYtave',TYtave,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'FWtave',FWtave,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'SFxtave',SFxtave,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'SICtave',SICtave,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'MXLtave',MXLtave,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'SSTtave',SSTtave,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'SSStave',SSStave,myThid)
          CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'vSqtave',vSqtave,myThid)
          IF ( ocnCpl_exchange_DIC ) THEN
            CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'aCO2tave',aCO2tave,myThid)
            CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'sWSpdtave',sWSpdtave,myThid)
            CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'iceftave',iceftave,myThid)
            CALL MNC_CW_RL_W(pf,'cpl_tave',0,0,
     &         'fCO2tave',fCO2tave,myThid)
          ENDIF
       ENDIF
#endif /*  ALLOW_MNC  */

C       Reset diagnostic counters
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_RESET(SLPtave,1,bi,bj,myThid)
            CALL TIMEAVE_RESET(HFtave ,1,bi,bj,myThid)
            CALL TIMEAVE_RESET(QSWtave,1,bi,bj,myThid)
c           CALL TIMEAVE_RESET(QLTtave,1,bi,bj,myThid)
c           CALL TIMEAVE_RESET(QSNtave,1,bi,bj,myThid)
c           CALL TIMEAVE_RESET(QLWtave,1,bi,bj,myThid)
c           CALL TIMEAVE_RESET(UGtave ,1,bi,bj,myThid)
c           CALL TIMEAVE_RESET(VGtave ,1,bi,bj,myThid)
            CALL TIMEAVE_RESET(TXtave ,1,bi,bj,myThid)
            CALL TIMEAVE_RESET(TYtave ,1,bi,bj,myThid)
            CALL TIMEAVE_RESET(FWtave ,1,bi,bj,myThid)
            CALL TIMEAVE_RESET(SFxtave,1,bi,bj,myThid)
            CALL TIMEAVE_RESET(SICtave,1,bi,bj,myThid)
            CALL TIMEAVE_RESET(MXLtave,1,bi,bj,myThid)
            CALL TIMEAVE_RESET(SSTtave,1,bi,bj,myThid)
            CALL TIMEAVE_RESET(SSStave,1,bi,bj,myThid)
            CALL TIMEAVE_RESET(vSqtave,1,bi,bj,myThid)
            IF ( ocnCpl_exchange_DIC ) THEN
              CALL TIMEAVE_RESET(aCO2tave ,1,bi,bj,myThid)
              CALL TIMEAVE_RESET(sWSpdtave,1,bi,bj,myThid)
              CALL TIMEAVE_RESET(iceftave ,1,bi,bj,myThid)
              CALL TIMEAVE_RESET(fCO2tave ,1,bi,bj,myThid)
            ENDIF
            CPL_TimeAve(1,bi,bj) = 0.
          ENDDO
        ENDDO

      ENDIF

#endif /* ALLOW_TIMEAVE */

#endif /* COMPONENT_MODULE */

      RETURN
      END
