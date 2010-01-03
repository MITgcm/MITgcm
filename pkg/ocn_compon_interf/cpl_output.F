C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_output.F,v 1.2 2010/01/03 19:26:54 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_OUTPUT
C     !INTERFACE:
      SUBROUTINE CPL_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CPL_OUTPUT
C     | o Write CPL time-average output
C     | - Oceanic version -
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "OCNIDS.h"
#include "OCNCPL.h"
#include "CPL_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
#ifdef ALLOW_TIMEAVE

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: Tile index
C     fn     :: File name suffix
      INTEGER bi,bj
      CHARACTER*(MAX_LEN_FNAM) fn
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif /*  ALLOW_MNC  */

C     Dump files and restart average computation if needed
      IF ( myIter.NE.nIter0 .AND.
     &     DIFFERENT_MULTIPLE(cpl_taveFreq,myTime,deltaTClock)
     &   ) THEN

C       Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_NORMALIZE(SLPtave,CPL_timeAve,1,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(HFtave ,CPL_timeAve,1,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(QSWtave,CPL_timeAve,1,bi,bj,myThid)
c         CALL TIMEAVE_NORMALIZE(QLTtave,CPL_timeAve,1,bi,bj,myThid)
c         CALL TIMEAVE_NORMALIZE(QSNtave,CPL_timeAve,1,bi,bj,myThid)
c         CALL TIMEAVE_NORMALIZE(QLWtave,CPL_timeAve,1,bi,bj,myThid)
c         CALL TIMEAVE_NORMALIZE(UGtave ,CPL_timeAve,1,bi,bj,myThid)
c         CALL TIMEAVE_NORMALIZE(VGtave ,CPL_timeAve,1,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(TXtave ,CPL_timeAve,1,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(TYtave ,CPL_timeAve,1,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(FWtave ,CPL_timeAve,1,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(SFxtave,CPL_timeAve,1,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(SICtave,CPL_timeAve,1,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(MXLtave,CPL_timeAve,1,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(SSTtave,CPL_timeAve,1,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(SSStave,CPL_timeAve,1,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(vSqtave,CPL_timeAve,1,bi,bj,myThid)
          IF ( ocnCpl_exchange_DIC ) THEN
           CALL TIMEAVE_NORMALIZE(aCO2tave, CPL_timeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZE(sWSpdtave,CPL_timeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZE(iceftave, CPL_timeAve,1,bi,bj,myThid)
           CALL TIMEAVE_NORMALIZE(fCO2tave, CPL_timeAve,1,bi,bj,myThid)
          ENDIF
         ENDDO
        ENDDO

        IF ( cpl_timeave_mdsio ) THEN
          WRITE(fn,'(A,I10.10)') 'cplFldtave.', myIter
          CALL WRITE_REC_XY_RL(  fn, SLPtave  ,  1, myIter, myThid )
          CALL WRITE_REC_XY_RL(  fn, HFtave   ,  2, myIter, myThid )
          CALL WRITE_REC_XY_RL(  fn, QSWtave  ,  3, myIter, myThid )
          CALL WRITE_REC_XY_RL(  fn, TXtave   ,  4, myIter, myThid )
          CALL WRITE_REC_XY_RL(  fn, TYtave   ,  5, myIter, myThid )
          CALL WRITE_REC_XY_RL(  fn, FWtave   ,  6, myIter, myThid )
          CALL WRITE_REC_XY_RL(  fn, SFxtave  ,  7, myIter, myThid )
          CALL WRITE_REC_XY_RL(  fn, SICtave  ,  8, myIter, myThid )
          CALL WRITE_REC_XY_RL(  fn, MXLtave  ,  9, myIter, myThid )
          CALL WRITE_REC_XY_RL(  fn, SSTtave  , 10, myIter, myThid )
          CALL WRITE_REC_XY_RL(  fn, SSStave  , 11, myIter, myThid )
          CALL WRITE_REC_XY_RL(  fn, vSqtave  , 12, myIter, myThid )
          IF ( ocnCpl_exchange_DIC ) THEN
           CALL WRITE_REC_XY_RL( fn, aCO2tave , 13, myIter, myThid )
           CALL WRITE_REC_XY_RL( fn, sWSpdtave, 14, myIter, myThid )
           CALL WRITE_REC_XY_RL( fn, iceftave , 15, myIter, myThid )
           CALL WRITE_REC_XY_RL( fn, fCO2tave , 16, myIter, myThid )
          ENDIF
        ENDIF

#ifdef ALLOW_MNC
        IF ( useMNC.AND.cpl_timeave_mnc ) THEN
          IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
            pf(1:1) = 'D'
          ELSE
            pf(1:1) = 'R'
          ENDIF
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
            CALL TIMEAVE_RESET( SLPtave, 1, bi, bj, myThid )
            CALL TIMEAVE_RESET( HFtave , 1, bi, bj, myThid )
            CALL TIMEAVE_RESET( QSWtave, 1, bi, bj, myThid )
c           CALL TIMEAVE_RESET( QLTtave, 1, bi, bj, myThid )
c           CALL TIMEAVE_RESET( QSNtave, 1, bi, bj, myThid )
c           CALL TIMEAVE_RESET( QLWtave, 1, bi, bj, myThid )
c           CALL TIMEAVE_RESET( UGtave , 1, bi, bj, myThid )
c           CALL TIMEAVE_RESET( VGtave , 1, bi, bj, myThid )
            CALL TIMEAVE_RESET( TXtave , 1, bi, bj, myThid )
            CALL TIMEAVE_RESET( TYtave , 1, bi, bj, myThid )
            CALL TIMEAVE_RESET( FWtave , 1, bi, bj, myThid )
            CALL TIMEAVE_RESET( SFxtave, 1, bi, bj, myThid )
            CALL TIMEAVE_RESET( SICtave, 1, bi, bj, myThid )
            CALL TIMEAVE_RESET( MXLtave, 1, bi, bj, myThid )
            CALL TIMEAVE_RESET( SSTtave, 1, bi, bj, myThid )
            CALL TIMEAVE_RESET( SSStave, 1, bi, bj, myThid )
            CALL TIMEAVE_RESET( vSqtave, 1, bi, bj, myThid )
            IF ( ocnCpl_exchange_DIC ) THEN
              CALL TIMEAVE_RESET( aCO2tave , 1, bi, bj, myThid )
              CALL TIMEAVE_RESET( sWSpdtave, 1, bi, bj, myThid )
              CALL TIMEAVE_RESET( iceftave , 1, bi, bj, myThid )
              CALL TIMEAVE_RESET( fCO2tave , 1, bi, bj, myThid )
            ENDIF
            CPL_timeAve(bi,bj) = 0.
          ENDDO
        ENDDO

      ENDIF

#endif /* ALLOW_TIMEAVE */
#endif /* COMPONENT_MODULE */

      RETURN
      END
