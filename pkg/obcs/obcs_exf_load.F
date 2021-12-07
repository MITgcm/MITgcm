# include "OBCS_OPTIONS.h"

C--  File obcs_exf_load.F: Routines to read of OBC fields with EXF
C--   Contents
C--   o OBCS_EXF_LOAD
C--   o OBCS_EXF_READ_XZ
C--   o OBCS_EXF_READ_YZ

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: OBCS_EXF_LOAD
C     !INTERFACE:
      SUBROUTINE OBCS_EXF_LOAD (
     I                           myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==============================================================*
C     | SUBROUTINE OBCS_EXF_LOAD
C     *==============================================================*
C     | read open boundary conditions from file
C     | N.B.: * uses exf and cal routines for file/record handling
C     |       * uses ctrl routines for control variable handling
C     *==============================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS_PARAMS.h"
c#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#include "OBCS_SEAICE.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

C     !INPUT/OUTPUT PARAMETERS:
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if ( defined ALLOW_EXF ) && ( defined ALLOW_OBCS_PRESCRIBE )
C     !LOCAL VARIABLES:
CEOP

# ifdef ALLOW_OBCS_NORTH
      CALL OBCS_EXF_READ_XZ (
     I     'N', useOBCSYearlyFields,
     I     obcsNstartTime, obcsNperiod, obcsNrepCycle,
     U     OBNu,   OBNu0,   OBNu1,   OBNufile,
     U     OBNv,   OBNv0,   OBNv1,   OBNvfile,
     U     OBNt,   OBNt0,   OBNt1,   OBNtfile,
     U     OBNs,   OBNs0,   OBNs1,   OBNsfile,
#  ifdef NONLIN_FRSURF
     U     OBNeta, OBNeta0, OBNeta1, OBNetafile,
#  endif
#  ifdef ALLOW_SEAICE
     I     siobNstartTime, siobNperiod, siobNrepCycle,
     U     OBNa,   OBNa0,   OBNa1,   OBNafile,
     U     OBNh,   OBNh0,   OBNh1,   OBNhfile,
     U     OBNsl,  OBNsl0,  OBNsl1,  OBNslfile,
     U     OBNsn,  OBNsn0,  OBNsn1,  OBNsnfile,
     U     OBNuice,OBNuice0,OBNuice1,OBNuicefile,
     U     OBNvice,OBNvice0,OBNvice1,OBNvicefile,
#  endif
#  ifdef ALLOW_PTRACERS
     U     OBNptr ,OBNptr0, OBNptr1, OBNptrFile,
#  endif
     I     myTime, myIter, myThid )
# endif /* ALLOW_OBCS_NORTH */

# ifdef ALLOW_OBCS_SOUTH
      CALL OBCS_EXF_READ_XZ (
     I     'S', useOBCSYearlyFields,
     I     obcsSstartTime, obcsSperiod, obcsSrepCycle,
     U     OBSu,   OBSu0,   OBSu1,   OBSufile,
     U     OBSv,   OBSv0,   OBSv1,   OBSvfile,
     U     OBSt,   OBSt0,   OBSt1,   OBStfile,
     U     OBSs,   OBSs0,   OBSs1,   OBSsfile,
#  ifdef NONLIN_FRSURF
     U     OBSeta, OBSeta0, OBSeta1, OBSetafile,
#  endif
#  ifdef ALLOW_SEAICE
     I     siobSstartTime, siobSperiod, siobSrepCycle,
     U     OBSa,   OBSa0,   OBSa1,   OBSafile,
     U     OBSh,   OBSh0,   OBSh1,   OBShfile,
     U     OBSsl,  OBSsl0,  OBSsl1,  OBSslfile,
     U     OBSsn,  OBSsn0,  OBSsn1,  OBSsnfile,
     U     OBSuice,OBSuice0,OBSuice1,OBSuicefile,
     U     OBSvice,OBSvice0,OBSvice1,OBSvicefile,
#  endif
#  ifdef ALLOW_PTRACERS
     U     OBSptr ,OBSptr0, OBSptr1, OBSptrFile,
#  endif
     I     myTime, myIter, myThid )
# endif /* ALLOW_OBCS_SOUTH */

# ifdef ALLOW_OBCS_EAST
      CALL OBCS_EXF_READ_YZ (
     I     'E', useOBCSYearlyFields,
     I     obcsEstartTime, obcsEperiod, obcsErepCycle,
     U     OBEu,   OBEu0,   OBEu1,   OBEufile,
     U     OBEv,   OBEv0,   OBEv1,   OBEvfile,
     U     OBEt,   OBEt0,   OBEt1,   OBEtfile,
     U     OBEs,   OBEs0,   OBEs1,   OBEsfile,
#  ifdef NONLIN_FRSURF
     U     OBEeta, OBEeta0, OBEeta1, OBEetafile,
#  endif
#  ifdef ALLOW_SEAICE
     I     siobEstartTime, siobEperiod, siobErepCycle,
     U     OBEa,   OBEa0,   OBEa1,   OBEafile,
     U     OBEh,   OBEh0,   OBEh1,   OBEhfile,
     U     OBEsl,  OBEsl0,  OBEsl1,  OBEslfile,
     U     OBEsn,  OBEsn0,  OBEsn1,  OBEsnfile,
     U     OBEuice,OBEuice0,OBEuice1,OBEuicefile,
     U     OBEvice,OBEvice0,OBEvice1,OBEvicefile,
#  endif
#  ifdef ALLOW_PTRACERS
     U     OBEptr ,OBEptr0, OBEptr1, OBEptrFile,
#  endif
     I     myTime, myIter, myThid )
# endif /* ALLOW_OBCS_EAST */

# ifdef ALLOW_OBCS_WEST
      CALL OBCS_EXF_READ_YZ (
     I     'W', useOBCSYearlyFields,
     I     obcsWstartTime, obcsWperiod, obcsWrepCycle,
     U     OBWu,   OBWu0,   OBWu1,   OBWufile,
     U     OBWv,   OBWv0,   OBWv1,   OBWvfile,
     U     OBWt,   OBWt0,   OBWt1,   OBWtfile,
     U     OBWs,   OBWs0,   OBWs1,   OBWsfile,
#  ifdef NONLIN_FRSURF
     U     OBWeta, OBWeta0, OBWeta1, OBWetafile,
#  endif
#  ifdef ALLOW_SEAICE
     I     siobWstartTime, siobWperiod, siobWrepCycle,
     U     OBWa,   OBWa0,   OBWa1,   OBWafile,
     U     OBWh,   OBWh0,   OBWh1,   OBWhfile,
     U     OBWsl,  OBWsl0,  OBWsl1,  OBWslfile,
     U     OBWsn,  OBWsn0,  OBWsn1,  OBWsnfile,
     U     OBWuice,OBWuice0,OBWuice1,OBWuicefile,
     U     OBWvice,OBWvice0,OBWvice1,OBWvicefile,
#  endif
#  ifdef ALLOW_PTRACERS
     U     OBWptr ,OBWptr0, OBWptr1, OBWptrFile,
#  endif
     I     myTime, myIter, myThid )
# endif /* ALLOW_OBCS_WEST */

#endif /* ALLOW_EXF and ALLOW_OBCS_PRESCRIBE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: OBCS_EXF_READ_XZ
C     !INTERFACE:
      SUBROUTINE OBCS_EXF_READ_XZ (
     I     obName, useYearlyFields,
     I     obcsStartTime, obcsPeriod, obcsRepeatCycle,
     U     OBu,   OBu0,   OBu1,   OBufile,
     U     OBv,   OBv0,   OBv1,   OBvfile,
     U     OBt,   OBt0,   OBt1,   OBtfile,
     U     OBs,   OBs0,   OBs1,   OBsfile,
#ifdef NONLIN_FRSURF
     U     OBeta, OBeta0, OBeta1, OBetafile,
#endif
#ifdef ALLOW_SEAICE
     I     siobStartTime, siobPeriod, siobRepeatCycle,
     U     OBa,   OBa0,   OBa1,   OBafile,
     U     OBh,   OBh0,   OBh1,   OBhfile,
     U     OBsl,  OBsl0,  OBsl1,  OBslfile,
     U     OBsn,  OBsn0,  OBsn1,  OBsnfile,
     U     OBuice,OBuice0,OBuice1,OBuicefile,
     U     OBvice,OBvice0,OBvice1,OBvicefile,
#endif
#ifdef ALLOW_PTRACERS
     U     OBptr ,OBptr0, OBptr1, OBptrFile,
#endif
     I     myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==============================================================*
C     | SUBROUTINE OBCS_EXF_READ_XZ
C     *==============================================================*
C     | read open boundary conditions from file
C     | N.B.: * uses exf and cal routines for file/record handling
C     |       * uses ctrl routines for control variable handling
C     *==============================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
#endif /* ALLOW_PTRACERS */

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) obName
      LOGICAL useYearlyFields
      _RL     obcsStartTime, obcsPeriod, obcsRepeatCycle
      _RL OBu     (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBv     (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBt     (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBs     (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBu0    (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBv0    (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBt0    (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBs0    (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBu1    (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBv1    (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBt1    (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL OBs1    (1-OLx:sNx+OLx,Nr,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) OBuFile,OBvFile,OBtFile,OBsFile
#ifdef NONLIN_FRSURF
      _RL OBeta   (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBeta0  (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBeta1  (1-OLx:sNx+OLx,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) OBetaFile
#endif
#ifdef ALLOW_SEAICE
      _RL     siobStartTime, siobPeriod, siobRepeatCycle
      _RL OBa     (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBh     (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBa0    (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBh0    (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBa1    (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBh1    (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBsl    (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBsn    (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBsl0   (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBsn0   (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBsl1   (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBsn1   (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBuice  (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBvice  (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBuice0 (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBvice0 (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBuice1 (1-OLx:sNx+OLx,nSx,nSy)
      _RL OBvice1 (1-OLx:sNx+OLx,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM)
     &     OBaFile,OBhFile,OBslFile,OBsnFile,OBuiceFile,OBviceFile
#endif /* ALLOW_SEAICE */
#ifdef ALLOW_PTRACERS
      _RL OBptr (1-OLx:sNx+OLx,Nr,nSx,nSy,PTRACERS_num)
      _RL OBptr0(1-OLx:sNx+OLx,Nr,nSx,nSy,PTRACERS_num)
      _RL OBptr1(1-OLx:sNx+OLx,Nr,nSx,nSy,PTRACERS_num)
      CHARACTER*(MAX_LEN_FNAM) OBptrFile(PTRACERS_num)
#endif /* ALLOW_PTRACERS */
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if ( defined ALLOW_EXF ) && ( defined ALLOW_OBCS_PRESCRIBE )
C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(6) fldName
      LOGICAL first, changed
      INTEGER count0, count1
      INTEGER year0, year1
      _RL     fac
# ifdef ALLOW_PTRACERS
      INTEGER iTracer
# endif /* ALLOW_PTRACERS */
CEOP

      IF ( useCAL .AND. obcsPeriod .EQ. -12. _d 0 ) THEN
# ifdef ALLOW_CAL
C-    obcsPeriod=-12 means input file contains 12 monthly means
C     records, corresponding to Jan. (rec=1) through Dec. (rec=12)
        CALL cal_GetMonthsRec(
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
# endif /* ALLOW_CAL */
      ELSEIF ( useCal .AND. obcsPeriod .EQ. -1. _d 0 ) THEN
C-    obcsPeriod=-1 means fields are monthly means.
C     With useYearlyFields=.TRUE., each yearly input file contains
C     12 monthly mean records.  Otherwise, a single input file contains
C     monthly mean records starting at the month obcsStartTime falls in.
# ifdef ALLOW_CAL
        CALL EXF_GetMonthsRec(
     I              obcsStartTime, useYearlyFields,
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
# endif /* ALLOW_CAL */
      ELSEIF ( obcsPeriod .LT. 0. _d 0 ) THEN
        WRITE(msgBuf,'(A,1PE16.8,3A)')
     &     'OBCS_EXF_READ_XZ: Invalid obcsPeriod=', obcsPeriod,
     &     ' for ', obName, ' OBCS files'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_EXF_READ_XZ'
      ELSE
C-    get record numbers and interpolation factor
        fldName = 'obcs'//obName
        CALL EXF_GetFFieldRec(
     I              obcsStartTime, obcsPeriod, obcsRepeatCycle,
     I              fldName, useYearlyFields,
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
      ENDIF
      IF ( exf_debugLev.GE.debLevD ) THEN
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(4A)') ' OBCS_EXF_READ_XZ: ',
     &     'processing ', obName, '-OBCS files'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A,I10,2I7)') ' OBCS_EXF_READ_XZ: ',
     &     ' myIter, count0, count1:', myIter, count0, count1
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A,2(L2,2X),E16.9)') ' OBCS_EXF_READ_XZ: ',
     &     ' first, changed, fac:  ', first, changed, fac
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
      ENDIF

      CALL EXF_SET_OBCS_XZ( OBu, OBu0, OBu1, OBufile, 'u', Nr,
     I                      fac, first, changed, useYearlyFields,
     I                      obcsPeriod, count0, count1, year0, year1,
     I                      myTime, myIter, myThid )
      CALL EXF_SET_OBCS_XZ( OBv, OBv0, OBv1, OBvfile, 'v', Nr,
     I                      fac, first, changed, useYearlyFields,
     I                      obcsPeriod, count0, count1, year0, year1,
     I                      myTime, myIter, myThid )
      CALL EXF_SET_OBCS_XZ( OBt, OBt0, OBt1, OBtfile, 's', Nr,
     I                      fac, first, changed, useYearlyFields,
     I                      obcsPeriod, count0, count1, year0, year1,
     I                      myTime, myIter, myThid )
      CALL EXF_SET_OBCS_XZ( OBs, OBs0, OBs1, OBsfile, 's', Nr,
     I                      fac, first, changed, useYearlyFields,
     I                      obcsPeriod, count0, count1, year0, year1,
     I                      myTime, myIter, myThid )
# ifdef NONLIN_FRSURF
       CALL EXF_SET_OBCS_XZ( OBeta, OBeta0, OBeta1, OBetaFile, 's', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       obcsPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
# endif /* NONLIN_FRSURF */
# ifdef ALLOW_PTRACERS
      IF ( usePTRACERS ) THEN
       DO iTracer = 1, PTRACERS_numInUse
        CALL EXF_SET_OBCS_XZ( OBptr (1-OLx,1,1,1,iTracer),
     I                        OBptr0(1-OLx,1,1,1,iTracer),
     I                        OBptr1(1-OLx,1,1,1,iTracer),
     I                        OBptrFile(iTracer), 's', Nr,
     I                        fac, first, changed, useYearlyFields,
     I                        obcsPeriod, count0, count1, year0, year1,
     I                        myTime, myIter, myThid )
       ENDDO
      ENDIF
# endif /* ALLOW_PTRACERS */

# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       IF ( useCAL .AND. siobPeriod .EQ. -12. _d 0 ) THEN
#  ifdef ALLOW_CAL
C-    obcsPeriod=-12 means input file contains 12 monthly means
C     records, corresponding to Jan. (rec=1) through Dec. (rec=12)
        CALL cal_GetMonthsRec(
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
#  endif /* ALLOW_CAL */
       ELSEIF ( useCal .AND. siobPeriod .EQ. -1. _d 0 ) THEN
C-    siobPeriod=-1 means fields are monthly means.
C     With useYearlyFields=.TRUE., each yearly input file contains
C     12 monthly mean records.  Otherwise, a single input file contains
C     monthly mean records starting at the month siobStartTime falls in.
# ifdef ALLOW_CAL
        CALL EXF_GetMonthsRec(
     I              siobStartTime, useYearlyFields,
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
# endif /* ALLOW_CAL */
       ELSEIF ( siobPeriod .LT. 0. _d 0 ) THEN
        WRITE(msgBuf,'(A,1PE16.8,3A)')
     &     'OBCS_EXF_READ_XZ: Invalid siobPeriod=', siobPeriod,
     &     ' for ', obName, ' OBCS files'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_EXF_READ_XZ'
       ELSE
C-    get record numbers and interpolation factor
        fldName = 'SIob'//obName
        CALL EXF_GetFFieldRec(
     I              siobStartTime, siobPeriod, siobRepeatCycle,
     I              fldName, useYearlyFields,
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
       ENDIF
       IF ( exf_debugLev.GE.debLevD ) THEN
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(4A)') ' OBCS_EXF_READ_XZ: ',
     &     'processing SEAICE ', obName, '-OBCS files'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A,I10,2I7)') ' OBCS_EXF_READ_XZ: ',
     &     ' myIter, count0, count1:', myIter, count0, count1
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A,2(L2,2X),E16.9)') ' OBCS_EXF_READ_XZ: ',
     &     ' first, changed, fac:  ', first, changed, fac
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
       ENDIF

       CALL EXF_SET_OBCS_XZ( OBa, OBa0, OBa1, OBafile, 's', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
       CALL EXF_SET_OBCS_XZ( OBh, OBh0, OBh1, OBhfile, 's', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
       CALL EXF_SET_OBCS_XZ( OBsl, OBsl0, OBsl1, OBslfile, 's', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
       CALL EXF_SET_OBCS_XZ( OBsn, OBsn0, OBsn1, OBsnfile, 's', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
       CALL EXF_SET_OBCS_XZ( OBuice,OBuice0,OBuice1,OBuicefile,'u', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
       CALL EXF_SET_OBCS_XZ( OBvice,OBvice0,OBvice1,OBvicefile,'v', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
      ENDIF
# endif /* ALLOW_SEAICE */

#endif /* ALLOW_EXF and ALLOW_OBCS_PRESCRIBE */
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: OBCS_EXF_READ_YZ
C     !INTERFACE:
      SUBROUTINE OBCS_EXF_READ_YZ (
     I     obName, useYearlyFields,
     I     obcsStartTime, obcsPeriod, obcsRepeatCycle,
     U     OBu,   OBu0,   OBu1,   OBufile,
     U     OBv,   OBv0,   OBv1,   OBvfile,
     U     OBt,   OBt0,   OBt1,   OBtfile,
     U     OBs,   OBs0,   OBs1,   OBsfile,
#ifdef NONLIN_FRSURF
     U     OBeta, OBeta0, OBeta1, OBetafile,
#endif
#ifdef ALLOW_SEAICE
     I     siobStartTime, siobPeriod, siobRepeatCycle,
     U     OBa,   OBa0,   OBa1,   OBafile,
     U     OBh,   OBh0,   OBh1,   OBhfile,
     U     OBsl,  OBsl0,  OBsl1,  OBslfile,
     U     OBsn,  OBsn0,  OBsn1,  OBsnfile,
     U     OBuice,OBuice0,OBuice1,OBuicefile,
     U     OBvice,OBvice0,OBvice1,OBvicefile,
#endif
#ifdef ALLOW_PTRACERS
     U     OBptr ,OBptr0, OBptr1, OBptrFile,
#endif
     I     myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==============================================================*
C     | SUBROUTINE OBCS_EXF_READ_YZ
C     *==============================================================*
C     | read open boundary conditions from file
C     | N.B.: * uses exf and cal routines for file/record handling
C     |       * uses ctrl routines for control variable handling
C     *==============================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
#endif /* ALLOW_PTRACERS */

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) obName
      LOGICAL useYearlyFields
      _RL     obcsStartTime, obcsPeriod, obcsRepeatCycle
      _RL OBu     (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBv     (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBt     (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBs     (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBu0    (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBv0    (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBt0    (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBs0    (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBu1    (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBv1    (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBt1    (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL OBs1    (1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) OBuFile,OBvFile,OBtFile,OBsFile
#ifdef NONLIN_FRSURF
      _RL OBeta   (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBeta0  (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBeta1  (1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) OBetaFile
#endif
#ifdef ALLOW_SEAICE
      _RL     siobStartTime, siobPeriod, siobRepeatCycle
      _RL OBa     (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBh     (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBa0    (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBh0    (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBa1    (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBh1    (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBsl    (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBsn    (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBsl0   (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBsn0   (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBsl1   (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBsn1   (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBuice  (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBvice  (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBuice0 (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBvice0 (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBuice1 (1-OLy:sNy+OLy,nSx,nSy)
      _RL OBvice1 (1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM)
     &     OBaFile,OBhFile,OBslFile,OBsnFile,OBuiceFile,OBviceFile
#endif /* ALLOW_SEAICE */
#ifdef ALLOW_PTRACERS
      _RL OBptr (1-OLy:sNy+OLy,Nr,nSx,nSy,PTRACERS_num)
      _RL OBptr0(1-OLy:sNy+OLy,Nr,nSx,nSy,PTRACERS_num)
      _RL OBptr1(1-OLy:sNy+OLy,Nr,nSx,nSy,PTRACERS_num)
      CHARACTER*(MAX_LEN_FNAM) OBptrFile(PTRACERS_num)
#endif /* ALLOW_PTRACERS */
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if ( defined ALLOW_EXF ) && ( defined ALLOW_OBCS_PRESCRIBE )
C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(6) fldName
      LOGICAL first, changed
      INTEGER count0, count1
      INTEGER year0, year1
      _RL     fac
# ifdef ALLOW_PTRACERS
      INTEGER iTracer
# endif /* ALLOW_PTRACERS */
CEOP

      IF ( useCAL .AND. obcsPeriod .EQ. -12. _d 0 ) THEN
# ifdef ALLOW_CAL
C-    obcsPeriod=-12 means input file contains 12 monthly means
C     records, corresponding to Jan. (rec=1) through Dec. (rec=12)
        CALL cal_GetMonthsRec(
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
# endif /* ALLOW_CAL */
      ELSEIF ( useCal .AND. obcsPeriod .EQ. -1. _d 0 ) THEN
C-    obcsPeriod=-1 means fields are monthly means.
C     With useYearlyFields=.TRUE., each yearly input file contains
C     12 monthly mean records.  Otherwise, a single input file contains
C     monthly mean records starting at the month obcsStartTime falls in.
# ifdef ALLOW_CAL
        CALL EXF_GetMonthsRec(
     I              obcsStartTime, useYearlyFields,
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
# endif /* ALLOW_CAL */
      ELSEIF ( obcsPeriod .LT. 0. _d 0 ) THEN
        WRITE(msgBuf,'(A,1PE16.8,3A)')
     &     'OBCS_EXF_READ_YZ: Invalid obcsPeriod=', obcsPeriod,
     &     ' for ', obName, ' OBCS files'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_EXF_READ_YZ'
      ELSE
C-    get record numbers and interpolation factor
        fldName = 'obcs'//obName
        CALL EXF_GetFFieldRec(
     I              obcsStartTime, obcsPeriod, obcsRepeatCycle,
     I              fldName, useYearlyFields,
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
      ENDIF
      IF ( exf_debugLev.GE.debLevD ) THEN
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(4A)') ' OBCS_EXF_READ_XZ: ',
     &     'processing ', obName, '-OBCS files'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A,I10,2I7)') ' OBCS_EXF_READ_YZ: ',
     &     ' myIter, count0, count1:', myIter, count0, count1
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A,2(L2,2X),E16.9)') ' OBCS_EXF_READ_YZ: ',
     &     ' first, changed, fac:  ', first, changed, fac
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
      ENDIF

      CALL EXF_SET_OBCS_YZ( OBu, OBu0, OBu1, OBufile, 'u', Nr,
     I                      fac, first, changed, useYearlyFields,
     I                      obcsPeriod, count0, count1, year0, year1,
     I                      myTime, myIter, myThid )
      CALL EXF_SET_OBCS_YZ( OBv, OBv0, OBv1, OBvfile, 'v', Nr,
     I                      fac, first, changed, useYearlyFields,
     I                      obcsPeriod, count0, count1, year0, year1,
     I                      myTime, myIter, myThid )
      CALL EXF_SET_OBCS_YZ( OBt, OBt0, OBt1, OBtfile, 's', Nr,
     I                      fac, first, changed, useYearlyFields,
     I                      obcsPeriod, count0, count1, year0, year1,
     I                      myTime, myIter, myThid )
      CALL EXF_SET_OBCS_YZ( OBs, OBs0, OBs1, OBsfile, 's', Nr,
     I                      fac, first, changed, useYearlyFields,
     I                      obcsPeriod, count0, count1, year0, year1,
     I                      myTime, myIter, myThid )
# ifdef NONLIN_FRSURF
       CALL EXF_SET_OBCS_YZ( OBeta, OBeta0, OBeta1, OBetaFile, 's', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       obcsPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
# endif /* NONLIN_FRSURF */
# ifdef ALLOW_PTRACERS
      IF ( usePTRACERS ) THEN
       DO iTracer = 1, PTRACERS_numInUse
        CALL EXF_SET_OBCS_YZ( OBptr (1-OLx,1,1,1,iTracer),
     I                        OBptr0(1-OLx,1,1,1,iTracer),
     I                        OBptr1(1-OLx,1,1,1,iTracer),
     I                        OBptrFile(iTracer), 's', Nr,
     I                        fac, first, changed, useYearlyFields,
     I                        obcsPeriod, count0, count1, year0, year1,
     I                        myTime, myIter, myThid )
       ENDDO
      ENDIF
# endif /* ALLOW_PTRACERS */

# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       IF ( useCAL .AND. siobPeriod .EQ. -12. _d 0 ) THEN
#  ifdef ALLOW_CAL
C-    obcsPeriod=-12 means input file contains 12 monthly means
C     records, corresponding to Jan. (rec=1) through Dec. (rec=12)
        CALL cal_GetMonthsRec(
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
#  endif /* ALLOW_CAL */
       ELSEIF ( useCal .AND. siobPeriod .EQ. -1. _d 0 ) THEN
C-    siobPeriod=-1 means fields are monthly means.
C     With useYearlyFields=.TRUE., each yearly input file contains
C     12 monthly mean records.  Otherwise, a single input file contains
C     monthly mean records starting at the month siobStartTime falls in.
# ifdef ALLOW_CAL
        CALL EXF_GetMonthsRec(
     I              siobStartTime, useYearlyFields,
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
# endif /* ALLOW_CAL */
       ELSEIF ( siobPeriod .LT. 0. _d 0 ) THEN
        WRITE(msgBuf,'(A,1PE16.8,3A)')
     &     'OBCS_EXF_READ_YZ: Invalid siobPeriod=', siobPeriod,
     &     ' for ', obName, ' OBCS files'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_EXF_READ_YZ'
       ELSE
C-    get record numbers and interpolation factor
        fldName = 'SIob'//obName
        CALL EXF_GetFFieldRec(
     I              siobStartTime, siobPeriod, siobRepeatCycle,
     I              fldName, useYearlyFields,
     O              fac, first, changed,
     O              count0, count1, year0, year1,
     I              myTime, myIter, myThid )
       ENDIF
       IF ( exf_debugLev.GE.debLevD ) THEN
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(4A)') ' OBCS_EXF_READ_XZ: ',
     &     'processing SEAICE ', obName, '-OBCS files'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A,I10,2I7)') ' OBCS_EXF_READ_YZ: ',
     &     ' myIter, count0, count1:', myIter, count0, count1
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A,2(L2,2X),E16.9)') ' OBCS_EXF_READ_YZ: ',
     &     ' first, changed, fac:  ', first, changed, fac
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
       ENDIF

       CALL EXF_SET_OBCS_YZ( OBa, OBa0, OBa1, OBafile, 's', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
       CALL EXF_SET_OBCS_YZ( OBh, OBh0, OBh1, OBhfile, 's', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
       CALL EXF_SET_OBCS_YZ( OBsl, OBsl0, OBsl1, OBslfile, 's', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
       CALL EXF_SET_OBCS_YZ( OBsn, OBsn0, OBsn1, OBsnfile, 's', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
       CALL EXF_SET_OBCS_YZ( OBuice,OBuice0,OBuice1,OBuicefile,'u', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
       CALL EXF_SET_OBCS_YZ( OBvice,OBvice0,OBvice1,OBvicefile,'v', 1,
     I                       fac, first, changed, useYearlyFields,
     I                       siobPeriod, count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
      ENDIF
# endif /* ALLOW_SEAICE */

#endif /* ALLOW_EXF and ALLOW_OBCS_PRESCRIBE */
      RETURN
      END
