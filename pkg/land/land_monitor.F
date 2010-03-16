C $Header: /u/gcmpack/MITgcm/pkg/land/land_monitor.F,v 1.14 2010/03/16 00:14:47 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_MONITOR
C     !INTERFACE:
      SUBROUTINE LAND_MONITOR( land_frc, myTime, myIter, myThid )

C     !DESCRIPTION:
C     Do land global and Hemispheric diagnostic

C     !USES:
      IMPLICIT NONE
#include "LAND_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"
#ifdef ALLOW_MONITOR
# include "MONITOR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     land_frc :: land fraction [0-1]
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Iteration number
C     myThid   ::  Number of this instance of INI_FORCING
      _RS land_frc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_LAND
#ifdef ALLOW_MONITOR

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO

C     == Local variables ==
C     nLatBnd    :: Number of latitude bands
C     msgBuf     :: Informational/error message buffer
C     mon_var    :: Variable sufix name
C     mon_sufx   :: Latitude band sufix
C     n, k       :: loop counter
C     yBand      :: latitude separation
C     locDr      :: thickness (= 1. here)
C     theMin     :: lat. band minimum value
C     theMax     :: lat. band maximum value
C     theMean    :: lat. band mean value
C     theVar     :: lat. band variance
C     theVol     :: lat. band volume (or area if locDr=1.)
C     theMeanG   :: global mean value
C     theVarG    :: global variance
C     theVolG    :: global volume (or area if locDr=1.)
C     theEng     :: lat. band energy content
C     theEnergy  :: total energy
      INTEGER nLatBnd
      PARAMETER ( nLatBnd = 3 )
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*10 mon_var
      CHARACTER*2 mon_sufx(0:nLatBnd)
      INTEGER n, k
      _RS yBand(nLatBnd), locDr(land_nLev)
      _RL theMin(nLatBnd), theMax(nLatBnd)
      _RL theMean(nLatBnd), theVar(nLatBnd), theVol(nLatBnd)
      _RL theMeanG, theVolG
      _RL theEng(nLatBnd), theEnergy

      DATA yBand /  0. , -24. , 24. /
      DATA mon_sufx / '_G' , '_S' , '_T' , '_N' /

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( DIFFERENT_MULTIPLE(land_monFreq,myTime,deltaTClock)
     &     .OR. myIter.EQ.nIter0 ) THEN

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

         IF ( land_mon_stdio ) THEN
           mon_write_stdout = .TRUE.
         ELSE
           mon_write_stdout = .FALSE.
         ENDIF
         mon_write_mnc    = .FALSE.
#ifdef ALLOW_MNC
         IF ( useMNC .AND. land_mon_mnc ) THEN
           DO k = 1,MAX_LEN_MBUF
             mon_fname(k:k) = ' '
           ENDDO
           mon_fname(1:12) = 'monitor_land'
           CALL MNC_CW_APPEND_VNAME(
     &          'T', '-_-_--__-__t', 0,0, myThid)
           CALL MNC_CW_SET_UDIM(mon_fname, -1, myThid)
           CALL MNC_CW_I_W_S(
     &         'I',mon_fname,1,1,'T', myIter, myThid)
           CALL MNC_CW_SET_UDIM(mon_fname, 0, myThid)
           mon_write_mnc = .TRUE.
         ENDIF
#endif /* ALLOW_MNC */

         IF ( mon_write_stdout ) THEN
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(A)') '// Begin MONITOR Land statistics'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          ENDIF

C--   endif master cpu io
        ENDIF

        CALL MON_SET_PREF('land_',myThid)
        CALL MON_OUT_RL('time_sec', myTime,mon_string_none,myThid)

        DO k=1,land_nLev
          locDr(k)= 1.
        ENDDO

C-- Snow thickness :
        CALL MON_STATS_LATBND_RL(
     I                1, 1, 1, nLatBnd, yBand,
     I                land_hSnow, land_frc, maskInC, rA, yC, locDr,
     O                theMin, theMax, theMean, theVar, theVol,
     I                myThid )
        theVolG = 0.
        theMeanG= 0.
        DO n=1,nLatBnd
         theVolG  = theVolG  + theVol(n)
         theMeanG = theMeanG + theMean(n)*theVol(n)
         theEng(n)= -land_rhoSnow*land_Lfreez*theMean(n)*theVol(n)
        ENDDO
        IF (theVolG.GT.0.) theMeanG = theMeanG / theVolG

        mon_var='SnwH_ave'
        CALL MON_OUT_RL(mon_var,theMeanG  , mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var,theMean(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var,theMean(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var,theMean(3), mon_sufx(3), myThid)
        mon_var='SnwH_max'
        CALL MON_OUT_RL(mon_var, theMax(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMax(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var, theMax(3), mon_sufx(3), myThid)

        IF ( myIter.EQ.1+nIter0 ) THEN
          _BEGIN_MASTER(myThid)
          WRITE(msgBuf,'(A,1PE16.9,A,0P9F7.2)') '%MON LAND : Area=',
     &      theVolG, ' ; Lat sep=', (yBand(n),n=2,nLatBnd)
          CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          WRITE(msgBuf,'(A,1P9E16.9)') '%MON LAND : LatA=',
     &                              (theVol(n),n=1,nLatBnd)
          CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          _END_MASTER(myThid)
        ENDIF

C-- Surface Temp. :
        CALL MON_STATS_LATBND_RL(
     I                1, 1, 1, nLatBnd, yBand,
     I                land_skinT, land_frc, maskInC, rA, yC, locDr,
     O                theMin, theMax, theMean, theVar, theVol,
     I                myThid )
        theVolG = 0.
        theMeanG= 0.
        DO n=1,nLatBnd
         theVolG  = theVolG  + theVol(n)
         theMeanG = theMeanG + theMean(n)*theVol(n)
        ENDDO
        IF (theVolG.GT.0.) theMeanG = theMeanG / theVolG

        mon_var='Tsrf_ave'
        CALL MON_OUT_RL(mon_var,theMeanG  , mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var,theMean(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var,theMean(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var,theMean(3), mon_sufx(3), myThid)
        mon_var='Tsrf_min'
        CALL MON_OUT_RL(mon_var, theMin(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMin(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var, theMin(3), mon_sufx(3), myThid)
        mon_var='Tsrf_max'
        CALL MON_OUT_RL(mon_var, theMax(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMax(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var, theMax(3), mon_sufx(3), myThid)

C-- 1rst level (volume-mean) Temp. :
        CALL MON_STATS_LATBND_RL(
     I                land_nLev, 1, 1, nLatBnd, yBand,
     I                land_groundT, land_frc, maskInC, rA, yC, locDr,
     O                theMin, theMax, theMean, theVar, theVol,
     I                myThid )
        theVolG = 0.
        theMeanG= 0.
        DO n=1,nLatBnd
         theVolG  = theVolG  + theVol(n)
         theMeanG = theMeanG + theMean(n)*theVol(n)
        ENDDO
        IF (theVolG.GT.0.) theMeanG = theMeanG / theVolG

        mon_var='Tgr1_ave'
        CALL MON_OUT_RL(mon_var,theMeanG  , mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var,theMean(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var,theMean(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var,theMean(3), mon_sufx(3), myThid)
        mon_var='Tgr1_min'
        CALL MON_OUT_RL(mon_var, theMin(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMin(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var, theMin(3), mon_sufx(3), myThid)
        mon_var='Tgr1_max'
        CALL MON_OUT_RL(mon_var, theMax(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMax(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var, theMax(3), mon_sufx(3), myThid)

C-- 2nd  level (volume-mean) Temp. :
        CALL MON_STATS_LATBND_RL(
     I                land_nLev, 1, 2, nLatBnd, yBand,
     I                land_groundT, land_frc, maskInC, rA, yC, locDr,
     O                theMin, theMax, theMean, theVar, theVol,
     I                myThid )
        theVolG = 0.
        theMeanG= 0.
        DO n=1,nLatBnd
         theVolG  = theVolG  + theVol(n)
         theMeanG = theMeanG + theMean(n)*theVol(n)
        ENDDO
        IF (theVolG.GT.0.) theMeanG = theMeanG / theVolG

        mon_var='Tgr2_ave'
        CALL MON_OUT_RL(mon_var,theMeanG  , mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var,theMean(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var,theMean(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var,theMean(3), mon_sufx(3), myThid)
        mon_var='Tgr2_min'
        CALL MON_OUT_RL(mon_var, theMin(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMin(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var, theMin(3), mon_sufx(3), myThid)
        mon_var='Tgr2_max'
        CALL MON_OUT_RL(mon_var, theMax(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMax(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var, theMax(3), mon_sufx(3), myThid)

C-- Total Energy :
        DO k=1,land_nLev
          locDr(k)= land_dzF(k)
        ENDDO
        CALL MON_STATS_LATBND_RL(
     I                land_nLev, 1, 0, nLatBnd, yBand,
     I                land_enthalp, land_frc, maskInC, rA, yC, locDr,
     O                theMin, theMax, theMean, theVar, theVol,
     I                myThid )
        theEnergy = 0.
        DO n=1,nLatBnd
         theEng(n) = theEng(n) + theMean(n)*theVol(n)
         theEnergy = theEnergy + theEng(n)
        ENDDO
        mon_var='TotEnerg'
        CALL MON_OUT_RL(mon_var,theEnergy, mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var,theEng(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var,theEng(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var,theEng(3), mon_sufx(3), myThid)

C-- Soil water content (level 1+2):
        CALL MON_STATS_LATBND_RL(
     I                land_nLev, 1, 0, nLatBnd, yBand,
     I                land_groundW, land_frc, maskInC, rA, yC, locDr,
     O                theMin, theMax, theMean, theVar, theVol,
     I                myThid )
        theVolG = 0.
        theMeanG= 0.
        DO n=1,nLatBnd
         theVolG  = theVolG  + theVol(n)
         theMeanG = theMeanG + theMean(n)*theVol(n)
        ENDDO
        IF (theVolG.GT.0.) theMeanG = theMeanG / theVolG

        mon_var='grdW_ave'
        CALL MON_OUT_RL(mon_var,theMeanG  , mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var,theMean(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var,theMean(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var,theMean(3), mon_sufx(3), myThid)
        mon_var='grdW_min'
        CALL MON_OUT_RL(mon_var, theMin(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMin(2), mon_sufx(2), myThid)
        CALL MON_OUT_RL(mon_var, theMin(3), mon_sufx(3), myThid)
c       mon_var='grdW_max'
c       CALL MON_OUT_RL(mon_var, theMax(1), mon_sufx(1), myThid)
c       CALL MON_OUT_RL(mon_var, theMax(2), mon_sufx(2), myThid)
c       CALL MON_OUT_RL(mon_var, theMax(3), mon_sufx(3), myThid)

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

          IF (mon_write_stdout) THEN
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(A)') '// End MONITOR Land statistics'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          ENDIF

          mon_write_stdout = .FALSE.
          mon_write_mnc    = .FALSE.

C--   endif master cpu io
        ENDIF

C     endif different multiple
      ENDIF

#endif /* ALLOW_MONITOR */
#endif /* ALLOW_LAND */

      RETURN
      END
