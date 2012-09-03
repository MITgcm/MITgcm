C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_monitor.F,v 1.19 2012/09/03 19:41:14 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_MONITOR
C     !INTERFACE
      SUBROUTINE THSICE_MONITOR( myTime, myIter, myThid )

C     !DESCRIPTION:
C     Do ICE global and Hemispheric monitor output

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#ifdef ALLOW_MONITOR
# include "MONITOR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time of simulation ( s )
C     myIter :: Iteration number
C     myThid :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
#ifdef ALLOW_MONITOR

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*10 mon_var
      CHARACTER*2 mon_sufx(0:2)
      _RS locMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS yBand(2), locDr(1)
      _RL theMin(2), theMax(2)
      _RL theMean(2), theVar(2), theVol(2)
      _RL theMeanG, theVolG
      _RL theMean1, theMean2, theEnergy
      _RL theMin0, theMax0, theSD, theDel2
c     _RL dummyRL(6)
      INTEGER i,j,bi,bj
#ifdef ALLOW_MNC
      INTEGER k
#endif

      DATA yBand / 0. , 0. /
      DATA locDr / 1. /
      DATA mon_sufx / '_G' , '_S' , '_N' /

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (
     &   DIFFERENT_MULTIPLE(thSIce_monFreq,myTime,deltaTclock)
     &   .OR. myIter.EQ.nIter0 ) THEN

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

          IF ( thSIce_mon_stdio ) THEN
            mon_write_stdout = .TRUE.
          ELSE
            mon_write_stdout = .FALSE.
          ENDIF
          mon_write_mnc    = .FALSE.
#ifdef ALLOW_MNC
          IF (useMNC .AND. thSIce_mon_mnc) THEN
            DO k = 1,MAX_LEN_MBUF
              mon_fname(k:k) = ' '
            ENDDO
            mon_fname(1:12) = 'monitor_sice'
            CALL MNC_CW_SET_UDIM(mon_fname, -1, myThid)
            CALL MNC_CW_I_W_S(
     &          'I',mon_fname,1,1,'iter', myIter, myThid)
            CALL MNC_CW_SET_UDIM(mon_fname, 0, myThid)
            CALL MNC_CW_RL_W_S(
     &          'D',mon_fname,1,1,'T', myTime, myThid)
            mon_write_mnc = .TRUE.
          ENDIF
#endif /* ALLOW_MNC */

          IF (mon_write_stdout) THEN
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(A)')
     &             '// Begin MONITOR Therm.SeaIce statistics'
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          ENDIF

C--   endif master cpu io
        ENDIF

C--   make a local copy of iceMask into "RS" array:
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            locMask(i,j,bi,bj) = iceMask(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        CALL MON_SET_PREF('thSI_',myThid)
        CALL MON_OUT_RL('time_sec', myTime,mon_string_none,myThid)

C-- Ice area and Ice thickness :
        CALL MON_STATS_LATBND_RL(
     I                1, 1, 1, 2, yBand,
     I                iceHeight, locMask, maskInC, rA, yC, locDr,
     O                theMin, theMax, theMean, theVar, theVol,
     I                myThid )
        theVolG= theVol(1)+theVol(2)
        theMeanG= theMean(1)*theVol(1)+theMean(2)*theVol(2)
        IF (theVolG.GT.0.) theMeanG = theMeanG / theVolG

        mon_var='Ice_Area'
        CALL MON_OUT_RL(mon_var, theVolG  , mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var, theVol(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theVol(2), mon_sufx(2), myThid)
        mon_var='IceH_ave'
        CALL MON_OUT_RL(mon_var,theMeanG  , mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var,theMean(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var,theMean(2), mon_sufx(2), myThid)
        mon_var='IceH_max'
        CALL MON_OUT_RL(mon_var, theMax(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMax(2), mon_sufx(2), myThid)

C-- Snow thickness :
        CALL MON_STATS_LATBND_RL(
     I                1, 1, 1, 2, yBand,
     I                snowHeight, locMask, maskInC, rA, yC, locDr,
     O                theMin, theMax, theMean, theVar, theVol,
     I                myThid )
        theVolG= theVol(1)+theVol(2)
        theMeanG= theMean(1)*theVol(1)+theMean(2)*theVol(2)
        theEnergy = -rhos*Lfresh*theMeanG
        IF (theVolG.GT.0.) theMeanG = theMeanG / theVolG

        mon_var='SnwH_ave'
        CALL MON_OUT_RL(mon_var,theMeanG  , mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var,theMean(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var,theMean(2), mon_sufx(2), myThid)
        mon_var='SnwH_max'
        CALL MON_OUT_RL(mon_var, theMax(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMax(2), mon_sufx(2), myThid)

C-- Surface Temp. :
        CALL MON_STATS_LATBND_RL(
     I                1, 1, 1, 2, yBand,
     I                Tsrf, locMask, maskInC, rA, yC, locDr,
     O                theMin, theMax, theMean, theVar, theVol,
     I                myThid )
        theVolG= theVol(1)+theVol(2)
        theMeanG= theMean(1)*theVol(1)+theMean(2)*theVol(2)
        IF (theVolG.GT.0.) theMeanG = theMeanG / theVolG

        mon_var='Tsrf_ave'
        CALL MON_OUT_RL(mon_var,theMeanG  , mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var,theMean(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var,theMean(2), mon_sufx(2), myThid)
        mon_var='Tsrf_min'
        CALL MON_OUT_RL(mon_var, theMin(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMin(2), mon_sufx(2), myThid)
        mon_var='Tsrf_max'
        CALL MON_OUT_RL(mon_var, theMax(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMax(2), mon_sufx(2), myThid)

C--   make a local copy of iceMask*iceHeight into "RS" array:
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            locMask(i,j,bi,bj)=iceMask(i,j,bi,bj)*iceHeight(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C-- 1rst level (volume-mean) Temp. :
        CALL MON_STATS_LATBND_RL(
     I                1, 1, 1, 2, yBand,
     I                Tice1, locMask, maskInC, rA, yC, locDr,
     O                theMin, theMax, theMean, theVar, theVol,
     I                myThid )
        theVolG = theVol(1)+theVol(2)
        theMeanG= theMean(1)*theVol(1)+theMean(2)*theVol(2)
        IF (theVolG.GT.0.) theMeanG = theMeanG / theVolG

c       mon_var='IceVolum'
c       CALL MON_OUT_RL(mon_var, theVolG  , mon_sufx(0), myThid)
c       CALL MON_OUT_RL(mon_var, theVol(1), mon_sufx(1), myThid)
c       CALL MON_OUT_RL(mon_var, theVol(2), mon_sufx(2), myThid)
        mon_var='Tic1_ave'
        CALL MON_OUT_RL(mon_var,theMeanG  , mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var,theMean(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var,theMean(2), mon_sufx(2), myThid)
        mon_var='Tic1_min'
        CALL MON_OUT_RL(mon_var, theMin(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMin(2), mon_sufx(2), myThid)
        mon_var='Tic1_max'
        CALL MON_OUT_RL(mon_var, theMax(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMax(2), mon_sufx(2), myThid)

C-- 2nd  level (volume-mean) Temp. :
        CALL MON_STATS_LATBND_RL(
     I                1, 1, 1, 2, yBand,
     I                Tice2, locMask, maskInC, rA, yC, locDr,
     O                theMin, theMax, theMean, theVar, theVol,
     I                myThid )
        theMeanG= theMean(1)*theVol(1)+theMean(2)*theVol(2)
        IF (theVolG.GT.0.) theMeanG = theMeanG / theVolG

        mon_var='Tic2_ave'
        CALL MON_OUT_RL(mon_var,theMeanG  , mon_sufx(0), myThid)
        CALL MON_OUT_RL(mon_var,theMean(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var,theMean(2), mon_sufx(2), myThid)
        mon_var='Tic2_min'
        CALL MON_OUT_RL(mon_var, theMin(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMin(2), mon_sufx(2), myThid)
        mon_var='Tic2_max'
        CALL MON_OUT_RL(mon_var, theMax(1), mon_sufx(1), myThid)
        CALL MON_OUT_RL(mon_var, theMax(2), mon_sufx(2), myThid)

C-- Total Energy :
        CALL MON_CALC_STATS_RL(
     I                1, Qice1, locMask, maskInC, rA, locDr,
     O                theMin0,theMax0,theMean1,theSD,theDel2,theVolG,
     I                myThid )
        CALL MON_CALC_STATS_RL(
     I                1, Qice2, locMask, maskInC, rA, locDr,
     O                theMin0,theMax0,theMean2,theSD,theDel2,theVolG,
     I                myThid )
        theEnergy = theEnergy -rhoi*(theMean1+theMean2)*theVolG/2
        mon_var='TotEnerg'
        CALL MON_OUT_RL(mon_var, theEnergy, mon_sufx(0), myThid)

C-- Surface fluxes
c       IF ( fluidIsWater .AND. monitorSelect.GE.3 ) THEN
c          CALL MON_WRITESTATS_RL( 1, icFrwAtm,'atmFrW',
c    &            maskInC, maskInC, rA , drF, dummyRL, myThid )
c       ENDIF
        IF ( thSIceBalanceAtmFW.NE.0 ) THEN
         CALL MON_OUT_RL('adjustFrW',adjustFrW,mon_string_none,myThid)
        ENDIF

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

          IF (mon_write_stdout) THEN
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(A)')
     &             '// End MONITOR Therm.SeaIce statistics'
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          ENDIF

          mon_write_stdout = .FALSE.
          mon_write_mnc    = .FALSE.

C--   endif master cpu io
        ENDIF

C     endif different multiple
      ENDIF

#endif /* ALLOW_MONITOR */
#endif /* ALLOW_THSICE */

      RETURN
      END
