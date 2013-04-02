C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_zenithangle.F,v 1.5 2013/04/02 13:02:49 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      SUBROUTINE EXF_ZENITHANGLE( myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE exf_zenithangle
C     ==================================================================
C
C     o compute zenith angle, derive albedo and
C       the incoming flux at the top of the atm.
C
C     ==================================================================
C     SUBROUTINE exf_zenithangle
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"
# include "cal.h"

C     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DOWNWARD_RADIATION
#ifdef ALLOW_ZENITHANGLE
C     == local variables ==
      INTEGER bi,bj
      INTEGER i,j
      INTEGER iLat1,iLat2,iTyear1,iTyear2
      _RL     wLat1,wLat2,wTyear1,wTyear2
      _RL H0, dD0dDsq, CZENdaily, CZENdiurnal
      _RL TDAY, TYEAR, ALBSEA1, ALPHA, CZEN, CZEN2
      _RL DECLI, ZS, ZC, SJ, CJ, TMPA, TMPB, TMPL, hlim
      _RL SOLC, CSR1, CSR2, FLAT2, FSOL
      INTEGER year0,mydate(4),difftime(4)
      INTEGER dayStartDate(4),yearStartDate(4)
      _RL secondsInYear, myDateSeconds
      INTEGER  cal_IsLeap
      EXTERNAL cal_IsLeap
C     == end of interface ==

C solar constant
C --------------
      SOLC   = 1368. _d 0
C note: it is fourth (342. _d 0) is called SOLC in pkg/aim_v23

C determine time of year/day
C --------------------------

       secondsInYear = ndaysnoleap * secondsperday
       IF ( cal_IsLeap(year0,myThid) .EQ. 2)
     &       secondsInYear = ndaysleap * secondsperday

       CALL cal_GetDate( myIter, myTime, mydate, myThid )
       year0            = int(mydate(1)/10000.)
       yearStartDate(1) = year0 * 10000 + 101
       yearStartDate(2) = 0
       yearStartDate(3) = mydate(3)
       yearStartDate(4) = mydate(4)
       CALL cal_TimePassed(yearStartDate,mydate,difftime,myThid)
       CALL cal_ToSeconds (difftime,myDateSeconds,myThid)

       TYEAR=myDateSeconds/secondsInYear

       dayStartDate(1) = mydate(1)
       dayStartDate(2) = 0
       dayStartDate(3) = mydate(3)
       dayStartDate(4) = mydate(4)
       CALL cal_TimePassed(dayStartDate,mydate,difftime,myThid)
       CALL cal_ToSeconds (difftime,myDateSeconds,myThid)

       TDAY= myDateSeconds / ( 86400 . _d 0 )


       IF ( useExfZenAlbedo ) THEN

       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx

          IF ( select_ZenAlbedo.EQ. 0) THEN

          ALBSEA1=exf_albedo

          ELSEIF ( select_ZenAlbedo.EQ. 1) then

C     This is the default option: daily mean albedo (i.e. without diurnal
C     cycle) obtained from the reference table that was computed in
C     exf_zenithangle_table.F. Using either daily or 6 hourly fields, this
C     option yields correct values of daily upward sw flux.
C     This is not the case for select_ZenAlbedo.GT.1 (see comments below).

          iTyear1= 1 + 365.*TYEAR
          wTyear1= iTyear1 - 365.*TYEAR
          iTyear2= iTyear1 + 1
          wTyear2= 1.0 _d 0 - wTyear1

          IF ( zen_albedo_pointer(i,j,bi,bj).EQ. 181. _d 0 ) THEN
            iLat1=181
            wLat1=0.5  _d 0
            iLat2=181
            wLat2=0.5  _d 0
          ELSE
            iLat1= zen_albedo_pointer(i,j,bi,bj)
            wLat1= 1. _d 0 + iLat1 - zen_albedo_pointer(i,j,bi,bj)
            iLat2= iLat1 + 1
            wLat2= 1. _d 0 - wLat1
          ENDIF

          ALBSEA1=
     &    wTyear1*wLat1*zen_albedo_table(iTyear1,iLat1)+
     &    wTyear1*wLat2*zen_albedo_table(iTyear1,iLat2)+
     &    wTyear2*wLat1*zen_albedo_table(iTyear2,iLat1)+
     &    wTyear2*wLat2*zen_albedo_table(iTyear2,iLat2)

C         if ( select_ZenAlbedo.GT. 1), else
          ELSE

C determine solar declination
C ---------------------------
C       (formula from Hartmann textbook, after Spencer 1971)
        ALPHA= 2. _d 0*PI*TYEAR
        DECLI = 0.006918 _d 0
     &       - 0.399912 _d 0 * cos ( 1. _d 0 * ALPHA )
     &       + 0.070257 _d 0 * sin ( 1. _d 0 * ALPHA )
     &       - 0.006758 _d 0 * cos ( 2. _d 0 * ALPHA )
     &       + 0.000907 _d 0 * sin ( 2. _d 0 * ALPHA )
     &       - 0.002697 _d 0 * cos ( 3. _d 0 * ALPHA )
     &       + 0.001480 _d 0 * sin ( 3. _d 0 * ALPHA )

C note: alternative formulas include
C   1) formula from aim_surf_bc.F, neglecting eccentricity:
C        ALPHA= 2. _d 0*PI*(TYEAR+10. _d 0/365. _d 0)
C        DECLI = COS(ALPHA) * ( -23.45 _d 0 * deg2rad)
C   2) formulas that accounts for minor astronomic effects, e.g.
C    Yallop, B. D., Position of the sun to 1 minute of arc precision,
C     H. M. Nautical Almanac Office, Royal Greenwich Observatory,
C     Herstmonceux Castle, Hailsham, Sussex BN27 1RP, 1977.

        ZC = COS(DECLI)
        ZS = SIN(DECLI)
        SJ = SIN(yC(i,j,bi,bj) * deg2rad)
        CJ = COS(yC(i,j,bi,bj) * deg2rad)
        TMPA = SJ*ZS
        TMPB = CJ*ZC

C determine DAILY VARYING cos of solar zenith angle CZEN
C ------------------------------------------------------
C       (formula from Hartmann textbook, classic trigo)
        CZENdiurnal = TMPA + TMPB *
     &         cos( 2. _d 0 *PI* TDAY + xC(i,j,bi,bj) * deg2rad )
C note: a more complicated hour angle formula is given by Yallop 1977
        IF ( CZENdiurnal .LE.0 ) CZENdiurnal = 0. _d 0

C determine DAILY MEAN cos of solar zenith angle CZEN
C ---------------------------------------------------
C       ( formula from aim_surf_bc.F <--> mean(CZEN*CZEN)/mean(CZEN) )
        TMPL = -TMPA/TMPB
        IF (TMPL .GE. 1.0 _d 0) THEN
         CZEN = 0.0 _d 0
        ELSEIF (TMPL .LE. -1.0 _d 0) THEN
         CZEN = (2.0 _d 0)*TMPA*PI
         CZEN2= PI*((2.0 _d 0)*TMPA*TMPA + TMPB*TMPB)
         CZEN = CZEN2/CZEN
        ELSE
         hlim = ACOS(TMPL)
         CZEN = 2.0 _d 0*(TMPA*hlim + TMPB*SIN(hlim))
         CZEN2= 2.0 _d 0*TMPA*TMPA*hlim
     &          + 4.0 _d 0*TMPA*TMPB*SIN(hlim)
     &          + TMPB*TMPB*( hlim + 0.5 _d 0*SIN(2.0 _d 0*hlim) )
         CZEN = CZEN2/CZEN
        ENDIF

        CZENdaily=CZEN


C determine direct ocean albedo
C -----------------------------
C     (formula from Briegleb, Minnis, et al 1986)
C     comments on select_ZenAlbedo.GT.1 methods:
C     - CZENdaily as computed in aim was found to imply sizable biases in
C       daily upward sw fluxes.  It is not advised to use it, but it is kept
C       in connection to pkg/aim_v23.
C     - CZENdiurnal should never be used with daily mean input fields.
C       Furthermore, at this point, it is not advised to use it even with 6
C       hourly swdown input fields. This is because we simply time interpolate
C       between 6 hourly swdown fields, so each day there will be times when
C       CZENdiurnal correctly reflects that it is night time, but swdown.NE.0.
C       does not. CZENdiurnal may actually be rather harmful in this context,
C       since an inconsistency of phase between CZENdiurnal and swdown will
C       yield biases in daily mean upward sw fluxes. So ...

        IF ( select_ZenAlbedo.EQ. 2) THEN
          CZEN=CZENdaily
        ELSEIF ( select_ZenAlbedo.EQ. 3) THEN
          CZEN=CZENdiurnal
        ELSE
          print *, 'select_ZenAlbedo is out of range'
          STOP 'ABNORMAL END: S/R EXF_ZENITHANGLE'
        ENDIF

        ALBSEA1 = ( ( 2.6 _d 0 / (CZEN**(1.7 _d 0) + 0.065 _d 0) )
     &          + ( 15. _d 0 * (CZEN-0.1 _d 0) * (CZEN-0.5 _d 0)
     &          * (CZEN-1.0 _d 0) ) ) / 100.0 _d 0

C       end if ( select_ZenAlbedo.EQ. 0)
        ENDIF

C determine overall albedo
C ------------------------
C       (approximation: half direct and half diffu.)
        zen_albedo (i,j,bi,bj) =
     &          0.5 _d 0 * exf_albedo + 0.5 _d 0 * ALBSEA1

          ENDDO
         ENDDO
        ENDDO
       ENDDO

C      end if ( useExfZenAlbedo )
       ENDIF


      IF ( useExfZenIncoming ) THEN

       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx

C compute incoming flux at the top of the atm.:
C ---------------------------------------------
C       (formula from Hartmann textbook, after Spencer 1971)
        ALPHA= 2. _d 0*PI*TYEAR
        ALPHA= 2. _d 0*PI*TYEAR
        DECLI = 0.006918 _d 0
     &       - 0.399912 _d 0 * cos ( 1. _d 0 * ALPHA )
     &       + 0.070257 _d 0 * sin ( 1. _d 0 * ALPHA )
     &       - 0.006758 _d 0 * cos ( 2. _d 0 * ALPHA )
     &       + 0.000907 _d 0 * sin ( 2. _d 0 * ALPHA )
     &       - 0.002697 _d 0 * cos ( 3. _d 0 * ALPHA )
     &       + 0.001480 _d 0 * sin ( 3. _d 0 * ALPHA )
       dD0dDsq = 1.000110 _d 0
     &         + 0.034221 _d 0 * cos ( 1. _d 0 * ALPHA )
     &         + 0.001280 _d 0 * sin ( 1. _d 0 * ALPHA )
     &         + 0.000719 _d 0 * cos ( 2. _d 0 * ALPHA )
     &         + 0.000077 _d 0 * sin ( 2. _d 0 * ALPHA )
C DAILY VARYING value:
        ZC = COS(DECLI)
        ZS = SIN(DECLI)
        SJ = SIN(yC(i,j,bi,bj) * deg2rad)
        CJ = COS(yC(i,j,bi,bj) * deg2rad)
        TMPA = SJ*ZS
        TMPB = CJ*ZC
        CZEN = TMPA + TMPB *
     &         cos( 2. _d 0 *PI* TDAY + xC(i,j,bi,bj) * deg2rad )
        IF ( CZEN .LE.0 ) CZEN = 0. _d 0
       FSOL = SOLC * dD0dDsq * MAX( 0. _d 0, CZEN )
       zen_fsol_diurnal (i,j,bi,bj) = FSOL
C DAILY MEAN value:
       H0 = -tan( yC(i,j,bi,bj) *deg2rad ) * tan( DECLI )
       IF ( H0.LT.-1. _d 0 ) H0 = -1. _d 0
       IF ( H0.GT.1. _d 0 ) H0 = 1. _d 0
       H0 = acos( H0 )
       FSOL= SOLC * dD0dDsq / pi *
     &       ( H0 * TMPA + sin(H0) * TMPB )
       zen_fsol_daily (i,j,bi,bj) = FSOL

C note: an alternative for the DAILY MEAN is, as done in pkg/aim_v23,
C       ALPHA= 2. _d 0*PI*(TYEAR+10. _d 0/365. _d 0)
C       CSR1=-0.796 _d 0*COS(ALPHA)
C       CSR2= 0.147 _d 0*COS(2. _d 0*ALPHA)-0.477 _d 0
C       FLAT2 = 1.5 _d 0*SJ**2 - 0.5 _d 0
C       FSOL = 0.25 _d 0 * SOLC * MAX( 0. _d 0, 1. _d 0+CSR1*SJ+CSR2*FLAT2 )
C       zen_fsol_daily (i,j,bi,bj) = FSOL

          ENDDO
         ENDDO
        ENDDO
       ENDDO

C      end if ( useExfZenIncoming )
       ENDIF

#endif /* ALLOW_ZENITHANGLE */
#endif /* ALLOW_DOWNWARD_RADIATION */

      RETURN
      END
