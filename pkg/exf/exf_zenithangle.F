C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_zenithangle.F,v 1.1 2010/04/13 06:57:33 gforget Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      SUBROUTINE EXF_ZENITHANGLE(myTime, myIter, myThid)

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
#include "DYNVARS.h"
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

	  integer iLat1,iLat2,iTyear1,iTyear2
	  _RL     wLat1,wLat2,wTyear1,wTyear2

      _RL H0, dD0dDsq, CZENdaily, CZENdiurnal
      _RL TDAY, TYEAR, ALBSEA1, ALPHA, CZEN, CZEN2
      _RL DECLI, ZS, ZC, SJ, CJ, TMPA, TMPB, TMPL, hlim
      _RL SOLC, CSR1, CSR2, FLAT2, FSOL
      integer year0,mydate(4),difftime(4)
      integer dayStartDate(4),yearStartDate(4)
      _RL secondsInYear, myDateSeconds
      integer  cal_IsLeap
      external cal_IsLeap

C     == end of interface ==


c solar constant
c --------------
      SOLC   = 1368. _d 0
c note: it's fourth (342. _d 0) is called SOLC in pkg/aim_v23

c determine time of year/day
c --------------------------

       secondsInYear = ndaysnoleap * secondsperday
       if ( cal_IsLeap(year0,mythid) .eq. 2)
     &       secondsInYear = ndaysleap * secondsperday
c
       call cal_GetDate( myiter, mytime, mydate, mythid )
       year0            = int(mydate(1)/10000.)
       yearStartDate(1) = year0 * 10000 + 101
       yearStartDate(2) = 0
       yearStartDate(3) = mydate(3)
       yearStartDate(4) = mydate(4)
       CALL cal_TimePassed(yearStartDate,mydate,difftime,myThid)
       CALL cal_ToSeconds (difftime,myDateSeconds,myThid)
c
       TYEAR=myDateSeconds/secondsInYear
c
       dayStartDate(1) = mydate(1)
       dayStartDate(2) = 0
       dayStartDate(3) = mydate(3)
       dayStartDate(4) = mydate(4)
       CALL cal_TimePassed(dayStartDate,mydate,difftime,myThid)
       CALL cal_ToSeconds (difftime,myDateSeconds,myThid)
c
       TDAY= myDateSeconds / ( 86400 . _d 0 )
c

       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx

#if !defined(ALLOW_DIURNAL_ALBEDO) && !defined(ALLOW_DAILY_ALBEDO_AIM)

c       This is the default option: daily mean albedo (i.e. without diurnal cycle)
c       obtained from the reference table that was computed in exf_zenithangle_table.F.
c
c       Using either daily or 6 hourly fields, this option yields correct values of daily upward sw flux.
c
c       This is not the case for ALLOW_DIURNAL_ALBEDO (see comments below).

          iTyear1= 1 + 365.*TYEAR
          wTyear1= iTyear1 - 365.*TYEAR
	  iTyear2= iTyear1 + 1
	  wTyear2= 1.0 _d 0 - wTyear1
		  
          if ( zen_albedo_pointer(i,j,bi,bj).EQ. 181. _d 0 ) then
  	    iLat1=181
	    wLat1=0.5  _d 0
  	    iLat2=181
	    wLat2=0.5  _d 0
          else
	    iLat1= zen_albedo_pointer(i,j,bi,bj)
	    wLat1= 1. _d 0 + iLat1 - zen_albedo_pointer(i,j,bi,bj)
            iLat2= iLat1 + 1
	    wLat2= 1. _d 0 - wLat1
          endif
		  
	  ALBSEA1=
     &    wTyear1*wLat1*zen_albedo_table(iTyear1,iLat1,bi,bj)+
     &    wTyear1*wLat2*zen_albedo_table(iTyear1,iLat2,bi,bj)+
     &    wTyear2*wLat1*zen_albedo_table(iTyear2,iLat1,bi,bj)+
     &    wTyear2*wLat2*zen_albedo_table(iTyear2,iLat2,bi,bj)
	  
#else

c determine solar declination
c ---------------------------
c       (formula from Hartmann textbook, after Spencer 1971)
        ALPHA= 2. _d 0*PI*TYEAR
        DECLI = 0.006918 _d 0
     &       - 0.399912 _d 0 * cos ( 1. _d 0 * ALPHA )
     &       + 0.070257 _d 0 * sin ( 1. _d 0 * ALPHA )
     &       - 0.006758 _d 0 * cos ( 2. _d 0 * ALPHA )
     &       + 0.000907 _d 0 * sin ( 2. _d 0 * ALPHA )
     &       - 0.002697 _d 0 * cos ( 3. _d 0 * ALPHA )
     &       + 0.001480 _d 0 * sin ( 3. _d 0 * ALPHA )
 
c note: alternative formulas include
c   1) formula from aim_surf_bc.F, neglecting eccentricity:
c        ALPHA= 2. _d 0*PI*(TYEAR+10. _d 0/365. _d 0)
c        DECLI = COS(ALPHA) * ( -23.45 _d 0 * deg2rad)
c   2) formulas that accounts for minor astronomic effects, e.g.
c    Yallop, B. D., Position of the sun to 1' of arc precision, H. M. Nautical Almanac Office,
c    Royal Greenwich Observatory, Herstmonceux Castle, Hailsham, Sussex BN27 1RP, 1977.
 
        ZC = COS(DECLI)
        ZS = SIN(DECLI)
        SJ = SIN(yC(i,j,bi,bj) * deg2rad)
        CJ = COS(yC(i,j,bi,bj) * deg2rad)
        TMPA = SJ*ZS
        TMPB = CJ*ZC

c determine DAILY VARYING cos of solar zenith angle CZEN
c ------------------------------------------------------
c       (formula from Hartmann textbook, classic trigo)
        CZENdiurnal = TMPA + TMPB * 
     &         cos( 2. _d 0 *PI* TDAY + xC(i,j,bi,bj) * deg2rad )
c note: a more complicated hour angle formula is given by Yallop 1977
        if ( CZENdiurnal .LE.0 ) CZENdiurnal = 0. _d 0

c determine DAILY MEAN cos of solar zenith angle CZEN
c ---------------------------------------------------
c       ( formula from aim_surf_bc.F <--> mean(CZEN*CZEN)/mean(CZEN) )
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


c determine direct ocean albedo
c -----------------------------
c       (formula from Briegleb, Minnis, et al 1986)
c
c       comments on ALLOW_DAILY_ALBEDO_AIM and ALLOW_DIURNAL_ALBEDO:
c       - CZENdaily as computed in aim was found to imply sizable biases in daily upward sw fluxes.
c	  It is not advised to use it, but it is kept in connection to pkg/aim_v23.
c       - CZENdiurnal should never be used with daily mean input fields. 
c	  Furthermore, at this point, it is not advised to use it even with 6 hourly swdown input fields. 
c	  This is because we simply time interpolate between 6 hourly swdown fields, so each day there 
c	  will be times when CZENdiurnal correctly reflects that it is night time, but swdown.NE.0. does not.
c	  CZENdiurnal may actually be rather harmful in this context, since an inconsistency of phase between
c         CZENdiurnal and swdown will yield biases in daily mean upward sw fluxes. So ... 

#ifdef ALLOW_DIURNAL_ALBEDO
        CZEN=CZENdiurnal
        ALBSEA1 = ( ( 2.6 _d 0 / (CZEN**(1.7 _d 0) + 0.065 _d 0) )
     &          + ( 15. _d 0 * (CZEN-0.1 _d 0) * (CZEN-0.5 _d 0)
     &          * (CZEN-1.0 _d 0) ) ) / 100.0 _d 0
#else ALLOW_DAILY_ALBEDO_AIM
        CZEN=CZENdaily
        ALBSEA1 = ( ( 2.6 _d 0 / (CZEN**(1.7 _d 0) + 0.065 _d 0) )
     &          + ( 15. _d 0 * (CZEN-0.1 _d 0) * (CZEN-0.5 _d 0)
     &          * (CZEN-1.0 _d 0) ) ) / 100.0 _d 0
#endif
c
#endif /* !defined(ALLOW_DIURNAL_ALBEDO) ... */

c determine overall albedo 
c ------------------------
c       (approximation: half direct and half diffu.)
        zen_albedo (i,j,bi,bj) = 
     &          0.5 _d 0 * exf_albedo + 0.5 _d 0 * ALBSEA1

c compute incoming flux at the top of the atm.:
c ---------------------------------------------
c       (formula from Hartmann textbook, after Spencer 1971)
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
c DAILY VARYING value:
        ZC = COS(DECLI)
        ZS = SIN(DECLI)
        SJ = SIN(yC(i,j,bi,bj) * deg2rad)
        CJ = COS(yC(i,j,bi,bj) * deg2rad)
        TMPA = SJ*ZS
        TMPB = CJ*ZC
        CZEN = TMPA + TMPB *
     &         cos( 2. _d 0 *PI* TDAY + xC(i,j,bi,bj) * deg2rad )
        if ( CZEN .LE.0 ) CZEN = 0. _d 0
       FSOL = SOLC * dD0dDsq * MAX( 0. _d 0, CZEN )
       zen_fsol_diurnal (i,j,bi,bj) = FSOL
c DAILY MEAN value:
       H0 = -tan( yC(i,j,bi,bj) *deg2rad ) * tan( DECLI )
       if ( H0.LT.-1. _d 0 ) H0 = -1. _d 0
       if ( H0.GT.1. _d 0 ) H0 = 1. _d 0
       H0 = acos( H0 )
       FSOL= SOLC * dD0dDsq / pi *
     &       ( H0 * TMPA + sin(H0) * TMPB )
       zen_fsol_daily (i,j,bi,bj) = FSOL

c note: an alternative for the DAILY MEAN is, as done in pkg/aim_v23,
c       ALPHA= 2. _d 0*PI*(TYEAR+10. _d 0/365. _d 0)
c       CSR1=-0.796 _d 0*COS(ALPHA)
c       CSR2= 0.147 _d 0*COS(2. _d 0*ALPHA)-0.477 _d 0
c       FLAT2 = 1.5 _d 0*SJ**2 - 0.5 _d 0
c       FSOL = 0.25 _d 0 * SOLC * MAX( 0. _d 0, 1. _d 0+CSR1*SJ+CSR2*FLAT2 )
c       zen_fsol_daily (i,j,bi,bj) = FSOL

          ENDDO
         ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_ZENITHANGLE */
#endif /* ALLOW_DOWNWARD_RADIATION */

      RETURN
      END
