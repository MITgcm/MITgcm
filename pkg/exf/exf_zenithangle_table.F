C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_zenithangle_table.F,v 1.2 2010/04/13 16:24:50 gforget Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      SUBROUTINE EXF_ZENITHANGLE_TABLE(myThid)

C     ==================================================================
C     SUBROUTINE exf_zenithangle_table
C     ==================================================================
C
C     o compute table of daily mean albedo that will be used in exf_zenithangle.F
C
C     ==================================================================
C     SUBROUTINE exf_zenithangle_table
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

C     == routine arguments ==

      INTEGER myThid

#ifdef ALLOW_DOWNWARD_RADIATION
#ifdef ALLOW_ZENITHANGLE
C     == local variables ==

      INTEGER bi,bj
      INTEGER i,j

      _RL FSOL, dD0dDsq, SOLC, tmpINT1, tmpINT2
      _RL LLLAT, TYEAR, TDAY, ALPHA, CZEN, ALBSEA1
      _RL DECLI, ZS, ZC, SJ, CJ, TMPA, TMPB
      integer iLat,iTyear,iTday

C     == end of interface ==


c solar constant
c --------------
      SOLC   = 1368. _d 0

       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO iLat=1,181
          DO iTyear=1,366

        LLLAT=(iLat-91. _d 0)
        TYEAR=(iTyear-1. _d 0)/365. _d 0

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

        ZC = COS(DECLI)
        ZS = SIN(DECLI)
        SJ = SIN(LLLAT * deg2rad)
        CJ = COS(LLLAT * deg2rad)
        TMPA = SJ*ZS
        TMPB = CJ*ZC

c compute squared earth-sun distance ratio
c ----------------------------------------
c       (formula from Hartmann textbook, after Spencer 1971)
        dD0dDsq = 1.000110 _d 0 
     &         + 0.034221 _d 0 * cos ( 1. _d 0 * ALPHA )
     &         + 0.001280 _d 0 * sin ( 1. _d 0 * ALPHA )
     &         + 0.000719 _d 0 * cos ( 2. _d 0 * ALPHA )
     &         + 0.000077 _d 0 * sin ( 2. _d 0 * ALPHA )

        tmpINT1=0. _d 0
        tmpINT2=0. _d 0	   
        DO iTday=1,100
            TDAY=iTday/100. _d 0
c determine DAILY VARYING cos of solar zenith angle CZEN
c ------------------------------------------------------
            CZEN = TMPA + TMPB * 
     &         cos( 2. _d 0 *PI* TDAY + 0. _d 0 * deg2rad )
            if ( CZEN .LE.0 ) CZEN = 0. _d 0
c compute incoming flux at the top of the atm.:
c ---------------------------------------------
            FSOL = SOLC * dD0dDsq * MAX( 0. _d 0, CZEN )
c determine direct ocean albedo
c -----------------------------
c       (formula from Briegleb, Minnis, et al 1986)
            ALBSEA1 = ( ( 2.6 _d 0 / (CZEN**(1.7 _d 0) + 0.065 _d 0) )
     &          + ( 15. _d 0 * (CZEN-0.1 _d 0) * (CZEN-0.5 _d 0)
     &          * (CZEN-1.0 _d 0) ) ) / 100.0 _d 0
c accumulate averages
c -------------------
            tmpINT1=tmpINT1+FSOL*ALBSEA1/100. _d 0
            tmpINT2=tmpINT2+FSOL/100. _d 0
         ENDDO
c compute weighted average of albedo
c ----------------------------------
         if ( 0.5 _d 0 * tmpINT2 .GT. tmpINT1) then
            zen_albedo_table(iTyear,iLat,bi,bj)=tmpINT1/tmpINT2
         else
            zen_albedo_table(iTyear,iLat,bi,bj)=0.5 _d 0
         endif

          ENDDO
         ENDDO
        ENDDO
       ENDDO



c determine interpolation coefficient for each grid point
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           LLLAT=yC(i,j,bi,bj)+91. _d 0
c ensure that it is in valid range   
           LLLAT=max(LLLAT, 1._d 0)
           LLLAT=min(LLLAT, 181._d 0)
c store
           zen_albedo_pointer(i,j,bi,bj)=LLLAT
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_ZENITHANGLE */
#endif /* ALLOW_DOWNWARD_RADIATION */

      RETURN
      END
