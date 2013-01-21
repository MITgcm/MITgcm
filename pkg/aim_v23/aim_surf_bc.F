C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_surf_bc.F,v 1.20 2013/01/21 21:49:33 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_SURF_BC
C     !INTERFACE:
      SUBROUTINE AIM_SURF_BC(
     U                        tYear,
     O                        aim_sWght0, aim_sWght1,
     I                        bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | S/R AIM_SURF_BC
C     | Set surface Boundary Conditions
C     |  for the atmospheric physics package
C     *================================================================*
c     | was part of S/R FORDATE in Franco Molteni SPEEDY code (ver23).
C     | For now, surface BC are loaded from files (S/R AIM_FIELDS_LOAD)
C     |  and imposed (= surf. forcing).
C     | In the future, will add
C     |  a land model and a coupling interface with an ocean GCM
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     -------------- Global variables --------------
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

C-- MITgcm
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c #include "DYNVARS.h"
c #include "SURFACE.h"

C-- Physics package
#include "AIM_PARAMS.h"
#include "AIM_FFIELDS.h"
c #include "AIM_GRID.h"
#include "com_forcon.h"
#include "com_forcing.h"
c #include "com_physvar.h"
#include "AIM_CO2.h"

C-- Coupled to the Ocean :
#ifdef COMPONENT_MODULE
#include "CPL_PARAMS.h"
#include "ATMCPL.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     tYear      :: Fraction into year
C     aim_sWght0 :: weight for time interpolation of surface BC
C     aim_sWght1 :: 0/1 = time period before/after the current time
C     bi,bj      :: Tile indices
C     myTime     :: Current time of simulation ( s )
C     myIter     :: Current iteration number in simulation
C     myThid     :: my Thread number Id.
      _RL     tYear
      _RL     aim_sWght0, aim_sWght1
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_AIM
C     !FUNCTIONS:
C     !LOCAL VARIABLES:
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Local Variables originally (Speedy) in common bloc (com_forcing.h):
C--   COMMON /FORDAY/ Daily forcing fields (updated in FORDATE)
C     oice1      :: sea ice fraction
C     snow1      :: snow depth (mm water)
      _RL     oice1(NGP)
      _RL     snow1(NGP)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     == Local variables ==
C     i,j,k,I2,k   :: Loop counters
      INTEGER i,j,I2,k, nm0
      _RL t0prd, tNcyc, tmprd, dTprd
      _RL SDEP1, IDEP2, SDEP2, SWWIL2, RSW, soilw_0, soilw_1
      _RL RSD, alb_land, oceTfreez, ALBSEA1, ALPHA, CZEN, CZEN2
      _RL RZEN, ZS, ZC, SJ, CJ, TMPA, TMPB, TMPL, hlim
c     _RL DALB, alb_sea
#ifdef ALLOW_AIM_CO2
#ifdef ALLOW_DIAGNOSTICS
      _RL pCO2scl
#endif
#endif /* ALLOW_AIM_CO2 */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    Set Land-sea mask (in [0,1]) from aim_landFr to fMask1:
      DO j=1,sNy
        DO i=1,sNx
          I2 = i+(j-1)*sNx
          fMask1(I2,1,myThid) = aim_landFr(i,j,bi,bj)
        ENDDO
      ENDDO

      IF (aim_useFMsurfBC) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     aim_surfForc_TimePeriod :: Length of forcing time period (e.g. 1 month)
C     aim_surfForc_NppCycle   :: Number of time period per Cycle (e.g. 12)
C     aim_surfForc_TransRatio ::
C-     define how fast the (linear) transition is from one month to the next
C       = 1                 -> linear between 2 midle month
C       > TimePeriod/deltaT -> jump from one month to the next one

C--   Calculate weight for linear interpolation between 2 month centers
        t0prd = myTime / aim_surfForc_TimePeriod
        tNcyc = aim_surfForc_NppCycle
        tmprd = t0prd - 0.5 _d 0 + tNcyc
        tmprd = MOD(tmprd,tNcyc)
C-     indices of previous month (nm0) and next month (nm1):
        nm0 = 1 + INT(tmprd)
c       nm1 = 1 + MOD(nm0,aim_surfForc_NppCycle)
C-     interpolation weight:
        dTprd = tmprd - (nm0 - 1)
        aim_sWght1 = 0.5 _d 0+(dTprd-0.5 _d 0)*aim_surfForc_TransRatio
        aim_sWght1 = MAX( 0. _d 0, MIN(1. _d 0, aim_sWght1) )
        aim_sWght0 = 1. _d 0 - aim_sWght1

C--   Compute surface forcing at present time (linear Interp in time)
C     using F.Molteni surface BC form ; fields needed are:
C     1. Sea  Surface temperatures  (in situ Temp. [K])
C     2. Land Surface temperatures  (in situ Temp. [K])
C     3. Soil moisture         (between 0-1)
C     4. Snow depth, Sea Ice : used to compute albedo (=> local arrays)
C     5. Albedo                (between 0-1)

C-    Surface Temperature:
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          sst1(I2,myThid) = aim_sWght0*aim_sst0(i,j,bi,bj)
     &                    + aim_sWght1*aim_sst1(i,j,bi,bj)
          stl1(I2,myThid) = aim_sWght0*aim_lst0(i,j,bi,bj)
     &                    + aim_sWght1*aim_lst1(i,j,bi,bj)
         ENDDO
        ENDDO

C-    Soil Water availability : (from F.M. INFORC S/R)
        SDEP1 = 70. _d 0
        IDEP2 =  3. _d 0
        SDEP2 = IDEP2*SDEP1

        SWWIL2= SDEP2*SWWIL
        RSW   = 1. _d 0/(SDEP1*SWCAP+SDEP2*(SWCAP-SWWIL))

        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          soilw_0 = ( aim_sw10(i,j,bi,bj)
     &     +aim_veget(i,j,bi,bj)*
     &      MAX(IDEP2*aim_sw20(i,j,bi,bj)-SWWIL2, 0. _d 0)
     &              )*RSW
          soilw_1 = ( aim_sw11(i,j,bi,bj)
     &     +aim_veget(i,j,bi,bj)*
     &      MAX(IDEP2*aim_sw21(i,j,bi,bj)-SWWIL2, 0. _d 0)
     &              )*RSW
          soilw1(I2,myThid) = aim_sWght0*soilw_0
     &                      + aim_sWght1*soilw_1
          soilw1(I2,myThid) = MIN(1. _d 0, soilw1(I2,myThid) )
         ENDDO
        ENDDO

C-    Set snow depth & sea-ice fraction :
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          snow1(I2) = aim_sWght0*aim_snw0(i,j,bi,bj)
     &              + aim_sWght1*aim_snw1(i,j,bi,bj)
          oice1(I2) = aim_sWght0*aim_oic0(i,j,bi,bj)
     &              + aim_sWght1*aim_oic1(i,j,bi,bj)
         ENDDO
        ENDDO

        IF (aim_splitSIOsFx) THEN
C-    Split Ocean and Sea-Ice surf. temp. ; remove ice-fraction < 1 %
c        oceTfreez = tFreeze - 1.9 _d 0
         oceTfreez = celsius2K - 1.9 _d 0
         DO J=1,NGP
          sti1(J,myThid) = sst1(J,myThid)
          IF ( oice1(J) .GT. 1. _d -2 ) THEN
            sst1(J,myThid) = MAX(sst1(J,myThid),oceTfreez)
            sti1(J,myThid) = sst1(J,myThid)
     &                     +(sti1(J,myThid)-sst1(J,myThid))/oice1(J)
          ELSE
            oice1(J) = 0. _d 0
          ENDIF
         ENDDO
        ELSE
         DO J=1,NGP
          sti1(J,myThid) = sst1(J,myThid)
         ENDDO
        ENDIF

C-    Surface Albedo : (from F.M. FORDATE S/R)
c_FM    DALB=ALBICE-ALBSEA
        RSD=1. _d 0/SDALB
        ALPHA= 2. _d 0*PI*(TYEAR+10. _d 0/365. _d 0)
#ifdef ALLOW_INSOLATION
        ZS = - SIN(OBLIQ * deg2rad) * COS(ALPHA)
        ZC =   ASIN( ZS )
        ZC =   COS(ZC)
#else /* ALLOW_INSOLATION */
        RZEN = COS(ALPHA) * ( -23.45 _d 0 * deg2rad)
        ZC = COS(RZEN)
        ZS = SIN(RZEN)
#endif /* ALLOW_INSOLATION */
        DO j=1,sNy
         DO i=1,sNx
c_FM      SNOWC=MIN(1.,RSD*SNOW1(I,J))
c_FM      ALBL=ALB0(I,J)+MAX(ALBSN-ALB0(I,J),0.0)*SNOWC
c_FM      ALBS=ALBSEA+DALB*OICE1(I,J)
c_FM      ALB1(I,J)=FMASK1(I,J)*ALBL+FMASK0(I,J)*ALBS
          I2 = i+(j-1)*sNx
          alb_land = aim_albedo(i,j,bi,bj)
     &       + MAX( 0. _d 0, ALBSN-aim_albedo(i,j,bi,bj) )
     &        *MIN( 1. _d 0, RSD*snow1(I2))
c         alb_sea  = ALBSEA + DALB*oice1(I2)
c         alb1(I2,0,myThid) = alb_sea
c    &        + (alb_land - alb_sea)*fMask1(I2,1,myThid)
          ALBSEA1 = ALBSEA
          IF ( aim_selectOceAlbedo .EQ. 1) THEN
           SJ = SIN(yC(i,j,bi,bj) * deg2rad)
           CJ = COS(yC(i,j,bi,bj) * deg2rad)
           TMPA = SJ*ZS
           TMPB = CJ*ZC
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
           ALBSEA1 = ( ( 2.6 _d 0 / (CZEN**(1.7 _d 0) + 0.065 _d 0) )
     &          + ( 15. _d 0 * (CZEN-0.1 _d 0) * (CZEN-0.5 _d 0)
     &          * (CZEN-1.0 _d 0) ) ) / 100.0 _d 0
          ENDIF
          alb1(I2,1,myThid) = alb_land
C_DE      alb1(I2,2,myThid) = ALBSEA
          alb1(I2,2,myThid) = 0.5 _d 0 * ALBSEA
     &        + 0.5 _d 0 * ALBSEA1
          alb1(I2,3,myThid) = ALBICE
         ENDDO
        ENDDO

C-- else aim_useFMsurfBC
      ELSE
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    safer to initialise output argument aim_sWght0,1
C     even if they are not used when aim_useFMsurfBC=F
        aim_sWght1 = 0. _d 0
        aim_sWght0 = 1. _d 0

C-    Set surface forcing fields needed by atmos. physics package
C     1. Albedo                (between 0-1)
C     2. Sea  Surface temperatures  (in situ Temp. [K])
C     3. Land Surface temperatures  (in situ Temp. [K])
C     4. Soil moisture         (between 0-1)
C        Snow depth, Sea Ice (<- no need for now)

C      Set surface albedo data (in [0,1]) from aim_albedo to alb1 :
       IF (aim_useMMsurfFc) THEN
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          alb1(I2,1,myThid) = aim_albedo(i,j,bi,bj)
          alb1(I2,2,myThid) = aim_albedo(i,j,bi,bj)
          alb1(I2,3,myThid) = aim_albedo(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          alb1(I2,1,myThid) = 0.
          alb1(I2,2,myThid) = 0.
          alb1(I2,3,myThid) = 0.
         ENDDO
        ENDDO
       ENDIF
C      Set surface temperature data from aim_S/LSurfTemp to sst1 & stl1 :
       IF (aim_useMMsurfFc) THEN
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          sst1(I2,myThid) = aim_sst0(i,j,bi,bj)
          stl1(I2,myThid) = aim_sst0(i,j,bi,bj)
          sti1(I2,myThid) = aim_sst0(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          sst1(I2,myThid) = 300.
          stl1(I2,myThid) = 300.
          sti1(I2,myThid) = 300.
         ENDDO
        ENDDO
       ENDIF

C-     Set soil water availability (in [0,1]) from aim_sw10 to soilw1 :
       IF (aim_useMMsurfFc) THEN
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          soilw1(I2,myThid) = aim_sw10(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          soilw1(I2,myThid) = 0.
         ENDDO
        ENDDO
       ENDIF

C-     Set Snow depth and Sea Ice
C      (not needed here since albedo is loaded from file)
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          oice1(I2) = 0.
          snow1(I2) = 0.
         ENDDO
        ENDDO

C-- endif/else aim_useFMsurfBC
      ENDIF

#ifdef COMPONENT_MODULE
      IF ( useCoupler ) THEN
C--   take surface data from the ocean component
C     to replace MxL fields (if use sea-ice) or directly AIM SST
        CALL ATM_APPLY_IMPORT(
     I           aim_landFr,
     U           sst1(1,myThid), oice1,
     I           myTime, myIter, bi, bj, myThid )
      ENDIF
#endif /* COMPONENT_MODULE */

#ifdef ALLOW_AIM_CO2
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
           aim_CO2(i,j,bi,bj)= atm_pCO2
        ENDDO
      ENDDO
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
         pCO2scl = 1. _d 6
         CALL DIAGNOSTICS_SCALE_FILL( aim_CO2(1-OLx,1-OLy,bi,bj),
     &               pCO2scl, 1, 'aim_pCO2', 1, 1, 2, bi, bj, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_AIM_CO2 */

#ifdef ALLOW_LAND
      IF (useLand) THEN
C-    Use land model output instead of prescribed Temp & moisture
        CALL AIM_LAND2AIM(
     I           aim_landFr, aim_veget, aim_albedo, snow1,
     U           stl1(1,myThid), soilw1(1,myThid), alb1(1,1,myThid),
     I           myTime, myIter, bi, bj, myThid )
      ENDIF
#endif /* ALLOW_LAND */

#ifdef ALLOW_THSICE
      IF (useThSIce) THEN
C-    Use thermo. sea-ice model output instead of prescribed Temp & albedo
        CALL AIM_SICE2AIM(
     I           aim_landFr,
     U           sst1(1,myThid), oice1,
     O           sti1(1,myThid), alb1(1,3,myThid),
     I           myTime, myIter, bi, bj, myThid )
      ENDIF
#endif /* ALLOW_THSICE */

C-- set the sea-ice & open ocean fraction :
        DO J=1,NGP
          fMask1(J,3,myThid) =(1. _d 0 - fMask1(J,1,myThid))
     &                        *oice1(J)
          fMask1(J,2,myThid) = 1. _d 0 - fMask1(J,1,myThid)
     &                                 - fMask1(J,3,myThid)
        ENDDO

C-- set the mean albedo :
        DO J=1,NGP
          alb1(J,0,myThid) = fMask1(J,1,myThid)*alb1(J,1,myThid)
     &                     + fMask1(J,2,myThid)*alb1(J,2,myThid)
     &                     + fMask1(J,3,myThid)*alb1(J,3,myThid)
        ENDDO

C-- initialize surf. temp. change to zero:
        DO k=1,3
         DO J=1,NGP
          dTsurf(J,k,myThid) = 0.
         ENDDO
        ENDDO

        IF (.NOT.aim_splitSIOsFx) THEN
         DO J=1,NGP
          fMask1(J,3,myThid) = 0. _d 0
          fMask1(J,2,myThid) = 1. _d 0 - fMask1(J,1,myThid)
         ENDDO
        ENDIF

#endif /* ALLOW_AIM */

      RETURN
      END
