C $Header: /u/gcmpack/MITgcm/verification/aim.5l_Equatorial_Channel/code/aim_surf_bc.F,v 1.1 2004/01/24 20:41:25 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE AIM_SURF_BC( tYear, myTime, myIter, bi, bj, myThid ) 
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
      IMPLICIT NONE

C     -------------- Global variables --------------
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

C-- MITgcm
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "DYNVARS.h"
#include "GRID.h"
c #include "SURFACE.h"

C-- Physics package
#include "AIM_PARAMS.h"
#include "AIM_FFIELDS.h"
c #include "AIM_GRID.h"
#include "com_forcon.h"
#include "com_forcing.h"
c #include "com_physvar.h"

C-- Coupled to the Ocean :
#ifdef COMPONENT_MODULE
#include "CPL_PARAMS.h"
#include "ATMCPL.h"
#endif

C     == Routine arguments ==
C     tYear  - Fraction into year
C     myTime - Current time of simulation ( s )
C     myIter - Current iteration number in simulation
C     bi,bj  - Tile index 
C     myThid - Number of this instance of the routine
      INTEGER myIter, bi, bj, myThid
      _RL tYear, myTime

#ifdef ALLOW_AIM
C     == Local variables ==
C     i,j,k,I2      - Loop counters
      INTEGER i,j,I2
      _RL SDEP1, IDEP2, SDEP2, SWWIL2, RSW, soilw_0, soilw_1
      _RL DALB, RSD, alb_sea, alb_land

C_EqCh: start
      CHARACTER*(MAX_LEN_MBUF) suff
      _RL xBump, yBump, dxBump, dyBump
      xBump = thetaMin + delX(1)*64.
      yBump = phiMin   + delY(1)*11.5
      dxBump=  delX(1)*12.
      dyBump=  delY(1)*6.
C_EqCh: Fix solar insolation with Sun directly overhead on Equator
      tYear = 0.25 _d 0 - 10. _d 0/365. _d 0
C_EqCh: end

      IF (aim_useFMsurfBC) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Compute surface forcing at present time (linear Interp in time)
C     using F.Molteni surface BC form ; fields needed are:
C     1. Land sea mask  
C     2. Sea  Surface temperatures  (in situ Temp. [K])
C     3. Land Surface temperatures  (in situ Temp. [K])
C     4. Soil moisture         (between 0-1)
C     5. Snow depth, Sea Ice : used to compute albedo (=> local arrays)
C     6. Albedo                (between 0-1)

C-     Set Land-sea mask (in [0,1]) from aim_landFr to fMask1:
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          fMask1(I2,myThid) = aim_landFr(i,j,bi,bj)
         ENDDO
        ENDDO

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

C-    Surface Albedo : (from F.M. FORDATE S/R)
        DALB=ALBICE-ALBSEA
        RSD=1. _d 0/SDALB
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
          alb_sea  = ALBSEA + DALB*oice1(I2)
          alb1(I2,myThid) = alb_sea 
     &        + (alb_land - alb_sea)*fMask1(I2,myThid)
         ENDDO
        ENDDO

C-- else aim_useFMsurfBC
      ELSE
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Set surface forcing fields needed by atmos. physics package
C     1. Albedo                (between 0-1)
C     2. Sea  Surface temperatures  (in situ Temp. [K])
C     3. Land Surface temperatures  (in situ Temp. [K])
C     4. Soil moisture         (between 0-1)
C     5. Land sea mask  (infer from exact zeros in soil moisture)
C        Snow depth, Sea Ice (<- no need for now)   

C      Set surface albedo data (in [0,1]) from aim_albedo to alb1 :
       IF (aim_useMMsurfFc) THEN
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          alb1(I2,myThid) = aim_albedo(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          alb1(I2,myThid) = 0.
         ENDDO
        ENDDO
       ENDIF
C      Set surface temperature data from aim_S/LSurfTemp to sst1 & stl1 :
       IF (aim_useMMsurfFc) THEN
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          sst1(I2,myThid) = aim_surfTemp(i,j,bi,bj)
          stl1(I2,myThid) = aim_surfTemp(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          sst1(I2,myThid) = 300.
          stl1(I2,myThid) = 300.
C_EqCh: start
          sst1(I2,myThid) = 280.
     &     +20. _d 0 *exp( -((xC(i,j,bi,bj)-xBump)/dxBump)**2  
     &                     -((yC(i,j,bi,bj)-yBump)/dyBump)**2 )
          stl1(I2,myThid) = sst1(I2,myThid)
C_EqCh: end
         ENDDO
        ENDDO
C_EqCh: start
        IF (myIter.EQ.nIter0) THEN 
         WRITE(suff,'(I10.10)') myIter
         CALL AIM_WRITE_LOCAL('aim_SST.',suff,1,sst1(1,myThid),
     &                                   bi,bj,1,myIter,myThid)
        ENDIF
C_EqCh: end
       ENDIF

C-     Set soil water availability (in [0,1]) from aim_soilWater to soilw1 :
       IF (aim_useMMsurfFc) THEN
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          soilw1(I2,myThid) = aim_soilWater(i,j,bi,bj)
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

C-     Set Land-sea mask (in [0,1]) 
C          from aim_landFr to fMask1 (aim_useFMsurfBC)
C       or from where soil moisture is exactly zero (aim_useMMsurfFc)
       IF (aim_useMMsurfFc) THEN
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          fMask1(I2,myThid) = 1.
          IF ( soilw1(I2,myThid).EQ.0. ) fMask1(I2,myThid) = 0.
         ENDDO
        ENDDO
       ELSE
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          fMask1(I2,myThid) = 0.
         ENDDO
        ENDDO
       ENDIF

C-     Set Snow depth and Sea Ice 
C      (not needed here since albedo is loaded from file)
c       DO j=1,sNy
c        DO i=1,sNx
c         I2 = i+(j-1)*sNx
c         oice1(I2,myThid) = 0.
c         snow1(I2,myThid) = 0.
c        ENDDO
c       ENDDO

C-- endif/else aim_useFMsurfBC
      ENDIF

#ifdef COMPONENT_MODULE
      IF ( useCoupler ) THEN
       IF ( useImportSST ) THEN
        DO J=1,sNy
         DO I=1,sNx

c         IF ( SSTocn(I,J,bi,bj) .NE. 0. ) THEN
          IF ( aim_landFr(i,j,bi,bj) .LT. 1. ) THEN
           I2 = (sNx)*(J-1)+I
C--   take SST from the ocean compon. where Sea-Ice fraction is zero
           IF ( oice1(I2).EQ.0. ) THEN
            sst1(I2,myThid) = SSTocn(i,j,bi,bj)+celsius2K
C--   take SST from the ocean compon. if clearly warmer than freezing
C       and reset sea-ice fraction & albedo
           ELSEIF ( SSTocn(i,j,bi,bj).GE. -1. _d 0)  THEN
            sst1(I2,myThid) = SSTocn(i,j,bi,bj)+celsius2K
            IF (aim_useFMsurfBC) THEN
             oice1(I2) = 0.
             IF (aim_landFr(i,j,bi,bj).EQ. 0.) THEN
              alb1(I2,myThid) = ALBSEA
             ELSE
C- note: this part never used with current coupled set-up (only full cell 
C        land / sea)
              alb_land = aim_albedo(i,j,bi,bj)
     &        + MAX( 0. _d 0, ALBSN-aim_albedo(i,j,bi,bj) )
     &         *MIN( 1. _d 0, RSD*snow1(I2))
              alb_sea  = ALBSEA + DALB*oice1(I2)
              alb1(I2,myThid) = alb_sea 
     &         + (alb_land - alb_sea)*fMask1(I2,myThid)
             ENDIF
            ENDIF
           ENDIF
          ENDIF

         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif /* COMPONENT_MODULE */

#ifdef ALLOW_LAND
      IF (useLand) THEN
C-    Use land model output instead of prescribed Temp & moisture
        CALL AIM_LAND2AIM( myTime, myIter, bi, bj, myThid ) 
      ENDIF
#endif /* ALLOW_LAND */

#endif /* ALLOW_AIM */

      RETURN
      END
