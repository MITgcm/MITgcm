C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_land_impl.F,v 1.3 2004/06/24 23:43:11 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
#ifdef ALLOW_LAND
#include "LAND_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: AIM_LAND_IMPL
C     !INTERFACE:
      SUBROUTINE AIM_LAND_IMPL(
     I               FMASK, dTskin,
     I               Shf0, dShf, Evp0, dEvp, Slr0, dSlr,
     U               sFlx,
     U               Tsurf, SHF, EVAP, SLRU,
     O               dTsurf,
     I               bi, bj, myTime, myIter, myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_LAND_IMPL
C     | o AIM Interface to the implicit part of the land model
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 

#include "EEPARAMS.h"
#include "PARAMS.h"

#include "AIM_FFIELDS.h"
#include "com_physcon.h"
c #include "com_physvar.h"

#ifdef ALLOW_LAND
#include "LAND_SIZE.h" 
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     FMASK    :: land fraction [0-1]
C     dTskin   :: temp. correction for daily-cycle heating [K]
C     Shf0     :: sensible heat flux over freezing surf.
C     dShf     :: sensible heat flux derivative relative to surf. temp
C     Evp0     :: evaporation computed over freezing surface (Ts=0.oC)
C     dEvp     :: evaporation derivative relative to surf. temp
C     Slr0     :: upward long wave radiation over freezing surf.
C     dSlr     :: upward long wave derivative relative to surf. temp
C     sFlx     :: net surface flux (+=down) function of surf. temp Ts:
C                 0: Flux(Ts=0.oC) ; 1: Flux(Ts^n) ; 2: d.Flux/d.Ts(Ts^n)
C     Tsurf    :: surface temperature        (2-dim)
C     SHF      :: sensible heat flux              (2-dim)
C     EVAP     :: evaporation [g/(m^2 s)]         (2-dim)
C     SLRU     :: sfc lw radiation (upward flux)  (2-dim)
C     dTsurf   :: surf. temp change after 1 implicit time step [oC]
C     bi,bj    :: Tile index
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: Number of this instance of the routine
      _RL  FMASK(NGP), dTskin(NGP)
      _RL  Shf0(NGP), dShf(NGP), Evp0(NGP), dEvp(NGP)
      _RL  Slr0(NGP), dSlr(NGP), sFlx(NGP,0:2)
      _RL  Tsurf(NGP), SHF(NGP), EVAP(NGP), SLRU(NGP)
      _RL  dTsurf(NGP)
      INTEGER bi, bj, myIter, myThid
      _RL myTime
CEOP

#ifdef ALLOW_AIM
#ifdef ALLOW_LAND
C     == Local variables ==
C     i,j, I2      :: loop counters
      INTEGER i,j, I2

      IF ( useLand .AND. land_impl_grT ) THEN

C-     Initialisation :
       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx

C-    initialize temp. changes and fresh water flux :
         dTsurf(I2) = 0.
         land_Pr_m_Ev(i,j,bi,bj) = 0. _d 0
         land_EnWFlux(i,j,bi,bj) = 0. _d 0

        ENDDO
       ENDDO

       IF ( land_calc_snow ) THEN
C-     Evap of snow: substract Latent Heat of freezing from heatFlux
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          IF ( land_skinT(i,j,bi,bj).LT. 0. _d 0 .OR.
     &         land_hSnow(i,j,bi,bj).GT. 0. _d 0 ) THEN
           sFlx(I2,0) = sFlx(I2,0) - ALHF*Evp0(I2)
           sFlx(I2,1) = sFlx(I2,1) - ALHF*EVAP(I2)
           sFlx(I2,2) = sFlx(I2,2) - ALHF*dEvp(I2)
           land_EnWFlux(i,j,bi,bj) = -ALHF
          ENDIF
         ENDDO
        ENDDO
       ENDIF

       CALL LAND_IMPL_TEMP(
     I               aim_landFr, 
     I               dTskin, sFlx,
     O               dTsurf,
     I               bi, bj, myTime, myIter, myThid)

C--    Surface B.C. for atmospheric physics:
C-     Update Evap, Upward SW according to surf. temp. changes
       DO J=1,NGP
        IF ( dTsurf(J) .GT. 999. ) THEN
         SHF (J)  = Shf0(J)
         EVAP(J)  = Evp0(J)
         SLRU(J)  = Slr0(J)
        ELSE
         SHF (J)  = SHF (J) + dTsurf(J)*dShf(J)
         EVAP(J)  = EVAP(J) + dTsurf(J)*dEvp(J)
         SLRU(J)  = SLRU(J) + dTsurf(J)*dSlr(J)
        ENDIF
       ENDDO

C--    Update surface fluxes for Land model: 
       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx
C-     net surface downward heat flux :
         IF ( dTsurf(I2) .GT. 999. ) THEN
          land_HeatFlx(i,j,bi,bj) = sFlx(I2,0)
         ELSE
          land_HeatFlx(i,j,bi,bj) = sFlx(I2,1)+dTsurf(I2)*sFlx(I2,2)
         ENDIF
C-     energy flux associated with Evap of Snow
         land_EnWFlux(i,j,bi,bj) = -land_EnWFlux(i,j,bi,bj)*EVAP(I2)
        ENDDO
       ENDDO

C-     Update Surf.Temp.:
       DO J=1,NGP
        IF ( dTsurf(J) .GT. 999. ) THEN
         dTsurf(J)= tFreeze - Tsurf(J)
         Tsurf(J) = tFreeze
        ELSE
         Tsurf(J) = Tsurf(J)+ dTsurf(J)
        ENDIF
       ENDDO

C- end (if useLand & land_impl_grT)
      ENDIF

#endif /* ALLOW_LAND */
#endif /* ALLOW_AIM */

      RETURN
      END
