C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_land_impl.F,v 1.1 2004/03/11 14:33:18 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
#ifdef ALLOW_LAND
#include "LAND_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: AIM_LAND_IMPL
C     !INTERFACE:
      SUBROUTINE AIM_LAND_IMPL(
     I               FMASK, 
     I               dTskin, sFlx,
     I               Evp0, dEvp, Slr0, dSlr,
     U               Tsurf, EVAP, SLRU,
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
C     sFlx     :: net surface flux (+=down) function of surf. temp Ts:
C                 0: Flux(Ts=0.oC) ; 1: Flux(Ts^n) ; 2: d.Flux/d.Ts(Ts^n)
C     Evp0     :: evaporation computed over freezing surface (Ts=0.oC)
C     dEvp     :: evaporation derivative relative to surf. temp
C     Slr0     :: upward long wave radiation over freezing surf.
C     Tsurf    :: surface temperature        (2-dim)
C     EVAP     :: evaporation [g/(m^2 s)]         (2-dim)
C     SLRU     :: sfc lw radiation (upward flux)  (2-dim)
C     bi,bj    :: Tile index
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: Number of this instance of the routine
      _RL  FMASK(NGP), dTskin(NGP), sFlx(NGP,0:2)
      _RL  Evp0(NGP), dEvp(NGP), Slr0(NGP), dSlr(NGP)
      _RL  Tsurf(NGP), EVAP(NGP), SLRU(NGP)
      INTEGER bi, bj, myIter, myThid
      _RL myTime
CEOP

#ifdef ALLOW_AIM
#ifdef ALLOW_LAND
C     == Local variables ==
C     i,j, I2      :: loop counters
C     dTsurf       :: surf. temp change after 1 implicit time step [oC]
      _RL  dTsurf(NGP)
      INTEGER i,j, I2

C--   Physics tendency term

      IF ( land_impl_grT ) THEN

       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx

C-    total surface downward heat flux :
         land_HeatFLx(i,j,bi,bj) = sFlx(I2,1)
 
C-    initialize temp. changes and fresh water flux :
         dTsurf(I2) = 0.
         land_Pr_m_Ev(i,j,bi,bj) = 0. _d 0

        ENDDO
       ENDDO

       CALL LAND_IMPL_TEMP(
     I               aim_landFr, 
     I               dTskin, sFlx,
     O               dTsurf,
     I               bi, bj, myTime, myIter, myThid)

C-     Update Surf.Temp., Evap, Upward SW according to surf. temp. changes
       DO J=1,NGP
        IF ( dTsurf(J) .GT. 999. ) THEN
         Tsurf(J) = tFreeze
         EVAP(J)  = Evp0(J)
         SLRU(J)  = Slr0(J)
        ELSE
         Tsurf(J) = Tsurf(J)+ dTsurf(J)
         EVAP(J)  = EVAP(J) + dTsurf(J)*dEvp(J)
         SLRU(J)  = SLRU(J) + dTsurf(J)*dSlr(J)
        ENDIF
       ENDDO

C- end (if land_impl_grT)
      ENDIF

#endif /* ALLOW_LAND */
#endif /* ALLOW_AIM */

      RETURN
      END
