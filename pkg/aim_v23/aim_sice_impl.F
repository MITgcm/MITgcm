C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_sice_impl.F,v 1.3 2006/05/25 18:05:24 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
c#ifdef ALLOW_THSICE
c#include "THSICE_OPTIONS.h"
c#endif

CBOP
C     !ROUTINE: AIM_SICE_IMPL
C     !INTERFACE:
      SUBROUTINE AIM_SICE_IMPL(
     I               FMASK, netSW, sFlx,
     I               Shf0, dShf, Evp0, dEvp, Slr0, dSlr,
     U               Tsurf, SHF, EVAP, SLRU,
     O               dTsurf,
     I               bi, bj, myTime, myIter, myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_SICE_IMPL
C     | o AIM Interface to the implicit part of the sea-ice model
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

#ifdef ALLOW_THSICE
c#include "THSICE_SIZE.h"
c#include "THSICE_PARAMS.h"
c#include "THSICE_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     FMASK    :: sea-ice fraction [0-1]
C     netSW    :: net Short Wave surf. flux (+=down) [W/m2]
C     sFlx     :: net heat flux (+=down) except SW, function of surf. temp Ts:
C                 0: Flux(Ts=0.oC) ; 1: Flux(Ts^n) ; 2: d.Flux/d.Ts(Ts^n)
C     Shf0     :: sensible heat flux over freezing surf.
C     dShf     :: sensible heat flux derivative relative to surf. temp
C     Evp0     :: evaporation computed over freezing surface (Ts=0.oC)
C     dEvp     :: evaporation derivative relative to surf. temp
C     Slr0     :: upward long wave radiation over freezing surf.
C     Tsurf    :: surface temperature        (2-dim)
C     SHF      :: sensible heat flux              (2-dim)
C     EVAP     :: evaporation [g/(m^2 s)]         (2-dim)
C     SLRU     :: sfc lw radiation (upward flux)  (2-dim)
C     dTsurf   :: surf. temp change after 1 implicit time step [oC]
C     bi,bj    :: Tile index
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: Number of this instance of the routine
      _RL  FMASK(NGP), netSW(NGP), sFlx(NGP,0:2)
      _RL  Shf0(NGP), dShf(NGP), Evp0(NGP), dEvp(NGP)
      _RL  Slr0(NGP), dSlr(NGP)
      _RL  Tsurf(NGP), SHF(NGP), EVAP(NGP), SLRU(NGP)
      _RL  dTsurf(NGP)
      INTEGER bi, bj, myIter, myThid
      _RL myTime
CEOP

#ifdef ALLOW_AIM
#ifdef ALLOW_THSICE
C     == Local variables ==
C       J          :: loop counters
      INTEGER J

C--   Physics tendency term

      IF ( useThSIce ) THEN

       DO J=1,NGP
C-    total surface downward heat flux :

C-    initialize temp. changes and fresh water flux :
         dTsurf(J) = 0.
       ENDDO

       CALL THSICE_IMPL_TEMP(
     I               netSW, sFlx,
     O               dTsurf,
     I               bi, bj, myTime, myIter, myThid)

C-     Update Surf.Temp., Evap, Upward SW according to surf. temp. changes
       DO J=1,NGP
        IF ( dTsurf(J) .GT. 999. ) THEN
         dTsurf(J)= tFreeze - Tsurf(J)
         Tsurf(J) = tFreeze
         SHF (J)  = Shf0(J)
         EVAP(J)  = Evp0(J)
         SLRU(J)  = Slr0(J)
        ELSE
         Tsurf(J) = Tsurf(J)+ dTsurf(J)
         SHF (J)  = SHF (J) + dTsurf(J)*dShf(J)
         EVAP(J)  = EVAP(J) + dTsurf(J)*dEvp(J)
         SLRU(J)  = SLRU(J) + dTsurf(J)*dSlr(J)
        ENDIF
       ENDDO

      ENDIF

#endif /* ALLOW_THSICE */
#endif /* ALLOW_AIM */

      RETURN
      END
