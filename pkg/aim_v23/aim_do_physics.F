C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_do_physics.F,v 1.12 2006/10/18 20:08:15 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_DO_PHYSICS
C     !INTERFACE:
      SUBROUTINE AIM_DO_PHYSICS( bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==================================================================*
C     | S/R AIM_DO_PHYSICS
C     *==================================================================*
C     | Interface between atmospheric physics package and the
C     | dynamical model.
C     | Routine calls physics pacakge after setting surface BC.
C     | Package should derive and set tendency terms
C     | which can be included as external forcing terms in the dynamical
C     | tendency routines. Packages should communicate this information
C     | through common blocks.
C     *==================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     -------------- Global variables ------------------------------------
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

C-- MITgcm
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"

C-- Physics package
#include "AIM_PARAMS.h"
#include "AIM_FFIELDS.h"
#include "AIM_GRID.h"
#include "com_physvar.h"
#include "com_forcing.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj  ::  Tile indices
C     myTime ::  Current time of simulation ( s )
C     myIter ::  Current iteration number in simulation
C     myThid ::  Number of this instance of the routine
      INTEGER bi, bj, myIter, myThid
      _RL myTime
CEOP

#ifdef ALLOW_AIM
C     == Local variables ==
C     i,j,k,I2   :: Loop counters
C     tYear      :: Fraction into year
C     aim_sWght0 :: weight for time interpolation of surface BC
C     aim_sWght1 :: 0/1 = time period before/after the current time
C     prcAtm     :: total precip from the atmosphere [kg/m2/s]
      INTEGER i,j,k,I2
      _RL     tYear, yearLength
      _RL     aim_sWght0, aim_sWght1
      _RL prcAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C_jmc: Because AIM physics LSC is not applied in the stratosphere (top level),
C      ==> move water wapor from the stratos to the surface level.
      DO j = 1-Oly, sNy+Oly
       DO i = 1-Olx, sNx+Olx
        k = ksurfC(i,j,bi,bj)
        IF (k.LE.Nr)
     &    salt(i,j,k,bi,bj) = salt(i,j,k,bi,bj)
     &                      + salt(i,j,Nr,bi,bj)*drF(Nr)*recip_drF(k)
     &                  *hFacC(i,j,Nr,bi,bj)*recip_hFacC(i,j,k,bi,bj)
        salt(i,j,Nr,bi,bj) = 0.
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Physics package needs to know time of year as a fraction
      yearLength = 86400.*360.
      tYear = MOD(myTime/yearLength, 1. _d 0)

C--   Set surface Boundary Conditions for atmos. physics package:
C     (Albedo, Soil moisture, Surf Temp, Land sea mask)
C     includes some parts of S/R FORDATE from F.Molteni SPEDDY code (ver23)
      CALL AIM_SURF_BC(
     U                  tYear,
     O                  aim_sWght0, aim_sWght1,
     I                  bi, bj, myTime, myIter, myThid )

C--   Set surface geopotential: (g * orographic height)
      DO j=1,sNy
       DO i=1,sNx
         I2 = i+(j-1)*sNx
         PHI0(I2) = gravity*topoZ(i,j,bi,bj)
       ENDDO
      ENDDO

C--   Set topographic dependent FOROG var (originally in common SFLFIX);
C      used to compute for wind stress over land

c_FM  IF (IDAY.EQ.0) THEN
c_FM    CALL SFLSET (PHIS0)
        CALL SFLSET (PHI0, fOrogr(1,myThid), bi,bj,myThid)
c_FM  ENDIF
c_FM  CALL SOL_OZ (SOLC,TYEAR)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Compute atmospheric-physics tendencies (call the main AIM S/R)
      CALL PHY_DRIVER( tYear, useDiagnostics,
     I                  bi, bj, myTime, myIter, myThid )

      CALL AIM_AIM2DYN( bi, bj, myTime, myIter, myThid )

#ifdef ALLOW_LAND
      IF (useLand) THEN
C-    prepare Surface flux over land for land package
        CALL AIM_AIM2LAND( aim_landFr, bi, bj,
     I                     myTime, myIter, myThid )

C-    Step forward land model
        CALL LAND_STEPFWD( aim_landFr, bi, bj,
     I                     myTime, myIter, myThid )

C-    Land diagnostics : write snap-shot & cumulate for TimeAve output
      CALL LAND_DO_DIAGS(  aim_landFr, bi, bj,
     I                     myTime, myIter, myThid )

      ENDIF
#endif /* ALLOW_LAND */

C-    surface fluxes over ocean (ice-free & ice covered)
C       used for diagnostics, thsice package and coupler
        CALL AIM_AIM2SIOCE( aim_landFr, fmask1(1,3,myThid),
     O                      prcAtm,
     I                      bi, bj, myTime, myIter, myThid )

#ifdef ALLOW_THSICE
      IF ( useThSIce ) THEN

C-    Step forward sea-ice model
        CALL THSICE_STEP_FWD( bi, bj, 1, sNx, 1, sNy,
     I                        prcAtm,
     I                        myTime, myIter, myThid )

C-    Slab Ocean : step forward ocean mixed-layer temp. & salinity
        CALL THSICE_SLAB_OCEAN(
     I                      aim_sWght0, aim_sWght1,
     O                      dTsurf(1,2,myThid),
     I                      bi, bj, myTime, myIter, myThid )

        CALL THSICE_AVE(
     I                   bi, bj, myTime, myIter, myThid )

      ENDIF
#endif /* ALLOW_THSICE */

#ifdef COMPONENT_MODULE
      IF ( useCoupler ) THEN
        CALL ATM_STORE_MY_DATA( bi, bj, myIter, myTime, myThid )
      ENDIF
#endif /* COMPONENT_MODULE */

C-    AIM diagnostics : write snap-shot & cumulate for TimeAve output
      CALL AIM_DIAGNOSTICS( bi, bj, myTime, myIter, myThid )

#endif /* ALLOW_AIM */

      RETURN
      END
