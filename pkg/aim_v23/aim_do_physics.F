C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_do_physics.F,v 1.1 2002/11/22 17:17:03 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE AIM_DO_PHYSICS( bi, bj, myTime, myIter, myThid ) 
                                      
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
#include "AIM_FFIELDS.h"
#include "AIM_GRID.h"
#include "com_physvar.h"
#include "com_forcing.h"

C     == Routine arguments ==
C     bi,bj  - Tile index 
C     myTime - Current time of simulation ( s )
C     myIter - Current iteration number in simulation
C     myThid - Number of this instance of the routine
      INTEGER bi, bj, myIter, myThid
      _RL myTime

#ifdef ALLOW_AIM
C     == Local variables ==
C     I,J,K,I2      - Loop counters
C     tYear         - Fraction into year
C     Katm          - Atmospheric K index
      INTEGER I,J,K,I2
c     INTEGER Katm
      _RL     tYear, yearLength

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

c      CALL FORDATE
C- extract parts of S/R FORDATE from Franco Molteni Physics :

C-    Physics package needs to know time of year as a fraction
      yearLength = 86400.*360.
      tYear = mod(myTime/yearLength, 1. _d 0)
c     tYear = myTime/(86400.*360.) -
c    &        FLOAT(INT(myTime/(86400.*360.)))

C-    Load external data needed by physics package
C     1. Albedo                (between 0-1)
C     2. Soil moisture         (between 0-1)
C     3. Surface temperatures  (in situ Temp. [K])
C     4. Snow depth            - assume no snow for now
C     5. Sea ice               - assume no sea ice for now
C     6. Land sea mask         - infer from exact zeros in soil moisture dataset
C     7. Surface geopotential  - to be done when orography is in
C                                dynamical kernel. Assume 0. for now.

C      Load in surface albedo data (in [0,1]) from aim_albedo to alb1 :
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         alb1(I2,myThid) = 0.
         alb1(I2,myThid) = aim_albedo(I,J,bi,bj)
        ENDDO
       ENDDO
C      Load in surface temperature data from aim_surfTemp to stl1 & sst1 :
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         sst1(I2,myThid) = 300.
         stl1(I2,myThid) = 300.
         sst1(I2,myThid) = aim_surfTemp(I,J,bi,bj)
         stl1(I2,myThid) = aim_surfTemp(I,J,bi,bj)
        ENDDO
       ENDDO

C      Load in soil water availability (in [0,1]) from aim_soilWater to soilw1 :
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         soilw1(I2,myThid) = 0.
         soilw1(I2,myThid) = aim_soilWater(I,J,bi,bj)
        ENDDO
       ENDDO
C      Set snow depth, sea ice to zero for now
C      Land-sea mask ( figure this out from where 
C                      soil moisture is exactly zero ).
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         fMask1(I2,myThid) = 1.
         IF ( soilw1(I2,myThid) .EQ. 0. ) fMask1(I2,myThid) = 0.
         oice1(I2,myThid) = 0.
         snow1(I2,myThid) = 0.
        ENDDO
       ENDDO
C      open(77,file='lsmask',form='unformatted')
C      write(77) fmask1
C      close(77)

C--   Set surface geopotential: (g * orographic height)
      DO J=1,sNy
       DO I=1,sNx
         I2 = I+(J-1)*sNx
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
      CALL PHY_DRIVER( tYear, myTime, myIter, bi, bj, myThid )

C-    AIM diagnostics : write snap-shot & cumulate for TimeAve output
      CALL AIM_DIAGNOSTICS( bi, bj, myTime, myIter, myThid )

      CALL AIM_AIM2DYN( bi, bj, myTime, myIter, myThid )

#endif /* ALLOW_AIM */

      RETURN
      END
