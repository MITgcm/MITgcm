C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_land2aim.F,v 1.2 2003/07/31 18:24:54 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
#ifdef ALLOW_LAND
#include "LAND_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: AIM_LAND2AIM
C     !INTERFACE:
      SUBROUTINE AIM_LAND2AIM( myTime, myIter, bi, bj, myThid ) 

C     !DESCRIPTION: \bv
C     *================================================================*
C     | S/R AIM_LAND2AIM
C     | provide surface Boundary Conditions over land (from land pkg)
C     |  to atmospheric physics package AIM
C     *================================================================*
C     | note: by default, use AIM surf.B.C. fields as initial state
C     |   (myIter=0) for the land model 
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

C-- MITgcm
#include "EEPARAMS.h"
#include "PARAMS.h"

C-- Physics package
#include "AIM_PARAMS.h"
#include "com_forcing.h"

#ifdef ALLOW_LAND
C-- Land package
#include "LAND_SIZE.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"
#endif

C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Current iteration number in simulation
C     bi,bj  - Tile index 
C     myThid - Number of this instance of the routine
      INTEGER myIter, bi, bj, myThid
      _RL myTime
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AIM
#ifdef ALLOW_LAND

C     == Local variables ==
C     i,j,k,I2      - Loop counters
      INTEGER i,j,k,I2

c     IF (useLand) THEN

       IF ( land_calc_grT ) THEN
C-    Use land-pkg output instead of prescribed ground Temp
        DO j=1,sNy
         DO i=1,sNx
           I2 = i+(j-1)*sNx
           stl1(I2,myThid) = land_groundT(i,j,1,bi,bj)+celsius2K
         ENDDO
        ENDDO
       ELSE
C-    Fill in land-pkg ground Temp. using AIM surf. fields
        DO k=1,land_nLev
         DO j=1,sNy
          DO i=1,sNx
           I2 = i+(j-1)*sNx
           land_groundT(i,j,k,bi,bj) = stl1(I2,myThid)-celsius2K
          ENDDO
         ENDDO
        ENDDO
       ENDIF

       IF (land_calc_grW) THEN
C-    Use land-pkg output instead of prescribed soil moisture
        DO j=1,sNy
         DO i=1,sNx
           I2 = i+(j-1)*sNx
           soilw1(I2,myThid) = land_groundW(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ELSE
C-    Fill in land-pkg soil moisture using AIM surf. fields
        DO k=1,land_nLev
         DO j=1,sNy
          DO i=1,sNx
           I2 = i+(j-1)*sNx
           land_groundW(i,j,k,bi,bj) = soilw1(I2,myThid)
          ENDDO
         ENDDO
        ENDDO
       ENDIF

c     ENDIF

#endif /* ALLOW_LAND */
#endif /* ALLOW_AIM */

      RETURN
      END
