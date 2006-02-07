C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_thermodynamics.F,v 1.1 2006/02/07 11:45:21 mlosch Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"
 
CBOP
C     !ROUTINE: SHELFICE_THERMODYNAMICS
C     !INTERFACE:
      SUBROUTINE SHELFICE_THERMODYNAMICS( 
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  SHELFICE_THERMODYNAMICS             
C     | o shelf-ice main routine. 
C     |   compute temperature and (virtual) salt flux at the 
C     |   shelf-ice ocean interface
C     |
C     | stresses at the ice/water interface are computed in separate
C     | routines that are called from mom_fluxform/mom_vecinv
C     *=============================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
CML#include "EOS.h"
#include "SHELFICE.h"
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER I,J,K
      INTEGER bi,bj
      _RL tLoc, sLoc, pLoc
      _RL Tfreezing, a0, a1, a2, b, c0

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      a0 = -0.0575   _d  0
      a1 = 1.710523  _d -3
      a2 = -2.154996 _d -4
      b  = -7.53     _d -4
      c0 = 0. _d 0
CMLC     linearized version
CML      a0 = -0.0575   _d  0
CML      c0 =  0.0901   _d  0
CML      b  =  -7.61    _d -4

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_ISOMIP_TD
        IF ( useISOMIPTD ) THEN
         DO J = 1, sNy
          DO I = 1, sNx
           K    = kTopC(I,J,bi,bj)
           pLoc = ABS(R_shelfIce(I,J,bi,bj))
           IF ( K .NE. 0 .AND. pLoc .GT. 0. _d 0 ) THEN
C--   Calculate the in-situ temperature 
            tLoc = theta(I,J,K,bi,bj)
            sLoc = max(salt(I,J,K,bi,bj), 0. _d 0)
C--   Calculate freezing temperature as a function of salinity and pressure
CML           Tfreezing=-1.9 _d 0
            Tfreezing = sLoc * ( a0 + a1*sqrt(sLoc) + a2*sLoc )
     &           + b*pLoc + c0

C--   Calculate the upward heat and  fresh water fluxes
            shelfIceHeatFlux(I,J,bi,bj) = 
     &           SHELFICEexchangeVelocity * ( tLoc - Tfreezing )
     &           *HeatCapacity_Cp*recip_horiVertRatio*rhoConst
C     upward heat flux into the shelf-ice implies basal melting,
C     thus a downward (negative upward) fresh water flux, and vice versa
            shelfIceFreshWaterFlux(I,J,bi,bj) = 
     &           - shelfIceHeatFlux(I,J,bi,bj)
     &           *recip_rhoConst*recip_SHELFICElatentHeat
C--   compute surface tendencies
            shelficeForcingT(i,j,bi,bj) =
     &           - shelfIceHeatFlux(I,J,bi,bj)
     &           *recip_Cp*horiVertRatio*recip_rhoConst
            shelficeForcingS(i,j,bi,bj) = 
     &           shelfIceFreshWaterFlux(I,J,bi,bj)
     &           * convertFW2Salt * convertEmP2rUnit
            surfaceForcingT(i,j,bi,bj) = shelficeForcingT(i,j,bi,bj)
            surfaceForcingS(i,j,bi,bj) = shelficeForcingS(i,j,bi,bj)
C--   stress at the ice/water interface is computed in separate
C     routines that are called from mom_fluxform/mom_vecinv
           ELSE
            shelficeForcingT(i,j,bi,bj) = 0. _d 0
            shelficeForcingS(i,j,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ELSE
#else 
        IF ( .TRUE. ) THEN
#endif /* ALLOW_ISOMIP_TD */
#ifdef ALLOW_HELLMER
#endif /* ALLOW_HELLMER */
        ENDIF
       ENDDO
      ENDDO
      
#endif /* ALLOW_SHELFICE */
      RETURN
      END
