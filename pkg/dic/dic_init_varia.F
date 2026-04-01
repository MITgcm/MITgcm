#include "DIC_OPTIONS.h"
#undef USE_OLD_READ_PICKUP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DIC_INIT_VARIA

C     !INTERFACE:
      SUBROUTINE DIC_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Initialize variable quantities

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
#include "DIC_ATMOS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#ifdef ALLOW_COST
# include "DIC_COST.h"
#endif
C     !INPUT PARAMETERS:
C     myThid               :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIC
      INTEGER i,j, bi,bj
# ifdef DIC_CALCITE_SAT
      INTEGER k
# endif
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Initialise variable in common block DIC_ATMOS
#ifndef ALLOW_AUTODIFF
      _BEGIN_MASTER(myThid)
#endif
      total_atmos_carbon = 0. _d 0
      total_ocean_carbon = 0. _d 0
      total_atmos_carbon_year  = 0. _d 0
      total_atmos_carbon_start = 0. _d 0
      total_ocean_carbon_year  = 0. _d 0
      total_ocean_carbon_start = 0. _d 0
      atpco2 = 0. _d 0
#ifdef ALLOW_COST
      totcost =  0. _d 0
#endif
C 2d surface pH
      pH_isLoaded(1) = .FALSE.
C 3d full ocean pH
      pH_isLoaded(2) = .FALSE.
#ifndef ALLOW_AUTODIFF
      _END_MASTER(myThid)
      _BARRIER
#endif

C--   Initialise variables other than forcing (done in ini_forcing):
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           FluxCO2(i,j,bi,bj)  = 0. _d 0
           pH(i,j,bi,bj)       = 8. _d 0
         ENDDO
        ENDDO
#ifdef DIC_BIOTIC
C-    Initialise alpha & rain_ratio fields with fixed (& Uniform) values
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           alpha(i,j,bi,bj)      = alphaUniform
           rain_ratio(i,j,bi,bj) = rainRatioUniform
         ENDDO
        ENDDO
#endif /* DIC_BIOTIC */
#ifdef DIC_CALCITE_SAT
        DO k = 1, Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           omegaC(i,j,k,bi,bj) = 0. _d 0
           pH3D(i,j,k,bi,bj)   = 7.9 _d 0
          ENDDO
         ENDDO
        ENDDO
#endif /* DIC_CALCITE_SAT */
       ENDDO
      ENDDO

C--   Initialise forcing field variables (was called from gchem_init_vari.F)
      CALL DIC_INI_FORCING(myThid)

C-  read pickup
      IF ( nIter0.GT.PTRACERS_Iter0 .OR.
     &    (nIter0.EQ.PTRACERS_Iter0 .AND. pickupSuff.NE.' ')
     &   ) THEN
C       Read pH from a pickup file if needed
        CALL DIC_READ_PICKUP( nIter0, myThid )
      ENDIF

C-    Move these S/R call here (were previously in gchem_init_vari.F), except
C     DIC_INI_ATMOS which was called from the top of DIC_SURFFORCING_INIT
      CALL DIC_INI_ATMOS( startTime, nIter0, myThid )
      CALL DIC_SURFFORCING_INIT(myThid)
# ifdef ALLOW_CTRL
      CALL DIC_SET_CONTROL(myThid)
# endif

#endif /* ALLOW_DIC */

      RETURN
      END
