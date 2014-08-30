C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_init_varia.F,v 1.5 2014/08/30 00:34:02 jmc Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

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
#ifdef ALLOW_COST
# include "DIC_COST.h"
#endif
C     !INPUT PARAMETERS:
C     myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIC
      INTEGER i,j, bi,bj
      INTEGER k
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Initialise variable in common block DIC_ATMOS
      _BEGIN_MASTER(myThid)
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
      _END_MASTER(myThid)
      _BARRIER

#ifdef DIC_BIOTIC
C--   Initialise alpha & rain_ratio fields with fixed (& Uniform) values
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            alpha(i,j,bi,bj)      = alphaUniform
            rain_ratio(i,j,bi,bj) = rainRatioUniform
          ENDDO
        ENDDO
        DO k = 1, Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            omegaC(i,j,k,bi,bj) =  0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* DIC_BIOTIC */

#endif /* ALLOW_DIC */

      RETURN
      END
