#include "EXF_OPTIONS.h"
#ifdef ALLOW_EXCH2
# include "W2_OPTIONS.h"
#endif /* ALLOW_EXCH2 */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: EXF_INIT_INTERP

C     !INTERFACE:
      SUBROUTINE EXF_INIT_INTERP( myThid )

C     !DESCRIPTION:
C     Define input grid setting to be used for EXF interpolated fields
C     as default input grid.
C     Note: default input grid matches (in case of simple Lat-Lonp grid)
C     model grid cell-center position, leading to trivial interpolation.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef USE_EXF_INTERPOLATION
# ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
# include "W2_EXCH2_TOPOLOGY.h"
# endif /* ALLOW_EXCH2 */
# include "SET_GRID.h"
#endif /* USE_EXF_INTERPOLATION */
#include "EXF_INTERP_SIZE.h"
#include "EXF_INTERP_PARAM.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef USE_EXF_INTERPOLATION
C     !LOCAL VARIABLES:
      INTEGER j
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

# ifdef ALLOW_EXCH2
      inp_gNx = exch2_mydNx(1)
      inp_gNy = exch2_mydNy(1)
# else /* ALLOW_EXCH2 */
      inp_gNx = Nx
      inp_gNy = Ny
# endif /* ALLOW_EXCH2 */

      inp_lon0 = xgOrigin + delX(1)*halfRL
      inp_lat0 = ygOrigin + delY(1)*halfRL

      inp_dLon = delX(1)
      DO j=1,MAX_LAT_INC
        IF (j.LT.inp_gNy) THEN
          inp_dLat(j) = (delY(j) + delY(j+1))*halfRL
        ELSE
          inp_dLat(j) = 0.
        ENDIF
      ENDDO

      _END_MASTER(myThid)
#endif /* USE_EXF_INTERPOLATION */

      RETURN
      END
