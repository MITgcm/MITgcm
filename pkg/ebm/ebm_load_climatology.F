C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_load_climatology.F,v 1.7 2011/12/12 19:03:09 jmc Exp $
C $Name:  $

#include "EBM_OPTIONS.h"
#ifdef ALLOW_EXCH2
# include "W2_OPTIONS.h"
#endif /* ALLOW_EXCH2 */

CBOP 0
C !ROUTINE: EBM_LOAD_CLIMATOLOGY

C !INTERFACE:
      SUBROUTINE EBM_LOAD_CLIMATOLOGY( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R EBM_LOAD_CLIMATOLOGY
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
# include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */
#include "GRID.h"
#include "SET_GRID.h"
#include "FFIELDS.h"
#ifdef ALLOW_EBM
# include "EBM.h"
#endif

C     !INPUT PARAMETERS:
C     myThid     :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_EBM
C     !LOCAL VARIABLES:
      INTEGER gridNy
      INTEGER bi,bj,i,j
      _RL distY,tauX,tauMax,lY

#ifdef ALLOW_EXCH2
      gridNy = exch2_mydNy(1)
#else /* ALLOW_EXCH2 */
      gridNy = Ny
#endif /* ALLOW_EXCH2 */

      IF ( RunoffFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( RunoffFile, ' ', Run, 0, myThid )
      ENDIF

      IF ( zonalWindFile .EQ. ' ' ) THEN
C      In cartesian yc, delY and ly are meters.
C      In spherical polar yc, delY and ly are degrees
       tauMax = 0.1 _d 0
       tauMax = 1.0 * 1./(delR(1)*rhonil)
       lY = 0. _d 0
       DO j=1,gridNy-1
        lY = lY + delY(j)
       ENDDO
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
c          distY = (yC(i,j,bi,bj)-(yC0))/lY
c jmc:    yC0 has been removed (not used except here,
c         and since it was not initialised, was generaly zero):
           distY = (yC(i,j,bi,bj)- 0.  )/lY
c          tauX  = -tauMax*cos(2. _d 0*PI*distY)
           tauX  = tauMax*sin(PI*distY)
           fu(i,j,bi,bj) = tauX
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      _EXCH_XY_RS( Run, myThid )

      IF ( debugLevel.GE.debLevC ) THEN
        CALL PLOT_FIELD_XYRS( SST, 'Theta Climatology' , 1, myThid )
        CALL PLOT_FIELD_XYRS( SSS, 'Salt  Climatology' , 1, myThid )
        CALL PLOT_FIELD_XYRS( Run, 'Runoff Climatology', 1, myThid )
        CALL PLOT_FIELD_XYRS(fu,'Wind Stress Climatology FU',1,myThid)
        CALL PLOT_FIELD_XYRS(fv,'Wind Stress Climatology FV',1,myThid)
      ENDIF

#endif /* ALLOW_EBM */

      RETURN
      END
