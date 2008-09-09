C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_load_climatology.F,v 1.2 2008/09/09 19:58:26 jmc Exp $
C $Name:  $

#include "EBM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE EBM_LOAD_CLIMATOLOGY( myThid )
C     |==========================================================|
C     | S/R EBM_LOAD_CLIMATOLOGY                                 |
C     |==========================================================|

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#ifdef ALLOW_EBM
# include "EBM.h"
#endif
C     === Routine arguments ===
      INTEGER myThid

CEndOfInterface

#ifdef ALLOW_EBM

C     === Local variables ===
C     msgBuf - Error message buffer
      INTEGER bi,bj,i,j
      _RL distY,tauX,tauMax,lY

      IF ( saltClimFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( saltClimFile, ' ', SSS, 0, myThid )
      ENDIF

      IF ( thetaClimFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( thetaClimFile, ' ', SST, 0, myThid )
      ENDIF

      IF ( RunoffFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( RunoffFile, ' ', Run, 0, myThid )
      ENDIF

      IF ( zonalWindFile .EQ. ' ' ) THEN
C      In cartesian yc, delY and ly are meters.
C      In spherical polar yc, delY and ly are degrees
       tauMax = 0.1 _d 0
       tauMax = 1.0 * 1./(delR(1)*rhonil)
       lY = 0. _d 0
       DO j=1,Ny-1
        lY = lY + delY(j)
       ENDDO
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           distY = (yC(i,j,bi,bj)-(yC0))/lY
C          tauX  = -tauMax*cos(2. _d 0*PI*distY)
           tauX  = tauMax*sin(PI*distY)
           fu(i,j,bi,bj) = tauX
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       CALL READ_FLD_XY_RS( zonalWindFile, ' ', fu, 0, myThid )
      ENDIF

      IF (meridWindFile .EQ. ' ' ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fv(i,j,bi,bj) = 0.0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       CALL READ_FLD_XY_RS( meridWindFile, ' ', fv, 0, myThid )
      ENDIF

      _EXCH_XY_R4( SSS, myThid )
      _EXCH_XY_R4( SST, myThid )
      _EXCH_XY_R4( Run, myThid )
      CALL EXCH_UV_XY_RS( fu, fv, .TRUE., myThid )

      CALL PLOT_FIELD_XYRS( SST, 'Theta Climatology' , 1, myThid )
      CALL PLOT_FIELD_XYRS( SSS, 'Salt  Climatology' , 1, myThid )
      CALL PLOT_FIELD_XYRS( Run, 'Runoff Climatology' , 1, myThid )
      CALL PLOT_FIELD_XYRS( fu,
     &   'WIND_STRESS_CLIMATOLOGY FU',1,myThid)
      CALL PLOT_FIELD_XYRS( fv,
     &   'WIND_STRESS_CLIMATOLOGY FV',1,myThid)

#endif

      RETURN
      END
