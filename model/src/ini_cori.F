C $Header: /u/gcmpack/MITgcm/model/src/ini_cori.F,v 1.20 2004/10/13 04:37:37 edhill Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_CORI

C     !INTERFACE:
      SUBROUTINE INI_CORI( myThid )
C     !DESCRIPTION:
C     Initialise coriolis term.                               

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_MONITOR
#include "MONITOR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     bi,bj  - Loop counters
C     I,J,K
C     facGrid - Factor for grid to meter conversion
      INTEGER bi, bj
      INTEGER  I,  J, K
      _RL facGrid

C     Initialise coriolis parameter
      IF     ( useConstantF ) THEN
C       Constant F case
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO K=1,Nr
              DO J=1-Oly,sNy+Oly
                DO I=1-Olx,sNx+Olx
                  fCori(i,j,bi,bj)=f0
                  fCoriG(i,j,bi,bj)=f0
                  fCoriCos(i,j,bi,bj)=0.
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ELSEIF ( useBetaPlaneF ) THEN
C       Beta plane case
        facGrid = 1. _d 0
        IF ( usingSphericalPolarGrid ) facGrid = deg2rad*rSphere
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO K=1,Nr
              DO J=1-Oly,sNy+Oly
                DO I=1-Olx,sNx+Olx
                  fCori(i,j,bi,bj)=f0+beta*_yC(i,j,bi,bj)*facGrid
                  fCoriG(i,j,bi,bj)=f0+beta*yG(i,j,bi,bj)*facGrid
                  fCoriCos(i,j,bi,bj)=0.
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ELSEIF ( useSphereF ) THEN
C       Spherical case
C       Note in this case we assume yC is in degrees.
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO K=1,Nr
              DO J=1-Oly,sNy+Oly
                DO I=1-Olx,sNx+Olx
                  fCori(i,j,bi,bj)=
     &                 2. _d 0*omega*sin(_yC(i,j,bi,bj)*deg2rad)
                  fCoriG(i,j,bi,bj)=
     &                 2. _d 0*omega*sin(yG(i,j,bi,bj)*deg2rad)
                  fCoriCos(i,j,bi,bj)=
     &                 2. _d 0*omega*cos(_yC(i,j,bi,bj)*deg2rad)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ELSE
C       Special custom form
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO K=1,Nr
              DO J=1-Oly,sNy+Oly
                DO I=1-Olx,sNx+Olx
                  fCori(i,j,bi,bj)=0.
                  fCoriG(i,j,bi,bj)=0.
                  fCoriCos(i,j,bi,bj)=0.
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C     
C     _EXCH_XY_R4(fCori,myThid)
C     CALL EXCH_Z_XY_RS(fCoriG,myThid)
C     _EXCH_XY_R4(fCoriCos,myThid)

#ifdef ALLOW_MONITOR
      mon_write_stdout = .FALSE.
      mon_write_mnc    = .FALSE.
      IF (monitor_stdio) THEN
        mon_write_stdout = .TRUE.
      ENDIF
      
#ifdef ALLOW_MNC
      IF (useMNC .AND. monitor_mnc) THEN
        DO i = 1,MAX_LEN_MBUF
          mon_fname(i:i) = ' '
        ENDDO
        mon_fname(1:12) = 'monitor_grid'
        CALL MNC_CW_SET_UDIM(mon_fname, 1, myThid)
        mon_write_mnc = .TRUE.
      ENDIF
#endif /*  ALLOW_MNC  */
      
      CALL MON_PRINTSTATS_RS(1,fCori,'fCori',myThid)
      CALL MON_PRINTSTATS_RS(1,fCoriG,'fCoriG',myThid)
      CALL MON_PRINTSTATS_RS(1,fCoriCos,'fCoriCos',myThid)

      mon_write_stdout = .FALSE.
      mon_write_mnc    = .FALSE.
#endif

      RETURN
      END
