C $Header: /u/gcmpack/MITgcm/model/src/ini_cori.F,v 1.23 2006/07/13 03:00:24 jmc Exp $
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
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_MONITOR
#include "MONITOR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     bi,bj   :: Tile Indices counters
C     i, j    :: Loop counters
C     facGrid :: Factor for grid to meter conversion
      INTEGER bi,bj
      INTEGER i, j
      _RL facGrid

C     Initialise coriolis parameter
      IF     ( useConstantF ) THEN
C       Constant F case
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-Oly,sNy+Oly
              DO i=1-Olx,sNx+Olx
                fCori(i,j,bi,bj)  = f0
                fCoriG(i,j,bi,bj) = f0
                fCoriCos(i,j,bi,bj)=0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ELSEIF ( useBetaPlaneF ) THEN
C       Beta plane case
        facGrid = 1. _d 0
        IF ( usingSphericalPolarGrid
     &     .OR. usingCurvilinearGrid ) facGrid = deg2rad*rSphere
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-Oly,sNy+Oly
              DO i=1-Olx,sNx+Olx
                fCori(i,j,bi,bj)  = f0+beta*_yC(i,j,bi,bj)*facGrid
                fCoriG(i,j,bi,bj) = f0+beta* yG(i,j,bi,bj)*facGrid
                fCoriCos(i,j,bi,bj)=0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ELSEIF ( useSphereF ) THEN
C       Spherical case
C       Note in this case we assume yC is in degrees.
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-Oly,sNy+Oly
              DO i=1-Olx,sNx+Olx
                fCori(i,j,bi,bj)  =
     &                 2. _d 0*omega*sin(_yC(i,j,bi,bj)*deg2rad)
                fCoriG(i,j,bi,bj) =
     &                 2. _d 0*omega*sin(yG(i,j,bi,bj)*deg2rad)
                fCoriCos(i,j,bi,bj)=
     &                 2. _d 0*omega*cos(_yC(i,j,bi,bj)*deg2rad)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
c       CALL WRITE_FLD_XY_RL('fCoriC',' ',fCori ,  0,myThid)
c       CALL WRITE_FLD_XY_RL('fCoriG',' ',fCoriG , 0,myThid)
c       CALL WRITE_FLD_XY_RL('fCorCs',' ',fCoriCos,0,myThid)
      ELSE
C       Special custom form
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-Oly,sNy+Oly
              DO i=1-Olx,sNx+Olx
                fCori(i,j,bi,bj)  = 0. _d 0
                fCoriG(i,j,bi,bj) = 0. _d 0
                fCoriCos(i,j,bi,bj)=0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        CALL READ_REC_XY_RS( 'fCoriC.bin', fCori,   1, 0, myThid )
        CALL READ_REC_XY_RS( 'fCoriG.bin', fCoriG,  1, 0, myThid )
        CALL READ_REC_XY_RS( 'fCorCs.bin', fCoriCos,1, 0, myThid )
        IF ( useCubedSphereExchange ) THEN
C-       deal with the 2 missing corners (for fCoriG):
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
C-  Notes: this will only works with 6 tiles (1 per face) and
C    with 2 polar faces + 4 equatorials:
           IF (bi.LE.3 .OR. bi.GE.5) THEN
             fCoriG(sNx+1,1,bi,bj) = fCoriG(1,1,bi,bj)
           ELSE
             fCoriG(sNx+1,1,bi,bj) = -fCoriG(1,1,bi,bj)
           ENDIF
           IF (bi.GE.3) THEN
             fCoriG(1,sNy+1,bi,bj) = fCoriG(1,1,bi,bj)
             fCoriG(sNx+1,sNy+1,bi,bj) = fCoriG(sNx+1,1,bi,bj)
           ELSE
             fCoriG(1,sNy+1,bi,bj) = -fCoriG(1,1,bi,bj)
             fCoriG(sNx+1,sNy+1,bi,bj) = -fCoriG(sNx+1,1,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDIF

        _EXCH_XY_R4(fCori,myThid)
        CALL EXCH_Z_XY_RS(fCoriG,myThid)
        _EXCH_XY_R4(fCoriCos,myThid)
      ENDIF

#ifdef ALLOW_MONITOR
#ifdef ALLOW_USE_MPI
      IF ( .NOT.useSingleCPUIO .OR. mpiMyId.EQ.0 ) THEN
#endif /* ALLOW_USE_MPI */
        _BEGIN_MASTER(myThid)
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it's the only thread that uses those flags):

        IF (monitor_stdio) THEN
          mon_write_stdout = .TRUE.
        ELSE
          mon_write_stdout = .FALSE.
        ENDIF
        mon_write_mnc = .FALSE.
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

        _END_MASTER(myThid)
#ifdef ALLOW_USE_MPI
      ENDIF
#endif /* ALLOW_USE_MPI */

      CALL MON_PRINTSTATS_RS(1,fCori,'fCori',myThid)
      CALL MON_PRINTSTATS_RS(1,fCoriG,'fCoriG',myThid)
      CALL MON_PRINTSTATS_RS(1,fCoriCos,'fCoriCos',myThid)

#ifdef ALLOW_USE_MPI
      IF ( .NOT.useSingleCPUIO .OR. mpiMyId.EQ.0 ) THEN
#endif /* ALLOW_USE_MPI */
        _BEGIN_MASTER(myThid)

        mon_write_stdout = .FALSE.
        mon_write_mnc    = .FALSE.

        _END_MASTER(myThid)
#ifdef ALLOW_USE_MPI
      ENDIF
#endif /* ALLOW_USE_MPI */
#endif /* ALLOW_MONITOR */

      RETURN
      END
