C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_surfmean.F,v 1.1 2014/05/06 20:46:53 jahn Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_SURFMEAN(
     I                    tracer,
     O                    gsm_tracer,
     I                    myThid )

C !DESCRIPTION:
C  Calculate surface mean of tracer

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C !OUTPUT PARAMETERS: ===================================================
C  gcm_tracer           :: surface mean of tracer
      _RL gsm_tracer

C !LOCAL VARIABLES: ====================================================
      INTEGER i,j,bi,bj
      INTEGER ks
      _RL sumTracerTile(nSx,nSy)
      _RL sumTracer
CEOP

      IF ( usingPCoords ) THEN
       ks = Nr
      ELSE
       ks = 1
      ENDIF

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         sumTracerTile(bi,bj) = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
            sumTracerTile(bi,bj) = sumTracerTile(bi,bj)
     &        + tracer(i,j,ks,bi,bj)*rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL GLOBAL_SUM_TILE_RL(sumTracerTile,sumTracer,myThid)
       gsm_tracer = sumTracer/globalArea

      RETURN
      END
