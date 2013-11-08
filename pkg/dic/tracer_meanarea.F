C $Header: /u/gcmpack/MITgcm/pkg/dic/Attic/tracer_meanarea.F,v 1.12 2013/11/08 20:03:12 jahn Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE TRACER_MEANAREA(
     I                    tracer,
     O                    gsm_tracer,
     I                    myThid )

C !DESCRIPTION:
C  Calculate surface means of tracer

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
c#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C !OUTPUT PARAMETERS: ===================================================
C  gcm_tracer               :: surface mean of tracer
      _RL gsm_tracer

C !LOCAL VARIABLES: ====================================================
      INTEGER i,j,bi,bj
      INTEGER kLev
      _RL sumTracerTile(nSx,nSy)
      _RL sumTracer
CEOP

      IF ( usingPCoords ) THEN
       kLev = Nr
      ELSE
       kLev = 1
      ENDIF

       sumTracerTile= 0. _d 0

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
            sumTracerTile(bi,bj)=sumTracerTile(bi,bj)+
     &          tracer(i,j,kLev,bi,bj)*rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL GLOBAL_SUM_TILE_RL(sumTracerTile,sumTracer,myThid)
       gsm_tracer=sumTracer/globalArea

       RETURN
       END
