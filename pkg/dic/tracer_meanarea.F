C $Header: /u/gcmpack/MITgcm/pkg/dic/Attic/tracer_meanarea.F,v 1.8 2007/08/13 17:22:17 dfer Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
c#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE TRACER_MEANAREA(myThid,tracer, kLev,
     O                    gsm_tracer)

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
      INTEGER kLev
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C !OUTPUT PARAMETERS: ===================================================
C  gcm_tracer               :: surface mean of tracer
      _RL gsm_tracer

C !LOCAL VARIABLES: ====================================================
      INTEGER i,j,bi,bj
      _RL sumTracer
CEOP

       sumTracer= 0. _d 0

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
            sumTracer=sumTracer+
     &          tracer(i,j,kLev,bi,bj)*rA(i,j,bi,bj)*
     &                        maskC(i,j,kLev,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _GLOBAL_SUM_R8(sumTracer,myThid)
       gsm_tracer=sumTracer/globalArea

       RETURN
       END
