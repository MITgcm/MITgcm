#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE TRACER_MEANAREA(myThid,tracer, kLev,
     &                    gsm_tracer)
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
      INTEGER kLev
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL gsm_tracer

c local variables
      INTEGER i,j,bi,bj
      _RL sumarea

      _BEGIN_MASTER(myThid)

       sumarea= 0. _d 0
       gsm_tracer= 0. _d 0

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO i=1,sNx
          DO j=1,sNy
            sumarea=sumarea+
     &            rA(i,j,bi,bj)*maskC(i,j,kLev,bi,bj)
            gsm_tracer=gsm_tracer+
     &          tracer(i,j,kLev,bi,bj)*rA(i,j,bi,bj)*
     &                        maskC(i,j,kLev,bi,bj)
          ENDDO
          ENDDO
         ENDDO
        ENDDO
        gsm_tracer=gsm_tracer/sumarea

      _END_MASTER(myThid)

C--   Everyone else must wait 
      _BARRIER 


       RETURN
       END

