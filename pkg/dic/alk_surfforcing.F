#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE ALK_SURFFORCING( PTR_ALK , GALK, 
     I           bi,bj,imin,imax,jmin,jmax,
     I           myIter,myTime,myThid)

C     /==========================================================\
C     | SUBROUTINE ALK_SURFFORCING                               |
C     | o Calculate the alkalinity change due to freshwater flux |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DIC_ABIOTIC.h"

C     == Routine arguments ==
      INTEGER myIter, myThid
      _RL myTime
      _RL  PTR_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax,jMin,jMax, bi, bj

#ifdef ALLOW_PTRACERS
#ifdef DIC_BIOTIC
C     == Local variables ==
       INTEGER I,J

        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            IF (maskC(i,j,1,bi,bj).NE.0.) THEN
c calculate virtual flux
c EminusPforV = dS/dt*(1/Sglob)
C NOTE: Be very careful with signs here!
C Positive EminusPforV => loss of water to atmos and increase
C in salinity. Thus, also increase in other surface tracers
C (i.e. positive virtual flux into surface layer)
cQQ           GALK(i,j)=gsm_ALK*ftotflux(i,j,bi,bj)/gsm_S
              GALK(i,j)=gsm_ALK*surfaceTendencyS(i,j,bi,bj)/gsm_S
c OR
c let virtual flux be zero
c             GALK(i,j)=0.d0
c
            ELSE
              GALK(i,j)=0. _d 0
            ENDIF
          ENDDO
         ENDDO

#endif
#endif
        RETURN
        END
