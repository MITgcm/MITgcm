C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_gt.F,v 1.34 2001/09/18 19:07:35 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE CALC_GT( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskUp,
     I           KappaRT,
     U           fVerT,
     I           myCurrentTime, myThid )
C     /==========================================================\
C     | SUBROUTINE CALC_GT                                       |
C     | o Calculate the temperature tendency terms.              |
C     |==========================================================|
C     | A procedure called EXTERNAL_FORCING_T is called from     |
C     | here. These procedures can be used to add per problem    |
C     | heat flux source terms.                                  |
C     | Note: Although it is slightly counter-intuitive the      |
C     |       EXTERNAL_FORCING routine is not the place to put   |
C     |       file I/O. Instead files that are required to       |
C     |       calculate the external source terms are generally  |
C     |       read during the model main loop. This makes the    |
C     |       logisitics of multi-processing simpler and also    |
C     |       makes the adjoint generation simpler. It also      |
C     |       allows for I/O to overlap computation where that   |
C     |       is supported by hardware.                          |
C     | Aside from the problem specific term the code here       |
C     | forms the tendency terms due to advection and mixing     |
C     | The baseline implementation here uses a centered         |
C     | difference form for the advection term and a tensorial   |
C     | divergence of a flux form for the diffusive term. The    |
C     | diffusive term is formulated so that isopycnal mixing and|
C     | GM-style subgrid-scale terms can be incorporated b simply|
C     | setting the diffusion tensor terms appropriately.        |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"

C     == Routine arguments ==
C     fVerT   - Flux of temperature (T) in the vertical 
C               direction at the upper(U) and lower(D) faces of a cell.
C     maskUp  - Land mask used to denote base of the domain.
C     xA      - Tracer cell face area normal to X
C     yA      - Tracer cell face area normal to X
C     uTrans  - Zonal volume transport through cell face
C     vTrans  - Meridional volume transport through cell face
C     rTrans  - Vertical volume transport through cell face
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     myThid - Instance number for this innvocation of CALC_GT
      _RL fVerT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER k,kUp,kDown,kM1
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid
      _RL     myCurrentTime

C     == Local variables ==

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kUp part of fverT is set in this subroutine
C--   the kDown is still required
      fVerT(1,1,kDown) = fVerT(1,1,kDown)
#endif

      CALL GAD_CALC_RHS(
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskUp,
     I           diffKhT, diffK4T, KappaRT, theta,
     I           GAD_TEMPERATURE, tempAdvScheme,
     U           fVerT, gT,
     I           myThid )

#ifdef INCLUDE_T_FORCING_CODE
C--   External thermal forcing term(s)
      CALL EXTERNAL_FORCING_T(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myCurrentTime,myThid)
#endif /*  INCLUDE_T_FORCING_CODE */

      RETURN
      END
