C $Header: /u/gcmpack/MITgcm/verification/aim.5l_LatLon/code/Attic/calc_gt.F,v 1.1 2001/09/27 15:36:29 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GT
C     !INTERFACE:
      SUBROUTINE CALC_GT( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskUp,
     I           KappaRT,
     U           fVerT,
     I           myTime,myIter,myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_GT                                        
C     | o Calculate the temperature tendency terms.               
C     *==========================================================*
C     | A procedure called EXTERNAL_FORCING_T is called from      
C     | here. These procedures can be used to add per problem     
C     | heat flux source terms.                                   
C     | Note: Although it is slightly counter-intuitive the       
C     |       EXTERNAL_FORCING routine is not the place to put    
C     |       file I/O. Instead files that are required to        
C     |       calculate the external source terms are generally   
C     |       read during the model main loop. This makes the     
C     |       logisitics of multi-processing simpler and also     
C     |       makes the adjoint generation simpler. It also       
C     |       allows for I/O to overlap computation where that    
C     |       is supported by hardware.                           
C     | Aside from the problem specific term the code here        
C     | forms the tendency terms due to advection and mixing      
C     | The baseline implementation here uses a centered          
C     | difference form for the advection term and a tensorial    
C     | divergence of a flux form for the diffusive term. The     
C     | diffusive term is formulated so that isopycnal mixing and 
C     | GM-style subgrid-scale terms can be incorporated b simply 
C     | setting the diffusion tensor terms appropriately.         
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fVerT   :: Flux of temperature (T) in the vertical 
C                direction at the upper(U) and lower(D) faces of a cell.
C     maskUp  :: Land mask used to denote base of the domain.
C     xA      :: Tracer cell face area normal to X
C     yA      :: Tracer cell face area normal to X
C     uTrans  :: Zonal volume transport through cell face
C     vTrans  :: Meridional volume transport through cell face
C     rTrans  :: Vertical volume transport through cell face
C     bi, bj, iMin, iMax, jMin, jMax :: Range of points for which calculation
C                                       results will be set.
C     myThid :: Instance number for this innvocation of CALC_GT
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
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

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
c     CALL EXTERNAL_FORCING_T(
c    I     iMin,iMax,jMin,jMax,bi,bj,k,
c    I     myTime,myThid)
#endif /*  INCLUDE_T_FORCING_CODE */

      IF ( tempAdvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.tempAdvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.tempAdvScheme.EQ.ENUM_CENTERED_4TH ) THEN
        CALL ADAMS_BASHFORTH2(
     I                        bi, bj, K,
     U                        gT, gTnm1,
     I                        myIter, myThid )
      ENDIF

#ifdef INCLUDE_T_FORCING_CODE
C--   External thermal forcing term(s)
      CALL EXTERNAL_FORCING_T(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
#endif /*  INCLUDE_T_FORCING_CODE */

#ifdef NONLIN_FRSURF
      IF (nonlinFreeSurf.GT.0) THEN
        CALL FREESURF_RESCALE_G(
     I                          bi, bj, K,
     U                          gT,
     I                          myThid )
      ENDIF
#endif /* NONLIN_FRSURF */

      RETURN
      END
