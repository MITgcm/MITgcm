C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_gs.F,v 1.32 2001/09/26 18:09:14 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GS
C     !INTERFACE:
      SUBROUTINE CALC_GS( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskUp,
     I           KappaRS,
     U           fVerS,
     I           myTime,myIter,myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_GS                                        
C     | o Calculate the salt tendency terms.                      
C     *==========================================================*
C     | A procedure called EXTERNAL_FORCING_S is called from      
C     | here. These procedures can be used to add per problem     
C     | E-P  flux source terms.                                   
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
C     fVerS   :: Flux of salt (S) in the vertical 
C               direction at the upper(U) and lower(D) faces of a cell.
C     maskUp  :: Land mask used to denote base of the domain.
C     xA      :: Tracer cell face area normal to X
C     yA      :: Tracer cell face area normal to X
C     uTrans  :: Zonal volume transport through cell face
C     vTrans  :: Meridional volume transport through cell face
C     rTrans  :: Vertical volume transport through cell face
C     bi, bj, iMin, iMax, jMin, jMax :: Range of points for which calculation
C                                      results will be set.
C     myThid :: Instance number for this innvocation of CALC_GT
      _RL fVerS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER k,kUp,kDown,kM1
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

CEOP

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kUp part of fverS is set in this subroutine
C--   the kDown is still required
      fVerS(1,1,kDown) = fVerS(1,1,kDown)
#endif

      CALL GAD_CALC_RHS(
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskUp,
     I           diffKhS, diffK4S, KappaRS, Salt,
     I           GAD_SALINITY, saltAdvScheme,
     U           fVerS, gS,
     I           myThid )

C--   External forcing term(s)
      CALL EXTERNAL_FORCING_S(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)

      IF ( saltAdvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.saltAdvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.saltAdvScheme.EQ.ENUM_CENTERED_4TH ) THEN
        CALL ADAMS_BASHFORTH2(
     I                        bi, bj, K,
     U                        gS, gSnm1,
     I                        myIter, myThid )
      ENDIF

#ifdef NONLIN_FRSURF
      IF (nonlinFreeSurf.GT.0) THEN
        CALL FREESURF_RESCALE_G(
     I                          bi, bj, K,
     U                          gS,
     I                          myThid )
      ENDIF
#endif /* NONLIN_FRSURF */

      RETURN
      END
