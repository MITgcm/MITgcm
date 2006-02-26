C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_gs.F,v 1.43 2006/02/26 01:57:24 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GS
C     !INTERFACE:
      SUBROUTINE CALC_GS( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,rTransKp1,maskUp,
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
#ifdef ALLOW_GENERIC_ADVDIFF
#include "GAD.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fVerS   :: Flux of salt (S) in the vertical 
C               direction at the upper(U) and lower(D) faces of a cell.
C     maskUp  :: Land mask used to denote base of the domain.
C     xA      :: Tracer cell face area normal to X
C     yA      :: Tracer cell face area normal to X
C     uTrans  :: Zonal volume transport through cell face
C     vTrans  :: Meridional volume transport through cell face
C     rTrans  ::   Vertical volume transport at interface k
C     rTransKp1 :: Vertical volume transport at inteface k+1
C     bi, bj, iMin, iMax, jMin, jMax :: Range of points for which calculation
C                                      results will be set.
C     myThid :: Instance number for this innvocation of CALC_GT
      _RL fVerS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k,kUp,kDown,kM1
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

CEOP

#ifdef ALLOW_GENERIC_ADVDIFF
C     === Local variables ===
      LOGICAL calcAdvection
      INTEGER iterNb
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER m1, m2
#endif

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          itdkey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
          kkey = (itdkey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kUp part of fverS is set in this subroutine
C--   the kDown is still required
      fVerS(1,1,kDown) = fVerS(1,1,kDown)
# ifdef NONLIN_FRSURF
CADJ STORE fVerS(:,:,:) = comlev1_bibj_k, key=kkey, byte=isbyte
# endif
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      calcAdvection = saltAdvection .AND. .NOT.saltMultiDimAdvec
      iterNb = myIter
      IF (staggerTimeStep) iterNb = myIter - 1

#ifdef ALLOW_ADAMSBASHFORTH_3
      IF ( AdamsBashforth_S ) THEN
        m1 = 1 + MOD(iterNb+1,2)
        m2 = 1 + MOD( iterNb ,2)
        CALL GAD_CALC_RHS(
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,rTransKp1,maskUp,
     I           uVel, vVel, wVel,
     I           diffKhS, diffK4S, KappaRS,
     I           gsNm(1-Olx,1-Oly,1,1,1,m2), salt,
     I           GAD_SALINITY, saltAdvScheme, saltVertAdvScheme,
     I           calcAdvection, saltImplVertAdv,
     U           fVerS, gS,
     I           myTime, myIter, myThid )
      ELSE
        CALL GAD_CALC_RHS(
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,rTransKp1,maskUp,
     I           uVel, vVel, wVel,
     I           diffKhS, diffK4S, KappaRS, salt, salt,
     I           GAD_SALINITY, saltAdvScheme, saltVertAdvScheme,
     I           calcAdvection, saltImplVertAdv,
     U           fVerS, gS,
     I           myTime, myIter, myThid )
      ENDIF
#else /* ALLOW_ADAMSBASHFORTH_3 */
        CALL GAD_CALC_RHS(
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,rTransKp1,maskUp,
     I           uVel, vVel, wVel,
     I           diffKhS, diffK4S, KappaRS, gsNm1, salt,
     I           GAD_SALINITY, saltAdvScheme, saltVertAdvScheme,
     I           calcAdvection, saltImplVertAdv,
     U           fVerS, gS,
     I           myTime, myIter, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */

C--   External salinity forcing term(s) inside Adams-Bashforth:
      IF ( saltForcing .AND. forcing_In_AB )
     & CALL EXTERNAL_FORCING_S(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)

      IF ( AdamsBashforthGs ) THEN
#ifdef ALLOW_ADAMSBASHFORTH_3
        CALL ADAMS_BASHFORTH3(
     I                        bi, bj, k,
     U                        gS, gsNm,
     I                        saltStartAB, iterNb, myThid )
#else
        CALL ADAMS_BASHFORTH2(
     I                        bi, bj, k,
     U                        gS, gsNm1,
     I                        iterNb, myThid )
#endif
      ENDIF

C--   External salinity forcing term(s) outside Adams-Bashforth:
      IF ( saltForcing .AND. .NOT.forcing_In_AB )
     & CALL EXTERNAL_FORCING_S(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)

#ifdef NONLIN_FRSURF
      IF (nonlinFreeSurf.GT.0) THEN
        CALL FREESURF_RESCALE_G(
     I                          bi, bj, k,
     U                          gS,
     I                          myThid )
        IF ( AdamsBashforthGs ) THEN
#ifdef ALLOW_ADAMSBASHFORTH_3
        CALL FREESURF_RESCALE_G(
     I                          bi, bj, k,
     U                          gsNm(1-OLx,1-OLy,1,1,1,1),
     I                          myThid )
        CALL FREESURF_RESCALE_G(
     I                          bi, bj, k,
     U                          gsNm(1-OLx,1-OLy,1,1,1,2),
     I                          myThid )
#else
        CALL FREESURF_RESCALE_G(
     I                          bi, bj, k,
     U                          gsNm1,
     I                          myThid )
#endif
        ENDIF
      ENDIF
#endif /* NONLIN_FRSURF */

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END
