C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_integrate.F,v 1.11 2003/10/26 01:10:34 heimbach Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
cswdptr -- add ---
#ifdef ALLOW_GCHEM
# include "GCHEM_OPTIONS.h"
#endif
cswdptr -- end add ---

CBOP
C !ROUTINE: PTRACERS_INTEGRATE

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_INTEGRATE(
     I                               bi,bj,k,
     I                               xA,yA,uTrans,vTrans,rTrans,maskUp,
     X                               rFlx,KappaRtr,
     I                               myIter,myTime,myThid )

C !DESCRIPTION:
C     Calculates tendancy for passive tracers and integrates forward
C     in time.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS.h"
#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level number
C  xA                   :: face area at U points in level k
C  yA                   :: face area at V points in level k
C  uTrans               :: zonal transport in level k
C  vTrans               :: meridional transport in level k
C  rTrans               :: vertical transport across level k
C  maskUp               :: mask for vertical transport
C  KappaRtr		:: vertical diffusion of passive tracers
C            NOTE!         This is infact KappaRS from thermodynamics()
C                          and is being used only temporarily
C                          until we removed the need to store
C                          a "3D" diffusivity
C  myIter               :: time-step number
C  myTime               :: model time
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRtr(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myIter
      _RL myTime
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  i,j,k,bi,bj,iTracer  :: loop indices
C  iMin,iMax,jMin,jMax  :: loop ranges
C  kUp,kDown            :: toggle indices for even/odd level fluxes
C  km1                  :: =min(1,k-1)
C  rFlx			:: vertical flux
C  GAD_TR               :: passive tracer id (GAD_TR1+iTracer-1)
      INTEGER i,j,iTracer
      INTEGER iMin,iMax,jMin,jMax
      INTEGER kUp,kDown,km1
      _RL rFlx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,PTRACERS_num)
      INTEGER GAD_TR
      LOGICAL calcAdvection
CEOP

C Loop over tracers
      DO iTracer=1,PTRACERS_numInUse

C Loop ranges for daughter routines
       iMin = 1-OLx+2
       iMax = sNx+OLx-1
       jMin = 1-OLy+2
       jMax = sNy+OLy-1

       km1  = MAX(1,k-1)
       kUp  = 1+MOD(k+1,2)
       kDown= 1+MOD(k,2)

C Calculate active tracer tendencies (gPtr) due to internal processes
C (advection, [explicit] diffusion, parameterizations,...)
       calcAdvection = .NOT.multiDimAdvection
     &      .OR. PTRACERS_advScheme(iTracer).EQ.ENUM_CENTERED_2ND
     &      .OR. PTRACERS_advScheme(iTracer).EQ.ENUM_UPWIND_3RD
     &      .OR. PTRACERS_advScheme(iTracer).EQ.ENUM_CENTERED_4TH
       GAD_TR = GAD_TR1 + iTracer - 1
         CALL GAD_CALC_RHS(
     I                     bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I                     xA,yA,uTrans,vTrans,rTrans,maskUp,
     I                     PTRACERS_diffKh(iTracer),
     I                     PTRACERS_diffK4(iTracer),
     I                     KappaRtr,
     I                     pTracer(1-Olx,1-Oly,1,1,1,iTracer),
     I                     GAD_TR,
     I                     PTRACERS_advScheme(iTracer),calcAdvection,
     U                     rFlx(1-Olx,1-Oly,1,iTracer),
     U                     gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I                     myThid )

C External forcing term(s)
cswdptr --add --
#ifdef ALLOW_GCHEM
#ifndef PTRACERS_SEPARATE_FORCING
       IF ( forcing_In_AB )
     &    CALL GCHEM_FORCING_INT(
     I                        bi,bj,iMin,iMax,jMin,jMax,k,
     I                        iTracer,
     I                        myTime,myIter, myThid)
#endif
#else
cswdptr - end add ---
       IF ( forcing_In_AB )
     &   CALL PTRACERS_FORCING(
     I                      bi,bj,iMin,iMax,jMin,jMax,k,
     U                      gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I                      surfaceTendencyPtr(1-Olx,1-Oly,1,1,iTracer),
     I                      myIter,myTime,myThid)
cswdptr --add---
#endif
cswdptr -- end add ---

C If using Adams-Bashforth II, then extrapolate tendancies
      IF ( PTRACERS_advScheme(iTracer).EQ.ENUM_CENTERED_2ND
     & .OR.PTRACERS_advScheme(iTracer).EQ.ENUM_UPWIND_3RD
     & .OR.PTRACERS_advScheme(iTracer).EQ.ENUM_CENTERED_4TH ) THEN
        CALL ADAMS_BASHFORTH2(
     I                        bi,bj,K,
     U                        gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     U                        gPtrNm1(1-Olx,1-Oly,1,1,1,iTracer),
     I                        myIter,myThid )
      ENDIF

C External forcing term(s)
cswdptr - add--
#ifdef ALLOW_GCHEM
#ifndef PTRACERS_SEPARATE_FORCING
       IF ( .NOT.forcing_In_AB )
     &    CALL GCHEM_FORCING_INT(
     I                        bi,bj,iMin,iMax,jMin,jMax,k,
     I                        iTracer,
     I                        myTime,myIter, myThid)
#endif
#else
cswdptr - end add ---
       IF ( .NOT.forcing_In_AB )
     &   CALL PTRACERS_FORCING(
     I                      bi,bj,iMin,iMax,jMin,jMax,k,
     U                      gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I                      surfaceTendencyPtr(1-Olx,1-Oly,1,1,iTracer),
     I                      myIter,myTime,myThid)
cswdptr - add--
#endif
cswdptr -- end add ---

#ifdef NONLIN_FRSURF
C Account for change in level thickness
      IF (nonlinFreeSurf.GT.0) THEN
        CALL FREESURF_RESCALE_G(
     I                          bi,bj,K,
     U                          gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I                          myThid )
        IF ( PTRACERS_advScheme(iTracer).EQ.ENUM_CENTERED_2ND
     &   .OR.PTRACERS_advScheme(iTracer).EQ.ENUM_UPWIND_3RD
     &   .OR.PTRACERS_advScheme(iTracer).EQ.ENUM_CENTERED_4TH )
     &  CALL FREESURF_RESCALE_G(
     I                          bi,bj,K,
     U                          gPtrNm1(1-Olx,1-Oly,1,1,1,iTracer),
     I                          myThid )
      ENDIF
#endif /* NONLIN_FRSURF */

C Integrate forward in time, storing in gPtr:  G=T+dt*G
         CALL TIMESTEP_TRACER(
     I                        bi,bj,iMin,iMax,jMin,jMax,k,
     I                        PTRACERS_advScheme(iTracer),
     I                        pTracer(1-Olx,1-Oly,1,1,1,iTracer),
     I                        gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I                        myIter,myThid )

C end of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
