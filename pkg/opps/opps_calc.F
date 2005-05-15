C$Header: /u/gcmpack/MITgcm/pkg/opps/opps_calc.F,v 1.5 2005/05/15 03:04:57 jmc Exp $
C$Name:  $

#include "OPPS_OPTIONS.h"

CBOP
C !ROUTINE: OPPS_CALC

C !INTERFACE: ======================================================
      subroutine OPPS_CALC(
     U     tracerEnv,
     I     wVel, 
     I     kMax, nTracer, nTracerInuse, 
     I     I, J, bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     /=====================================================================\
C     | SUBROUTINE OPPS_CALC                                                |
C     | o Compute all OPPS fields defined in OPPS.h                         |
C     |=====================================================================|
C     | This subroutine is based on the routine 3dconvection.F              |
C     | by E. Skyllingstad (?)                                              |
C     | plenty of modifications to make it work:                            |
C     | - removed many unused parameters and variables                      |
C     | - turned everything (back) into 1D code                             |
C     | - pass variables, that are orginially in common blocks:             |
C     |   maxDepth                                                          |
C     | - pass vertical velocity, set in OPPS_INTERFACE                     |
C     | - do not use convadj for now (whatever that is)                     |
C     | - changed two .LT. 0 to .LE. 0 statements (because of possible      |
C     |   division)                                                         |
C     | - replaced statement function state1 by call to a real function     |
C     | - removed range check, actually moved it up to OPPS_INTERFACE       |
C     | - avoid division by zero: if (Wd.EQ.0) dt = ...1/Wd                 |
C     | - cleaned-up debugging                                              |
C     | - replaced local dz and GridThickness by global drF                 |
C     | - replaced 1/dz by 1*recip_drF                                      |
C     | - replaced 9.81 with gravity (=9.81)                                |
C     | - added a lot of comments that relate code to equation in paper     |
C     |   (Paluszkiewicz+Romea, 1997, Dynamics of Atmospheres and Oceans,   |
C     |   26, pp. 95-130)                                                   |
C     | - included passive tracer support. This is the main change and may  |
C     |   not improve the readability of the code because of the joint      |
C     |   treatment of active (theta, salt) and passive tracers. The array  |
C     |   tracerEnv(Nr,2+PTRACERS_num) contains                             |
C     |   theta    = tracerEnv(:,1),                                        |
C     |   salt     = tracerEnv(:,2), and                                    | 
C     |   ptracers = tracerEnv(:,3:PTRACERS_num+2).                         |
C     |   All related array names have been changed accordingly, so that    |
C     |   instead of Sd(Nr) and Td(Nr) (plume salinity and temperature), we |
C     |   have Pd(Nr,nTracer) (tracer in plume), with Sd(:) = Pd(:,2),      |
C     |   Td(:) = Pd(:,1), etc.                                             |
C     | o TODO:                                                             |
C     |   clean up the logic of the vertical loops and get rid off the      |
C     |   GOTO statements                                                   |
C     \=====================================================================/
      IMPLICIT NONE
C
C--------------------------------------------------------------------

C \ev

C !USES: ============================================================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OPPS.h"
#include "FFIELDS.h"
#include "GRID.h"

      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE

C !INPUT PARAMETERS: ===================================================
c Routine arguments
c     bi, bj - array indices on which to apply calculations
c     myTime - Current time in simulation

      INTEGER I, J, bi, bj, KMax, nTracer, nTracerInUse
      INTEGER myThid, myIter
      _RL     myTime
      _RL tracerEnv(Nr,nTracer),wVel(Nr)

#ifdef ALLOW_OPPS
C !LOCAL VARIABLES: ====================================================
c Local constants
C     imin, imax, jmin, jmax  - array computation indices
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER K, K2, K2m1, K2p1, ktr
      INTEGER ntime,nn,kmx,ic
      INTEGER maxDepth

      _RL wsqr,oldflux,newflux,entrainrate
      _RL pmix
      _RL D1,D2,state1
      _RL dz1,dz2
      _RL radius,StartingFlux
      _RL dtts,dt
C     Arrays
      _RL Paa(Nr,nTracer)
      _RL wda(Nr), mda(Nr), pda(Nr,nTracer)
C
C     Pd, Wd           - tracers, vertical velocity in plume
C     Md               - plume mass flux (?)
C     Ad               - fractional area covered by plume
C     Dd               - density in plume
C     De               - density of environment
C     PlumeEntrainment -
      _RL Ad(Nr),Wd(Nr),Dd(Nr),Md(Nr)
      _RL De(Nr)
      _RL PlumeEntrainment(Nr)
      _RL Pd(Nr,nTracer)
CEOP


C--   Check to see if should convect now
C      IF ( DIFFERENT_MULTIPLE(cAdjFreq,myTime,deltaTClock) ) THEN
      IF ( .true. ) THEN
C     local initialization

C     Copy some arrays
      dtts = dTtracerLev(1)
C     
C     start k-loop
C

      DO k=1,KMax-1
c
c initialize the plume T,S,density, and w velocity
c
       DO ktr=1,nTracerInUse
        Pd(k,ktr) = tracerEnv(k,ktr)
       ENDDO
       Dd(k)=state1(Pd(k,2),Pd(k,1),i,j,k,bi,bj,myThid)
       De(k)=Dd(k)
CML       print *, 'ml-opps:', i,j,k,tracerEnv(k,2),tracerEnv(k,1),
CML     &      Dd(k),Pd(k,1),Pd(k,2)
CML compute vertical velocity at cell centers from GCM velocity
       Wd(k)= - .5*(wVel(K)+wVel(K+1))
CML(
CML	avoid division by zero
CML       IF (Wd(K) .EQ. 0.D0) Wd(K) = 2.23e-16
CML)
c
c guess at initial top grid cell vertical velocity
c
CML          Wd(k) = 0.03
c
c these estimates of initial plume velocity based on plume size and
c top grid cell water mass
c
c          Wd(k) = 0.5*drF(k)/(dtts*FRACTIONAL_AREA)
c          Wd(k) = 0.5*drF(k)/dtts
c
       wsqr=Wd(k)*Wd(k)
       PlumeEntrainment(k) = 0.0
c
c
c 
#ifdef ALLOW_OPPS_DEBUG
       IF ( OPPSdebugLevel.GE.debLevB ) THEN
        WRITE(msgBuf,'(A,I3)')
     &       'S/R OPPS_CALC: doing old lowerparcel', k  
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , 1)
       ENDIF
#endif /* ALLOW_OPPS_DEBUG */
       radius=PlumeRadius
       StartingFlux=radius*radius*Wd(k)*Dd(k)
       oldflux=StartingFlux
       
       dz2=DrF(k)
       DO k2=k,KMax-1
        D1=state1( Pd(k2,2), Pd(k2,1),i,j,k2+1,bi,bj,myThid)
        D2=state1( tracerEnv(k2+1,2), tracerEnv(k2+1,1),
     &                                i,j,k2+1,bi,bj,myThid)
        De(k2+1)=D2
c
c To start downward, parcel has to initially be heavier than environment
c but after it has started moving, we continue plume until plume tke or
c flux goes negative
c 
CML     &       _hFacC(i,j,k-1,bi,bj)
CML     &       *_hFacC(i,j,k,bi,bj) .GT. 0.
CML     &  .AND.
        IF (D2-D1 .LT. STABILITY_THRESHOLD.or.k2.ne.k) THEN
         dz1=dz2
         dz2=DrF(k2+1)
c     
C     find mass flux according to eq.(3) from paper by vertical integration
c
         newflux=oldflux+e2*radius*Wd(k2)*Dd(k2)*
     &        .5*(dz1+dz2)
CML         print *, 'ml-opps:', i,j,k,oldflux,newflux,e2,radius,
CML     &        Wd(k2),Dd(k2),Pd(k2,1),Pd(k2,2),dz1,dz2
c     
         PlumeEntrainment(k2+1) = newflux/StartingFlux
c     
         IF(newflux.LE.0.0) then
#ifdef ALLOW_OPPS_DEBUG
          IF ( OPPSdebugLevel.GE.debLevA ) THEN
           WRITE(msgBuf,'(A,I3)') 
     &          'S/R OPPS_CALC: Plume entrained to zero at level ', k2
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT , 1)
          ENDIF
#endif /* ALLOW_OPPS_DEBUG */
          maxdepth = k2
          if(maxdepth.eq.k) goto 1000
          goto 1
         endif
c     
c entrainment rate is basically a scaled mass flux dM/M
c
         entrainrate = (newflux - oldflux)/newflux
         oldflux = newflux
c
c
c mix var's are the average environmental values over the two grid levels
c
         DO ktr=1,nTracerInUse
          pmix=(dz1*tracerEnv(k2,ktr)+dz2*tracerEnv(k2+1,ktr))
     &         /(dz1+dz2)
          Pd(k2+1,ktr)=Pd(k2,ktr) 
     &         - entrainrate*(pmix - Pd(k2,ktr))
         ENDDO
c
c compute the density at this level for the buoyancy term in the
c vertical k.e. equation
c
         Dd(k2+1)=state1(Pd(k2+1,2),Pd(k2+1,1),i,j,k2+1,bi,bj,myThid)
c
c next, solve for the vertical velocity k.e. using combined eq. (4)
c and eq (5) from the paper
c
#ifdef ALLOW_OPPS_DEBUG
         IF ( OPPSdebugLevel.GE.debLevA ) THEN
          WRITE(msgBuf,'(A,3E12.4,I3)') 
     &    'S/R OPPS_CALC: Dd,De,entr,k ',Dd(k2),De(k2),entrainrate,k2
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &         SQUEEZE_RIGHT , 1)
         ENDIF
#endif /* ALLOW_OPPS_DEBUG */
CML   insert Eq. (4) into Eq. (5) to get something like this for wp^2
         wsqr = wsqr - wsqr*abs(entrainrate)+ gravity*
     &        (dz1*(Dd(k2)-De(k2))/De(k2)
     &        +dz2*(Dd(k2+1)-De(k2+1))/De(k2+1))
c     
c if negative k.e. then plume has reached max depth, get out of loop
c
         IF(wsqr.LE.0.0)then
          maxdepth = k2
#ifdef ALLOW_OPPS_DEBUG
          IF ( OPPSdebugLevel.GE.debLevA ) THEN
           WRITE(msgBuf,'(A,I3)') 
     &     'S/R OPPS_CALC: Plume velocity went to zero at level ', k2
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT , 1)
           WRITE(msgBuf,'(A,4A14)') 
     &          'S/R OPPS_CALC: ', 'wsqr', 'entrainrate', 
     &          '(Dd-De)/De up', '(Dd-De)/De do'
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT , 1)
           WRITE(msgBuf,'(A,4E14.6)') 
     &          'S/R OPPS_CALC: ', wsqr, entrainrate, 
     &          (Dd(k2)-De(k2))/De(k2), (Dd(k2+1)-De(k2+1))/De(k2+1)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT , 1)
          ENDIF
#endif /* ALLOW_OPPS_DEBUG */
          if(maxdepth.eq.k) goto 1000
          goto 1
         endif
         Wd(k2+1)=sqrt(wsqr)
C
C     compute a new radius based on the new mass flux at this grid level
C     from Eq. (4)
C
         radius=sqrt(newflux/(Wd(k2)*Dd(k2)))
        ELSE
         maxdepth=k2
         if(maxdepth.eq.k) goto 1000
         GOTO 1
        ENDIF
       ENDDO
c
c plume has reached the bottom
c
       MaxDepth=kMax
c
 1     CONTINUE
c     
       Ad(k)=FRACTIONAL_AREA
       IC=0
c
c start iteration on fractional area, not used in OGCM implementation
c
c
       DO IC=1,Max_ABE_Iterations
c
c
c next compute the mass flux beteen each grid box using the entrainment
c
        Md(k)=Wd(k)*Ad(k)
c     
        DO k2=k+1,maxDepth
         Md(k2)=Md(k)*PlumeEntrainment(k2)
#ifdef ALLOW_OPPS_DEBUG
         IF ( OPPSdebugLevel.GE.debLevA ) THEN
          WRITE(msgBuf,'(A,2E12.4,I3)') 
     &         'S/R OPPS_CALC: Md, Wd, and  k are ',Md(k2),Wd(k2),k2
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &         SQUEEZE_RIGHT , 1)
         ENDIF
#endif /* ALLOW_OPPS_DEBUG */
        ENDDO
c
c Now move on to calculate new temperature using flux from
c Td, Sd, Wd, ta, sa, and we. Values for these variables are at
c center of grid cell, use weighted average to get boundary values
c
c use a timestep limited by the GCM model timestep and the maximum plume
c velocity (CFL criteria)
c
c
c calculate the weighted wd, td, and sd
c
        dt = dtts
        do k2=k,maxDepth-1
         IF ( Wd(K2) .NE. 0. _d 0 ) dt = min(dt,drF(k2)/Wd(k2))
c     
c time integration will be integer number of steps to get one
c gcm time step
c
         ntime = nint(0.5*int(dtts/dt))
         if(ntime.eq.0) then
          ntime = 1
         endif
c
c make sure area weighted vertical velocities match; in other words
c make sure mass in equals mass out at the intersection of each grid
c cell. Eq. (20)
c     
         mda(k2) = (md(k2)*drF(k2)+md(k2+1)*drF(k2+1))/
     *        (drF(k2)+drF(k2+1))
c     
         wda(k2) = (wd(k2)*drF(k2)+wd(k2+1)*drF(k2+1))/
     *        (drF(k2)+drF(k2+1))
c     
         DO ktr = 1, nTracerInUse
          Pda(k2,ktr) = Pd(k2,ktr)
          Paa(k2,ktr) = tracerEnv(k2+1,ktr)
         ENDDO	
c
        enddo
        dt = min(dt,dtts)
#ifdef ALLOW_OPPS_DEBUG
        IF ( OPPSdebugLevel.GE.debLevA ) THEN
         WRITE(msgBuf,'(A,F14.4)') 
     &        'S/R OPPS_CALC: time step = ', dt
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , 1)
        ENDIF
#endif /* ALLOW_OPPS_DEBUG */
        DO ktr=1,nTracerInUse
         Pda(maxdepth,ktr) = Pd(maxdepth,ktr)
        ENDDO	
C
        kmx = maxdepth-1
        do nn=1,ntime
C
C     top point 
C
         DO ktr = 1,nTracerInUse
          tracerEnv(k,ktr) =  tracerEnv(k,ktr)-
     &        (mda(k)*(Pda(k,ktr)-Paa(k,ktr)))*dt*recip_drF(k)
         ENDDO	
c     
c now do inner points if there are any
c
CML         if(Maxdepth-k.gt.1) then
CML    This if statement is superfluous     
CML         IF ( k .LT. Maxdepth-1 ) THEN
CML         DO k2=k+1,Maxdepth-1
CML         mda(maxDepth) = 0.
         DO k2=k+1,kmx
          k2m1 = max(k,k2-1)
          k2p1 = max(k2+1,maxDepth)
c     
           DO ktr = 1,nTracerInUse
            tracerEnv(k2,ktr) = tracerEnv(k2,ktr) +
     &           (mda(k2m1)*(Pda(k2m1,ktr)-Paa(k2m1,ktr))
     &           -mda(k2)  *(Pda(k2,ktr)  -Paa(k2,ktr))  )
     &           *dt*recip_drF(k2)
           ENDDO           
          ENDDO
CML    This if statement is superfluous     
CML         ENDIF
C
C     bottom point
C
         DO ktr=1,nTracerInUse
          tracerEnv(kmx+1,ktr) =  tracerEnv(kmx+1,ktr)+
     &        mda(kmx)*(Pda(kmx,ktr)-Paa(kmx,ktr))*dt*recip_drF(kmx+1)
         ENDDO
c     
c     set the environmental temp and salinity to equal new fields
c     
         DO ktr=1,nTracerInUse
          DO k2=1,kmx
           paa(k2,ktr) = tracerEnv(k2+1,ktr)
          ENDDO
         ENDDO	
c     
c end loop on number of time integration steps
c
        enddo
       ENDDO
 999   continue
C     
C     count convection event in this grid cell
C     
       OPPSconvectCount(I,J,K,bi,bj) = 
     &      OPPSconvectCount(I,J,K,bi,bj) + 1. _d 0
C
C     jump here if k = maxdepth or if level not unstable, go to next
C     profile point
C
 1000  continue
c
C
C     end  of k-loop
C
      ENDDO 

C--   End IF (DIFFERENT_MULTIPLE)
      ENDIF

      RETURN
      END
      _RL FUNCTION STATE1(sLoc,tLoc,I,J,KREF,bi,bj,mythid)
C     !DESCRIPTION: \bv
C     *===============================================================*
C     | o SUBROUTINE STATE1
C     |   Calculates rho(S,T,p)
C     |   It is absolutely necessary to compute
C     |   the full rho and not sigma=rho-rhoConst, because
C     |   density is used as a scale factor for fluxes and velocities
C     *===============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER I,J,kRef,bi,bj,myThid
      _RL tLoc,sLoc

C     !LOCAL VARIABLES:
C     == Local variables ==
      _RL rhoLoc, dRho
      _RL pLoc
      _RL t1, t2, t3, t4, s1, s3o2, p1, p2, sp5, p1t1
      _RL rfresh, rsalt, rhoP0
      _RL bMfresh, bMsalt, bMpres, BulkMod
      _RL rhoNum, rhoDen, den, epsln
      PARAMETER ( epsln = 0.D0 )

      character*(max_len_mbuf) msgbuf

CMLC     estimate pressure from depth at cell centers
CML      mtoSI = gravity*rhoConst
CML      pLoc = ABS(rC(kRef))*mtoSI

      IF ( buoyancyRelation .EQ. 'OCEANIC' ) THEN
C     in Z coordinates the pressure is rho0 * (hydrostatic) Potential
       IF ( useDynP_inEos_Zc ) THEN  
C----------
C     NOTE: For now, totPhiHyd only contains the Potential anomaly
C           since PhiRef is not available for Atmos and has not (yet) 
C           been added in S/R DIAGS_PHI_HYD 
C----------
        pLoc = rhoConst*( totPhiHyd(i,j,kRef,bi,bj)
     &                   -rC(kRef)*gravity 
     &                   )*maskC(i,j,kRef,bi,bj)
       ELSE
        pLoc = -rhoConst*rC(kRef)*gravity*maskC(i,j,kRef,bi,bj)
       ENDIF
      ELSEIF ( buoyancyRelation .EQ. 'OCEANICP' ) THEN
C     in P coordinates the pressure is just the coordinate of
C     the tracer point
       pLoc = rC(kRef)* maskC(i,j,kRef,bi,bj)
      ENDIF

      rhoLoc  = 0. _d 0
      rhoP0   = 0. _d 0
      bulkMod = 0. _d 0
      rfresh  = 0. _d 0
      rsalt   = 0. _d 0
      bMfresh = 0. _d 0
      bMsalt  = 0. _d 0
      bMpres  = 0. _d 0
      rhoNum  = 0. _d 0
      rhoDen  = 0. _d 0
      den     = 0. _d 0

      t1 = tLoc
      t2 = t1*t1
      t3 = t2*t1
      t4 = t3*t1
      
      s1  = sLoc

      IF ( equationOfState .EQ. 'LINEAR' ) THEN

       dRho = rhoNil-rhoConst
       rhoLoc=rhoNil* (
     &        sBeta *(sLoc-sRef(kRef))
     &      - tAlpha*(tLoc-tRef(KREF)) ) + dRho

      ELSEIF (equationOfState.EQ.'POLY3') THEN

C     this is not correct, there is a field eosSig0 which should be use here
C     but I DO not intent to include the reference level in this routine
         WRITE(*,'(a)') 
     &        ' FIND_RHO_SCALAR: for POLY3, the density is not'
         WRITE(*,'(a)') 
     &         '                 computed correctly in this routine'
         rhoLoc = 0. _d 0

      ELSEIF ( equationOfState(1:5).EQ.'JMD95'
     &      .OR. equationOfState.EQ.'UNESCO' ) THEN
C     nonlinear equation of state in pressure coordinates

         s3o2 = s1*SQRT(s1)
         
         p1 = pLoc*SItoBar
         p2 = p1*p1

C     density of freshwater at the surface
         rfresh = 
     &          eosJMDCFw(1) 
     &        + eosJMDCFw(2)*t1
     &        + eosJMDCFw(3)*t2
     &        + eosJMDCFw(4)*t3
     &        + eosJMDCFw(5)*t4
     &        + eosJMDCFw(6)*t4*t1
C     density of sea water at the surface
         rsalt = 
     &        s1*(
     &             eosJMDCSw(1)
     &           + eosJMDCSw(2)*t1
     &           + eosJMDCSw(3)*t2
     &           + eosJMDCSw(4)*t3
     &           + eosJMDCSw(5)*t4
     &           )
     &        + s3o2*(
     &             eosJMDCSw(6)
     &           + eosJMDCSw(7)*t1
     &           + eosJMDCSw(8)*t2
     &           )
     &           + eosJMDCSw(9)*s1*s1

         rhoP0 = rfresh + rsalt

C     secant bulk modulus of fresh water at the surface
         bMfresh = 
     &             eosJMDCKFw(1)
     &           + eosJMDCKFw(2)*t1
     &           + eosJMDCKFw(3)*t2
     &           + eosJMDCKFw(4)*t3
     &           + eosJMDCKFw(5)*t4
C     secant bulk modulus of sea water at the surface
         bMsalt =
     &        s1*( eosJMDCKSw(1)
     &           + eosJMDCKSw(2)*t1
     &           + eosJMDCKSw(3)*t2
     &           + eosJMDCKSw(4)*t3
     &           )
     &    + s3o2*( eosJMDCKSw(5)
     &           + eosJMDCKSw(6)*t1
     &           + eosJMDCKSw(7)*t2
     &           )
C     secant bulk modulus of sea water at pressure p
         bMpres = 
     &        p1*( eosJMDCKP(1)
     &           + eosJMDCKP(2)*t1
     &           + eosJMDCKP(3)*t2
     &           + eosJMDCKP(4)*t3
     &           )
     &   + p1*s1*( eosJMDCKP(5)
     &           + eosJMDCKP(6)*t1
     &           + eosJMDCKP(7)*t2
     &           )
     &      + p1*s3o2*eosJMDCKP(8)
     &      + p2*( eosJMDCKP(9)
     &           + eosJMDCKP(10)*t1
     &           + eosJMDCKP(11)*t2
     &           )
     &    + p2*s1*( eosJMDCKP(12)
     &           + eosJMDCKP(13)*t1
     &           + eosJMDCKP(14)*t2
     &           )

         bulkMod = bMfresh + bMsalt + bMpres
         
C     density of sea water at pressure p
         rhoLoc = rhoP0/(1. _d 0 - p1/bulkMod) - rhoConst

      ELSEIF ( equationOfState.EQ.'MDJWF' ) THEN

         sp5 = SQRT(s1) 
            
         p1   = pLoc*SItodBar
         p1t1 = p1*t1

         rhoNum = eosMDJWFnum(0)
     &        + t1*(eosMDJWFnum(1) 
     &        +     t1*(eosMDJWFnum(2) + eosMDJWFnum(3)*t1) )  
     &        + s1*(eosMDJWFnum(4)
     &        +     eosMDJWFnum(5)*t1  + eosMDJWFnum(6)*s1)      
     &        + p1*(eosMDJWFnum(7) + eosMDJWFnum(8)*t2 
     &        +     eosMDJWFnum(9)*s1 
     &        +     p1*(eosMDJWFnum(10) + eosMDJWFnum(11)*t2) )

            
         den = eosMDJWFden(0)
     &        + t1*(eosMDJWFden(1)
     &        +     t1*(eosMDJWFden(2)
     &        +         t1*(eosMDJWFden(3) + t1*eosMDJWFden(4) ) ) )
     &        + s1*(eosMDJWFden(5)
     &        +     t1*(eosMDJWFden(6) 
     &        +         eosMDJWFden(7)*t2) 
     &        +     sp5*(eosMDJWFden(8) + eosMDJWFden(9)*t2) ) 
     &        + p1*(eosMDJWFden(10)
     &        +     p1t1*(eosMDJWFden(11)*t2 + eosMDJWFden(12)*p1) )
            
         rhoDen = 1.0/(epsln+den) 

         rhoLoc = rhoNum*rhoDen - rhoConst

      ELSEIF( equationOfState .EQ. 'IDEALG' ) THEN
C     
      ELSE
       WRITE(msgbuf,'(3A)')
     &        ' STATE1 : equationOfState = "',
     &        equationOfState,'"'
       CALL PRINT_ERROR( msgbuf, mythid )
       STOP 'ABNORMAL END: S/R STATE1 in OPPS_CALC'
      ENDIF

      state1 = rhoLoc + rhoConst

#endif /* ALLOW_OPPS */
      RETURN
      END

      
#undef OPPS_ORGCODE
#ifdef OPPS_ORGCODE
c Listed below is the subroutine for use in parallel 3-d circulation code.
c It has been used in the parallel semtner-chervin code and is now being used
c In the POP code.  The subroutine is called nlopps (long story to explain why).

c I've attached the version of lopps that we've been using in the simulations.
c There is one common block that is different from the standard model commons 
c (countc) and it is not needed if the array convadj is not used.  The routine
c does need "kmp" which is why the boundc common is included. For massively
c parallel codes (like POP) we think this will work well when converted from a 
c "slab" (i=is,ie) to a column, which just means removing the "do i=is,ie" loop.  c There are differences between this
c code and the 1-d code and the earlier scheme implemented in 3-d models. These c differences are described below.


      subroutine nlopps(j,is,ie,ta,sa,gcmdz)
c
      parameter (imt = 361 , jmt = 301 , km = 30 )
c
c     Nlopps:   E. Skyllingstad and T. Paluszkiewicz
c
c     Version: December 11, 1996
c
c     Nlopps:  This version of lopps is significantly different from
c     the original code developed by R. Romea and T. Paluskiewicz.  The
c     code uses a flux constraint to control the change in T and S at
c     each grid level.  First, a plume profile of T,S, and W are
c     determined using the standard plume model, but with a detraining
c     mass instead of entraining.  Thus, the T and S plume
c     characteristics still change, but the plume contracts in size
c     rather than expanding ala classical entraining plumes.  This 
c     is heuristically more in line with large eddy simulation results.
c     At each grid level, the convergence of plume velocity determines
c     the flux of T and S, which is conserved by using an upstream
c     advection.  The vertical velocity is balanced so that the area
c     weighted upward velocity equals the area weighted downdraft 
c     velocity, ensuring mass conservation. The present implementation
c     adjusts the plume for a time period equal to the time for 1/2 of
c     the mass of the fastest moving level to move downward.  As a 
c     consequence, the model does not completely adjust the profile at
c     each model time step, but provides a smooth adjustment over time.
c
c     


c

c      include "params.h"
c      include "plume_fast_inc.h"
c      include "plume_fast.h"
c #include "loppsd.h"

      real ta(imt,km),sa(imt,km),gcmdz(km),dz(km)
      real pdensity,wsqr,oldflux,newflux,entrainrate,adtemp
      REAL Del,D,dza1,dza2,kd,kd1,Smix,Thmix,PlumeS,PlumeT,PlumeD
c
c

      INTEGER i,j,k
clfh
      integer is,ie,k2
clfh
      REAL D1,D2,state1,Density
      REAL dz1,dz2
      REAL radius,StartingFlux
      real ttemp(km),stemp(km),taa(km),saa(km)
      real wda(km),tda(km),sda(km),mda(km)
      real dtts,dt,sumo,sumn
      integer ntime,nn,kmx,ic
c
c
      LOGICAL debug,done
      INTEGER MAX_ABE_ITERATIONS
      PARAMETER(MAX_ABE_ITERATIONS=1)
      REAL PlumeRadius
      REAL STABILITY_THRESHOLD
      REAL FRACTIONAL_AREA
      REAL MAX_FRACTIONAL_AREA
      REAL VERTICAL_VELOCITY
      REAL ENTRAINMENT_RATE
      REAL e2
      PARAMETER ( PlumeRadius          =  100.D0   )
      PARAMETER ( STABILITY_THRESHOLD  =  -1.E-4   )
      PARAMETER ( FRACTIONAL_AREA      =  .1E0    )
      PARAMETER ( MAX_FRACTIONAL_AREA  =  .8E0     )
      PARAMETER ( VERTICAL_VELOCITY    =  .02E0   )
      PARAMETER ( ENTRAINMENT_RATE     =  -.05E0     )
      PARAMETER ( e2    =   2.E0*ENTRAINMENT_RATE  )
      ! Arrays.
      REAL Ad(km),Sd(km),Td(km),Wd(km),Dd(km),Md(km)
      REAL Se(km),Te(km),We(km),De(km)
      REAL PlumeEntrainment(km)
      REAL GridThickness(km)

c
c input kmp through a common block
c
      common / boundc / wsx(imt,jmt),wsy(imt,jmt),hfs(imt,jmt),
     1                  ple(imt,jmt),kmp(imt,jmt),kmq(imt,jmt)
cwmseas
     &                 ,wsx1(imt,jmt),wsy1(imt,jmt)
     1                 ,wsx2(imt,jmt),wsy2(imt,jmt)
c
c input the variables through a common
c
      logical problem
      common /countc/ convadj(imt,jmt,km),ics,depth(km),problem


c-----may want to setup an option to get this only on first call
c     otherwise it is repetive
c     griddz is initialize by call to setupgrid
c
c

        dtts = 2400
c
        do k=1,km
          dz(k) = 0.01*gcmdz(k)
        enddo
c
        do k=1,km
           GridThickness(k) = dz(k)
        enddo
c
c modified to loop over slab
c
      DO i=is,ie

        numgridpoints=kmp(i,j)
c
c  go to next column if only 1 grid point or on land
c
        if(numgridpoints.le.1) goto 1100
c
c loop over depth
c
c
      debug = .false.
c
c first save copy of initial profile
c
      DO k=1,NumGridPoints
         stemp(k)=sa(i,k)
         ttemp(k)=ta(i,k)
c
c do a check of t and s range, if out of bounds set flag
c
         if(problem) then
            write(0,*)"Code in trouble before this nlopps call"
            return
         endif
c
         if(sa(i,k).gt.40..or.ta(i,k).lt.-4.0) then
            problem = .true.
            write(0,*)"t out of range at j ",j
            debug = .true.
            return
         endif
      ENDDO

      if(debug) then
        write(*,*)"T and S Profile at  ",i,j
        write(*,*)(ta(i,k),sa(i,k),k=1,NumGridPoints)
      endif

      DO k=1,NumGridPoints-1
c
c initialize the plume T,S,density, and w velocity
c
          Sd(k)=stemp(k)
          Td(k)=ttemp(k)
          Dd(k)=state1(stemp(k),ttemp(k),k)
          De(k)=Dd(k)
c          Wd(k)=VERTICAL_VELOCITY
c
c guess at initial top grid cell vertical velocity
c
          Wd(k) = 0.03
c
c these estimates of initial plume velocity based on plume size and
c top grid cell water mass
c
c          Wd(k) = 0.5*dz(k)/(dtts*FRACTIONAL_AREA)
c          Wd(k) = 0.5*dz(k)/dtts
c
          wsqr=Wd(k)*Wd(k)
          PlumeEntrainment(k) = 0.0
c
c
c 
          if(debug) write(0,*) 'Doing old lowerparcel'
          radius=PlumeRadius
          StartingFlux=radius*radius*Wd(k)*Dd(k)
          oldflux=StartingFlux
      
          dz2=GridThickness(k)
          DO k2=k,NumGridPoints-1
            D1=state1(Sd(k2),Td(k2),k2+1)
            D2=state1(stemp(k2+1),ttemp(k2+1),k2+1)
            De(k2+1)=D2
c
c To start downward, parcel has to initially be heavier than environment
c but after it has started moving, we continue plume until plume tke or
c flux goes negative
c 
            IF (D2-D1 .LT. STABILITY_THRESHOLD.or.k2.ne.k) THEN
                 dz1=dz2
                 dz2=GridThickness(k2+1)
c
c define mass flux according to eq. 4 from paper
c
                 newflux=oldflux+e2*radius*Wd(k2)*Dd(k2)*0.50*
     .              (dz1+dz2)
c
                 PlumeEntrainment(k2+1) = newflux/StartingFlux
c
                 IF(newflux.LT.0.0) then
                     if(debug) then
                      write(0,*)"Plume entrained to zero at ",k2
                     endif
                     maxdepth = k2
                     if(maxdepth.eq.k) goto 1000
                     goto 1
                 endif
c
c entrainment rate is basically a scaled mass flux dM/M
c
                 entrainrate = (newflux - oldflux)/newflux
                 oldflux = newflux
c
c
c mix var's are the average environmental values over the two grid levels
c
                 smix=(dz1*stemp(k2)+dz2*stemp(k2+1))/(dz1+dz2)
                 thmix=(dz1*ttemp(k2)+dz2*ttemp(k2+1))/(dz1+dz2)
c
c first compute the new salinity and temperature for this level
c using equations 3.6 and 3.7 from the paper
c
c
c
                  sd(k2+1)=sd(k2) - entrainrate*(smix - sd(k2))
                  td(k2+1)=td(k2) - entrainrate*(thmix - td(k2))
c                
c
c compute the density at this level for the buoyancy term in the
c vertical k.e. equation
c
                 Dd(k2+1)=state1(Sd(k2+1),Td(k2+1),k2+1)
c
c next, solve for the vertical velocity k.e. using combined eq. 4
c and eq 5 from the paper
c
                 if(debug) then
                   write(0,*)"Dd,De,entr,k ",Dd(k2),De(k2),entrainrate,k2
                 endif
c
                 wsqr = wsqr - wsqr*abs(entrainrate)+ 9.81*
     .             (dz1*(Dd(k2)-De(k2))/De(k2)
     .             +dz2*(Dd(k2+1)-De(k2+1))/De(k2+1))
c
c if negative k.e. then plume has reached max depth, get out of loop
c
                 IF(wsqr.LT.0.0)then
                     maxdepth = k2
                     if(debug) then
                      write(0,*)"Plume velocity went to zero at ",k2
                     endif
                     if(maxdepth.eq.k) goto 1000
                     goto 1
                 endif
                 Wd(k2+1)=sqrt(wsqr)
c
c compute a new radius based on the new mass flux at this grid level
c
                 radius=sqrt(newflux/(Wd(k2)*Dd(k2)))
              ELSE
                 maxdepth=k2
                 if(maxdepth.eq.k) goto 1000
                 GOTO 1
              ENDIF
          ENDDO
c
c plume has reached the bottom
c
          MaxDepth=NumGridPoints
c
1         continue
c
          Ad(k)=FRACTIONAL_AREA
          IC=0
c
c start iteration on fractional area, not used in OGCM implementation
c
c
          DO IC=1,Max_ABE_Iterations
c
c
c next compute the mass flux beteen each grid box using the entrainment
c
 92          continue
             Md(k)=Wd(k)*Ad(k)
c
             DO k2=k+1,MaxDepth
               Md(k2)=Md(k)*PlumeEntrainment(k2)
               if(debug) then
                 write(0,*)"Md, Wd, and  k are ",Md(k2),Wd(k2),k2
               endif
             ENDDO
c
c Now move on to calculate new temperature using flux from
c Td, Sd, Wd, ta, sa, and we. Values for these variables are at
c center of grid cell, use weighted average to get boundary values
c
c use a timestep limited by the GCM model timestep and the maximum plume
c velocity (CFL criteria)
c
c
c calculate the weighted wd, td, and sd
c
             dt = dtts
             do k2=k,maxdepth-1
                dt = min(dt,dz(k2)/wd(k2))
c
c time integration will be integer number of steps to get one
c gcm time step
c
                ntime = nint(0.5*int(dtts/dt))
                if(ntime.eq.0) then
                   ntime = 1
                endif
c
c make sure area weighted vertical velocities match; in other words
c make sure mass in equals mass out at the intersection of each grid
c cell.
c
                mda(k2) = (md(k2)*dz(k2)+md(k2+1)*dz(k2+1))/
     *                    (dz(k2)+dz(k2+1))
c
                wda(k2) = (wd(k2)*dz(k2)+wd(k2+1)*dz(k2+1))/
     *                    (dz(k2)+dz(k2+1))
c
                tda(k2) = td(k2)
                sda(k2) = sd(k2)
c
                taa(k2) = ttemp(k2+1)
                saa(k2) = stemp(k2+1)
c
             enddo
             dt = min(dt,dtts)
             if(debug) then
               write(0,*)"Time step is ", dt
             endif
             tda(maxdepth) = td(maxdepth)
             sda(maxdepth) = sd(maxdepth)
c
c do top and bottom points first
c
             kmx = maxdepth-1
             do nn=1,ntime

               ttemp(k) =  ttemp(k)-
     *                  (mda(k)*(tda(k)-taa(k)))*dt*recip_drF(k)
c
               stemp(k) =  stemp(k)-
     *                  (mda(k)*(sda(k)-saa(k)))*dt*recip_drF(k)
c
c
c now do inner points if there are any
c
               if(Maxdepth-k.gt.1) then
                 do k2=k+1,Maxdepth-1
c
                   ttemp(k2) = ttemp(k2) +
     *              (mda(k2-1)*(tda(k2-1)-taa(k2-1))-
     *              mda(k2)*(tda(k2)-taa(k2)))
     *              *dt*recip_drF(k2)

c
                  stemp(k2) = stemp(k2) +
     *              (mda(k2-1)*(sda(k2-1)-saa(k2-1))-
     *              mda(k2)*(sda(k2)-saa(k2)))
     *              *dt*recip_drF(k2)

c
                 enddo
               endif
               ttemp(kmx+1) =  ttemp(kmx+1)+
     *                  (mda(kmx)*(tda(kmx)-taa(kmx)))*
     *                  dt*recip_drF(kmx+1)
c
               stemp(kmx+1) =  stemp(kmx+1)+
     *                  (mda(kmx)*(sda(kmx)-saa(kmx)))*
     *                  dt*recip_drF(kmx+1)
c
c set the environmental temp and salinity to equal new fields
c
                do k2=1,maxdepth-1
                  taa(k2) = ttemp(k2+1)
                  saa(k2) = stemp(k2+1)
                enddo
c
c end loop on number of time integration steps
c
             enddo
          ENDDO
999       continue
c
c assume that it converged, so update the ta and sa with new fields
c
c          if(i.gt.180.and.j.gt.200.and.i.lt.240) then
c            write(*,*)"Converged ",i,j,k,maxdepth,ttemp(k+1),ta(i,k+1)
c          endif
          do k2=k,maxdepth
            convadj(i,j,k2) = convadj(i,j,k2) + (ttemp(k2)-
     *          ta(i,k2))
            sa(i,k2) = stemp(k2)
            ta(i,k2) = ttemp(k2)
c          if(i.gt.180.and.j.gt.200.and.i.lt.240) then
c            write(*,*)"convadj ",convadj(i,j,k2)
c          endif
c
c see if nlopps messed up
c
            if(sa(i,k).gt.40..or.ta(i,k).lt.-4.0) then
               problem = .true.
               write(0,*)"t out of range at j after adjust",j
               debug = .true.
            endif
c
          enddo
c
c jump here if k = maxdepth or if level not unstable, go to next
c profile point
c
1000      continue
c
c
c end loop on k, move on to next possible plume
c
      ENDDO 
1100  continue
c
c i loop
c
      ENDDO
      END

#endif /* OPPS_ORGCODE */
