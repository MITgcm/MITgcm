#include "OPPS_OPTIONS.h"
#undef OPPS_ORGCODE

C--  File opps_calc.F:
C--   Contents
C--   o OPPS_CALC
C--   o STATE1
C--   o NLOPPS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: OPPS_CALC

C !INTERFACE: ======================================================
      SUBROUTINE OPPS_CALC(
     U     tracerEnv,
     O     OPPSconvectCount,
     I     wVel,
     I     kMax, nTracer, nTracerInuse,
     I     I, J, bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *=====================================================================*
C     | SUBROUTINE OPPS_CALC                                                |
C     | o Compute all OPPS fields defined in OPPS.h                         |
C     *=====================================================================*
C     | This subroutine is based on the routine 3dconvection.F              |
C     | by E. Skyllingstad (?)                                              |
C     | plenty of modifications to make it work:                            |
C     | - removed many unused parameters and variables                      |
C     | - turned everything (back) into 1D code                             |
C     | - pass variables, that are originally in common blocks:             |
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
C     *=====================================================================*
C \ev

      IMPLICIT NONE

C !USES: ==============================================================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OPPS.h"
#include "GRID.h"

C !INPUT/OUTPUT PARAMETERS: ============================================
C Routine arguments
C     OPPSconvectCount :: counter for freqency of convection events
C     bi, bj :: array indices on which to apply calculations
C     myTime :: Current time in simulation
      INTEGER KMax, nTracer, nTracerInUse
      _RL tracerEnv(Nr,nTracer)
      _RL OPPSconvectCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wVel(Nr)
      INTEGER I, J, bi, bj
      _RL     myTime
      INTEGER myThid, myIter

#ifdef ALLOW_OPPS
C !FUNCTIONS: ==========================================================
c     EXTERNAL DIFFERENT_MULTIPLE
c     LOGICAL  DIFFERENT_MULTIPLE
      _RL STATE1

C !LOCAL VARIABLES: ====================================================
C Local constants
C     msgBuf      :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER K, K2, K2m1, K2p1, ktr
      INTEGER ntime,nn,kmx,ic
      INTEGER maxDepth

      _RL wsqr,oldflux,newflux,entrainrate
      _RL pmix
      _RL D1, D2
      _RL dz1,dz2
      _RL radius,StartingFlux
      _RL dtts,dt
C     Arrays
      _RL Paa(Nr,nTracer)
      _RL wda(Nr), mda(Nr), pda(Nr,nTracer)
C
C     Pd, Wd           :: tracers, vertical velocity in plume
C     Md               :: plume mass flux (?)
C     Ad               :: fractional area covered by plume
C     Dd               :: density in plume
C     De               :: density of environment
C     PlumeEntrainment ::
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

C     start k-loop

      DO k=1,KMax-1

C initialize the plume T,S,density, and w velocity

       DO ktr=1,nTracerInUse
        Pd(k,ktr) = tracerEnv(k,ktr)
       ENDDO
       Dd(k)=STATE1(Pd(k,2),Pd(k,1),i,j,k,bi,bj,myThid)
       De(k)=Dd(k)
CML       print *, 'ml-opps:', i,j,k,tracerEnv(k,2),tracerEnv(k,1),
CML     &      Dd(k),Pd(k,1),Pd(k,2)
CML compute vertical velocity at cell centers from GCM velocity
       Wd(k)= - .5*(wVel(K)+wVel(K+1))
CML(
CML    avoid division by zero
CML       IF (Wd(K) .EQ. 0.D0) Wd(K) = 2.23e-16
CML)

C guess at initial top grid cell vertical velocity

CML          Wd(k) = 0.03

C these estimates of initial plume velocity based on plume size and
C top grid cell water mass

c          Wd(k) = 0.5*drF(k)/(dtts*FRACTIONAL_AREA)
c          Wd(k) = 0.5*drF(k)/dtts

       wsqr=Wd(k)*Wd(k)
       PlumeEntrainment(k) = 0.0

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
        D1=STATE1( Pd(k2,2), Pd(k2,1),i,j,k2+1,bi,bj,myThid)
        D2=STATE1( tracerEnv(k2+1,2), tracerEnv(k2+1,1),
     &                                i,j,k2+1,bi,bj,myThid)
        De(k2+1)=D2

C To start downward, parcel has to initially be heavier than environment
C but after it has started moving, we continue plume until plume tke or
C flux goes negative

CML     &       _hFacC(i,j,k-1,bi,bj)
CML     &       *_hFacC(i,j,k,bi,bj) .GT. 0.
CML     &  .AND.
        IF (D2-D1 .LT. STABILITY_THRESHOLD.or.k2.ne.k) THEN
         dz1=dz2
         dz2=DrF(k2+1)

C     find mass flux according to eq.(3) from paper by vertical integration

         newflux=oldflux+e2*radius*Wd(k2)*Dd(k2)*
     &        .5*(dz1+dz2)
CML         print *, 'ml-opps:', i,j,k,oldflux,newflux,e2,radius,
CML     &        Wd(k2),Dd(k2),Pd(k2,1),Pd(k2,2),dz1,dz2

         PlumeEntrainment(k2+1) = newflux/StartingFlux

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

C entrainment rate is basically a scaled mass flux dM/M

         entrainrate = (newflux - oldflux)/newflux
         oldflux = newflux

C mix var(s) are the average environmental values over the two grid levels

         DO ktr=1,nTracerInUse
          pmix=(dz1*tracerEnv(k2,ktr)+dz2*tracerEnv(k2+1,ktr))
     &         /(dz1+dz2)
          Pd(k2+1,ktr)=Pd(k2,ktr)
     &         - entrainrate*(pmix - Pd(k2,ktr))
         ENDDO

C compute the density at this level for the buoyancy term in the
C vertical k.e. equation

         Dd(k2+1)=STATE1(Pd(k2+1,2),Pd(k2+1,1),i,j,k2+1,bi,bj,myThid)

C next, solve for the vertical velocity k.e. using combined eq. (4)
C and eq (5) from the paper

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

C if negative k.e. then plume has reached max depth, get out of loop

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

C     compute a new radius based on the new mass flux at this grid level
C     from Eq. (4)

         radius=sqrt(newflux/(Wd(k2)*Dd(k2)))
        ELSE
         maxdepth=k2
         if(maxdepth.eq.k) goto 1000
         GOTO 1
        ENDIF
       ENDDO

C plume has reached the bottom

       MaxDepth=kMax

 1     CONTINUE

       Ad(k)=FRACTIONAL_AREA
       IC=0

C start iteration on fractional area, not used in OGCM implementation

       DO IC=1,Max_ABE_Iterations

C next compute the mass flux beteen each grid box using the entrainment

        Md(k)=Wd(k)*Ad(k)

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

C Now move on to calculate new temperature using flux from
C Td, Sd, Wd, ta, sa, and we. Values for these variables are at
C center of grid cell, use weighted average to get boundary values

C use a timestep limited by the GCM model timestep and the maximum plume
C velocity (CFL criteria)

C calculate the weighted wd, td, and sd
        dt = dtts
        do k2=k,maxDepth-1
         IF ( Wd(K2) .NE. 0. _d 0 ) dt = min(dt,drF(k2)/Wd(k2))

C time integration will be integer number of steps to get one
C gcm time step

         ntime = nint(0.5*int(dtts/dt))
         if(ntime.eq.0) then
          ntime = 1
         endif

C make sure area weighted vertical velocities match; in other words
C make sure mass in equals mass out at the intersection of each grid
C cell. Eq. (20)

         mda(k2) = (md(k2)*drF(k2)+md(k2+1)*drF(k2+1))/
     *        (drF(k2)+drF(k2+1))

         wda(k2) = (wd(k2)*drF(k2)+wd(k2+1)*drF(k2+1))/
     *        (drF(k2)+drF(k2+1))

         DO ktr = 1, nTracerInUse
          Pda(k2,ktr) = Pd(k2,ktr)
          Paa(k2,ktr) = tracerEnv(k2+1,ktr)
         ENDDO

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

        kmx = maxdepth-1
        do nn=1,ntime

C     top point

         DO ktr = 1,nTracerInUse
          tracerEnv(k,ktr) =  tracerEnv(k,ktr)-
     &        (mda(k)*(Pda(k,ktr)-Paa(k,ktr)))*dt*recip_drF(k)
         ENDDO

C now do inner points if there are any

CML         if(Maxdepth-k.gt.1) then
CML    This if statement is superfluous
CML         IF ( k .LT. Maxdepth-1 ) THEN
CML         DO k2=k+1,Maxdepth-1
CML         mda(maxDepth) = 0.
         DO k2=k+1,kmx
          k2m1 = max(k,k2-1)
          k2p1 = max(k2+1,maxDepth)

           DO ktr = 1,nTracerInUse
            tracerEnv(k2,ktr) = tracerEnv(k2,ktr) +
     &           (mda(k2m1)*(Pda(k2m1,ktr)-Paa(k2m1,ktr))
     &           -mda(k2)  *(Pda(k2,ktr)  -Paa(k2,ktr))  )
     &           *dt*recip_drF(k2)
           ENDDO
          ENDDO
CML    This if statement is superfluous
CML         ENDIF

C     bottom point

         DO ktr=1,nTracerInUse
          tracerEnv(kmx+1,ktr) =  tracerEnv(kmx+1,ktr)+
     &        mda(kmx)*(Pda(kmx,ktr)-Paa(kmx,ktr))*dt*recip_drF(kmx+1)
         ENDDO

C     set the environmental temp and salinity to equal new fields

         DO ktr=1,nTracerInUse
          DO k2=1,kmx
           paa(k2,ktr) = tracerEnv(k2+1,ktr)
          ENDDO
         ENDDO

C end loop on number of time integration steps

        enddo
       ENDDO

C     count convection event in this grid cell

       OPPSconvectCount(I,J,K) = OPPSconvectCount(I,J,K) + 1. _d 0

C     jump here if k = maxdepth or if level not unstable, go to next
C     profile point

 1000  continue

C     end  of k-loop

      ENDDO

C--   End IF (DIFFERENT_MULTIPLE)
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _RL FUNCTION STATE1( sLoc, tLoc, i, j, kRef, bi, bj, myThid )

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
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
# include "NH_VARS.h"
#endif /* ALLOW_NONHYDROSTATIC */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER i, j, kRef, bi, bj, myThid
      _RL tLoc, sLoc

C     !LOCAL VARIABLES:
C     == Local variables ==
      _RL rhoLoc
      _RL pLoc

CMLC     estimate pressure from depth at cell centers
CML      mtoSI = gravity*rhoConst
CML      pLoc = ABS(rC(kRef))*mtoSI

      IF ( usingZCoords ) THEN
C     in Z coordinates the pressure is rho0 * (hydrostatic) Potential
#ifdef ALLOW_NONHYDROSTATIC
       IF ( selectP_inEOS_Zc.EQ.3 ) THEN
        pLoc = rhoConst*( totPhiHyd(i,j,kRef,bi,bj)
     &                  + phi_nh(i,j,kRef,bi,bj)
     &                  + phiRef(2*kRef)
     &                  )*maskC(i,j,kRef,bi,bj)
       ELSEIF ( selectP_inEOS_Zc.EQ.2 ) THEN
#else /* ALLOW_NONHYDROSTATIC */
       IF     ( selectP_inEOS_Zc.EQ.2 ) THEN
#endif /* ALLOW_NONHYDROSTATIC */
C----------
C     NOTE: For now, totPhiHyd only contains the Potential anomaly
C           since PhiRef has not (yet) been added in S/R DIAGS_PHI_HYD
C----------
        pLoc = rhoConst*( totPhiHyd(i,j,kRef,bi,bj)
     &                  + phiRef(2*kRef)
     &                  )*maskC(i,j,kRef,bi,bj)
c      ELSEIF ( selectP_inEOS_Zc.EQ.1 ) THEN
C note: for the case selectP_inEOS_Zc=0, also use pRef4EOS (set to
C       rhoConst*phiRef(2*k) ) to reproduce same previous machine truncation
       ELSEIF ( selectP_inEOS_Zc.LE.1 ) THEN
        pLoc = pRef4EOS(kRef)*maskC(i,j,kRef,bi,bj)
       ELSE
        pLoc = rhoConst*phiRef(2*kRef)*maskC(i,j,kRef,bi,bj)
       ENDIF
      ELSEIF ( usingPCoords ) THEN
C     in P coordinates the pressure is just the coordinate of the tracer point
       pLoc = rC(kRef)* maskC(i,j,kRef,bi,bj)
      ENDIF

      CALL FIND_RHO_SCALAR( tLoc, sLoc, pLoc, rhoLoc, myThid )
      STATE1 = rhoLoc

#endif /* ALLOW_OPPS */
      RETURN
      END

#ifdef OPPS_ORGCODE
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C Listed below is the subroutine for use in parallel 3-d circulation code.
C It has been used in the parallel semtner-chervin code and is now being used
C In the POP code.  The subroutine is called nlopps (long story to explain why).

C I've attached the version of lopps that we've been using in the simulations.
C There is one common block that is different from the standard model commons
C (countc) and it is not needed if the array convadj is not used.  The routine
C does need "kmp" which is why the boundc common is included. For massively
C parallel codes (like POP) we think this will work well when converted from a
C "slab" (i=is,ie) to a column, which just means removing the "do i=is,ie" loop.
C There are differences between this
C code and the 1-d code and the earlier scheme implemented in 3-d models. These
C differences are described below.

      subroutine nlopps(j,is,ie,ta,sa,gcmdz)

      parameter (imt = 361 , jmt = 301 , km = 30 )

C     Nlopps:   E. Skyllingstad and T. Paluszkiewicz

C     Version: December 11, 1996

C     Nlopps:  This version of lopps is significantly different from
C     the original code developed by R. Romea and T. Paluskiewicz.  The
C     code uses a flux constraint to control the change in T and S at
C     each grid level.  First, a plume profile of T,S, and W are
C     determined using the standard plume model, but with a detraining
C     mass instead of entraining.  Thus, the T and S plume
C     characteristics still change, but the plume contracts in size
C     rather than expanding ala classical entraining plumes.  This
C     is heuristically more in line with large eddy simulation results.
C     At each grid level, the convergence of plume velocity determines
C     the flux of T and S, which is conserved by using an upstream
C     advection.  The vertical velocity is balanced so that the area
C     weighted upward velocity equals the area weighted downdraft
C     velocity, ensuring mass conservation. The present implementation
C     adjusts the plume for a time period equal to the time for 1/2 of
C     the mass of the fastest moving level to move downward.  As a
C     consequence, the model does not completely adjust the profile at
C     each model time step, but provides a smooth adjustment over time.

c      include "params.h"
c      include "plume_fast_inc.h"
c      include "plume_fast.h"
c #include "loppsd.h"

      real ta(imt,km),sa(imt,km),gcmdz(km),dz(km)
      real pdensity,wsqr,oldflux,newflux,entrainrate,adtemp
      REAL Del,D,dza1,dza2,kd,kd1,Smix,Thmix,PlumeS,PlumeT,PlumeD

      INTEGER i,j,k
Clfh
      integer is,ie,k2
Clfh
      REAL D1,D2,state1,Density
      REAL dz1,dz2
      REAL radius,StartingFlux
      real ttemp(km),stemp(km),taa(km),saa(km)
      real wda(km),tda(km),sda(km),mda(km)
      real dtts,dt,sumo,sumn
      integer ntime,nn,kmx,ic

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

C input kmp through a common block

      common / boundc / wsx(imt,jmt),wsy(imt,jmt),hfs(imt,jmt),
     1                  ple(imt,jmt),kmp(imt,jmt),kmq(imt,jmt)
cwmseas
     &                 ,wsx1(imt,jmt),wsy1(imt,jmt)
     1                 ,wsx2(imt,jmt),wsy2(imt,jmt)

C input the variables through a common

      logical problem
      common /countc/ convadj(imt,jmt,km),ics,depth(km),problem

C-----may want to setup an option to get this only on first call
C     otherwise it is repetive
C     griddz is initialize by call to setupgrid

        dtts = 2400

        do k=1,km
          dz(k) = 0.01*gcmdz(k)
        enddo

        do k=1,km
           GridThickness(k) = dz(k)
        enddo

C modified to loop over slab

      DO i=is,ie

        numgridpoints=kmp(i,j)

C  go to next column if only 1 grid point or on land

        if(numgridpoints.le.1) goto 1100

C loop over depth

      debug = .false.

C first save copy of initial profile

      DO k=1,NumGridPoints
         stemp(k)=sa(i,k)
         ttemp(k)=ta(i,k)

C do a check of t and s range, if out of bounds set flag

         if(problem) then
            write(0,*)"Code in trouble before this nlopps call"
            return
         endif

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

C initialize the plume T,S,density, and w velocity

          Sd(k)=stemp(k)
          Td(k)=ttemp(k)
          Dd(k)=state1(stemp(k),ttemp(k),k)
          De(k)=Dd(k)
c          Wd(k)=VERTICAL_VELOCITY

C guess at initial top grid cell vertical velocity

          Wd(k) = 0.03

C these estimates of initial plume velocity based on plume size and
C top grid cell water mass

c          Wd(k) = 0.5*dz(k)/(dtts*FRACTIONAL_AREA)
c          Wd(k) = 0.5*dz(k)/dtts

          wsqr=Wd(k)*Wd(k)
          PlumeEntrainment(k) = 0.0

          if(debug) write(0,*) 'Doing old lowerparcel'
          radius=PlumeRadius
          StartingFlux=radius*radius*Wd(k)*Dd(k)
          oldflux=StartingFlux

          dz2=GridThickness(k)
          DO k2=k,NumGridPoints-1
            D1=state1(Sd(k2),Td(k2),k2+1)
            D2=state1(stemp(k2+1),ttemp(k2+1),k2+1)
            De(k2+1)=D2

C To start downward, parcel has to initially be heavier than environment
C but after it has started moving, we continue plume until plume tke or
C flux goes negative

            IF (D2-D1 .LT. STABILITY_THRESHOLD.or.k2.ne.k) THEN
                 dz1=dz2
                 dz2=GridThickness(k2+1)

C define mass flux according to eq. 4 from paper

                 newflux=oldflux+e2*radius*Wd(k2)*Dd(k2)*0.50*
     .              (dz1+dz2)

                 PlumeEntrainment(k2+1) = newflux/StartingFlux

                 IF(newflux.LT.0.0) then
                     if(debug) then
                      write(0,*)"Plume entrained to zero at ",k2
                     endif
                     maxdepth = k2
                     if(maxdepth.eq.k) goto 1000
                     goto 1
                 endif

C entrainment rate is basically a scaled mass flux dM/M

                 entrainrate = (newflux - oldflux)/newflux
                 oldflux = newflux

C mix var(s) are the average environmental values over the two grid levels

                 smix=(dz1*stemp(k2)+dz2*stemp(k2+1))/(dz1+dz2)
                 thmix=(dz1*ttemp(k2)+dz2*ttemp(k2+1))/(dz1+dz2)

C first compute the new salinity and temperature for this level
C using equations 3.6 and 3.7 from the paper

                  sd(k2+1)=sd(k2) - entrainrate*(smix - sd(k2))
                  td(k2+1)=td(k2) - entrainrate*(thmix - td(k2))

C compute the density at this level for the buoyancy term in the
C vertical k.e. equation

                 Dd(k2+1)=state1(Sd(k2+1),Td(k2+1),k2+1)

C next, solve for the vertical velocity k.e. using combined eq. 4
C and eq 5 from the paper

                 if(debug) then
                  write(0,*)"Dd,De,entr,k ",Dd(k2),De(k2),entrainrate,k2
                 endif

                 wsqr = wsqr - wsqr*abs(entrainrate)+ 9.81*
     .             (dz1*(Dd(k2)-De(k2))/De(k2)
     .             +dz2*(Dd(k2+1)-De(k2+1))/De(k2+1))

C if negative k.e. then plume has reached max depth, get out of loop

                 IF(wsqr.LT.0.0)then
                     maxdepth = k2
                     if(debug) then
                      write(0,*)"Plume velocity went to zero at ",k2
                     endif
                     if(maxdepth.eq.k) goto 1000
                     goto 1
                 endif
                 Wd(k2+1)=sqrt(wsqr)

C compute a new radius based on the new mass flux at this grid level

                 radius=sqrt(newflux/(Wd(k2)*Dd(k2)))
              ELSE
                 maxdepth=k2
                 if(maxdepth.eq.k) goto 1000
                 GOTO 1
              ENDIF
          ENDDO

C plume has reached the bottom

          MaxDepth=NumGridPoints

1         continue

          Ad(k)=FRACTIONAL_AREA
          IC=0

C start iteration on fractional area, not used in OGCM implementation

          DO IC=1,Max_ABE_Iterations

C next compute the mass flux beteen each grid box using the entrainment

 92          continue
             Md(k)=Wd(k)*Ad(k)

             DO k2=k+1,MaxDepth
               Md(k2)=Md(k)*PlumeEntrainment(k2)
               if(debug) then
                 write(0,*)"Md, Wd, and  k are ",Md(k2),Wd(k2),k2
               endif
             ENDDO

C Now move on to calculate new temperature using flux from
C Td, Sd, Wd, ta, sa, and we. Values for these variables are at
C center of grid cell, use weighted average to get boundary values

C use a timestep limited by the GCM model timestep and the maximum plume
C velocity (CFL criteria)

C calculate the weighted wd, td, and sd

             dt = dtts
             do k2=k,maxdepth-1
                dt = min(dt,dz(k2)/wd(k2))

C time integration will be integer number of steps to get one
C gcm time step

                ntime = nint(0.5*int(dtts/dt))
                if(ntime.eq.0) then
                   ntime = 1
                endif

C make sure area weighted vertical velocities match; in other words
C make sure mass in equals mass out at the intersection of each grid
C cell.

                mda(k2) = (md(k2)*dz(k2)+md(k2+1)*dz(k2+1))/
     *                    (dz(k2)+dz(k2+1))

                wda(k2) = (wd(k2)*dz(k2)+wd(k2+1)*dz(k2+1))/
     *                    (dz(k2)+dz(k2+1))

                tda(k2) = td(k2)
                sda(k2) = sd(k2)

                taa(k2) = ttemp(k2+1)
                saa(k2) = stemp(k2+1)

             enddo
             dt = min(dt,dtts)
             if(debug) then
               write(0,*)"Time step is ", dt
             endif
             tda(maxdepth) = td(maxdepth)
             sda(maxdepth) = sd(maxdepth)

C do top and bottom points first

             kmx = maxdepth-1
             do nn=1,ntime

               ttemp(k) =  ttemp(k)-
     *                  (mda(k)*(tda(k)-taa(k)))*dt*recip_drF(k)

               stemp(k) =  stemp(k)-
     *                  (mda(k)*(sda(k)-saa(k)))*dt*recip_drF(k)

C now do inner points if there are any

               if(Maxdepth-k.gt.1) then
                 do k2=k+1,Maxdepth-1

                   ttemp(k2) = ttemp(k2) +
     *              (mda(k2-1)*(tda(k2-1)-taa(k2-1))-
     *              mda(k2)*(tda(k2)-taa(k2)))
     *              *dt*recip_drF(k2)

                  stemp(k2) = stemp(k2) +
     *              (mda(k2-1)*(sda(k2-1)-saa(k2-1))-
     *              mda(k2)*(sda(k2)-saa(k2)))
     *              *dt*recip_drF(k2)

                 enddo
               endif
               ttemp(kmx+1) =  ttemp(kmx+1)+
     *                  (mda(kmx)*(tda(kmx)-taa(kmx)))*
     *                  dt*recip_drF(kmx+1)

               stemp(kmx+1) =  stemp(kmx+1)+
     *                  (mda(kmx)*(sda(kmx)-saa(kmx)))*
     *                  dt*recip_drF(kmx+1)

C set the environmental temp and salinity to equal new fields

                do k2=1,maxdepth-1
                  taa(k2) = ttemp(k2+1)
                  saa(k2) = stemp(k2+1)
                enddo

C end loop on number of time integration steps

             enddo
          ENDDO
999       continue

C assume that it converged, so update the ta and sa with new fields

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

C see if nlopps messed up

            if(sa(i,k).gt.40..or.ta(i,k).lt.-4.0) then
               problem = .true.
               write(0,*)"t out of range at j after adjust",j
               debug = .true.
            endif

          enddo

C jump here if k = maxdepth or if level not unstable, go to next
C profile point

1000      continue

C end loop on k, move on to next possible plume

      ENDDO
1100  continue

C i loop

      ENDDO
      END

#endif /* OPPS_ORGCODE */
