C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_advection.F,v 1.8 2001/09/28 16:49:54 adcroft Exp $
C $Name:  $

CBOI
C !TITLE: pkg/generic\_advdiff
C !AUTHORS: adcroft@mit.edu
C !INTRODUCTION: Generic Advection Diffusion Package
C
C Package "generic\_advdiff" provides a common set of routines for calculating
C advective/diffusive fluxes for tracers (cell centered quantities on a C-grid).
C
C Many different advection schemes are available: the standard centered
C second order, centered fourth order and upwind biased third order schemes
C are known as linear methods and require some stable time-stepping method
C such as Adams-Bashforth. Alternatives such as flux-limited schemes are
C stable in the forward sense and are best combined with the multi-dimensional
C method provided in gad\_advection.
C
C There are two high-level routines:
C  \begin{itemize}
C  \item{GAD\_CALC\_RHS} calculates all fluxes at time level "n" and is used
C  for the standard linear schemes. This must be used in conjuction with
C  Adams-Bashforth time-stepping. Diffusive and parameterized fluxes are
C  always calculated here.
C  \item{GAD\_ADVECTION} calculates just the advective fluxes using the
C  non-linear schemes and can not be used in conjuction with Adams-Bashforth
C  time-stepping.
C  \end{itemize}
CEOI

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_ADVECTION

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_ADVECTION(bi,bj,advectionScheme,tracerIdentity,
     U                         Tracer,Gtracer,
     I                         myTime,myIter,myThid)

C !DESCRIPTION:
C Calculates the tendancy of a tracer due to advection.
C It uses the multi-dimensional method given in \ref{sect:multiDimAdvection}
C and can only be used for the non-linear advection schemes such as the
C direct-space-time method and flux-limiters. 
C
C The algorithm is as follows:
C \begin{itemize}
C \item{$\theta^{(n+1/3)} = \theta^{(n)}
C      - \Delta t \partial_x (u\theta^{(n)}) + \theta^{(n)} \partial_x u$}
C \item{$\theta^{(n+2/3)} = \theta^{(n+1/3)}
C      - \Delta t \partial_y (v\theta^{(n+1/3)}) + \theta^{(n)} \partial_y v$}
C \item{$\theta^{(n+3/3)} = \theta^{(n+2/3)}
C      - \Delta t \partial_r (w\theta^{(n+2/3)}) + \theta^{(n)} \partial_r w$}
C \item{$G_\theta = ( \theta^{(n+3/3)} - \theta^{(n)} )/\Delta t$}
C \end{itemize}
C
C The tendancy (output) is over-written by this routine.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "GAD.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  advectionScheme      :: advection scheme to use
C  tracerIdentity       :: identifier for the tracer (required only for OBCS)
C  Tracer               :: tracer field
C  myTime               :: current time
C  myIter               :: iteration number
C  myThid               :: thread number
      INTEGER bi,bj
      INTEGER advectionScheme
      INTEGER tracerIdentity
      _RL Gtracer(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  gTracer              :: tendancy array
      _RL Tracer(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)

C !LOCAL VARIABLES: ====================================================
C  maskUp               :: 2-D array for mask at W points
C  iMin,iMax,jMin,jMax  :: loop range for called routines
C  i,j,k                :: loop indices
C  kup                  :: index into 2 1/2D array, toggles between 1 and 2
C  kdown                :: index into 2 1/2D array, toggles between 2 and 1
C  kp1                  :: =k+1 for k<Nr, =Nr for k=Nr
C  xA,yA                :: areas of X and Y face of tracer cells
C  uTrans,vTrans,rTrans :: 2-D arrays of volume transports at U,V and W points
C  af                   :: 2-D array for horizontal advective flux
C  fVerT                :: 2 1/2D arrays for vertical advective flux
C  localTij             :: 2-D array used as temporary local copy of tracer fld
C  localTijk            :: 3-D array used as temporary local copy of tracer fld
C  kp1Msk               :: flag (0,1) to act as over-riding mask for W levels
C  calc_fluxes_X        :: logical to indicate to calculate fluxes in X dir
C  calc_fluxes_Y        :: logical to indicate to calculate fluxes in Y dir
C  nipass               :: number of passes to make in multi-dimensional method
C  ipass                :: number of the current pass being made
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax,jMin,jMax
      INTEGER i,j,k,kup,kDown,kp1
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL localTij(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localTijk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL kp1Msk
      LOGICAL calc_fluxes_X,calc_fluxes_Y
      INTEGER nipass,ipass
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        xA(i,j)      = 0. _d 0
        yA(i,j)      = 0. _d 0
        uTrans(i,j)  = 0. _d 0
        vTrans(i,j)  = 0. _d 0
        rTrans(i,j)  = 0. _d 0
        fVerT(i,j,1) = 0. _d 0
        fVerT(i,j,2) = 0. _d 0
       ENDDO
      ENDDO

      iMin = 1-OLx
      iMax = sNx+OLx
      jMin = 1-OLy
      jMax = sNy+OLy

C--   Start of k loop for horizontal fluxes
      DO k=1,Nr
#ifdef ALLOW_AUTODIFF_TAMC 
         kkey = (ikey-1)*Nr + k
CADJ STORE tracer(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Get temporary terms used by tendency routines
      CALL CALC_COMMON_FACTORS (
     I         bi,bj,iMin,iMax,jMin,jMax,k,
     O         xA,yA,uTrans,vTrans,rTrans,maskUp,
     I         myThid)

C--   Make local copy of tracer array
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        localTij(i,j)=tracer(i,j,k,bi,bj)
       ENDDO
      ENDDO

      IF (useCubedSphereExchange) THEN
       nipass=3
      ELSE
       nipass=1
      ENDIF
cph       nipass=1

C--   Multiple passes for different directions on different tiles
      DO ipass=1,nipass
#ifdef ALLOW_AUTODIFF_TAMC
         passkey = ipass + (k-1)   *maxpass
     &                   + (ikey-1)*maxpass*Nr
         IF (nipass .GT. maxpass) THEN
          STOP 'GAD_ADVECTION: nipass > maxpass. check tamc.h'
         ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

      IF (nipass.EQ.3) THEN
       calc_fluxes_X=.FALSE.
       calc_fluxes_Y=.FALSE.
       IF (ipass.EQ.1 .AND. (bi.EQ.1 .OR. bi.EQ.2) ) THEN
        calc_fluxes_X=.TRUE.
       ELSEIF (ipass.EQ.1 .AND. (bi.EQ.4 .OR. bi.EQ.5) ) THEN
        calc_fluxes_Y=.TRUE.
       ELSEIF (ipass.EQ.2 .AND. (bi.EQ.1 .OR. bi.EQ.6) ) THEN
        calc_fluxes_Y=.TRUE.
       ELSEIF (ipass.EQ.2 .AND. (bi.EQ.3 .OR. bi.EQ.4) ) THEN
        calc_fluxes_X=.TRUE.
       ELSEIF (ipass.EQ.3 .AND. (bi.EQ.2 .OR. bi.EQ.3) ) THEN
        calc_fluxes_Y=.TRUE.
       ELSEIF (ipass.EQ.3 .AND. (bi.EQ.5 .OR. bi.EQ.6) ) THEN
        calc_fluxes_X=.TRUE.
       ENDIF
      ELSE
       calc_fluxes_X=.TRUE.
       calc_fluxes_Y=.TRUE.
      ENDIF
 
C--   X direction
      IF (calc_fluxes_X) THEN

C--   Internal exchange for calculations in X
      IF (useCubedSphereExchange) THEN
       DO j=1,Oly
        DO i=1,Olx
         localTij( 1-i , 1-j )=localTij( 1-j ,    i    )
         localTij( 1-i ,sNy+j)=localTij( 1-j , sNy+1-i )
         localTij(sNx+i, 1-j )=localTij(sNx+j,    i    )
         localTij(sNx+i,sNy+j)=localTij(sNx+j, sNy+1-i )
        ENDDO
       ENDDO
      ENDIF

C-    Advective flux in X
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        af(i,j) = 0.
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
#ifndef DISABLE_MULTIDIM_ADVECTION
CADJ STORE localTij(:,:)  = comlev1_bibj_pass, key=passkey, byte=isbyte
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

      IF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
       CALL GAD_FLUXLIMIT_ADV_X(
     &      bi,bj,k,deltaTtracer,uTrans,uVel,localTij,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
       CALL GAD_DST3_ADV_X(
     &       bi,bj,k,deltaTtracer,uTrans,uVel,localTij,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
       CALL GAD_DST3FL_ADV_X(
     &       bi,bj,k,deltaTtracer,uTrans,uVel,localTij,af,myThid)
      ELSE
       STOP 'GAD_ADVECTION: adv. scheme incompatibale with mutli-dim'
      ENDIF

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx-1
        localTij(i,j)=localTij(i,j)-deltaTtracer*
     &    _recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rA(i,j,bi,bj)
     &    *( af(i+1,j)-af(i,j)
     &      -tracer(i,j,k,bi,bj)*(uTrans(i+1,j)-uTrans(i,j))
     &     )
       ENDDO
      ENDDO

#ifdef ALLOW_OBCS
C--   Apply open boundary conditions
      IF (useOBCS) THEN
       IF (tracerIdentity.EQ.GAD_TEMPERATURE) THEN
        CALL OBCS_APPLY_TLOC( bi, bj, k, localTij, myThid )
       ELSEIF (tracerIdentity.EQ.GAD_SALINITY) THEN
        CALL OBCS_APPLY_SLOC( bi, bj, k, localTij, myThid )
       END IF
      END IF
#endif /* ALLOW_OBCS */

C--   End of X direction
      ENDIF

C--   Y direction
      IF (calc_fluxes_Y) THEN

C--   Internal exchange for calculations in Y
      IF (useCubedSphereExchange) THEN
       DO j=1,Oly
        DO i=1,Olx
         localTij( 1-i , 1-j )=localTij(   j   , 1-i )
         localTij( 1-i ,sNy+j)=localTij(   j   ,sNy+i)
         localTij(sNx+i, 1-j )=localTij(sNx+1-j, 1-i )
         localTij(sNx+i,sNy+j)=localTij(sNx+1-j,sNy+i)
        ENDDO
       ENDDO
      ENDIF

C-    Advective flux in Y
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        af(i,j) = 0.
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC 
#ifndef DISABLE_MULTIDIM_ADVECTION
CADJ STORE localTij(:,:)  = comlev1_bibj_pass, key=passkey, byte=isbyte
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

      IF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
       CALL GAD_FLUXLIMIT_ADV_Y(
     &       bi,bj,k,deltaTtracer,vTrans,vVel,localTij,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
       CALL GAD_DST3_ADV_Y(
     &       bi,bj,k,deltaTtracer,vTrans,vVel,localTij,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
       CALL GAD_DST3FL_ADV_Y(
     &       bi,bj,k,deltaTtracer,vTrans,vVel,localTij,af,myThid)
      ELSE
       STOP 'GAD_ADVECTION: adv. scheme incompatibale with mutli-dim'
      ENDIF

      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        localTij(i,j)=localTij(i,j)-deltaTtracer*
     &    _recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rA(i,j,bi,bj)
     &    *( af(i,j+1)-af(i,j)
     &      -tracer(i,j,k,bi,bj)*(vTrans(i,j+1)-vTrans(i,j))
     &     )
       ENDDO
      ENDDO

#ifdef ALLOW_OBCS
C--   Apply open boundary conditions
      IF (useOBCS) THEN
       IF (tracerIdentity.EQ.GAD_TEMPERATURE) THEN
        CALL OBCS_APPLY_TLOC( bi, bj, k, localTij, myThid )
       ELSEIF (tracerIdentity.EQ.GAD_SALINITY) THEN
        CALL OBCS_APPLY_SLOC( bi, bj, k, localTij, myThid )
       END IF
      END IF
#endif /* ALLOW_OBCS */

C--   End of Y direction
      ENDIF

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        localTijk(i,j,k)=localTij(i,j)
       ENDDO
      ENDDO

C--   End of ipass loop
      ENDDO

C--   End of K loop for horizontal fluxes
      ENDDO

C--   Start of k loop for vertical flux
      DO k=Nr,1,-1
#ifdef ALLOW_AUTODIFF_TAMC 
         kkey = (ikey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

C--   kup    Cycles through 1,2 to point to w-layer above
C--   kDown  Cycles through 2,1 to point to w-layer below
      kup  = 1+MOD(k+1,2)
      kDown= 1+MOD(k,2)

C--   Get temporary terms used by tendency routines
      CALL CALC_COMMON_FACTORS (
     I         bi,bj,iMin,iMax,jMin,jMax,k,
     O         xA,yA,uTrans,vTrans,rTrans,maskUp,
     I         myThid)

C-    Advective flux in R
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        af(i,j) = 0.
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC 
CADJ STORE localTijk(:,:,k)  
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C     Note: wVel needs to be masked 
      IF (K.GE.2) THEN
C-    Compute vertical advective flux in the interior:
       IF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
        CALL GAD_FLUXLIMIT_ADV_R(
     &       bi,bj,k,deltaTtracer,rTrans,wVel,localTijk,af,myThid)
       ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
        CALL GAD_DST3_ADV_R(
     &       bi,bj,k,deltaTtracer,rTrans,wVel,localTijk,af,myThid)
       ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
        CALL GAD_DST3FL_ADV_R(
     &       bi,bj,k,deltaTtracer,rTrans,wVel,localTijk,af,myThid)
       ELSE
        STOP 'GAD_ADVECTION: adv. scheme incompatibale with mutli-dim'
       ENDIF
C-    Surface "correction" term at k>1 :
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         af(i,j) = af(i,j)
     &           + (maskC(i,j,k,bi,bj)-maskC(i,j,k-1,bi,bj))*
     &             rTrans(i,j)*localTijk(i,j,k)
        ENDDO
       ENDDO 
      ELSE
C-    Surface "correction" term at k=1 :
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         af(i,j) = rTrans(i,j)*localTijk(i,j,k)
        ENDDO
       ENDDO 
      ENDIF
C-    add the advective flux to fVerT
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fVerT(i,j,kUp) = af(i,j)
       ENDDO
      ENDDO

C--   Divergence of fluxes
      kp1=min(Nr,k+1)
      kp1Msk=1.
      if (k.EQ.Nr) kp1Msk=0.
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        localTij(i,j)=localTijk(i,j,k)-deltaTtracer*
     &    _recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rA(i,j,bi,bj)
     &    *( fVerT(i,j,kUp)-fVerT(i,j,kDown)
     &      -tracer(i,j,k,bi,bj)*rA(i,j,bi,bj)*
     &        (wVel(i,j,k,bi,bj)-kp1Msk*wVel(i,j,kp1,bi,bj))
     &     )*rkFac
        gTracer(i,j,k,bi,bj)=
     &   (localTij(i,j)-tracer(i,j,k,bi,bj))/deltaTtracer
       ENDDO
      ENDDO
 
C--   End of K loop for vertical flux
      ENDDO

      RETURN
      END
