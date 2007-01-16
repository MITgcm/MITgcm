C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_som_advect.F,v 1.1 2007/01/16 04:38:34 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: GAD_SOM_ADVECT

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_SOM_ADVECT(
     I     implicitAdvection, advectionScheme, vertAdvecScheme,
     I     tracerIdentity,
     I     uVel, vVel, wVel, tracer,
     U     smTr,
     O     gTracer,
     I     bi,bj, myTime,myIter,myThid)

C !DESCRIPTION:
C Calculates the tendency of a tracer due to advection.
C It uses the 2nd-Order moment advection scheme with multi-dimensional method
C  see Prather, 1986, JGR, v.91, D-6, pp.6671-6681.
C
C The tendency (output) is over-written by this routine.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C  implicitAdvection :: implicit vertical advection (later on)
C  advectionScheme   :: advection scheme to use (Horizontal plane)
C  vertAdvecScheme   :: advection scheme to use (vertical direction)
C  tracerIdentity    :: tracer identifier (required only for OBCS)
C  uVel              :: velocity, zonal component
C  vVel              :: velocity, meridional component
C  wVel              :: velocity, vertical component
C  tracer            :: tracer field
C  bi,bj             :: tile indices
C  myTime            :: current time
C  myIter            :: iteration number
C  myThid            :: thread number
      LOGICAL implicitAdvection
      INTEGER advectionScheme, vertAdvecScheme
      INTEGER tracerIdentity
      _RL uVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER bi,bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  smTr              :: tracer 1rst & 2nd Order moments
C  gTracer           :: tendency array
      _RL smTr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,nSOM)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#ifdef GAD_ALLOW_SOM_ADVECT
C !LOCAL VARIABLES: ====================================================
C  maskLocW      :: 2-D array for mask at West points
C  maskLocS      :: 2-D array for mask at South points
C  maskUp        :: 2-D array mask for W points
C  iMin,iMax,    :: loop range for called routines
C  jMin,jMax     :: loop range for called routines
C [iMin,iMax]Upd :: loop range to update tracer field
C [jMin,jMax]Upd :: loop range to update tracer field
C  i,j,k         :: loop indices
C  kUp           :: index into 2 1/2D array, toggles between 1 and 2
C  kDown         :: index into 2 1/2D array, toggles between 2 and 1
C  kp1           :: =k+1 for k<Nr, =Nr for k=Nr
C  xA,yA         :: areas of X and Y face of tracer cells
C  uFld,vFld     :: 2-D local copy of horizontal velocity, U,V components
C  wFld          :: 2-D local copy of vertical velocity
C  uTrans,vTrans :: 2-D arrays of volume transports at U,V points
C  rTrans        :: 2-D arrays of volume transports at W points
C  rTransKp1     :: vertical volume transport at interface k+1
C  af            :: 2-D array for horizontal advective flux
C  afx           :: 2-D array for horizontal advective flux, x direction
C  afy           :: 2-D array for horizontal advective flux, y direction
C  afr           :: 2-D array for vertical advective flux
C  fVerT         :: 2 1/2D arrays for vertical advective flux
C  localTij      :: 2-D array, temporary local copy of tracer fld
C  localTijk     :: 3-D array, temporary local copy of tracer fld
C  kp1Msk        :: flag (0,1) for over-riding mask for W levels
C  calc_fluxes_X :: logical to indicate to calculate fluxes in X dir
C  calc_fluxes_Y :: logical to indicate to calculate fluxes in Y dir
C  interiorOnly  :: only update the interior of myTile, but not the edges
C  overlapOnly   :: only update the edges of myTile, but not the interior
C  npass         :: number of passes in multi-dimensional method
C  ipass         :: number of the current pass being made
C  myTile        :: variables used to determine which cube face
C  nCFace        :: owns a tile for cube grid runs using
C                :: multi-dim advection.
C [N,S,E,W]_edge :: true if N,S,E,W edge of myTile is an Edge of the cube
C  msgBuf        :: Informational/error meesage buffer
c     _RS maskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RS maskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax,jMin,jMax
c     INTEGER iMinUpd,iMaxUpd,jMinUpd,jMaxUpd
      INTEGER i,j,k,km1,kUp,kDown
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL rTransKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL af      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afx     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afr     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
ccc   _RL localTij(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL localTijk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  smVol  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  smTr0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  alp    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  aln    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_v   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_v   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_o   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_o   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_x   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_x   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_y   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_y   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_z   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_z   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_xx  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_xx  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_yy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_yy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_zz  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_zz  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_xy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_xy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_xz  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_xz  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_yz  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_yz  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  localTr
c     _RL kp1Msk
      LOGICAL calc_fluxes_X, calc_fluxes_Y
c     LOGICAL interiorOnly, overlapOnly
      INTEGER limiter
      INTEGER npass, ipass
c     INTEGER nCFace
c     LOGICAL N_edge, S_edge, E_edge, W_edge
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 GAD_DIAG_SUFX, diagSufx
      EXTERNAL    GAD_DIAG_SUFX
#endif
CEOP

#ifdef ALLOW_DIAGNOSTICS
C--   Set diagnostic suffix for the current tracer
      IF ( useDiagnostics ) THEN
        diagSufx = GAD_DIAG_SUFX( tracerIdentity, myThid )
      ENDIF
#endif

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.
c     DO j=1-OLy,sNy+OLy
c      DO i=1-OLx,sNx+OLx
C-    xA,yA,uFld,vFld,uTrans,vTrans are set over the full domain
C     in CALC_COMMON_FACTORS: no need for extra initialisation
c       xA(i,j)      = 0. _d 0
c       yA(i,j)      = 0. _d 0
c       uTrans(i,j)  = 0. _d 0
c       vTrans(i,j)  = 0. _d 0
C-    rTrans is set over the full domain: no need for extra initialisation
c       rTrans(i,j)  = 0. _d 0
c       rTransKp1(i,j)= 0. _d 0
c      ENDDO
c     ENDDO

C--   Set tile-specific parameters for horizontal fluxes
      IF (useCubedSphereExchange) THEN
        WRITE(msgBuf,'(2A)') 'S/R GAD_SOM_ADVECT: ',
     &     'not coded for CubedSphere (useCubedSphereExchange=T)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_SOM_ADVECT'
      ENDIF
      IF ( implicitAdvection ) THEN
        WRITE(msgBuf,'(2A)') 'S/R GAD_SOM_ADVECT: ',
     &     'not coded for implicit-vertical Advection'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_SOM_ADVECT'
      ENDIF
      IF ( vertAdvecScheme .NE. advectionScheme ) THEN
        WRITE(msgBuf,'(2A)') 'S/R GAD_SOM_ADVECT: ',
     &     'not coded for different vertAdvecScheme'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_SOM_ADVECT'
      ENDIF

      limiter = MOD(advectionScheme, 10)
      npass= 2
      iMin = 1-OLx
      iMax = sNx+OLx
      jMin = 1-OLy
      jMax = sNy+OLy

C--   Start of k loop for horizontal fluxes
      DO k=1,Nr

C--   Get temporary terms used by tendency routines
      CALL CALC_COMMON_FACTORS (
     I         uVel, vVel,
     O         uFld, vFld, uTrans, vTrans, xA, yA,
     I         k,bi,bj, myThid )

#ifdef ALLOW_GMREDI
C--   Residual transp = Bolus transp + Eulerian transp
      IF (useGMRedi)
     &   CALL GMREDI_CALC_UVFLOW(
     U                  uFld, vFld, uTrans, vTrans,
     I                  k, bi, bj, myThid )
#endif /* ALLOW_GMREDI */

C--   grid-box volume and tracer content (zero order moment)
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         smVol(i,j,k) = rA(i,j,bi,bj)*deepFac2C(k)
     &                *drF(k)*hFacC(i,j,k,bi,bj)
     &                *rhoFacC(k)
         smTr0(i,j,k) = tracer(i,j,k,bi,bj)*smVol(i,j,k)
C-    fill empty grid-box:
         smVol(i,j,k) = smVol(i,j,k)
     &                + (1. _d 0 - maskC(i,j,k,bi,bj))
       ENDDO
      ENDDO

c     IF (useCubedSphereExchange) THEN
c       CALL FILL_CS_CORNER_UV_RS(
c    &            .FALSE., maskLocW,maskLocS, bi,bj, myThid )
c     ENDIF

C--   Multiple passes for different directions on different tiles
C--   For cube need one pass for each of red, green and blue axes.
      DO ipass=1,npass

C-    not CubedSphere
        calc_fluxes_X = MOD(ipass,2).EQ.1
        calc_fluxes_Y = .NOT.calc_fluxes_X

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   X direction
      IF (calc_fluxes_X) THEN

C-     Advective flux in X
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          afx(i,j) = 0.
         ENDDO
        ENDDO

        IF ( advectionScheme.EQ.ENUM_SOM_PRATHER
     &     .OR. advectionScheme.EQ.ENUM_SOM_LIMITER ) THEN
          CALL GAD_SOM_ADV_X(
     I                     bi,bj,k, limiter,
     I                     dTtracerLev(k), uTrans,
     U                     smVol(1-OLx,1-OLy,k),
     U                     smTr0(1-OLx,1-OLy,k),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,1),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,2),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,3),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,4),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,5),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,6),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,7),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,8),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,9),
     O                     afx, myThid )
        ELSE
         STOP 'GAD_SOM_ADVECT: adv. scheme incompatibale with SOM'
        ENDIF

C-     Update the local tracer field where needed:

C-      keep advective flux (for diagnostics)
c       DO j=1-OLy,sNy+OLy
c        DO i=1-OLx,sNx+OLx
c         afx(i,j) = af(i,j)
c        ENDDO
c       ENDDO

#ifdef ALLOW_OBCS
C-     Apply open boundary conditions
c       IF ( useOBCS ) THEN
ccc        localTij(i,j) = smTr0(i,j)/smVol(i,j)
c        IF (tracerIdentity.EQ.GAD_TEMPERATURE) THEN
c         CALL OBCS_APPLY_TLOC( bi, bj, k, localTij, myThid )
c        ELSEIF (tracerIdentity.EQ.GAD_SALINITY) THEN
c         CALL OBCS_APPLY_SLOC( bi, bj, k, localTij, myThid )
#ifdef ALLOW_PTRACERS
c        ELSEIF (tracerIdentity.GE.GAD_TR1) THEN
c         CALL OBCS_APPLY_PTRACER( bi, bj, k,
c    &         tracerIdentity-GAD_TR1+1, localTij, myThid )
#endif /* ALLOW_PTRACERS */
c        ENDIF
ccc        smTr0(i,j) = localTij(i,j)*smVol(i,j)
c       ENDIF
#endif /* ALLOW_OBCS */

C--   End of X direction
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Y direction
C
      IF (calc_fluxes_Y) THEN

C-     Do not compute fluxes if
C       a) needed in overlap only
C   and b) the overlap of myTile are not cube-face edges
c      IF ( .NOT.overlapOnly .OR. E_edge .OR. W_edge ) THEN

C-     Advective flux in Y
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          afy(i,j) = 0.
         ENDDO
        ENDDO

        IF ( advectionScheme.EQ.ENUM_SOM_PRATHER
     &     .OR. advectionScheme.EQ.ENUM_SOM_LIMITER ) THEN
          CALL GAD_SOM_ADV_Y(
     I                     bi,bj,k, limiter,
     I                     dTtracerLev(k), vTrans,
     U                     smVol(1-OLx,1-OLy,k),
     U                     smTr0(1-OLx,1-OLy,k),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,1),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,2),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,3),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,4),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,5),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,6),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,7),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,8),
     U                     smTr(1-OLx,1-OLy,k,bi,bj,9),
     O                     afy, myThid )
        ELSE
         STOP 'GAD_SOM_ADVECT: adv. scheme incompatibale with SOM'
        ENDIF

C-     Advective flux in Y : done
c      ENDIF

C-     Update the local tracer field where needed:

C-      keep advective flux (for diagnostics)
c       DO j=1-OLy,sNy+OLy
c        DO i=1-OLx,sNx+OLx
c         afy(i,j) = af(i,j)
c        ENDDO
c       ENDDO

#ifdef ALLOW_OBCS
C-     Apply open boundary conditions
c       IF (useOBCS) THEN
ccc        localTij(i,j) = smTr0(i,j)/smVol(i,j)
c        IF (tracerIdentity.EQ.GAD_TEMPERATURE) THEN
c         CALL OBCS_APPLY_TLOC( bi, bj, k, localTij, myThid )
c        ELSEIF (tracerIdentity.EQ.GAD_SALINITY) THEN
c         CALL OBCS_APPLY_SLOC( bi, bj, k, localTij, myThid )
#ifdef ALLOW_PTRACERS
c        ELSEIF (tracerIdentity.GE.GAD_TR1) THEN
c         CALL OBCS_APPLY_PTRACER( bi, bj, k,
c    &         tracerIdentity-GAD_TR1+1, localTij, myThid )
#endif /* ALLOW_PTRACERS */
c        ENDIF
ccc        smTr0(i,j) = localTij(i,j)*smVol(i,j)
c       ENDIF
#endif /* ALLOW_OBCS */

C      end if/else update overlap-Only
c      ENDIF

C--   End of Y direction
      ENDIF

C--   End of ipass loop
      ENDDO

      IF ( implicitAdvection ) THEN
C-    explicit advection is done ; store tendency in gTracer:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          localTr = smTr0(i,j,k)/smVol(i,j,k)
          gTracer(i,j,k,bi,bj) = ( localTr - tracer(i,j,k,bi,bj) )
     &                         / dTtracerLev(k)
         ENDDO
        ENDDO
      ELSE
C-    horizontal advection done; store intermediate result in 3D array:
c       DO j=1-OLy,sNy+OLy
c        DO i=1-OLx,sNx+OLx
c         localTijk(i,j,k)=localTij(i,j)
c        ENDDO
c       ENDDO
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          diagName = 'ADVx'//diagSufx
          CALL DIAGNOSTICS_FILL(afx,diagName, k,1, 2,bi,bj, myThid)
          diagName = 'ADVy'//diagSufx
          CALL DIAGNOSTICS_FILL(afy,diagName, k,1, 2,bi,bj, myThid)
        ENDIF
#endif

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB
     &   .AND. tracerIdentity.EQ.GAD_TEMPERATURE
     &   .AND. k.LE.3 .AND. myIter.EQ.1+nIter0
     &   .AND. nPx.EQ.1 .AND. nPy.EQ.1
     &   .AND. useCubedSphereExchange ) THEN
        CALL DEBUG_CS_CORNER_UV( ' afx,afy from GAD_SOM_ADVECT',
     &             afx,afy, k, standardMessageUnit,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DEBUG */

C--   End of K loop for horizontal fluxes
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.implicitAdvection ) THEN
C--   Apply limiter (if any):
       CALL GAD_SOM_LIM_R( bi,bj, limiter,
     U                     smVol,
     U                     smTr0,
     U                     smTr(1-OLx,1-OLy,1,bi,bj,1),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,2),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,3),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,4),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,5),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,6),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,7),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,8),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,9),
     I                     myThid )

C--   Start of k loop for vertical flux
       DO k=Nr,1,-1
C--   kUp    Cycles through 1,2 to point to w-layer above
C--   kDown  Cycles through 2,1 to point to w-layer below
        kUp  = 1+MOD(Nr-k,2)
        kDown= 1+MOD(Nr-k+1,2)
c       kp1=min(Nr,k+1)
c       kp1Msk = 1. _d 0
        IF (k.EQ.Nr) THEN
c        kp1Msk = 0. _d 0
C--   Set advective fluxes at the very bottom:
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           alp  (i,j,kDown) = 0. _d 0
           aln  (i,j,kDown) = 0. _d 0
           fp_v (i,j,kDown) = 0. _d 0
           fn_v (i,j,kDown) = 0. _d 0
           fp_o (i,j,kDown) = 0. _d 0
           fn_o (i,j,kDown) = 0. _d 0
           fp_x (i,j,kDown) = 0. _d 0
           fn_x (i,j,kDown) = 0. _d 0
           fp_y (i,j,kDown) = 0. _d 0
           fn_y (i,j,kDown) = 0. _d 0
           fp_z (i,j,kDown) = 0. _d 0
           fn_z (i,j,kDown) = 0. _d 0
           fp_xx(i,j,kDown) = 0. _d 0
           fn_xx(i,j,kDown) = 0. _d 0
           fp_yy(i,j,kDown) = 0. _d 0
           fn_yy(i,j,kDown) = 0. _d 0
           fp_zz(i,j,kDown) = 0. _d 0
           fn_zz(i,j,kDown) = 0. _d 0
           fp_xy(i,j,kDown) = 0. _d 0
           fn_xy(i,j,kDown) = 0. _d 0
           fp_xz(i,j,kDown) = 0. _d 0
           fn_xz(i,j,kDown) = 0. _d 0
           fp_yz(i,j,kDown) = 0. _d 0
           fn_yz(i,j,kDown) = 0. _d 0
          ENDDO
         ENDDO
        ENDIF

C-- Compute Vertical transport
#ifdef ALLOW_AIM
C- a hack to prevent Water-Vapor vert.transport into the stratospheric level Nr
c       IF ( k.EQ.1 .OR.
c    &     (useAIM .AND. tracerIdentity.EQ.GAD_SALINITY .AND. k.EQ.Nr)
c    &              ) THEN
#else
c       IF ( k.EQ.1 ) THEN
#endif
        IF ( (rigidLid.OR.nonlinFreeSurf.GE.1) .AND. k.EQ.1 ) THEN
C- Surface interface :
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           wFld(i,j)   = 0.
           rTrans(i,j) = 0.
           maskUp(i,j) = 0.
          ENDDO
         ENDDO

        ELSEIF ( rigidLid.OR.nonlinFreeSurf.GE.1 ) THEN
C- Interior interface :
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           wFld(i,j)   = wVel(i,j,k,bi,bj)
           rTrans(i,j) = wVel(i,j,k,bi,bj)*rA(i,j,bi,bj)
     &                 *deepFac2F(k)*rhoFacF(k)
     &                 *maskC(i,j,k-1,bi,bj)
           maskUp(i,j) = 1.
          ENDDO
         ENDDO

        ELSE
C- Linear Free-Surface: do not mask rTrans :
         km1= MAX(k-1,1)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           wFld(i,j)   = wVel(i,j,k,bi,bj)
           rTrans(i,j) = wVel(i,j,k,bi,bj)*rA(i,j,bi,bj)
     &                 *deepFac2F(k)*rhoFacF(k)
           maskUp(i,j) = maskC(i,j,km1,bi,bj)*maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO

C- end Surface/Interior if bloc
        ENDIF

#ifdef ALLOW_GMREDI
C--   Residual transp = Bolus transp + Eulerian transp
         IF (useGMRedi .AND. k.GT.1 )
     &     CALL GMREDI_CALC_WFLOW(
     U                 wFld, rTrans,
     I                 k, bi, bj, myThid )
#endif /* ALLOW_GMREDI */

C-    Compute vertical advective flux in the interior:
        IF ( vertAdvecScheme.EQ.ENUM_SOM_PRATHER
     &      .OR. vertAdvecScheme.EQ.ENUM_SOM_LIMITER ) THEN
          CALL GAD_SOM_ADV_R(
     I                     bi,bj,k, kUp, kDown,
     I                     dTtracerLev(k), rTrans, maskUp,
     U                     smVol,
     U                     smTr0,
     U                     smTr(1-OLx,1-OLy,1,bi,bj,1),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,2),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,3),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,4),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,5),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,6),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,7),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,8),
     U                     smTr(1-OLx,1-OLy,1,bi,bj,9),
     U                     alp,   aln,   fp_v,  fn_v,  fp_o,  fn_o,
     U                     fp_x,  fn_x,  fp_y,  fn_y,  fp_z,  fn_z,
     U                     fp_xx, fn_xx, fp_yy, fn_yy, fp_zz, fn_zz,
     U                     fp_xy, fn_xy, fp_xz, fn_xz, fp_yz, fn_yz,
     O                     afr, myThid )
        ELSE
          STOP 'GAD_SOM_ADVECT: adv.scheme incompatibale with SOM'
        ENDIF

C--   Divergence of vertical fluxes
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
c         localTij(i,j) = localTijk(i,j,k)
c    &      -dTtracerLev(k)*recip_rhoFacC(k)
c    &       *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
c    &       *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
c    &       *( fVerT(i,j,kDown)-fVerT(i,j,kUp)
c    &         -tracer(i,j,k,bi,bj)*(rTransKp1(i,j)-rTrans(i,j))
c    &        )*rkSign
c         gTracer(i,j,k,bi,bj)=
c    &     (localTij(i,j)-tracer(i,j,k,bi,bj))/dTtracerLev(k)
          localTr = smTr0(i,j,k)
     &            *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &            *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
     &            *recip_rhoFacC(k)
c         localTr = smTr0(i,j,k)/smVol(i,j,k)
          gTracer(i,j,k,bi,bj) = ( localTr - tracer(i,j,k,bi,bj) )
     &                         / dTtracerLev(k)
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          diagName = 'ADVr'//diagSufx
          CALL DIAGNOSTICS_FILL( afr,
     &                           diagName, k,1, 2,bi,bj, myThid)
        ENDIF
#endif

C--   End of K loop for vertical flux
       ENDDO
C--   end of if not.implicitAdvection block
      ENDIF

#endif /* GAD_ALLOW_SOM_ADVECT */

      RETURN
      END
