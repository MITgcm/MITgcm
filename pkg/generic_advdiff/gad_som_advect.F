#include "GAD_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: GAD_SOM_ADVECT

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_SOM_ADVECT(
     I     implicitAdvection, advectionScheme, vertAdvecScheme,
     I     tracerIdentity, deltaTLev,
     I     uFld, vFld, wFld, tracer,
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
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
# endif
#endif
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

C !INPUT PARAMETERS: ===================================================
C  implicitAdvection :: implicit vertical advection (later on)
C  advectionScheme   :: advection scheme to use (Horizontal plane)
C  vertAdvecScheme   :: advection scheme to use (vertical direction)
C  tracerIdentity    :: tracer identifier (required only for OBCS)
C  uFld              :: Advection velocity field, zonal component
C  vFld              :: Advection velocity field, meridional component
C  wFld              :: Advection velocity field, vertical component
C  tracer            :: tracer field
C  bi,bj             :: tile indices
C  myTime            :: current time
C  myIter            :: iteration number
C  myThid            :: thread number
      LOGICAL implicitAdvection
      INTEGER advectionScheme, vertAdvecScheme
      INTEGER tracerIdentity
      _RL deltaTLev(Nr)
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER bi,bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  smTr              :: tracer 1rst & 2nd Order moments
C  gTracer           :: tendency array
      _RL smTr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,nSOM)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C !LOCAL VARIABLES: ====================================================
C  maskUp        :: 2-D array mask for W points
C  i,j,k         :: loop indices
C  kUp           :: index into 2 1/2D array, toggles between 1 and 2
C  kDown         :: index into 2 1/2D array, toggles between 2 and 1
C  xA,yA         :: areas of X and Y face of tracer cells
C  uTrans,vTrans :: 2-D arrays of volume transports at U,V points
C  rTrans        :: 2-D arrays of volume transports at W points
C  afx           :: 2-D array for horizontal advective flux, x direction
C  afy           :: 2-D array for horizontal advective flux, y direction
C  afr           :: 2-D array for vertical advective flux
C  fVerT         :: 2 1/2D arrays for vertical advective flux
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
C  msgBuf        :: Informational/error message buffer
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i,j,k,km1,kUp,kDown
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afx     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afr     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
      _RL  smCorners(OLx,OLy,4,-1:nSOM)
c     _RL  localTr
      LOGICAL calc_fluxes_X, calc_fluxes_Y
      LOGICAL interiorOnly, overlapOnly
      INTEGER limiter
      INTEGER npass, ipass
      INTEGER nCFace, n
      LOGICAL N_edge, S_edge, E_edge, W_edge
      LOGICAL noFlowAcrossSurf
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER ipasskey
#endif
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      LOGICAL     doDiagAdvX, doDiagAdvY, doDiagAdvR
C-    Functions:
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif
CEOP

#ifdef ALLOW_DIAGNOSTICS
C--   Set diagnostics flags and suffix for the current tracer
      doDiagAdvX = .FALSE.
      doDiagAdvY = .FALSE.
      doDiagAdvR = .FALSE.
      IF ( useDiagnostics ) THEN
        diagSufx = GAD_DIAG_SUFX( tracerIdentity, myThid )
        diagName = 'ADVx'//diagSufx
        doDiagAdvX = DIAGNOSTICS_IS_ON( diagName, myThid )
        diagName = 'ADVy'//diagSufx
        doDiagAdvY = DIAGNOSTICS_IS_ON( diagName, myThid )
        diagName = 'ADVr'//diagSufx
        doDiagAdvR = DIAGNOSTICS_IS_ON( diagName, myThid )
      ENDIF
#endif

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         afx(i,j) = 0.
         afy(i,j) = 0.
C-    xA,yA,uTrans,vTrans are set over the full domain
C      => no need for extra initialisation
c       xA(i,j)      = 0. _d 0
c       yA(i,j)      = 0. _d 0
c       uTrans(i,j)  = 0. _d 0
c       vTrans(i,j)  = 0. _d 0
C-    rTrans is set over the full domain: no need for extra initialisation
c       rTrans(i,j)  = 0. _d 0
       ENDDO
      ENDDO
      DO n=-1,nSOM
       DO k=1,4
        DO j=1,OLy
         DO i=1,OLx
          smCorners(i,j,k,n) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

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

C--   Set tile-specific parameters for horizontal fluxes
      IF (useCubedSphereExchange) THEN
       npass  = 3
#ifdef ALLOW_EXCH2
       myTile = W2_myTileList(bi,bj)
       nCFace = exch2_myFace(myTile)
       N_edge = exch2_isNedge(myTile).EQ.1
       S_edge = exch2_isSedge(myTile).EQ.1
       E_edge = exch2_isEedge(myTile).EQ.1
       W_edge = exch2_isWedge(myTile).EQ.1
#else
       nCFace = bi
       N_edge = .TRUE.
       S_edge = .TRUE.
       E_edge = .TRUE.
       W_edge = .TRUE.
#endif
      ELSE
       npass  = 2
       nCFace = 0
       N_edge = .FALSE.
       S_edge = .FALSE.
       E_edge = .FALSE.
       W_edge = .FALSE.
      ENDIF

      limiter = MOD(advectionScheme, 10)

#ifdef ALLOW_AUTODIFF_TAMC
      IF ( npass.GT.maxcube ) THEN
        WRITE(msgBuf,'(A,2(I3,A))') 'S/R GAD_SOM_ADVECT: npass =',
     &     npass, ' >', maxcube, ' = maxcube, ==> check "tamc.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_SOM_ADVECT'
      ENDIF
CADJ INIT somtape = COMMON, 1
CADJ INIT somtape_k = COMMON, Nr
CADJ INIT somtape_k_pass = COMMON, maxcube*Nr
#endif
C--   Start of k loop for horizontal fluxes
      DO k=1,Nr

C--   Get temporary terms used by tendency routines
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          xA(i,j) = _dyG(i,j,bi,bj)*deepFacC(k)
     &            *drF(k)*_hFacW(i,j,k,bi,bj)
          yA(i,j) = _dxG(i,j,bi,bj)*deepFacC(k)
     &            *drF(k)*_hFacS(i,j,k,bi,bj)
        ENDDO
       ENDDO
C--   Calculate "volume transports" through tracer cell faces.
C     anelastic: scaled by rhoFacC (~ mass transport)
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          uTrans(i,j) = uFld(i,j,k)*xA(i,j)*rhoFacC(k)
          vTrans(i,j) = vFld(i,j,k)*yA(i,j)*rhoFacC(k)
        ENDDO
       ENDDO

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

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE smtr(:,:,k,bi,bj,:) = somtape_k, key=k, kind=isbyte
CADJ STORE smtr0(:,:,k)        = somtape_k, key=k, kind=isbyte
CADJ STORE smVol(:,:,k)        = somtape_k, key=k, kind=isbyte
CADJ STORE smcorners           = somtape_k, key=k, kind=isbyte
#endif
C--   Multiple passes for different directions on different tiles
C--   For cube need one pass for each of red, green and blue axes.
       DO ipass=1,npass
#ifdef ALLOW_AUTODIFF_TAMC
        ipasskey = ipass + (k-1) * maxcube
#endif /* ALLOW_AUTODIFF_TAMC */

        interiorOnly = .FALSE.
        overlapOnly  = .FALSE.
        IF (useCubedSphereExchange) THEN
C-    CubedSphere : pass 3 times, with partial update of local tracer field
         IF (ipass.EQ.1) THEN
          overlapOnly   = MOD(nCFace,3).EQ.0
          interiorOnly  = MOD(nCFace,3).NE.0
          calc_fluxes_X = nCFace.EQ.6 .OR. nCFace.EQ.1 .OR. nCFace.EQ.2
          calc_fluxes_Y = nCFace.EQ.3 .OR. nCFace.EQ.4 .OR. nCFace.EQ.5
         ELSEIF (ipass.EQ.2) THEN
          overlapOnly   = MOD(nCFace,3).EQ.2
          interiorOnly  = MOD(nCFace,3).EQ.1
          calc_fluxes_X = nCFace.EQ.2 .OR. nCFace.EQ.3 .OR. nCFace.EQ.4
          calc_fluxes_Y = nCFace.EQ.5 .OR. nCFace.EQ.6 .OR. nCFace.EQ.1
         ELSE
          interiorOnly  = .TRUE.
          calc_fluxes_X = nCFace.EQ.5 .OR. nCFace.EQ.6
          calc_fluxes_Y = nCFace.EQ.2 .OR. nCFace.EQ.3
         ENDIF
        ELSE
C-    not CubedSphere
          calc_fluxes_X = MOD(ipass,2).EQ.1
          calc_fluxes_Y = .NOT.calc_fluxes_X
        ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE smtr(:,:,k,bi,bj,:) =somtape_k_pass,key=ipasskey,kind=isbyte
CADJ STORE smtr0(:,:,k)        =somtape_k_pass,key=ipasskey,kind=isbyte
CADJ STORE smVol(:,:,k)        =somtape_k_pass,key=ipasskey,kind=isbyte
CADJ STORE smcorners           =somtape_k_pass,key=ipasskey,kind=isbyte
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   X direction
C-     Do not compute fluxes if
C       a) needed in overlap only
C   and b) the overlap of myTile are not cube-face Edges
        IF ( calc_fluxes_X .AND.
     &      (.NOT.overlapOnly .OR. N_edge .OR. S_edge)
     &     ) THEN

C-     Internal exchange for calculations in X
          IF ( useCubedSphereExchange .AND. .NOT.interiorOnly ) THEN
           CALL GAD_SOM_PREP_CS_CORNER(
     U                     smVol, smTr0, smTr, smCorners,
     I                     .TRUE., overlapOnly, interiorOnly,
     I                     N_edge, S_edge, E_edge, W_edge,
     I                     ipass, k, Nr, bi, bj, myThid )
          ENDIF

C-     Solve advection in X and update moments
          IF ( advectionScheme.EQ.ENUM_SOM_PRATHER
     &       .OR. advectionScheme.EQ.ENUM_SOM_LIMITER ) THEN
           CALL GAD_SOM_ADV_X(
     I                     bi,bj,k, limiter,
     I                     overlapOnly, interiorOnly,
     I                     N_edge, S_edge, E_edge, W_edge,
     I                     deltaTLev(k), uTrans,
     I                     maskInC(1-OLx,1-OLy,bi,bj),
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

C--   End of X direction
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Y direction
C-     Do not compute fluxes if
C       a) needed in overlap only
C   and b) the overlap of myTile are not cube-face edges
        IF ( calc_fluxes_Y .AND.
     &      (.NOT.overlapOnly .OR. E_edge .OR. W_edge)
     &     ) THEN

C-     Internal exchange for calculations in Y
          IF ( useCubedSphereExchange .AND. .NOT.interiorOnly ) THEN
           CALL GAD_SOM_PREP_CS_CORNER(
     U                     smVol, smTr0, smTr, smCorners,
     I                     .FALSE., overlapOnly, interiorOnly,
     I                     N_edge, S_edge, E_edge, W_edge,
     I                     iPass, k, Nr, bi, bj, myThid )
          ENDIF

C-     Solve advection in Y and update moments
          IF ( advectionScheme.EQ.ENUM_SOM_PRATHER
     &       .OR. advectionScheme.EQ.ENUM_SOM_LIMITER ) THEN
           CALL GAD_SOM_ADV_Y(
     I                     bi,bj,k, limiter,
     I                     overlapOnly, interiorOnly,
     I                     N_edge, S_edge, E_edge, W_edge,
     I                     deltaTLev(k), vTrans,
     I                     maskInC(1-OLx,1-OLy,bi,bj),
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

C--   End of Y direction
        ENDIF
C--   End of ipass loop
       ENDDO

       IF ( implicitAdvection ) THEN
C-    explicit advection is done ; store tendency in gTracer:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C--  without rescaling of tendencies:
c         localTr = smTr0(i,j,k)/smVol(i,j,k)
c         gTracer(i,j,k) = ( localTr - tracer(i,j,k,bi,bj) )
c    &                   / deltaTLev(k)
C--  consistent with rescaling of tendencies (in FREESURF_RESCALE_G):
          gTracer(i,j,k) =
     &          ( smTr0(i,j,k) - tracer(i,j,k,bi,bj)*smVol(i,j,k) )
     &            *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &            *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
     &            *recip_rhoFacC(k)
     &            /deltaTLev(k)
         ENDDO
        ENDDO
       ENDIF

#ifdef ALLOW_DIAGNOSTICS
       IF ( doDiagAdvX ) THEN
         diagName = 'ADVx'//diagSufx
         CALL DIAGNOSTICS_FILL(afx,diagName, k,1, 2,bi,bj, myThid )
       ENDIF
       IF ( doDiagAdvY ) THEN
         diagName = 'ADVy'//diagSufx
         CALL DIAGNOSTICS_FILL(afy,diagName, k,1, 2,bi,bj, myThid )
       ENDIF
#ifdef ALLOW_LAYERS
       IF ( useLayers ) THEN
         CALL LAYERS_FILL(afx,tracerIdentity,'AFX',k,1,2,bi,bj,myThid)
         CALL LAYERS_FILL(afy,tracerIdentity,'AFY',k,1,2,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_LAYERS */
#endif

#ifdef ALLOW_DEBUG
       IF ( debugLevel .GE. debLevC
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

      noFlowAcrossSurf = rigidLid .OR. nonlinFreeSurf.GE.1
     &                            .OR. select_rStar.NE.0

      IF ( .NOT.implicitAdvection ) THEN
C--   Apply limiter (if any):
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE smtr(:,:,:,bi,bj,:) = somtape, key=1, kind=isbyte
CADJ STORE smtr0               = somtape, key=1, kind=isbyte
#endif
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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE smtr(:,:,k,bi,bj,:) = somtape_k, key=k, kind=isbyte
CADJ STORE smtr0(:,:,k)        = somtape_k, key=k, kind=isbyte
CADJ STORE smVol(:,:,k)        = somtape_k, key=k, kind=isbyte
CADJ STORE alp, aln   = somtape_k, key=k, kind=isbyte
CADJ STORE fp_v,fn_v,fp_o,fn_o,fp_x,fn_x,fp_y,fn_y,fp_z,fn_z
CADJ &                         = somtape_k, key=k, kind=isbyte
CADJ STORE fp_xx, fn_xx, fp_yy, fn_yy, fp_zz, fn_zz
CADJ &                         = somtape_k, key=k, kind=isbyte
CADJ STORE fp_xy, fn_xy, fp_xz, fn_xz, fp_yz, fn_yz
CADJ &                         = somtape_k, key=k, kind=isbyte
#endif
C--   kUp    Cycles through 1,2 to point to w-layer above
C--   kDown  Cycles through 2,1 to point to w-layer below
        kUp  = 1+MOD(Nr-k,2)
        kDown= 1+MOD(Nr-k+1,2)
        IF (k.EQ.Nr) THEN
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
        IF ( noFlowAcrossSurf .AND. k.EQ.1 ) THEN
C- Surface interface :
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           rTrans(i,j) = 0.
           maskUp(i,j) = 0.
          ENDDO
         ENDDO

        ELSEIF ( noFlowAcrossSurf ) THEN
C- Interior interface :
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           rTrans(i,j) = wFld(i,j,k)*rA(i,j,bi,bj)
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
           rTrans(i,j) = wFld(i,j,k)*rA(i,j,bi,bj)
     &                 *deepFac2F(k)*rhoFacF(k)
           maskUp(i,j) = maskC(i,j,km1,bi,bj)*maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO

C- end Surface/Interior if bloc
        ENDIF

C-    Compute vertical advective flux in the interior:
        IF ( vertAdvecScheme.EQ.ENUM_SOM_PRATHER
     &     .OR. vertAdvecScheme.EQ.ENUM_SOM_LIMITER ) THEN
           CALL GAD_SOM_ADV_R(
     I                     bi,bj,k, kUp, kDown,
     I                     deltaTLev(k), rTrans, maskUp,
     I                     maskInC(1-OLx,1-OLy,bi,bj),
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
           STOP 'GAD_SOM_ADVECT: adv. scheme incompatibale with SOM'
        ENDIF

C--   Compute new tracer value and store tracer tendency
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C--  without rescaling of tendencies:
c         localTr = smTr0(i,j,k)/smVol(i,j,k)
c         gTracer(i,j,k) = ( localTr - tracer(i,j,k,bi,bj) )
c    &                   / deltaTLev(k)
C--  Non-Lin Free-Surf: consistent with rescaling of tendencies
C     (in FREESURF_RESCALE_G) and RealFreshFlux/addMass.
C    Also valid for linear Free-Surf (r & r* coords) except that surf tracer
C    loss/gain is computed (in GAD_SOM_ADV_R) from partially updated tracer
C     (instead of from Tr^n as fresh-water dilution effect) resulting in
C    inaccurate linFSConserveTr and "surfExpan_" monitor.
          gTracer(i,j,k) =
     &          ( smTr0(i,j,k) - tracer(i,j,k,bi,bj)*smVol(i,j,k) )
     &            *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &            *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
     &            *recip_rhoFacC(k)
     &            /deltaTLev(k)
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( doDiagAdvR ) THEN
         diagName = 'ADVr'//diagSufx
         CALL DIAGNOSTICS_FILL( afr,
     &                          diagName, k,1, 2,bi,bj, myThid )
        ENDIF
#ifdef ALLOW_LAYERS
        IF ( useLayers ) THEN
          CALL LAYERS_FILL(afr,tracerIdentity,'AFR',
     &                     k,1,2,bi,bj,myThid)
        ENDIF
#endif /* ALLOW_LAYERS */
#endif

C--   End of k loop for vertical flux
       ENDDO
C--   end of if not.implicitAdvection block
      ENDIF

      RETURN
      END
