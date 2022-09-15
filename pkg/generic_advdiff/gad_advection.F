#include "GAD_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: GAD_ADVECTION

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_ADVECTION(
     I     implicitAdvection, advectionSchArg, vertAdvecSchArg,
     I     trIdentity, deltaTLev,
     I     uFld, vFld, wFld, tracer,
     O     gTracer,
     I     bi,bj, myTime,myIter,myThid)

C !DESCRIPTION:
C Calculates the tendency of a tracer due to advection.
C It uses the multi-dimensional method given in \ref{sect:multiDimAdvection}
C and can only be used for the non-linear advection schemes such as the
C direct-space-time method and flux-limiters.
C
C The algorithm is as follows:
C \begin{itemize}
C \item{$\theta^{(n+1/3)} = \theta^{(n)}
C      - \Delta t (\partial_x (u\theta^{(n)}) - \theta^{(n)} \partial_x u$)}
C \item{$\theta^{(n+2/3)} = \theta^{(n+1/3)}
C      - \Delta t (\partial_y (v\theta^{(n+1/3)}) - \theta^{(n)} \partial_y v$)}
C \item{$\theta^{(n+3/3)} = \theta^{(n+2/3)}
C      - \Delta t (\partial_r (w\theta^{(n+2/3)}) - \theta^{(n)} \partial_r w$)}
C \item{$G_\theta = ( \theta^{(n+3/3)} - \theta^{(n)} )/\Delta t$}
C \end{itemize}
C
C The tendency (output) is over-written by this routine.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_PARAMS.h"
# ifdef ALLOW_AUTODIFF_TAMC
#  include "tamc.h"
# endif
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
C  advectionSchArg   :: advection scheme to use (Horizontal plane)
C  vertAdvecSchArg   :: advection scheme to use (vertical direction)
C  trIdentity        :: tracer identifier
C  uFld              :: Advection velocity field, zonal component
C  vFld              :: Advection velocity field, meridional component
C  wFld              :: Advection velocity field, vertical component
C  tracer            :: tracer field
C  bi,bj             :: tile indices
C  myTime            :: current time
C  myIter            :: iteration number
C  myThid            :: thread number
      LOGICAL implicitAdvection
      INTEGER advectionSchArg, vertAdvecSchArg
      INTEGER trIdentity
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
C  gTracer           :: tendency array
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef DISABLE_MULTIDIM_ADVECTION
      STOP 'GAD_ADVECTION is empty with DISABLE_MULTIDIM_ADVECTION'
#else /*  DISABLE_MULTIDIM_ADVECTION */
C !FUNCTIONS: ==========================================================
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif

C !LOCAL VARIABLES: ====================================================
C  maskUp        :: 2-D array for mask at W points
C  maskLocW      :: 2-D array for mask at West points
C  maskLocS      :: 2-D array for mask at South points
C [iMin,iMax]Upd :: loop range to update tracer field
C [jMin,jMax]Upd :: loop range to update tracer field
C  i,j,k         :: loop indices
C  kUp           :: index into 2 1/2D array, toggles between 1 and 2
C  kDown         :: index into 2 1/2D array, toggles between 2 and 1
C advectionScheme:: local copy of routine argument advectionSchArg
C vertAdvecScheme:: local copy of routine argument vertAdvecSchArg
C  xA,yA         :: areas of X and Y face of tracer cells
C  uTrans,vTrans :: 2-D arrays of volume transports at U,V points
C  rTrans        :: 2-D arrays of volume transports at W points
C  rTransKp      :: vertical volume transport at interface k+1
C  rTran3d       :: 3-D array of volume transport at W points
C  afr           :: 3-D array of vertical advective flux
C  af            :: 2-D array for horizontal advective flux
C  afx           :: 2-D array for horizontal advective flux, x direction
C  afy           :: 2-D array for horizontal advective flux, y direction
C  fVerT         :: 2 1/2D arrays for vertical advective flux
C  localTij      :: 2-D array, temporary local copy of tracer field
C  localT3d      :: 3-D array, temporary local copy of tracer field
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
c     _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMinUpd,iMaxUpd,jMinUpd,jMaxUpd
      INTEGER i,j,k,kUp,kDown
      INTEGER advectionScheme, vertAdvecScheme
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifndef ALLOW_AUTODIFF
       LOGICAL usePPMvertAdv, usePQMvertAdv
      _RL rTran3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL afr     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
      _RL af      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afx     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL localTij(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localT3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef GAD_MULTIDIM_COMPRESSIBLE
      _RL tmpTrac
      _RL localVol(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL locVol3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
      _RL kp1Msk
      LOGICAL calc_fluxes_X, calc_fluxes_Y, withSigns
      LOGICAL interiorOnly, overlapOnly
      INTEGER npass, ipass
      INTEGER nCFace
      LOGICAL N_edge, S_edge, E_edge, W_edge
#ifdef ALLOW_AUTODIFF_TAMC
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ipasskey
# ifdef GAD_MULTIDIM_COMPRESSIBLE
      INTEGER ijkey
# endif
#endif
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      LOGICAL     doDiagAdvX, doDiagAdvY, doDiagAdvR
#endif /* ALLOW_DIAGNOSTICS */
CEOP

C     make local copies to be tampered with if necessary
      advectionScheme = advectionSchArg
      vertAdvecScheme = vertAdvecSchArg
#ifdef ALLOW_AUTODIFF_TAMC
      IF ( inAdMode .AND. useApproxAdvectionInAdMode ) THEN
C     In AD-mode, we change non-linear, potentially unstable AD advection
C     schemes to linear schemes with more stability. So far only DST3 with
C     flux limiting is replaced by DST3 without flux limiting, but any
C     combination is possible.
       IF ( advectionSchArg.EQ.ENUM_DST3_FLUX_LIMIT )
     &      advectionScheme = ENUM_DST3
       IF ( vertAdvecSchArg.EQ.ENUM_DST3_FLUX_LIMIT )
     &      vertAdvecScheme = ENUM_DST3
C     here is room for more advection schemes as this becomes necessary
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_DIAGNOSTICS
C--   Set diagnostics flags and suffix for the current tracer
      doDiagAdvX = .FALSE.
      doDiagAdvY = .FALSE.
      doDiagAdvR = .FALSE.
      IF ( useDiagnostics ) THEN
       diagSufx = GAD_DIAG_SUFX( trIdentity, myThid )
       diagName = 'ADVx'//diagSufx
       doDiagAdvX = DIAGNOSTICS_IS_ON( diagName, myThid )
       diagName = 'ADVy'//diagSufx
       doDiagAdvY = DIAGNOSTICS_IS_ON( diagName, myThid )
       diagName = 'ADVr'//diagSufx
       doDiagAdvR = DIAGNOSTICS_IS_ON( diagName, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
C-    xA,yA,vFld,uTrans,vTrans are set over the full domain
C      => no need for extra initialisation
c       xA(i,j)      = 0. _d 0
c       yA(i,j)      = 0. _d 0
c       uTrans(i,j)  = 0. _d 0
c       vTrans(i,j)  = 0. _d 0
C-    rTransKp is set over the full domain: no need for extra initialisation
c       rTransKp(i,j)= 0. _d 0
C-    rTrans and fVerT need to be initialised to zero:
        rTrans(i,j)  = 0. _d 0
        fVerT(i,j,1) = 0. _d 0
        fVerT(i,j,2) = 0. _d 0
#ifdef ALLOW_AUTODIFF
# ifdef GAD_MULTIDIM_COMPRESSIBLE
        localVol(i,j) = 0. _d 0
# endif
        localTij(i,j) = 0. _d 0
#endif /* ALLOW_AUTODIFF */
       ENDDO
      ENDDO

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

#ifdef ALLOW_AUTODIFF_TAMC
      IF ( npass.GT.maxcube ) THEN
        WRITE(msgBuf,'(A,2(I3,A))') 'S/R GAD_ADVECTION: npass =',
     &     npass, ' >', maxcube, ' = maxcube, ==> check "tamc.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GAD_ADVECTION'
      ENDIF
C     Define local tapes. Using local tapes makes it necessary to recompute
C     the forward routine (once) in AD-mode, but saves a lot of memory.
CADJ INIT loctape_gad_adv = COMMON, 1
CADJ INIT loctape_gad_adv_k = COMMON, Nr
CADJ INIT loctape_gad_adv_k_pass = COMMON, maxcube*Nr
# ifdef GAD_MULTIDIM_COMPRESSIBLE
CADJ INIT loctape_gad_adv_ijk_pass
CADJ &  = COMMON, maxcube*Nr*(sNx+2*OLx)*nSx*(sNy+2*OLy)*nSy
# endif
#endif
C--   Start of k loop for horizontal fluxes
      DO k=1,Nr

C--   Get temporary terms used by tendency routines
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         xA(i,j) = _dyG(i,j,bi,bj)*deepFacC(k)
     &           *drF(k)*_hFacW(i,j,k,bi,bj)
         yA(i,j) = _dxG(i,j,bi,bj)*deepFacC(k)
     &           *drF(k)*_hFacS(i,j,k,bi,bj)
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

C--   Make local copy of tracer array and mask West & South
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         localTij(i,j) = tracer(i,j,k,bi,bj)
#ifdef GAD_MULTIDIM_COMPRESSIBLE
         localVol(i,j) = rA(i,j,bi,bj)*deepFac2C(k)
     &                  *rhoFacC(k)*drF(k)*hFacC(i,j,k,bi,bj)
     &                 + ( oneRS - maskC(i,j,k,bi,bj) )
#endif
#ifdef ALLOW_OBCS
         maskLocW(i,j) = _maskW(i,j,k,bi,bj)*maskInW(i,j,bi,bj)
         maskLocS(i,j) = _maskS(i,j,k,bi,bj)*maskInS(i,j,bi,bj)
#else /* ALLOW_OBCS */
         maskLocW(i,j) = _maskW(i,j,k,bi,bj)
         maskLocS(i,j) = _maskS(i,j,k,bi,bj)
#endif /* ALLOW_OBCS */
        ENDDO
       ENDDO

       IF (useCubedSphereExchange) THEN
        withSigns = .FALSE.
        CALL FILL_CS_CORNER_UV_RS(
     &            withSigns, maskLocW,maskLocS, bi,bj, myThid )
       ENDIF

C--   Multiple passes for different directions on different tiles
C--   For cube need one pass for each of red, green and blue axes.
       DO ipass=1,npass
#ifdef ALLOW_AUTODIFF_TAMC
        ipasskey = ipass + (k-1) * maxcube
#endif

        interiorOnly = .FALSE.
        overlapOnly  = .FALSE.
        IF (useCubedSphereExchange) THEN
C-    CubedSphere : pass 3 times, with partial update of local tracer field
         IF (ipass.EQ.1) THEN
          overlapOnly  = MOD(nCFace,3).EQ.0
          interiorOnly = MOD(nCFace,3).NE.0
          calc_fluxes_X = nCFace.EQ.6 .OR. nCFace.EQ.1 .OR. nCFace.EQ.2
          calc_fluxes_Y = nCFace.EQ.3 .OR. nCFace.EQ.4 .OR. nCFace.EQ.5
         ELSEIF (ipass.EQ.2) THEN
          overlapOnly  = MOD(nCFace,3).EQ.2
          interiorOnly = MOD(nCFace,3).EQ.1
          calc_fluxes_X = nCFace.EQ.2 .OR. nCFace.EQ.3 .OR. nCFace.EQ.4
          calc_fluxes_Y = nCFace.EQ.5 .OR. nCFace.EQ.6 .OR. nCFace.EQ.1
         ELSE
          interiorOnly = .TRUE.
          calc_fluxes_X = nCFace.EQ.5 .OR. nCFace.EQ.6
          calc_fluxes_Y = nCFace.EQ.2 .OR. nCFace.EQ.3
         ENDIF
        ELSE
C-    not CubedSphere
         calc_fluxes_X = MOD(ipass,2).EQ.1
         calc_fluxes_Y = .NOT.calc_fluxes_X
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   X direction

#ifdef ALLOW_AUTODIFF
C-     Always reset advective flux in X
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          af(i,j) = 0.
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF */

        IF (calc_fluxes_X) THEN

C-     Do not compute fluxes if
C       a) needed in overlap only
C   and b) the overlap of myTile are not cube-face Edges
         IF ( .NOT.overlapOnly .OR. N_edge .OR. S_edge ) THEN

C-     Internal exchange for calculations in X
          IF ( overlapOnly ) THEN
           CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                                localTij, bi,bj, myThid )
          ENDIF

C-     Advective flux in X
#ifndef ALLOW_AUTODIFF
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            af(i,j) = 0.
           ENDDO
          ENDDO
#else /* ALLOW_AUTODIFF */
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE localTij = loctape_gad_adv_k_pass, key = ipasskey
CADJ &     , kind = isbyte
# endif
#endif /* ALLOW_AUTODIFF */

          IF ( advectionScheme.EQ.ENUM_UPWIND_1RST
     &         .OR. advectionScheme.EQ.ENUM_DST2 ) THEN
           CALL GAD_DST2U1_ADV_X(    bi,bj,k, advectionScheme, .TRUE.,
     I              deltaTLev(k),uTrans,uFld(1-OLx,1-OLy,k), localTij,
     O              af, myThid )
          ELSEIF ( advectionScheme.EQ.ENUM_FLUX_LIMIT ) THEN
           CALL GAD_FLUXLIMIT_ADV_X( bi,bj,k, .TRUE., deltaTLev(k),
     I              uTrans, uFld(1-OLx,1-OLy,k), maskLocW, localTij,
     O              af, myThid )
          ELSEIF ( advectionScheme.EQ.ENUM_DST3 ) THEN
           CALL GAD_DST3_ADV_X(      bi,bj,k, .TRUE., deltaTLev(k),
     I              uTrans, uFld(1-OLx,1-OLy,k), maskLocW, localTij,
     O              af, myThid )
          ELSEIF ( advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
           CALL GAD_DST3FL_ADV_X(    bi,bj,k, .TRUE., deltaTLev(k),
     I              uTrans, uFld(1-OLx,1-OLy,k), maskLocW, localTij,
     O              af, myThid )
#ifndef ALLOW_AUTODIFF
          ELSEIF ( advectionScheme.EQ.ENUM_OS7MP ) THEN
           CALL GAD_OS7MP_ADV_X(     bi,bj,k, .TRUE., deltaTLev(k),
     I              uTrans, uFld(1-OLx,1-OLy,k), maskLocW, localTij,
     O              af, myThid )
          ELSEIF ( advectionScheme.EQ.ENUM_PPM_NULL_LIMIT  .OR.
     &             advectionScheme.EQ.ENUM_PPM_MONO_LIMIT  .OR.
     &             advectionScheme.EQ.ENUM_PPM_WENO_LIMIT ) THEN
           CALL GAD_PPM_ADV_X( advectionScheme, bi, bj, k , .TRUE.,
     I              deltaTLev(k), uFld(1-OLx,1-OLy,k), uTrans, localTij,
     O              af, myThid )
          ELSEIF ( advectionScheme.EQ.ENUM_PQM_NULL_LIMIT  .OR.
     &             advectionScheme.EQ.ENUM_PQM_MONO_LIMIT  .OR.
     &             advectionScheme.EQ.ENUM_PQM_WENO_LIMIT ) THEN
           CALL GAD_PQM_ADV_X( advectionScheme, bi, bj, k , .TRUE.,
     I              deltaTLev(k), uFld(1-OLx,1-OLy,k), uTrans, localTij,
     O              af, myThid )
#endif /* ndef ALLOW_AUTODIFF */
          ELSE
           STOP 'GAD_ADVECTION: adv. scheme incompatible with multi-dim'
          ENDIF

#ifdef ALLOW_OBCS
          IF ( useOBCS ) THEN
C-      replace advective flux with 1st order upwind scheme estimate
           CALL OBCS_U1_ADV_TRACER( .TRUE., trIdentity, bi, bj, k,
     I                              maskLocW, uTrans, localTij,
     U                              af, myThid )
          ENDIF
#endif /* ALLOW_OBCS */

C-     Internal exchange for next calculations in Y
          IF ( overlapOnly .AND. ipass.EQ.1 ) THEN
           CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                                localTij, bi,bj, myThid )
          ENDIF

C-     Advective flux in X : done
         ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE af = loctape_gad_adv_k_pass, key = ipasskey, kind = isbyte
#endif
C-     Update the local tracer field where needed:
C      use "maksInC" to prevent updating tracer field in OB regions

C      update in overlap-Only
         IF ( overlapOnly ) THEN
          iMinUpd = 1-OLx+1
          iMaxUpd = sNx+OLx-1
C- notes: these 2 lines below have no real effect (because recip_hFac=0
C         in corner region) but safer to keep them.
          IF ( W_edge ) iMinUpd = 1
          IF ( E_edge ) iMaxUpd = sNx

          IF ( S_edge ) THEN
#if defined ALLOW_AUTODIFF_TAMC && defined GAD_MULTIDIM_COMPRESSIBLE
CADJ STORE localTij = loctape_gad_adv_k_pass, key = ipasskey
CADJ &     , kind = isbyte
#endif
           DO j=1-OLy,0
            DO i=iMinUpd,iMaxUpd
#ifdef GAD_MULTIDIM_COMPRESSIBLE
# ifdef ALLOW_AUTODIFF_TAMC
             ijkey = i + ((j-1) + (ipasskey-1)*(sNy+2*OLy))*(sNx+2*OLx)
CADJ STORE localVol(i,j) = loctape_gad_adv_ijk_pass, key=ijkey
CADJ &     , kind = isbyte
# endif
             tmpTrac = localTij(i,j)*localVol(i,j)
     &            -deltaTLev(k)*( af(i+1,j) - af(i,j) )
     &                         *maskInC(i,j,bi,bj)
             localVol(i,j) = localVol(i,j)
     &            -deltaTLev(k)*( uTrans(i+1,j) - uTrans(i,j) )
     &                         *maskInC(i,j,bi,bj)
             localTij(i,j) = tmpTrac/localVol(i,j)
#else /* GAD_MULTIDIM_COMPRESSIBLE */
             localTij(i,j) = localTij(i,j)
     &           -deltaTLev(k)*recip_rhoFacC(k)
     &            *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &            *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &            *( af(i+1,j)-af(i,j)
     &              -tracer(i,j,k,bi,bj)*(uTrans(i+1,j)-uTrans(i,j))
     &             )*maskInC(i,j,bi,bj)
#endif /* GAD_MULTIDIM_COMPRESSIBLE */
            ENDDO
           ENDDO
          ENDIF
          IF ( N_edge ) THEN
#if defined ALLOW_AUTODIFF_TAMC && defined GAD_MULTIDIM_COMPRESSIBLE
CADJ STORE localTij = loctape_gad_adv_k_pass, key = ipasskey
CADJ &     , kind = isbyte
#endif
           DO j=sNy+1,sNy+OLy
            DO i=iMinUpd,iMaxUpd
#ifdef GAD_MULTIDIM_COMPRESSIBLE
# ifdef ALLOW_AUTODIFF_TAMC
             ijkey = i + ((j-1) + (ipasskey-1)*(sNy+2*OLy))*(sNx+2*OLx)
CADJ STORE localVol(i,j) = loctape_gad_adv_ijk_pass, key=ijkey
CADJ &     , kind = isbyte
# endif
             tmpTrac = localTij(i,j)*localVol(i,j)
     &            -deltaTLev(k)*( af(i+1,j) - af(i,j) )
     &                         *maskInC(i,j,bi,bj)
             localVol(i,j) = localVol(i,j)
     &            -deltaTLev(k)*( uTrans(i+1,j) - uTrans(i,j) )
     &                         *maskInC(i,j,bi,bj)
             localTij(i,j) = tmpTrac/localVol(i,j)
#else /* GAD_MULTIDIM_COMPRESSIBLE */
             localTij(i,j) = localTij(i,j)
     &           -deltaTLev(k)*recip_rhoFacC(k)
     &            *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &            *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &            *( af(i+1,j)-af(i,j)
     &              -tracer(i,j,k,bi,bj)*(uTrans(i+1,j)-uTrans(i,j))
     &             )*maskInC(i,j,bi,bj)
#endif /* GAD_MULTIDIM_COMPRESSIBLE */
            ENDDO
           ENDDO
          ENDIF

         ELSE
#if defined ALLOW_AUTODIFF_TAMC && defined GAD_MULTIDIM_COMPRESSIBLE
CADJ STORE localTij = loctape_gad_adv_k_pass, key = ipasskey
CADJ &     , kind = isbyte
#endif
C      do not only update the overlap
          jMinUpd = 1-OLy
          jMaxUpd = sNy+OLy
          IF ( interiorOnly .AND. S_edge ) jMinUpd = 1
          IF ( interiorOnly .AND. N_edge ) jMaxUpd = sNy
          DO j=jMinUpd,jMaxUpd
           DO i=1-OLx+1,sNx+OLx-1
#ifdef GAD_MULTIDIM_COMPRESSIBLE
# ifdef ALLOW_AUTODIFF_TAMC
             ijkey = i + ((j-1) + (ipasskey-1)*(sNy+2*OLy))*(sNx+2*OLx)
CADJ STORE localVol(i,j) = loctape_gad_adv_ijk_pass, key=ijkey
CADJ &     , kind = isbyte
# endif
            tmpTrac = localTij(i,j)*localVol(i,j)
     &           -deltaTLev(k)*( af(i+1,j) - af(i,j) )
     &                        *maskInC(i,j,bi,bj)
            localVol(i,j) = localVol(i,j)
     &           -deltaTLev(k)*( uTrans(i+1,j) - uTrans(i,j) )
     &                        *maskInC(i,j,bi,bj)
            localTij(i,j) = tmpTrac/localVol(i,j)
#else /* GAD_MULTIDIM_COMPRESSIBLE */
            localTij(i,j) = localTij(i,j)
     &          -deltaTLev(k)*recip_rhoFacC(k)
     &           *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &           *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &           *( af(i+1,j)-af(i,j)
     &             -tracer(i,j,k,bi,bj)*(uTrans(i+1,j)-uTrans(i,j))
     &            )*maskInC(i,j,bi,bj)
#endif /* GAD_MULTIDIM_COMPRESSIBLE */
           ENDDO
          ENDDO
C-      keep advective flux (for diagnostics)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            afx(i,j) = af(i,j)
           ENDDO
          ENDDO

C-     end if/else update overlap-Only
         ENDIF

C--   End of X direction
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Y direction

#ifdef ALLOW_AUTODIFF
C-     Always reset advective flux in Y
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          af(i,j) = 0.
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF */

        IF (calc_fluxes_Y) THEN

C-     Do not compute fluxes if
C       a) needed in overlap only
C   and b) the overlap of myTile are not cube-face edges
         IF ( .NOT.overlapOnly .OR. E_edge .OR. W_edge ) THEN

C-     Internal exchange for calculations in Y
          IF ( overlapOnly ) THEN
           CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                                localTij, bi,bj, myThid )
          ENDIF

C-     Advective flux in Y
#ifndef ALLOW_AUTODIFF
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            af(i,j) = 0.
           ENDDO
          ENDDO
#else /* ALLOW_AUTODIFF */
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE localTij = loctape_gad_adv_k_pass, key = ipasskey
CADJ &     , kind = isbyte
# endif
#endif /* ALLOW_AUTODIFF */

          IF ( advectionScheme.EQ.ENUM_UPWIND_1RST
     &         .OR. advectionScheme.EQ.ENUM_DST2 ) THEN
           CALL GAD_DST2U1_ADV_Y(    bi,bj,k, advectionScheme, .TRUE.,
     I              deltaTLev(k),vTrans,vFld(1-OLx,1-OLy,k), localTij,
     O              af, myThid )
          ELSEIF ( advectionScheme.EQ.ENUM_FLUX_LIMIT ) THEN
           CALL GAD_FLUXLIMIT_ADV_Y( bi,bj,k, .TRUE., deltaTLev(k),
     I              vTrans, vFld(1-OLx,1-OLy,k), maskLocS, localTij,
     O              af, myThid )
          ELSEIF ( advectionScheme.EQ.ENUM_DST3 ) THEN
           CALL GAD_DST3_ADV_Y(      bi,bj,k, .TRUE., deltaTLev(k),
     I              vTrans, vFld(1-OLx,1-OLy,k), maskLocS, localTij,
     O              af, myThid )
          ELSEIF ( advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
           CALL GAD_DST3FL_ADV_Y(    bi,bj,k, .TRUE., deltaTLev(k),
     I              vTrans, vFld(1-OLx,1-OLy,k), maskLocS, localTij,
     O              af, myThid )
#ifndef ALLOW_AUTODIFF
          ELSEIF ( advectionScheme.EQ.ENUM_OS7MP ) THEN
           CALL GAD_OS7MP_ADV_Y(     bi,bj,k, .TRUE., deltaTLev(k),
     I              vTrans, vFld(1-OLx,1-OLy,k), maskLocS, localTij,
     O              af, myThid )
          ELSEIF ( advectionScheme.EQ.ENUM_PPM_NULL_LIMIT  .OR.
     &             advectionScheme.EQ.ENUM_PPM_MONO_LIMIT  .OR.
     &             advectionScheme.EQ.ENUM_PPM_WENO_LIMIT ) THEN
           CALL GAD_PPM_ADV_Y( advectionScheme, bi, bj, k , .TRUE.,
     I              deltaTLev(k), vFld(1-OLX,1-OLy,k), vTrans, localTij,
     O              af, myThid )
          ELSEIF ( advectionScheme.EQ.ENUM_PQM_NULL_LIMIT  .OR.
     &             advectionScheme.EQ.ENUM_PQM_MONO_LIMIT  .OR.
     &             advectionScheme.EQ.ENUM_PQM_WENO_LIMIT ) THEN
           CALL GAD_PQM_ADV_Y( advectionScheme, bi, bj, k , .TRUE.,
     I              deltaTLev(k), vFld(1-OLX,1-OLy,k), vTrans, localTij,
     O              af, myThid )
#endif /* ndef ALLOW_AUTODIFF */
          ELSE
           STOP 'GAD_ADVECTION: adv. scheme incompatible with mutli-dim'
          ENDIF

#ifdef ALLOW_OBCS
          IF ( useOBCS ) THEN
C-      replace advective flux with 1st order upwind scheme estimate
           CALL OBCS_U1_ADV_TRACER( .FALSE., trIdentity, bi, bj, k,
     I                              maskLocS, vTrans, localTij,
     U                              af, myThid )
          ENDIF
#endif /* ALLOW_OBCS */

C-     Internal exchange for next calculations in X
          IF ( overlapOnly .AND. ipass.EQ.1 ) THEN
           CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                                localTij, bi,bj, myThid )
          ENDIF

C-     Advective flux in Y : done
         ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE af = loctape_gad_adv_k_pass, key = ipasskey, kind = isbyte
#endif
C-     Update the local tracer field where needed:
C      use "maksInC" to prevent updating tracer field in OB regions

C      update in overlap-Only
         IF ( overlapOnly ) THEN
          jMinUpd = 1-OLy+1
          jMaxUpd = sNy+OLy-1
C- notes: these 2 lines below have no real effect (because recip_hFac=0
C         in corner region) but safer to keep them.
          IF ( S_edge ) jMinUpd = 1
          IF ( N_edge ) jMaxUpd = sNy

          IF ( W_edge ) THEN
#if defined ALLOW_AUTODIFF_TAMC && defined GAD_MULTIDIM_COMPRESSIBLE
CADJ STORE localTij = loctape_gad_adv_k_pass, key = ipasskey
CADJ &     , kind = isbyte
#endif
           DO j=jMinUpd,jMaxUpd
            DO i=1-OLx,0
#ifdef GAD_MULTIDIM_COMPRESSIBLE
# ifdef ALLOW_AUTODIFF_TAMC
             ijkey = i + ((j-1) + (ipasskey-1)*(sNy+2*OLy))*(sNx+2*OLx)
CADJ STORE localVol(i,j) = loctape_gad_adv_ijk_pass, key = ijkey
CADJ &     , kind = isbyte
# endif
             tmpTrac = localTij(i,j)*localVol(i,j)
     &            -deltaTLev(k)*( af(i,j+1) - af(i,j) )
     &                         *maskInC(i,j,bi,bj)
             localVol(i,j) = localVol(i,j)
     &            -deltaTLev(k)*( vTrans(i,j+1) - vTrans(i,j) )
     &                         *maskInC(i,j,bi,bj)
             localTij(i,j) = tmpTrac/localVol(i,j)
#else /* GAD_MULTIDIM_COMPRESSIBLE */
             localTij(i,j) = localTij(i,j)
     &           -deltaTLev(k)*recip_rhoFacC(k)
     &            *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &            *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &            *( af(i,j+1)-af(i,j)
     &              -tracer(i,j,k,bi,bj)*(vTrans(i,j+1)-vTrans(i,j))
     &             )*maskInC(i,j,bi,bj)
#endif /* GAD_MULTIDIM_COMPRESSIBLE */
            ENDDO
           ENDDO
          ENDIF
          IF ( E_edge ) THEN
#if defined ALLOW_AUTODIFF_TAMC && defined GAD_MULTIDIM_COMPRESSIBLE
CADJ STORE localTij = loctape_gad_adv_k_pass, key = ipasskey
CADJ &     , kind = isbyte
#endif
           DO j=jMinUpd,jMaxUpd
            DO i=sNx+1,sNx+OLx
#ifdef GAD_MULTIDIM_COMPRESSIBLE
# ifdef ALLOW_AUTODIFF_TAMC
             ijkey = i + ((j-1) + (ipasskey-1)*(sNy+2*OLy))*(sNx+2*OLx)
CADJ STORE localVol(i,j) = loctape_gad_adv_ijk_pass, key=ijkey
CADJ &     , kind = isbyte
# endif
             tmpTrac = localTij(i,j)*localVol(i,j)
     &            -deltaTLev(k)*( af(i,j+1) - af(i,j) )
     &                         *maskInC(i,j,bi,bj)
             localVol(i,j) = localVol(i,j)
     &            -deltaTLev(k)*( vTrans(i,j+1) - vTrans(i,j) )
     &                         *maskInC(i,j,bi,bj)
             localTij(i,j) = tmpTrac/localVol(i,j)
#else /* GAD_MULTIDIM_COMPRESSIBLE */
             localTij(i,j) = localTij(i,j)
     &           -deltaTLev(k)*recip_rhoFacC(k)
     &            *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &            *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &            *( af(i,j+1)-af(i,j)
     &              -tracer(i,j,k,bi,bj)*(vTrans(i,j+1)-vTrans(i,j))
     &             )*maskInC(i,j,bi,bj)
#endif /* GAD_MULTIDIM_COMPRESSIBLE */
            ENDDO
           ENDDO
          ENDIF

         ELSE
#if defined ALLOW_AUTODIFF_TAMC && defined GAD_MULTIDIM_COMPRESSIBLE
CADJ STORE localTij = loctape_gad_adv_k_pass, key = ipasskey
CADJ &     , kind = isbyte
#endif
C      do not only update the overlap
          iMinUpd = 1-OLx
          iMaxUpd = sNx+OLx
          IF ( interiorOnly .AND. W_edge ) iMinUpd = 1
          IF ( interiorOnly .AND. E_edge ) iMaxUpd = sNx
          DO j=1-OLy+1,sNy+OLy-1
           DO i=iMinUpd,iMaxUpd
#ifdef GAD_MULTIDIM_COMPRESSIBLE
# ifdef ALLOW_AUTODIFF_TAMC
             ijkey = i + ((j-1) + (ipasskey-1)*(sNy+2*OLy))*(sNx+2*OLx)
CADJ STORE localVol(i,j) = loctape_gad_adv_ijk_pass, key=ijkey
CADJ &     , kind = isbyte
# endif
            tmpTrac = localTij(i,j)*localVol(i,j)
     &           -deltaTLev(k)*( af(i,j+1) - af(i,j) )
     &                        *maskInC(i,j,bi,bj)
            localVol(i,j) = localVol(i,j)
     &           -deltaTLev(k)*( vTrans(i,j+1) - vTrans(i,j) )
     &                        *maskInC(i,j,bi,bj)
            localTij(i,j) = tmpTrac/localVol(i,j)
#else /* GAD_MULTIDIM_COMPRESSIBLE */
            localTij(i,j) = localTij(i,j)
     &          -deltaTLev(k)*recip_rhoFacC(k)
     &           *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &           *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &           *( af(i,j+1)-af(i,j)
     &             -tracer(i,j,k,bi,bj)*(vTrans(i,j+1)-vTrans(i,j))
     &            )*maskInC(i,j,bi,bj)
#endif /* GAD_MULTIDIM_COMPRESSIBLE */
           ENDDO
          ENDDO
C-      keep advective flux (for diagnostics)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            afy(i,j) = af(i,j)
           ENDDO
          ENDDO

C      end if/else update overlap-Only
         ENDIF

C--   End of Y direction
        ENDIF

C--   End of ipass loop
       ENDDO

       IF ( implicitAdvection ) THEN
C-    explicit advection is done ; store tendency in gTracer:
#ifdef GAD_MULTIDIM_COMPRESSIBLE
        STOP 'GAD_ADVECTION: missing code for implicitAdvection'
#endif /* GAD_MULTIDIM_COMPRESSIBLE */
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          gTracer(i,j,k) =
     &     ( localTij(i,j) - tracer(i,j,k,bi,bj) )/deltaTLev(k)
         ENDDO
        ENDDO
       ELSE
C-    horizontal advection done; store intermediate result in 3D array:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifdef GAD_MULTIDIM_COMPRESSIBLE
          locVol3d(i,j,k) = localVol(i,j)
#endif /* GAD_MULTIDIM_COMPRESSIBLE */
          localT3d(i,j,k) = localTij(i,j)
         ENDDO
        ENDDO
       ENDIF

#ifdef ALLOW_DIAGNOSTICS
       IF ( doDiagAdvX ) THEN
        diagName = 'ADVx'//diagSufx
        CALL DIAGNOSTICS_FILL( afx, diagName, k,1, 2,bi,bj, myThid )
       ENDIF
       IF ( doDiagAdvY ) THEN
        diagName = 'ADVy'//diagSufx
        CALL DIAGNOSTICS_FILL( afy, diagName, k,1, 2,bi,bj, myThid )
       ENDIF
#ifdef ALLOW_LAYERS
       IF ( useLayers ) THEN
        CALL LAYERS_FILL(afx,trIdentity,'AFX',k,1,2,bi,bj,myThid)
        CALL LAYERS_FILL(afy,trIdentity,'AFY',k,1,2,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_LAYERS */
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_DEBUG
       IF ( debugLevel .GE. debLevC
     &   .AND. trIdentity.EQ.GAD_TEMPERATURE
     &   .AND. k.LE.3 .AND. myIter.EQ.1+nIter0
     &   .AND. nPx.EQ.1 .AND. nPy.EQ.1
     &   .AND. useCubedSphereExchange ) THEN
        CALL DEBUG_CS_CORNER_UV( ' afx,afy from GAD_ADVECTION',
     &             afx,afy, k, standardMessageUnit,bi,bj,myThid )
       ENDIF
#endif /* ALLOW_DEBUG */

C--   End of K loop for horizontal fluxes
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.implicitAdvection ) THEN

#ifndef ALLOW_AUTODIFF
       usePPMvertAdv = vertAdvecScheme.EQ.ENUM_PPM_NULL_LIMIT
     &            .OR. vertAdvecScheme.EQ.ENUM_PPM_MONO_LIMIT
     &            .OR. vertAdvecScheme.EQ.ENUM_PPM_WENO_LIMIT
       usePQMvertAdv = vertAdvecScheme.EQ.ENUM_PQM_NULL_LIMIT
     &            .OR. vertAdvecScheme.EQ.ENUM_PQM_MONO_LIMIT
     &            .OR. vertAdvecScheme.EQ.ENUM_PQM_WENO_LIMIT
       IF ( usePPMvertAdv .OR. usePQMvertAdv ) THEN
C-- PPM-style or PQM-style vertical advection
        DO k=1,Nr
         IF (k.EQ.1) THEN
C-- vertical transport:  surface
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            rTran3d(i,j,k) = 0. _d 0
           ENDDO
          ENDDO
         ELSE
C-- vertical transport: interior
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            rTran3d(i,j,k) = wFld(i,j,k)*rA(i,j,bi,bj)
     &                      *deepFac2F(k)*rhoFacF(k)
     &                      *maskC(i,j,k-1,bi,bj)
           ENDDO
          ENDDO
         ENDIF
        ENDDO
        IF ( usePPMvertAdv ) THEN
C-- calc. PPM vertical flux data
         CALL GAD_PPM_ADV_R( vertAdvecScheme, bi, bj,
     I                deltaTLev, wFld, rTran3d, localT3d,
     O                afr, myThid )
        ENDIF
        IF ( usePQMvertAdv ) THEN
C-- calc. PQM vertical flux data
         CALL GAD_PQM_ADV_R( vertAdvecScheme, bi, bj,
     I                deltaTLev, wFld, rTran3d, localT3d,
     O                afr, myThid )
        ENDIF
C-- end PPM / PQM style vertical advection
       ENDIF
#endif /* ndef ALLOW_AUTODIFF */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE localT3d = loctape_gad_adv, key=1, kind = isbyte

# ifdef GAD_MULTIDIM_COMPRESSIBLE
CADJ STORE locVol3d = loctape_gad_adv, key=1, kind = isbyte
# endif /* GAD_MULTIDIM_COMPRESSIBLE */
#endif
#ifdef ALLOW_AUTODIFF
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         fVerT(i,j,1) = 0.
         fVerT(i,j,2) = 0.
        ENDDO
       ENDDO
#endif

C--   Start of k loop for vertical flux
       DO k=Nr,1,-1
C--   kUp    Cycles through 1,2 to point to w-layer above
C--   kDown  Cycles through 2,1 to point to w-layer below
        kUp  = 1+MOD(k+1,2)
        kDown= 1+MOD(k,2)
        kp1Msk=1.
        IF (k.EQ.Nr) kp1Msk=0.

#ifdef ALLOW_AUTODIFF_TAMC
C     This directive suppresses a warning and hence makes a store
C     directive unnecessary. Before inserting this directive, the store
C     directive did not resolve a warning about a potential conflict due
C     to the incomplete variable rtrans.
CADJ INCOMPLETE rtrans
#endif

C-- Compute Vertical transport
#ifdef ALLOW_AIM
C- a hack to prevent Water-Vapor vert.transport into the stratospheric level Nr
        IF ( k.EQ.1 .OR.
     &     (useAIM .AND. trIdentity.EQ.GAD_SALINITY .AND. k.EQ.Nr)
     &              ) THEN
#else
        IF ( k.EQ.1 ) THEN
#endif

C- Surface interface :
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           rTransKp(i,j) = kp1Msk*rTrans(i,j)
           rTrans(i,j) = 0.
           fVerT(i,j,kUp) = 0.
          ENDDO
         ENDDO

        ELSE

C- Interior interface :
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           rTransKp(i,j) = kp1Msk*rTrans(i,j)
           rTrans(i,j) = wFld(i,j,k)*rA(i,j,bi,bj)
     &                 *deepFac2F(k)*rhoFacF(k)
     &                 *maskC(i,j,k-1,bi,bj)
           fVerT(i,j,kUp) = 0.
          ENDDO
         ENDDO

C-    Compute vertical advective flux in the interior:
         IF ( vertAdvecScheme.EQ.ENUM_UPWIND_1RST
     &      .OR. vertAdvecScheme.EQ.ENUM_DST2 ) THEN
          CALL GAD_DST2U1_ADV_R(    bi,bj,k, advectionScheme,
     I              deltaTLev(k),rTrans,wFld(1-OLx,1-OLy,k),localT3d,
     O              fVerT(1-OLx,1-OLy,kUp), myThid )
         ELSEIF ( vertAdvecScheme.EQ.ENUM_FLUX_LIMIT ) THEN
          CALL GAD_FLUXLIMIT_ADV_R( bi,bj,k, deltaTLev(k),
     I              rTrans, wFld(1-OLx,1-OLy,k), localT3d,
     O              fVerT(1-OLx,1-OLy,kUp), myThid )
         ELSEIF (vertAdvecScheme.EQ.ENUM_DST3) THEN
          CALL GAD_DST3_ADV_R(      bi,bj,k, deltaTLev(k),
     I              rTrans, wFld(1-OLx,1-OLy,k), localT3d,
     O              fVerT(1-OLx,1-OLy,kUp), myThid )
         ELSEIF ( vertAdvecScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
          CALL GAD_DST3FL_ADV_R(    bi,bj,k, deltaTLev(k),
     I              rTrans, wFld(1-OLx,1-OLy,k), localT3d,
     O              fVerT(1-OLx,1-OLy,kUp), myThid )
#ifndef ALLOW_AUTODIFF
         ELSEIF ( vertAdvecScheme.EQ.ENUM_OS7MP ) THEN
          CALL GAD_OS7MP_ADV_R(     bi,bj,k, deltaTLev(k),
     I              rTrans, wFld(1-OLx,1-OLy,k), localT3d,
     O              fVerT(1-OLx,1-OLy,kUp), myThid )
         ELSEIF ( usePPMvertAdv .OR. usePQMvertAdv ) THEN
C- copy level from 3d flux data
          DO j = 1-OLy,sNy+OLy
           DO i = 1-OLx,sNx+OLx
            fVerT(i,j,kUp) = afr(i,j,k)
           ENDDO
          ENDDO
#endif /* ndef ALLOW_AUTODIFF */
         ELSE
          STOP 'GAD_ADVECTION: adv. scheme incompatible with mutli-dim'
         ENDIF

C- end Surface/Interior if bloc
        ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rtrans   = loctape_gad_adv_k, key = k, kind = isbyte
CADJ STORE rtransKp = loctape_gad_adv_k, key = k, kind = isbyte
cph --- following storing of fVerT is critical for correct
cph --- gradient with multiDimAdvection
cph --- Without it, kDown component is not properly recomputed
cph --- This is a TAF bug (and no warning available)
CADJ STORE fVerT(:,:,:) = loctape_gad_adv_k, key = k, kind = isbyte
#endif

C--   Divergence of vertical fluxes
#ifdef GAD_MULTIDIM_COMPRESSIBLE
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          tmpTrac = localT3d(i,j,k)*locVol3d(i,j,k)
     &      -deltaTLev(k)*( fVerT(i,j,kDown)-fVerT(i,j,kUp) )
     &                   *rkSign*maskInC(i,j,bi,bj)
          localVol(i,j) = locVol3d(i,j,k)
     &      -deltaTLev(k)*( rTransKp(i,j) - rTrans(i,j) )
     &                   *rkSign*maskInC(i,j,bi,bj)
C- localTij only needed for Variance Bugget: can be move there
          localTij(i,j) = tmpTrac/localVol(i,j)
C--  without rescaling of tendencies:
c         gTracer(i,j,k) =
c    &     ( localTij(i,j) - tracer(i,j,k,bi,bj) )/deltaTLev(k)
C--  Non-Lin Free-Surf: consistent with rescaling of tendencies
C     (in FREESURF_RESCALE_G) and RealFreshFlux/addMass.
C    Also valid for linear Free-Surf (r & r* coords) w/wout RealFreshFlux
C     and consistent with linFSConserveTr and "surfExpan_" monitor.
          gTracer(i,j,k) =
     &          ( tmpTrac - tracer(i,j,k,bi,bj)*localVol(i,j) )
     &            *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &            *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
     &            *recip_rhoFacC(k)
     &            /deltaTLev(k)
         ENDDO
        ENDDO
#else /* GAD_MULTIDIM_COMPRESSIBLE */
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          localTij(i,j) = localT3d(i,j,k)
     &      -deltaTLev(k)*recip_rhoFacC(k)
     &       *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &       *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &       *( fVerT(i,j,kDown)-fVerT(i,j,kUp)
     &         -tracer(i,j,k,bi,bj)*(rTransKp(i,j)-rTrans(i,j))
     &        )*rkSign*maskInC(i,j,bi,bj)
          gTracer(i,j,k) =
     &     ( localTij(i,j) - tracer(i,j,k,bi,bj) )/deltaTLev(k)
         ENDDO
        ENDDO
#endif /* GAD_MULTIDIM_COMPRESSIBLE */

#ifdef ALLOW_DIAGNOSTICS
        IF ( doDiagAdvR ) THEN
         diagName = 'ADVr'//diagSufx
         CALL DIAGNOSTICS_FILL( fVerT(1-OLx,1-OLy,kUp),
     &                          diagName, k,1, 2,bi,bj, myThid )
        ENDIF
#ifdef ALLOW_LAYERS
        IF ( useLayers ) THEN
         CALL LAYERS_FILL( fVerT(1-OLx,1-OLy,kUp), trIdentity,
     &                     'AFR', k, 1, 2,bi,bj, myThid)
        ENDIF
#endif /* ALLOW_LAYERS */
#endif /* ALLOW_DIAGNOSTICS */

C--   End of K loop for vertical flux
       ENDDO
C--   end of if not.implicitAdvection block
      ENDIF
#endif /* DISABLE_MULTIDIM_ADVECTION */

      RETURN
      END
