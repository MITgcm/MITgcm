#include "THSICE_OPTIONS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: THSICE_ADVECTION

C !INTERFACE: ==========================================================
      SUBROUTINE THSICE_ADVECTION(
     I     tracerIdentity,
     I     advectionScheme,
     I     useGridArea,
     I     uTrans, vTrans, maskOc, deltaTadvect, iceEps,
     U     iceVol, iceFld,
     O     afx, afy,
     I     bi, bj, myTime, myIter, myThid)

C !DESCRIPTION:
C Calculates the tendency of a sea-ice field due to advection.
C It uses the multi-dimensional method given in \ref{sect:multiDimAdvection}
C and can only be used for the non-linear advection schemes such as the
C direct-space-time method and flux-limiters.
C
C This routine is an adaption of the GAD_ADVECTION for 2D-fields.
C for Area, effective thickness or other sea-ice field,
C  the contribution iceFld*div(u) (that is present in gad_advection)
C  is not included here.
C
C The algorithm is as follows:
C \begin{itemize}
C \item{$\theta^{(n+1/2)} = \theta^{(n)}
C      - \Delta t \partial_x (u\theta^{(n)}) + \theta^{(n)} \partial_x u$}
C \item{$\theta^{(n+2/2)} = \theta^{(n+1/2)}
C      - \Delta t \partial_y (v\theta^{(n+1/2)}) + \theta^{(n)} \partial_y v$}
C \item{$G_\theta = ( \theta^{(n+2/2)} - \theta^{(n)} )/\Delta t$}
C \end{itemize}
C
C The tendency (output) is over-written by this routine.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#if ( defined ALLOW_DBUG_THSICE || defined ALLOW_AUTODIFF_TAMC )
# include "THSICE_SIZE.h"
#endif
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  tracerIdentity  :: tracer identifier (required only for OBCS)
C  advectionScheme :: advection scheme to use (Horizontal plane)
C  useGridArea     :: use grid-cell Area (instead of "iceVol" field)
C  uTrans,vTrans   :: volume transports at U,V points
C  maskOc          :: oceanic mask
C  iceVol          :: sea-ice volume
C  iceFld          :: sea-ice field
C  deltaTadvect    :: time-step used for advection [s]
C  iceEps          :: small volume (to avoid division by zero if iceVol==0)
C  bi,bj           :: tile indices
C  myTime          :: current time in simulation [s]
C  myIter          :: current iteration number
C  myThid          :: my thread Id. number
      INTEGER tracerIdentity
      INTEGER advectionScheme
      LOGICAL useGridArea
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskOc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceVol(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL deltaTadvect, iceEps
      INTEGER bi,bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  iceVol (Updated):: sea-ice volume
C  iceFld (Updated):: sea-ice field
C  afx             :: horizontal advective flux, x direction
C  afy             :: horizontal advective flux, y direction
      _RL afx   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_GENERIC_ADVDIFF
C !LOCAL VARIABLES: ====================================================
C  maskLocC      :: 2-D array mask at grid-cell center
C  maskLocW      :: 2-D array for mask at West points
C  maskLocS      :: 2-D array for mask at South points
C [iMin,iMax]Upd :: loop range to update sea-ice field
C [jMin,jMax]Upd :: loop range to update sea-ice field
C  i,j           :: loop indices
C  uCfl          :: CFL number, zonal direction
C  vCfl          :: CFL number, meridional direction
C  af            :: 2-D array for horizontal advective flux
C  calc_fluxes_X :: logical to indicate to calculate fluxes in X dir
C  calc_fluxes_Y :: logical to indicate to calculate fluxes in Y dir
C  interiorOnly  :: only update the interior of myTile, but not the edges
C  overlapOnly   :: only update the edges of myTile, but not the interior
C  nipass        :: number of passes in multi-dimensional method
C  ipass         :: number of the current pass being made
C  myTile        :: variables used to determine which cube face
C  nCFace        :: owns a tile for cube grid runs using
C                :: multi-dim advection.
C [N,S,E,W]_edge :: true if N,S,E,W edge of myTile is an Edge of the cube
      _RS maskLocC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMinUpd,iMaxUpd,jMinUpd,jMaxUpd
      INTEGER i,j,k
      _RL uCfl    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vCfl    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpVol
      LOGICAL calc_fluxes_X, calc_fluxes_Y, withSigns
      LOGICAL interiorOnly, overlapOnly
      INTEGER nipass,ipass
      INTEGER nCFace
      LOGICAL N_edge, S_edge, E_edge, W_edge
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act0, act1, act2, act3, act4
      INTEGER max0, max1, max2, max3
      INTEGER ticekey, ipasskey
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
#ifdef ALLOW_DBUG_THSICE
      LOGICAL dBugFlag
      INTEGER idb,jdb,biDb
      _RL tmpFac
#endif /* ALLOW_DBUG_THSICE */
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      k = 1
#ifdef ALLOW_AUTODIFF_TAMC
C     tracerIdentity has negative values from GAD_SI_FRAC=-5 to GAD_SI_QICE2=-9
      act0 = GAD_SI_FRAC - tracerIdentity
      max0 = thSIce_nAdv
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ticekey = (act0 + 1) + act1*max0
     &                     + act2*max0*max1
     &                     + act3*max0*max1*max2
     &                     + act4*max0*max1*max2*max3
      IF ( act0 .LT. 0 .OR. act0+1 .GT. thSIce_nAdv) THEN
       WRITE(msgBuf,'(A,2(I3,A))')
     &      'THSICE_ADVECTION: tracerIdentity =', tracerIdentity,
     &      ' not consistent with thSIce_nAdv =', thSIce_nAdv,
     &      ', ==> check "THSICE_SIZE.h"'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R THSICE_ADVECTION'
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_DBUG_THSICE
      dBugFlag = debugLevel.GE.debLevC
     &     .AND. myIter.EQ.nIter0
     &     .AND. ( tracerIdentity.EQ.GAD_SI_HICE .OR.
     &             tracerIdentity.EQ.GAD_SI_QICE2 )
c    &     .AND. tracerIdentity.EQ.GAD_SI_FRAC
      idb  = MIN(13,sNx)
      jdb  = MOD(17,sNy)
      biDb = nSx/2
#endif /* ALLOW_DBUG_THSICE */

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.

C--   Set tile-specific parameters for horizontal fluxes
      IF (useCubedSphereExchange) THEN
       nipass=3
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
       nipass=2
       nCFace = bi
       N_edge = .FALSE.
       S_edge = .FALSE.
       E_edge = .FALSE.
       W_edge = .FALSE.
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
      IF (nipass .GT. maxcube) THEN
       WRITE(msgBuf,'(A,2(I3,A))') 'S/R THSICE_ADVECTION: nipass =',
     &      nipass, ' >', maxcube, ' = maxcube, ==> check "tamc.h"'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R THSICE_ADVECTION'
      ENDIF
#endif

C--   Start horizontal fluxes

C--   set mask West & South (and local Centered mask)
      DO j=1-OLy,sNy+OLy
       maskLocW(1-OLx,j) = 0.
       DO i=2-OLx,sNx+OLx
        maskLocW(i,j) = MIN( maskOc(i-1,j), maskOc(i,j) )
#ifdef ALLOW_OBCS
     &                * maskInW(i,j,bi,bj)
#endif /* ALLOW_OBCS */
       ENDDO
      ENDDO
      DO i=1-OLx,sNx+OLx
       maskLocS(i,1-OLy) = 0.
      ENDDO
      DO j=2-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        maskLocS(i,j) = MIN( maskOc(i,j-1), maskOc(i,j) )
#ifdef ALLOW_OBCS
     &                * maskInS(i,j,bi,bj)
#endif /* ALLOW_OBCS */
       ENDDO
      ENDDO
C     maskLocC is just a local copy of Ocean mask (maskOc) except if using OBCS:
C     use "maksInC" to prevent updating tracer field in OB regions
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        maskLocC(i,j) = maskOc(i,j)
#ifdef ALLOW_OBCS
     &                * maskInC(i,j,bi,bj)
#endif /* ALLOW_OBCS */
       ENDDO
      ENDDO

      IF (useCubedSphereExchange) THEN
       withSigns = .FALSE.
       CALL FILL_CS_CORNER_UV_RS(
     &      withSigns, maskLocW,maskLocS, bi,bj, myThid )
      ENDIF

C--   Multiple passes for different directions on different tiles
C--   For cube need one pass for each of red, green and blue axes.
      DO ipass=1,nipass
#ifdef ALLOW_AUTODIFF_TAMC
       ipasskey = ipass + (ticekey-1) * maxcube
#endif

       interiorOnly = .FALSE.
       overlapOnly  = .FALSE.
       IF (useCubedSphereExchange) THEN
C--   CubedSphere : pass 3 times, with partial update of local seaice field
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
C--   not CubedSphere
        calc_fluxes_X = MOD(ipass,2).EQ.1
        calc_fluxes_Y = .NOT.calc_fluxes_X
       ENDIF
#ifdef ALLOW_DBUG_THSICE
       IF (dBugFlag.AND.bi.EQ.biDb ) WRITE(6,'(A,3I4,2I5,4L5)')
     &   'ICE_adv:', tracerIdentity, ipass, bi, idb, jdb,
     &   calc_fluxes_X, calc_fluxes_Y, overlapOnly, interiorOnly
#endif /* ALLOW_DBUG_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   X direction

       IF (calc_fluxes_X) THEN

C-     Do not compute fluxes if
C       a) needed in overlap only
C   and b) the overlap of myTile are not cube-face Edges
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceFld = comlev1_thsice_adv, key=ipasskey, byte=isbyte
CADJ STORE iceVol = comlev1_thsice_adv, key=ipasskey, byte=isbyte
#endif
        IF ( .NOT.overlapOnly .OR. N_edge .OR. S_edge ) THEN

C-     Advective flux in X
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            af(i,j) = 0.
          ENDDO
         ENDDO

C-     Internal exchange for calculations in X
         IF ( overlapOnly ) THEN
          CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                               iceFld, bi,bj, myThid )
          IF ( .NOT.useGridArea )
     &    CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                               iceVol, bi,bj, myThid )
         ENDIF

C-     Compute CFL number
         IF ( useGridArea ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=2-OLx,sNx+OLx
            uCfl(i,j) = deltaTadvect*(
     &          MAX( uTrans(i,j), 0. _d 0 )*recip_rA(i-1,j,bi,bj)
     &         +MAX(-uTrans(i,j), 0. _d 0 )*recip_rA( i ,j,bi,bj)
     &                               )
           ENDDO
          ENDDO
         ELSE
          DO j=1-OLy,sNy+OLy
           DO i=2-OLx,sNx+OLx
            uCfl(i,j) = deltaTadvect*(
     &        MAX( uTrans(i,j), 0. _d 0 )/MAX( iceVol(i-1,j), iceEps )
     &       +MAX(-uTrans(i,j), 0. _d 0 )/MAX( iceVol( i ,j), iceEps )
     &                               )
           ENDDO
          ENDDO
         ENDIF

         IF ( advectionScheme.EQ.ENUM_UPWIND_1RST
     &        .OR. advectionScheme.EQ.ENUM_DST2 ) THEN
          CALL GAD_DST2U1_ADV_X( bi,bj,k, advectionScheme, .FALSE.,
     I             deltaTadvect, uTrans, uCfl, iceFld,
     O             af, myThid )
#ifdef ALLOW_DBUG_THSICE
          IF (dBugFlag.AND.bi.EQ.biDb ) WRITE(6,'(A,1P4E14.6)')
     &      'ICE_adv: xFx=', af(idb,jdb), iceFld(idb,jdb),
     &       uTrans(idb,jdb), af(idb,jdb)/uTrans(idb,jdb)
#endif /* ALLOW_DBUG_THSICE */
         ELSEIF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
          CALL GAD_FLUXLIMIT_ADV_X( bi,bj,k, .FALSE., deltaTadvect,
     I             uTrans, uCfl, maskLocW, iceFld,
     O             af, myThid )
         ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
          CALL GAD_DST3_ADV_X(      bi,bj,k, .FALSE., deltaTadvect,
     I             uTrans, uCfl, maskLocW, iceFld,
     O             af, myThid )
         ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
          CALL GAD_DST3FL_ADV_X(    bi,bj,k, .FALSE., deltaTadvect,
     I             uTrans, uCfl, maskLocW, iceFld,
     O             af, myThid )
         ELSE
          STOP
     & 'THSICE_ADVECTION: adv. scheme incompatibale with multi-dim'
         ENDIF

C--   Internal exchange for next calculations in Y
         IF ( overlapOnly .AND. ipass.EQ.1 ) THEN
          CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                               iceFld, bi,bj, myThid )
          IF ( .NOT.useGridArea )
     &    CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                               iceVol, bi,bj, myThid )
         ENDIF

C--   Advective flux in X : done
        ENDIF

C-     Update the local seaice field where needed:

C     update in overlap-Only
        IF ( overlapOnly ) THEN
         iMinUpd = 1-OLx+1
         iMaxUpd = sNx+OLx-1
C--   notes: these 2 lines below have no real effect (because recip_hFac=0
C            in corner region) but safer to keep them.
         IF ( W_edge ) iMinUpd = 1
         IF ( E_edge ) iMaxUpd = sNx

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceFld = comlev1_thsice_adv, key=ipasskey, byte=isbyte
CADJ STORE iceVol = comlev1_thsice_adv, key=ipasskey, byte=isbyte
CADJ STORE af     = comlev1_thsice_adv, key=ipasskey, byte=isbyte
#endif
         IF ( S_edge ) THEN
          IF ( useGridArea ) THEN
           DO j=1-OLy,0
            DO i=iMinUpd,iMaxUpd
             iceFld(i,j) = iceFld(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                                 *recip_rA(i,j,bi,bj)
     &                                 *( af(i+1,j)-af(i,j) )
            ENDDO
           ENDDO
          ELSE
           DO j=1-OLy,0
            DO i=iMinUpd,iMaxUpd
             tmpVol = iceVol(i,j)
             iceVol(i,j) = iceVol(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                           *( uTrans(i+1,j)-uTrans(i,j) )
             IF ( iceVol(i,j).GT.iceEps )
     &       iceFld(i,j) = ( iceFld(i,j)*tmpVol
     &                      -deltaTadvect*maskLocC(i,j)
     &                                  *( af(i+1,j)-af(i,j) )
     &                     )/iceVol(i,j)
            ENDDO
           ENDDO
          ENDIF
C-    keep advective flux (for diagnostics)
          DO j=1-OLy,0
           DO i=1-OLx+1,sNx+OLx
             afx(i,j) = af(i,j)
           ENDDO
          ENDDO
C-    end if South Edge
         ENDIF
         IF ( N_edge ) THEN
          IF ( useGridArea ) THEN
           DO j=sNy+1,sNy+OLy
            DO i=iMinUpd,iMaxUpd
             iceFld(i,j) = iceFld(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                                 *recip_rA(i,j,bi,bj)
     &                                 *( af(i+1,j)-af(i,j) )
            ENDDO
           ENDDO
          ELSE
           DO j=sNy+1,sNy+OLy
            DO i=iMinUpd,iMaxUpd
             tmpVol = iceVol(i,j)
             iceVol(i,j) = iceVol(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                           *( uTrans(i+1,j)-uTrans(i,j) )
             IF ( iceVol(i,j).GT.iceEps )
     &       iceFld(i,j) = ( iceFld(i,j)*tmpVol
     &                      -deltaTadvect*maskLocC(i,j)
     &                                   *( af(i+1,j)-af(i,j) )
     &                     )/iceVol(i,j)
            ENDDO
           ENDDO
          ENDIF
C-    keep advective flux (for diagnostics)
          DO j=sNy+1,sNy+OLy
           DO i=1-OLx+1,sNx+OLx
             afx(i,j) = af(i,j)
           ENDDO
          ENDDO
C-    end if North Edge
         ENDIF

        ELSE
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceFld = comlev1_thsice_adv, key=ipasskey, byte=isbyte
CADJ STORE iceVol = comlev1_thsice_adv, key=ipasskey, byte=isbyte
CADJ STORE af     = comlev1_thsice_adv, key=ipasskey, byte=isbyte
#endif
C     do not only update the overlap
          jMinUpd = 1-OLy
          jMaxUpd = sNy+OLy
          IF ( interiorOnly .AND. S_edge ) jMinUpd = 1
          IF ( interiorOnly .AND. N_edge ) jMaxUpd = sNy
          IF ( useGridArea ) THEN
           DO j=jMinUpd,jMaxUpd
            DO i=1-OLx+1,sNx+OLx-1
             iceFld(i,j) = iceFld(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                                 *recip_rA(i,j,bi,bj)
     &                                 *( af(i+1,j)-af(i,j) )
            ENDDO
           ENDDO
          ELSE
           DO j=jMinUpd,jMaxUpd
            DO i=1-OLx+1,sNx+OLx-1
             tmpVol = iceVol(i,j)
             iceVol(i,j) = iceVol(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                           *( uTrans(i+1,j)-uTrans(i,j) )
             IF ( iceVol(i,j).GT.iceEps )
     &       iceFld(i,j) = ( iceFld(i,j)*tmpVol
     &                      -deltaTadvect*maskLocC(i,j)
     &                                   *( af(i+1,j)-af(i,j) )
     &                     )/iceVol(i,j)
            ENDDO
           ENDDO
          ENDIF
C-    keep advective flux (for diagnostics)
          DO j=jMinUpd,jMaxUpd
           DO i=1-OLx+1,sNx+OLx
             afx(i,j) = af(i,j)
           ENDDO
          ENDDO

C-     end if/else update overlap-Only
        ENDIF

C--   End of X direction
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Y direction

       IF (calc_fluxes_Y) THEN

C-     Do not compute fluxes if
C       a) needed in overlap only
C   and b) the overlap of myTile are not cube-face edges
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceFld = comlev1_thsice_adv, key=ipasskey, byte=isbyte
CADJ STORE iceVol = comlev1_thsice_adv, key=ipasskey, byte=isbyte
#endif
        IF ( .NOT.overlapOnly .OR. E_edge .OR. W_edge ) THEN

C-     Advective flux in Y
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            af(i,j) = 0.
          ENDDO
         ENDDO

C-     Internal exchange for calculations in Y
         IF ( overlapOnly ) THEN
          CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                               iceFld, bi,bj, myThid )
          IF ( .NOT.useGridArea )
     &    CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                               iceVol, bi,bj, myThid )
         ENDIF

C-     Compute CFL number
         IF ( useGridArea ) THEN
          DO j=2-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            vCfl(i,j) = deltaTadvect*(
     &          MAX( vTrans(i,j), 0. _d 0 )*recip_rA(i,j-1,bi,bj)
     &         +MAX(-vTrans(i,j), 0. _d 0 )*recip_rA(i, j ,bi,bj)
     &                               )
           ENDDO
          ENDDO
         ELSE
          DO j=2-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            vCfl(i,j) = deltaTadvect*(
     &        MAX( vTrans(i,j), 0. _d 0 )/MAX( iceVol(i,j-1), iceEps )
     &       +MAX(-vTrans(i,j), 0. _d 0 )/MAX( iceVol(i, j ), iceEps )
     &                               )
           ENDDO
          ENDDO
         ENDIF

         IF ( advectionScheme.EQ.ENUM_UPWIND_1RST
     &        .OR. advectionScheme.EQ.ENUM_DST2 ) THEN
          CALL GAD_DST2U1_ADV_Y( bi,bj,k, advectionScheme, .FALSE.,
     I             deltaTadvect, vTrans, vCfl, iceFld,
     O             af, myThid )
#ifdef ALLOW_DBUG_THSICE
          IF (dBugFlag.AND.bi.EQ.biDb ) WRITE(6,'(A,1P4E14.6)')
     &      'ICE_adv: yFx=', af(idb,jdb), iceFld(idb,jdb),
     &       vTrans(idb,jdb), af(idb,jdb)/vTrans(idb,jdb)
#endif /* ALLOW_DBUG_THSICE */
         ELSEIF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
          CALL GAD_FLUXLIMIT_ADV_Y( bi,bj,k, .FALSE., deltaTadvect,
     I             vTrans, vCfl, maskLocS, iceFld,
     O             af, myThid )
         ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
          CALL GAD_DST3_ADV_Y(      bi,bj,k, .FALSE., deltaTadvect,
     I             vTrans, vCfl, maskLocS, iceFld,
     O             af, myThid )
         ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
          CALL GAD_DST3FL_ADV_Y(    bi,bj,k, .FALSE., deltaTadvect,
     I             vTrans, vCfl, maskLocS, iceFld,
     O             af, myThid )
         ELSE
          STOP
     &  'THSICE_ADVECTION: adv. scheme incompatibale with mutli-dim'
         ENDIF

         IF ( overlapOnly .AND. ipass.EQ.1 ) THEN
          CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                               iceFld, bi,bj, myThid )
          IF ( .NOT.useGridArea )
     &    CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                               iceVol, bi,bj, myThid )
         ENDIF

C-     Advective flux in Y : done
        ENDIF

C-     Update the local seaice field where needed:

C      update in overlap-Only
        IF ( overlapOnly ) THEN
         jMinUpd = 1-OLy+1
         jMaxUpd = sNy+OLy-1
C- notes: these 2 lines below have no real effect (because recip_hFac=0
C         in corner region) but safer to keep them.
         IF ( S_edge ) jMinUpd = 1
         IF ( N_edge ) jMaxUpd = sNy

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceFld = comlev1_thsice_adv, key=ipasskey, byte=isbyte
CADJ STORE iceVol = comlev1_thsice_adv, key=ipasskey, byte=isbyte
CADJ STORE af     = comlev1_thsice_adv, key=ipasskey, byte=isbyte
#endif
         IF ( W_edge ) THEN
          IF ( useGridArea ) THEN
           DO j=jMinUpd,jMaxUpd
            DO i=1-OLx,0
             iceFld(i,j) = iceFld(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                                 *recip_rA(i,j,bi,bj)
     &                                 *( af(i,j+1)-af(i,j) )
            ENDDO
           ENDDO
          ELSE
           DO j=jMinUpd,jMaxUpd
            DO i=1-OLx,0
             tmpVol = iceVol(i,j)
             iceVol(i,j) = iceVol(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                           *( vTrans(i,j+1)-vTrans(i,j) )
             IF ( iceVol(i,j).GT.iceEps )
     &       iceFld(i,j) = ( iceFld(i,j)*tmpVol
     &                      -deltaTadvect*maskLocC(i,j)
     &                                   *( af(i,j+1)-af(i,j) )
     &                     )/iceVol(i,j)
            ENDDO
           ENDDO
          ENDIF
C-    keep advective flux (for diagnostics)
          DO j=1-OLy+1,sNy+OLy
           DO i=1-OLx,0
             afy(i,j) = af(i,j)
           ENDDO
          ENDDO
C-    end if West Edge
         ENDIF
         IF ( E_edge ) THEN
          IF ( useGridArea ) THEN
           DO j=jMinUpd,jMaxUpd
            DO i=sNx+1,sNx+OLx
             iceFld(i,j) = iceFld(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                                 *recip_rA(i,j,bi,bj)
     &                                 *( af(i,j+1)-af(i,j) )
            ENDDO
           ENDDO
          ELSE
           DO j=jMinUpd,jMaxUpd
            DO i=sNx+1,sNx+OLx
             tmpVol = iceVol(i,j)
             iceVol(i,j) = iceVol(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                           *( vTrans(i,j+1)-vTrans(i,j) )
             IF ( iceVol(i,j).GT.iceEps )
     &       iceFld(i,j) = ( iceFld(i,j)*tmpVol
     &                      -deltaTadvect*maskLocC(i,j)
     &                                   *( af(i,j+1)-af(i,j) )
     &                     )/iceVol(i,j)
            ENDDO
           ENDDO
          ENDIF
C-    keep advective flux (for diagnostics)
          DO j=1-OLy+1,sNy+OLy
           DO i=sNx+1,sNx+OLx
             afy(i,j) = af(i,j)
           ENDDO
          ENDDO
C-    end if East Edge
         ENDIF

        ELSE
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceFld = comlev1_thsice_adv, key=ipasskey, byte=isbyte
CADJ STORE iceVol = comlev1_thsice_adv, key=ipasskey, byte=isbyte
CADJ STORE af     = comlev1_thsice_adv, key=ipasskey, byte=isbyte
#endif
C     do not only update the overlap
          iMinUpd = 1-OLx
          iMaxUpd = sNx+OLx
          IF ( interiorOnly .AND. W_edge ) iMinUpd = 1
          IF ( interiorOnly .AND. E_edge ) iMaxUpd = sNx
          IF ( useGridArea ) THEN
           DO j=1-OLy+1,sNy+OLy-1
            DO i=iMinUpd,iMaxUpd
             iceFld(i,j) = iceFld(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                                 *recip_rA(i,j,bi,bj)
     &                                 *( af(i,j+1)-af(i,j) )
            ENDDO
           ENDDO
          ELSE
           DO j=1-OLy+1,sNy+OLy-1
            DO i=iMinUpd,iMaxUpd
             tmpVol = iceVol(i,j)
             iceVol(i,j) = iceVol(i,j)
     &                    -deltaTadvect*maskLocC(i,j)
     &                           *( vTrans(i,j+1)-vTrans(i,j) )
             IF ( iceVol(i,j).GT.iceEps )
     &       iceFld(i,j) = ( iceFld(i,j)*tmpVol
     &                      -deltaTadvect*maskLocC(i,j)
     &                                   *( af(i,j+1)-af(i,j) )
     &                     )/iceVol(i,j)
            ENDDO
           ENDDO
          ENDIF
C-    keep advective flux (for diagnostics)
          DO j=1-OLy+1,sNy+OLy
           DO i=iMinUpd,iMaxUpd
             afy(i,j) = af(i,j)
           ENDDO
          ENDDO

C      end if/else update overlap-Only
        ENDIF

C--   End of Y direction
       ENDIF

C--   End of ipass loop
      ENDDO

C-    explicit advection is done ; add some debugging print
#ifdef ALLOW_DBUG_THSICE
      IF ( dBugFlag ) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         IF ( i.EQ.idb .AND. j.EQ.jdb .AND. bi.EQ.biDb ) THEN
          tmpFac= deltaTadvect*recip_rA(i,j,bi,bj)
          WRITE(6,'(A,1P4E14.6)') 'ICE_adv:',
     &     afx(i,j)*tmpFac,afx(i+1,j)*tmpFac,
     &     afy(i,j)*tmpFac,afy(i,j+1)*tmpFac
         ENDIF
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevC
     &     .AND. tracerIdentity.EQ.GAD_SI_HICE
     &     .AND. k.LE.3 .AND. myIter.EQ.1+nIter0
     &     .AND. nPx.EQ.1 .AND. nPy.EQ.1
     &     .AND. useCubedSphereExchange ) THEN
       CALL DEBUG_CS_CORNER_UV( ' afx,afy from THSICE_ADVECTION',
     &      afx,afy, k, standardMessageUnit,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DEBUG */
#endif /* ALLOW_DBUG_THSICE */

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END
