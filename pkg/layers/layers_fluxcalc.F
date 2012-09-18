C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_fluxcalc.F,v 1.1 2012/09/18 23:02:10 gforget Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: LAYERS_FLUXCALC
C !INTERFACE:
      SUBROUTINE LAYERS_FLUXCALC(
     I                  uVel,vVel,tracer,tracer_G,
     O                  UH,VH,Hw,Hs,
     I                  myThid )

C !DESCRIPTION:
C ===================================================================
C     Calculate the transport in isotracer layers, for a chosen
C     tracer. This is the meat of the LAYERS package.
C ===================================================================

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
C #include "DYNVARS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif

C !INPUT PARAMETERS:
C     myThid    :: my Thread Id number
C     uVel  :: zonal velocity (m/s, i=1 held at western face)
C     vVel  :: meridional velocity (m/s, j=1 held at southern face)
C     tracer :: potential temperature, salt or potential density prho 
C      UH   :: U integrated over layer (m^2/s)
C      VH   :: V integrated over layer (m^2/s)
C      Hw   :: Layer thickness at the U point (m)
C      Hs   :: Layer thickness at the V point (m)
C      tracer_G :: boundaries of tracer layers
      INTEGER myThid
      _RL uVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL UH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL VH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL Hw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL Hs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL tracer_G(nLayers+1)
CEOP

#ifdef ALLOW_LAYERS

C !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     i,j      :: horizontal indices
C     k        :: vertical index for model grid
C     kci      :: index from CellIndex
C     kg       :: index for looping though tracer_G
C     kk       :: vertical index for ZZ (fine) grid
C     kgu,kgv  :: vertical index for isopycnal grid
C     prho     :: potential density referenced to layers_kref pressure
C     TatV     :: temperature at U point
C     TatV     :: temperature at V point

      INTEGER bi, bj
      INTEGER i,j,k,kk,kg,kci,kp1
      INTEGER kgu(sNx+1,sNy+1), kgv(sNx+1,sNy+1)
      _RL TatU, TatV
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#if (defined ALLOW_GMREDI) && (defined GM_BOLUS_ADVEC)
      INTEGER kcip1
      _RL delPsi, maskp1
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C --- The tile loops
      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

C     Initialize the search indices
      DO j = 1,sNy+1
        DO i = 1,sNx+1
C       The temperature index (layer_G) goes from cold to warm.
C       The water column goes from warm (k=1) to cold (k=Nr).
C       So initialize the search with the warmest value.
          kgu(i,j) = Nlayers
          kgv(i,j) = Nlayers
        ENDDO
      ENDDO

C     Reset the arrays
      DO kg=1,Nlayers
       DO j = 1,sNy+1
        DO i = 1,sNx+1
#ifdef LAYERS_UFLUX
         UH(i,j,kg,bi,bj) = 0. _d 0
#ifdef LAYERS_THICKNESS
         Hw(i,j,kg,bi,bj) = 0. _d 0
#endif /* LAYERS_THICKNESS */
#endif /* UH */
#ifdef LAYERS_VFLUX
         VH(i,j,kg,bi,bj) = 0. _d 0
#ifdef LAYERS_THICKNESS
         Hs(i,j,kg,bi,bj) = 0. _d 0
#endif /* LAYERS_THICKNESS */
#endif /* VH */
        ENDDO
       ENDDO
      ENDDO

      DO kk=1,NZZ
       k = MapIndex(kk)
       kci = CellIndex(kk)
       DO j = 1,sNy+1
        DO i = 1,sNx+1

#ifdef LAYERS_UFLUX
C ------ Find theta at the U point (west) on the fine Z grid
         kp1=k+1
         IF (hFacW(i,j,kp1,bi,bj) .EQ. 0.) kp1=k
         TatU = MapFact(kk) *
     &    0.5 _d 0 * (tracer(i-1,j,k,bi,bj)+tracer(i,j,k,bi,bj)) +
     &    (1-MapFact(kk)) *
     &    0.5 _d 0 * (tracer(i-1,j,kp1,bi,bj)+tracer(i,j,kp1,bi,bj))

C ------ Now that we know T everywhere, determine the binning.

         IF (TatU .GE. tracer_G(Nlayers)) THEN
C        the point is in the hottest bin or hotter
          kgu(i,j) = Nlayers
         ELSE IF (TatU .LT. tracer_G(2)) THEN
C        the point is in the coldest bin or colder
          kgu(i,j) = 1
         ELSE IF ( (TatU .GE. tracer_G(kgu(i,j)))
     &    .AND. (TatU .LT. tracer_G(kgu(i,j)+1)) ) THEN
C        already on the right bin -- do nothing
         ELSE IF (TatU .GE. tracer_G(kgu(i,j))) THEN
C        have to hunt for the right bin by getting hotter
          DO WHILE (TatU .GE. tracer_G(kgu(i,j)+1))
           kgu(i,j) = kgu(i,j) + 1
          ENDDO
C         now tracer_G(kgu(i,j)+1) < TatU <= tracer_G(kgu(i,j)+1)
         ELSE IF (TatU .LT. tracer_G(kgu(i,j)+1)) THEN
C        have to hunt for the right bin by getting colder
          DO WHILE (TatU .LT. tracer_G(kgu(i,j)))
           kgu(i,j) = kgu(i,j) - 1
          ENDDO
C         now tracer_G(kgu(i,j)+1) <= TatU < tracer_G(kgu(i,j)+1)
         ELSE
C        that should have covered all the options
          WRITE(msgBuf,'(A,1E14.6)')
     &     'S/R LAYERS_CALC: Couldnt find a bin in tracer_G for TatU=',
     &     TatU
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_INIT_FIXED'
         ENDIF

C ------ Augment the bin values
         UH(i,j,kgu(i,j),bi,bj) =
     &    UH(i,j,kgu(i,j),bi,bj) +
     &    dZZf(kk) * uVel(i,j,kci,bi,bj) * hFacW(i,j,kci,bi,bj)

#ifdef ALLOW_GMREDI
         IF ( GM_AdvForm .AND. useBOLUS ) THEN
           kcip1 = MIN(kci+1,Nr)
           maskp1 = 1.
           IF (kci.GE.Nr) maskp1 = 0.
           delPsi = GM_PsiX(i,j,kcip1,bi,bj)*maskp1
     &            - GM_PsiX(i,j, kci, bi,bj)
           UH(i,j,kgu(i,j),bi,bj) =
     &      UH(i,j,kgu(i,j),bi,bj)
     &      + delPsi*recip_drF(kci)*_recip_hFacW(i,j,kci,bi,bj)
     &      * dZZf(kk)*hFacW(i,j,kci,bi,bj)
         ELSEIF ( useBOLUS .AND. .NOT. GM_AdvForm ) THEN
           kcip1 = MIN(kci+1,Nr)
           maskp1 = 1.
           IF (kci.GE.Nr) maskp1 = 0.
           delPsi = 0.5*Kwx(i,j,kcip1,bi,bj)*maskp1
     &            - 0.5*Kwx(i,j, kci, bi,bj)
           UH(i,j,kgu(i,j),bi,bj) =
     &      UH(i,j,kgu(i,j),bi,bj)
     &      + delPsi*recip_drF(kci)*_recip_hFacW(i,j,kci,bi,bj)
     &      * dZZf(kk)*hFacW(i,j,kci,bi,bj)
         ENDIF
#endif

#ifdef LAYERS_THICKNESS
         Hw(i,j,kgu(i,j),bi,bj) = Hw(i,j,kgu(i,j),bi,bj)
     &    + dZZf(kk) * hFacW(i,j,kci,bi,bj)
#endif /* LAYERS_THICKNESS */

#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
C ------ Find theta at the V point (south) on the fine Z grid
         kp1=k+1
         IF (hFacS(i,j,kp1,bi,bj) .EQ. 0.) kp1=k
         TatV = MapFact(kk) *
     &    0.5 _d 0 * (tracer(i,j-1,k,bi,bj)+tracer(i,j,k,bi,bj)) +
     &    (1-MapFact(kk)) *
     &    0.5 _d 0 * (tracer(i,j-1,kp1,bi,bj)+tracer(i,j,kp1,bi,bj))
C          IF (LAYER_nb .EQ. 1) THEN
C C        Use theta to determine layers.
C          TatV = MapFact(kk) *
C      &    0.5 _d 0 * (theta(i,j-1,k,bi,bj)+theta(i,j,k,bi,bj)) +
C      &    (1-MapFact(kk)) *
C      &    0.5 _d 0 * (theta(i,j-1,kp1,bi,bj)+theta(i,j,kp1,bi,bj))
C          ELSEIF (LAYER_nb .EQ. 2) THEN
C C        Use salinity to determine layers.
C          TatV = MapFact(kk) *
C      &    0.5 _d 0 * (salt(i,j-1,k,bi,bj)+salt(i,j,k,bi,bj)) +
C      &    (1-MapFact(kk)) *
C      &    0.5 _d 0 * (salt(i,j-1,kp1,bi,bj)+salt(i,j,kp1,bi,bj))
C #ifdef LAYERS_PRHO_REF
C          ELSEIF (LAYER_nb .EQ. 3)THEN
C C        Use potential density to determine layers.
C          TatV = MapFact(kk) *
C      &    0.5 _d 0 * (prho(i,j-1,k,bi,bj)+prho(i,j,k,bi,bj)) +
C      &    (1-MapFact(kk)) *
C      &    0.5 _d 0 * (prho(i,j-1,kp1,bi,bj)+prho(i,j,kp1,bi,bj))
C #endif
C         ENDIF

C ------ Now that we know T everywhere, determine the binning
         IF (TatV .GE. tracer_G(Nlayers)) THEN
C         the point is in the hottest bin or hotter
          kgv(i,j) = Nlayers
         ELSE IF (TatV .LT. tracer_G(2)) THEN
C         the point is in the coldest bin or colder
          kgv(i,j) = 1
         ELSE IF ( (TatV .GE. tracer_G(kgv(i,j)))
     &    .AND. (TatV .LT. tracer_G(kgv(i,j)+1)) ) THEN
C         already on the right bin -- do nothing
         ELSE IF (TatV .GE. tracer_G(kgv(i,j))) THEN
C         have to hunt for the right bin by getting hotter
          DO WHILE (TatV .GE. tracer_G(kgv(i,j)+1))
           kgv(i,j) = kgv(i,j) + 1
          ENDDO
C         now tracer_G(kgv(i,j)+1) < TatV <= tracer_G(kgv(i,j)+1)
         ELSE IF (TatV .LT. tracer_G(kgv(i,j)+1)) THEN
C         have to hunt for the right bin by getting colder
          DO WHILE (TatV .LT. tracer_G(kgv(i,j)))
           kgv(i,j) = kgv(i,j) - 1
          ENDDO
C         now tracer_G(kgv(i,j)+1) <= TatV < tracer_G(kgv(i,j)+1)
         ELSE
C         that should have covered all the options
          WRITE(msgBuf,'(A,1E14.6)')
     &     'S/R LAYERS_CALC: Couldnt find a bin in tracer_G for TatV=',
     &     TatV
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_INIT_FIXED'
         ENDIF

C ------ Augment the bin values
         VH(i,j,kgv(i,j),bi,bj) =
     &    VH(i,j,kgv(i,j),bi,bj)
     &    + dZZf(kk) * vVel(i,j,kci,bi,bj) * hFacS(i,j,kci,bi,bj)

#ifdef ALLOW_GMREDI
         IF ( GM_AdvForm .AND. useBOLUS ) THEN
           kcip1 = MIN(kci+1,Nr)
           maskp1 = 1.
           IF (kci.GE.Nr) maskp1 = 0.
           delPsi = GM_PsiY(i,j,kcip1,bi,bj)*maskp1
     &            - GM_PsiY(i,j, kci, bi,bj)
           VH(i,j,kgv(i,j),bi,bj) =
     &      VH(i,j,kgv(i,j),bi,bj)
     &      + delPsi*recip_drF(kci)*_recip_hFacS(i,j,kci,bi,bj)
     &      * dZZf(kk)*hFacS(i,j,kci,bi,bj)
         ELSEIF ( useBOLUS .AND. .NOT. GM_AdvForm ) THEN
           kcip1 = MIN(kci+1,Nr)
           maskp1 = 1.
           IF (kci.GE.Nr) maskp1 = 0.
           delPsi = 0.5*Kwy(i,j,kcip1,bi,bj)*maskp1
     &            - 0.5*Kwy(i,j, kci, bi,bj)
           VH(i,j,kgv(i,j),bi,bj) =
     &      VH(i,j,kgv(i,j),bi,bj)
     &      + delPsi*recip_drF(kci)*_recip_hFacS(i,j,kci,bi,bj)
     &      * dZZf(kk)*hFacS(i,j,kci,bi,bj)
         ENDIF
#endif

#ifdef LAYERS_THICKNESS
         Hs(i,j,kgv(i,j),bi,bj) = Hs(i,j,kgv(i,j),bi,bj)
     &    + dZZf(kk) * hFacS(i,j,kci,bi,bj)
#endif /* LAYERS_THICKNESS */

#endif /* LAYERS_VFLUX */

        ENDDO
       ENDDO
      ENDDO

C --- End bi,bj loop
      ENDDO
      ENDDO

#endif /* ALLOW_LAYERS */

      RETURN
      END
