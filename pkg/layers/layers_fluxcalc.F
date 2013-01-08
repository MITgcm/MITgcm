C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_fluxcalc.F,v 1.4 2013/01/08 23:59:50 jmc Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: LAYERS_FLUXCALC
C !INTERFACE:
      SUBROUTINE LAYERS_FLUXCALC(
     I                  uVel,vVel,tracer,iLa,
     O                  UH,VH,Hw,Hs,PIw,PIs,U,V,
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
C      PIw  :: 1 if layer exists, 0 otherwise (at U point)
C      PIs  :: 1 if layer exists, 0 otherwise (at V point)
C      U    :: average U over layer (m/s)
C      V    :: average V over layer (m/s)
C      iLa  :: layer coordinate index
      INTEGER myThid
      _RL uVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL UH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL VH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL Hw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL Hs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL PIw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL PIs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL U(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL V(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      INTEGER iLa
CEOP

#ifdef ALLOW_LAYERS

C !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     i,j      :: horizontal indices
C     k        :: vertical index for model grid
C     kci      :: index from CellIndex
C     kg       :: index for looping though layers_bounds
C     kk       :: vertical index for ZZ (fine) grid
C     kgu,kgv  :: vertical index for isopycnal grid
C     prho     :: potential density referenced to layers_krho pressure
C     TatV     :: temperature at U point
C     TatV     :: temperature at V point

      INTEGER bi, bj
      INTEGER i,j,k,kk,kg,kci,kp1
      INTEGER kgu(sNx+1,sNy+1), kgv(sNx+1,sNy+1)
      _RL TatU, TatV
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_GMREDI
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
#ifdef ALLOW_GMREDI
       kcip1 = MIN(kci+1,Nr)
       maskp1 = 1.
       IF (kci.GE.Nr) maskp1 = 0.
#endif /* ALLOW_GMREDI */
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

         IF (TatU .GE. layers_bounds(Nlayers,iLa)) THEN
C        the point is in the hottest bin or hotter
          kgu(i,j) = Nlayers
         ELSE IF (TatU .LT. layers_bounds(2,iLa)) THEN
C        the point is in the coldest bin or colder
          kgu(i,j) = 1
         ELSE IF ( (TatU .GE. layers_bounds(kgu(i,j),iLa))
     &    .AND. (TatU .LT. layers_bounds(kgu(i,j)+1,iLa)) ) THEN
C        already on the right bin -- do nothing
         ELSE IF (TatU .GE. layers_bounds(kgu(i,j),iLa)) THEN
C        have to hunt for the right bin by getting hotter
          DO WHILE (TatU .GE. layers_bounds(kgu(i,j)+1,iLa))
           kgu(i,j) = kgu(i,j) + 1
          ENDDO
C         now layers_bounds(kgu(i,j)+1,iLa) < TatU <= layers_bounds(kgu(i,j)+1,iLa)
         ELSE IF (TatU .LT. layers_bounds(kgu(i,j)+1,iLa)) THEN
C        have to hunt for the right bin by getting colder
          DO WHILE (TatU .LT. layers_bounds(kgu(i,j),iLa))
           kgu(i,j) = kgu(i,j) - 1
          ENDDO
C         now layers_bounds(kgu(i,j)+1,iLa) <= TatU < layers_bounds(kgu(i,j)+1,iLa)
         ELSE
C        that should have covered all the options
          WRITE(msgBuf,'(2A,1E14.6)')
     &     'S/R LAYERS_CALC: Couldnt find a bin in ',
     &     'layers_bounds for TatU=',TatU
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_INIT_FIXED'
         ENDIF

C ------ Augment the bin values
         UH(i,j,kgu(i,j),bi,bj) =
     &    UH(i,j,kgu(i,j),bi,bj) +
     &    dZZf(kk) * uVel(i,j,kci,bi,bj) * hFacW(i,j,kci,bi,bj)

#ifdef ALLOW_GMREDI
         IF ( layers_bolus(iLa)  ) THEN
           IF ( .NOT.GM_AdvForm ) THEN
             delPsi = 0.5*Kwx(i,j,kcip1,bi,bj)*maskp1
     &              - 0.5*Kwx(i,j, kci, bi,bj)
#ifdef GM_BOLUS_ADVEC
           ELSE
             delPsi = GM_PsiX(i,j,kcip1,bi,bj)*maskp1
     &              - GM_PsiX(i,j, kci, bi,bj)
#endif
           ENDIF
           UH(i,j,kgu(i,j),bi,bj) = UH(i,j,kgu(i,j),bi,bj)
     &      + delPsi*recip_drF(kci)*_recip_hFacW(i,j,kci,bi,bj)
     &      * dZZf(kk)*hFacW(i,j,kci,bi,bj)
         ENDIF
#endif /* ALLOW_GMREDI */

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

C ------ Now that we know T everywhere, determine the binning
         IF (TatV .GE. layers_bounds(Nlayers,iLa)) THEN
C         the point is in the hottest bin or hotter
          kgv(i,j) = Nlayers
         ELSE IF (TatV .LT. layers_bounds(2,iLa)) THEN
C         the point is in the coldest bin or colder
          kgv(i,j) = 1
         ELSE IF ( (TatV .GE. layers_bounds(kgv(i,j),iLa))
     &    .AND. (TatV .LT. layers_bounds(kgv(i,j)+1,iLa)) ) THEN
C         already on the right bin -- do nothing
         ELSE IF (TatV .GE. layers_bounds(kgv(i,j),iLa)) THEN
C         have to hunt for the right bin by getting hotter
          DO WHILE (TatV .GE. layers_bounds(kgv(i,j)+1,iLa))
           kgv(i,j) = kgv(i,j) + 1
          ENDDO
C         now layers_bounds(kgv(i,j)+1,iLa) < TatV <= layers_bounds(kgv(i,j)+1,iLa)
         ELSE IF (TatV .LT. layers_bounds(kgv(i,j)+1,iLa)) THEN
C         have to hunt for the right bin by getting colder
          DO WHILE (TatV .LT. layers_bounds(kgv(i,j),iLa))
           kgv(i,j) = kgv(i,j) - 1
          ENDDO
C         now layers_bounds(kgv(i,j)+1,iLa) <= TatV < layers_bounds(kgv(i,j)+1,iLa)
         ELSE
C         that should have covered all the options
          WRITE(msgBuf,'(2A,1E14.6)')
     &     'S/R LAYERS_CALC: Couldnt find a bin in ',
     &     'layers_bounds for TatV=',
     &     TatV
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_INIT_FIXED'
         ENDIF

C ------ Augment the bin values
         VH(i,j,kgv(i,j),bi,bj) =
     &    VH(i,j,kgv(i,j),bi,bj)
     &    + dZZf(kk) * vVel(i,j,kci,bi,bj) * hFacS(i,j,kci,bi,bj)

#ifdef ALLOW_GMREDI
         IF ( layers_bolus(iLa) ) THEN
           IF ( .NOT.GM_AdvForm ) THEN
             delPsi = 0.5*Kwy(i,j,kcip1,bi,bj)*maskp1
     &              - 0.5*Kwy(i,j, kci, bi,bj)
#ifdef GM_BOLUS_ADVEC
           ELSE
             delPsi = GM_PsiY(i,j,kcip1,bi,bj)*maskp1
     &              - GM_PsiY(i,j, kci, bi,bj)
#endif
           ENDIF
           VH(i,j,kgv(i,j),bi,bj) = VH(i,j,kgv(i,j),bi,bj)
     &      + delPsi*recip_drF(kci)*_recip_hFacS(i,j,kci,bi,bj)
     &      * dZZf(kk)*hFacS(i,j,kci,bi,bj)
         ENDIF
#endif /* ALLOW_GMREDI */

#ifdef LAYERS_THICKNESS
         Hs(i,j,kgv(i,j),bi,bj) = Hs(i,j,kgv(i,j),bi,bj)
     &    + dZZf(kk) * hFacS(i,j,kci,bi,bj)
#endif /* LAYERS_THICKNESS */

#endif /* LAYERS_VFLUX */

        ENDDO
       ENDDO
      ENDDO

C--   Now that we know the thicknesses, compute the heaviside function
C--   (Needs another loop through Ng)
#ifdef LAYERS_THICKNESS
      DO kg=1,Nlayers
       DO j = 1,sNy+1
        DO i = 1,sNx+1
#ifdef LAYERS_UFLUX
         IF (Hw(i,j,kg,bi,bj) .GT. 0.) THEN
          PIw(i,j,kg,bi,bj) = 1. _d 0
          U(i,j,kg,bi,bj) =
     &        UH(i,j,kg,bi,bj) / Hw(i,j,kg,bi,bj)
         ENDIF
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
         IF (Hs(i,j,kg,bi,bj) .GT. 0.) THEN
          PIs(i,j,kg,bi,bj) = 1. _d 0
          V(i,j,kg,bi,bj) =
     &        VH(i,j,kg,bi,bj) / Hs(i,j,kg,bi,bj)
         ENDIF
#endif /* LAYERS_VFLUX */
        ENDDO
       ENDDO
      ENDDO
#endif /* LAYERS_THICKNESS */

C --- End bi,bj loop
      ENDDO
      ENDDO

#endif /* ALLOW_LAYERS */

      RETURN
      END
