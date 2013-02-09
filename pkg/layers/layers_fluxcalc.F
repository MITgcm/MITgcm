C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_fluxcalc.F,v 1.9 2013/02/09 14:52:29 jmc Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI_OPTIONS.h"
#endif

C--  File layers_fluxcalc.F:
C--   Contents
C--   o LAYERS_FLUXCALC
C--   o LAYERS_LOCATE

CBOP 0
C     !ROUTINE: LAYERS_FLUXCALC
C     !INTERFACE:
      SUBROUTINE LAYERS_FLUXCALC(
     I                  uVel,vVel,tracer,iLa,
     O                  UH,VH,Hw,Hs,PIw,PIs,U,V,
     I                  myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAYERS_FLUXCALC
C     | Calculate the transport in isotracer layers, for a chosen
C     | tracer. This is the meat of the LAYERS package.
C     *==========================================================*
C     \ev

C !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
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
      _RL uVel   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,     nSx,nSy)
      _RL vVel   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,     nSx,nSy)
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,     nSx,nSy)
      _RL UH     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL VH     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL Hw     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL Hs     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL PIw    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL PIs    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL U      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL V      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
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
C     kloc     :: local copy of kgu/v to reduce accesses to index arrays
C     mSteps   :: maximum number of steps for bisection method
C     prho     :: potential density referenced to layers_krho pressure
C     TatU     :: temperature at U point
C     TatV     :: temperature at V point

      INTEGER bi, bj
      INTEGER i,j,k,kk,kg,kci,kp1,kloc
      INTEGER mSteps
      INTEGER kgu(sNx+1,sNy+1), kgv(sNx+1,sNy+1)
      _RL TatU(sNx+1,sNy+1), TatV(sNx+1,sNy+1)
#ifdef ALLOW_GMREDI
      INTEGER kcip1
      _RL delPsi, maskp1
#endif
      LOGICAL errorFlag
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     compute maximum number of steps for bisection method (approx.
C     log2(Nlayers)) as log2(Nlayers) + 1 for safety
      mSteps = int(log10(dble(Nlayers))/log10(2. _d 0))+1

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
       DO j = 1-OLy,sNy+OLy
        DO i = 1-OLx,sNx+OLx
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
#ifdef LAYERS_UFLUX
       DO j = 1,sNy+1
        DO i = 1,sNx+1

C ------ Find theta at the U point (west) on the fine Z grid
         kp1=k+1
         IF (hFacW(i,j,kp1,bi,bj) .EQ. 0.) kp1=k
         TatU(i,j) = MapFact(kk) *
     &    0.5 _d 0 * (tracer(i-1,j,k,bi,bj)+tracer(i,j,k,bi,bj)) +
     &    (1-MapFact(kk)) *
     &    0.5 _d 0 * (tracer(i-1,j,kp1,bi,bj)+tracer(i,j,kp1,bi,bj))

        ENDDO
       ENDDO
C ------ Now that we know T everywhere, determine the binning.
C        find the layer indices kgu
       CALL LAYERS_LOCATE(
     I      layers_bounds(1,iLa),Nlayers,mSteps,sNx,sNy,TatU,
     O      kgu,
     I      myThid )
#ifndef TARGET_NEC_SX
C     check for failures
       IF ( debugLevel .GE. debLevC ) THEN
        errorFlag = .FALSE.
        DO j = 1,sNy+1
         DO i = 1,sNx+1
          IF ( kgu(i,j) .LE. 0 ) THEN
           WRITE(msgBuf,'(2A,I3,A,I3,A,1E14.6)')
     &          'S/R LAYERS_LOCATE: Could not find a bin in ',
     &          'layers_bounds for TatU(',i,',',j,',)=',TatU(i,j)
           CALL PRINT_ERROR( msgBuf, myThid )
           errorFlag = .TRUE.
          ENDIF
         ENDDO
        ENDDO
        IF ( errorFlag ) STOP 'ABNORMAL END: S/R LAYERS_FLUXCALC'
       ENDIF
#endif /* ndef TARGET_NEC_SX */
C
       DO j = 1,sNy+1
        DO i = 1,sNx+1

         kloc = kgu(i,j)
C ------ Augment the bin values
         UH(i,j,kloc,bi,bj) =
     &    UH(i,j,kloc,bi,bj) +
     &    dZZf(kk) * uVel(i,j,kci,bi,bj) * hFacW(i,j,kci,bi,bj)

#ifdef ALLOW_GMREDI
         IF ( layers_bolus(iLa)  ) THEN
           IF ( .NOT.GM_AdvForm ) THEN
             delPsi = 0.25 _d 0 *(
     &              ( rA(i-1,j,bi,bj)*Kwx(i-1,j,kcip1,bi,bj)
     &               +rA( i ,j,bi,bj)*Kwx( i ,j,kcip1,bi,bj)
     &              ) * maskW(i,j,kcip1,bi,bj) * maskp1
     &            - ( rA(i-1,j,bi,bj)*Kwx(i-1,j, kci ,bi,bj)
     &               +rA( i ,j,bi,bj)*Kwx( i ,j, kci ,bi,bj)
     &              ) * maskW(i,j, kci ,bi,bj)
     &                           ) * recip_rAw(i,j,bi,bj)
#ifdef GM_BOLUS_ADVEC
           ELSE
             delPsi = GM_PsiX(i,j,kcip1,bi,bj)*maskp1
     &              - GM_PsiX(i,j, kci, bi,bj)
#endif
           ENDIF
           UH(i,j,kloc,bi,bj) = UH(i,j,kloc,bi,bj)
     &      + delPsi*recip_drF(kci)*_recip_hFacW(i,j,kci,bi,bj)
     &      * dZZf(kk)*hFacW(i,j,kci,bi,bj)
         ENDIF
#endif /* ALLOW_GMREDI */

#ifdef LAYERS_THICKNESS
         Hw(i,j,kloc,bi,bj) = Hw(i,j,kloc,bi,bj)
     &    + dZZf(kk) * hFacW(i,j,kci,bi,bj)
#endif /* LAYERS_THICKNESS */

        ENDDO
       ENDDO
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
       DO j = 1,sNy+1
        DO i = 1,sNx+1
C ------ Find theta at the V point (south) on the fine Z grid
         kp1=k+1
         IF (hFacS(i,j,kp1,bi,bj) .EQ. 0.) kp1=k
         TatV(i,j) = MapFact(kk) *
     &    0.5 _d 0 * (tracer(i,j-1,k,bi,bj)+tracer(i,j,k,bi,bj)) +
     &    (1-MapFact(kk)) *
     &    0.5 _d 0 * (tracer(i,j-1,kp1,bi,bj)+tracer(i,j,kp1,bi,bj))

        ENDDO
       ENDDO
C ------ Now that we know T everywhere, determine the binning.
C        find the layer indices kgv
       CALL LAYERS_LOCATE(
     I      layers_bounds(1,iLa),Nlayers,mSteps,sNx,sNy,TatV,
     O      kgv,
     I      myThid )
#ifndef TARGET_NEC_SX
       IF ( debugLevel .GE. debLevC ) THEN
C     check for failures
        errorFlag = .FALSE.
        DO j = 1,sNy+1
         DO i = 1,sNx+1
          IF ( kgv(i,j) .LE. 0 ) THEN
           WRITE(msgBuf,'(2A,I3,A,I3,A,1E14.6)')
     &          'S/R LAYERS_LOCATE: Could not find a bin in ',
     &          'layers_bounds for TatV(',i,',',j,',)=',TatV(i,j)
           CALL PRINT_ERROR( msgBuf, myThid )
           errorFlag = .TRUE.
          ENDIF
         ENDDO
        ENDDO
        IF ( errorFlag ) STOP 'ABNORMAL END: S/R LAYERS_FLUXCALC'
       ENDIF
#endif /* ndef TARGET_NEC_SX */
C
       DO j = 1,sNy+1
        DO i = 1,sNx+1

         kloc = kgv(i,j)
C ------ Augment the bin values
         VH(i,j,kloc,bi,bj) =
     &    VH(i,j,kloc,bi,bj)
     &    + dZZf(kk) * vVel(i,j,kci,bi,bj) * hFacS(i,j,kci,bi,bj)

#ifdef ALLOW_GMREDI
         IF ( layers_bolus(iLa) ) THEN
           IF ( .NOT.GM_AdvForm ) THEN
             delPsi = 0.25 _d 0 *(
     &              ( rA(i,j-1,bi,bj)*Kwy(i,j-1,kcip1,bi,bj)
     &               +rA(i, j ,bi,bj)*Kwy(i, j ,kcip1,bi,bj)
     &              ) * maskS(i,j,kcip1,bi,bj) * maskp1
     &            - ( rA(i,j-1,bi,bj)*Kwy(i,j-1, kci ,bi,bj)
     &               +rA(i, j ,bi,bj)*Kwy(i, j , kci ,bi,bj)
     &              ) * maskS(i,j, kci ,bi,bj)
     &                           ) * recip_rAs(i,j,bi,bj)
#ifdef GM_BOLUS_ADVEC
           ELSE
             delPsi = GM_PsiY(i,j,kcip1,bi,bj)*maskp1
     &              - GM_PsiY(i,j, kci, bi,bj)
#endif
           ENDIF
           VH(i,j,kloc,bi,bj) = VH(i,j,kloc,bi,bj)
     &      + delPsi*recip_drF(kci)*_recip_hFacS(i,j,kci,bi,bj)
     &      * dZZf(kk)*hFacS(i,j,kci,bi,bj)
         ENDIF
#endif /* ALLOW_GMREDI */

#ifdef LAYERS_THICKNESS
         Hs(i,j,kloc,bi,bj) = Hs(i,j,kloc,bi,bj)
     &    + dZZf(kk) * hFacS(i,j,kci,bi,bj)
#endif /* LAYERS_THICKNESS */

        ENDDO
       ENDDO
#endif /* LAYERS_VFLUX */
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

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE LAYERS_LOCATE(
     I                          xx,n,m,sNx,sNy,x,
     O                          k,
     I                          myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | Find the index(-array) k such that x is between xx(k)
C     | and xx(k+1) by bisection, following Press et al.,
C     | Numerical Recipes in Fortran. xx must be monotonic.
C     *==========================================================*
C     \ev

C !USES:
      IMPLICIT NONE
C !INPUT PARAMETERS:
C     xx        :: array of bin-boundaries (layers_boundaries)
C     n         :: length of xx
C     m         :: int(log2(n)) + 1 = length of bisection loop
C     sNx,sNy   :: size of index array and input x
C     x         :: input array of values
C     k         :: index array (output)
C     myThid    :: my Thread Id number
      INTEGER n,m,sNx,sNy
      _RL     xx(1:n+1)
      _RL     x(snx+1,sny+1)
      INTEGER k(snx+1,sny+1)
      INTEGER myThid

C !LOCAL VARIABLES:
C     i,j      :: horizontal indices
C     l        :: bisection loop index
C     kl,ku,km :: work arrays and variables
      INTEGER i,j
CEOP
#ifdef TARGET_NEC_SX
      INTEGER l, km
      INTEGER kl(sNx+1,sNy+1), ku(sNx+1,sNy+1)

C     bisection, following Press et al., Numerical Recipes in Fortran,
C     mostly, because it can be vectorized
      DO j = 1,sNy+1
       DO i = 1,sNx+1
        kl(i,j)=1
        ku(i,j)=n+1
       END DO
      END DO
      DO l = 1,m
       DO j = 1,sNy+1
        DO i = 1,sNx+1
         IF (ku(i,j)-kl(i,j).GT.1) THEN
          km=(ku(i,j)+kl(i,j))/2
CML       IF ((xx(n).GE.xx(1)).EQV.(x(i,j).GE.xx(km))) THEN
          IF ( ((xx(n).GE.xx(1)).AND.(x(i,j).GE.xx(km))).OR.
     &         ((xx(n).GE.xx(1)).AND.(x(i,j).GE.xx(km))) ) THEN
           kl(i,j)=km
          ELSE
           ku(i,j)=km
          END IF
         END IF
        END DO
       END DO
      END DO
      DO j = 1,sNy+1
       DO i = 1,sNx+1
        IF ( x(i,j).LT.xx(2) ) THEN
         k(i,j)=1
        ELSE IF ( x(i,j).GE.xx(n) ) THEN
         k(i,j)=n
        ELSE
         k(i,j)=kl(i,j)
        END IF
       END DO
      END DO
#else
C     the old way
      DO j = 1,sNy+1
       DO i = 1,sNx+1
        IF (x(i,j) .GE. xx(n)) THEN
C     the point is in the hottest bin or hotter
         k(i,j) = n
        ELSE IF (x(i,j) .LT. xx(2)) THEN
C        the point is in the coldest bin or colder
         k(i,j) = 1
        ELSE IF ( (x(i,j) .GE. xx(k(i,j)))
     &    .AND.   (x(i,j) .LT. xx(k(i,j)+1)) ) THEN
C     already on the right bin -- do nothing
        ELSE IF (x(i,j) .GE. xx(k(i,j))) THEN
C     have to hunt for the right bin by getting hotter
         DO WHILE (x(i,j) .GE. xx(k(i,j)+1))
          k(i,j) = k(i,j) + 1
         ENDDO
C     now xx(k) < x <= xx(k+1)
        ELSE IF (x(i,j) .LT. xx(k(i,j)+1)) THEN
C     have to hunt for the right bin by getting colder
         DO WHILE (x(i,j) .LT. xx(k(i,j)))
          k(i,j) = k(i,j) - 1
         ENDDO
C     now xx(k) <= x < xx(k+1)
        ELSE
C     that should have covered all the options
         k(i,j) = -1
        ENDIF

       ENDDO
      ENDDO
#endif /* TARGET_NEC_SX */

#endif /* ALLOW_LAYERS */

      RETURN
      END
