C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_calc.F,v 1.1 2009/09/16 21:25:47 rpa Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE LAYERS_CALC(
     I    myTime, myIter, myThid )

C ===================================================================
C     Calculate the transport in isopycnal layers.
C     This is the meat of the LAYERS package.
C ===================================================================

      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

C  INPUT PARAMETERS:
C     bi, bj - array indices on which to apply calculations
C     myTime - Current time in simulati
C     myThid ::  my Thread Id number
      INTEGER bi, bj, myTime, myIter, myThid

#ifdef ALLOW_LAYERS

C     === Local variables ===
C     i,j      :: horizontal indices
C     k        :: vertical index for model grid
C     kci      :: index from CellIndex
C     kg       :: index for looping though layers_G
C     kk       :: vertical index for ZZ (fine) grid
C     kgu,kgv  :: vertical index for isopycnal grid
C     TatV     :: temperature at U point
C     TatV     :: temperature at V point

      INTEGER i,j,k,kk,kg,kci
      INTEGER kgu(sNx+1,sNy+1), kgv(sNx+1,sNy+1)
      _RL TatU, TatV
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C --- The thread loop
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C     Initialize the serach indices
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
         layers_UFlux(i,j,kg,bi,bj) = 0. _d 0
#ifdef LAYERS_THICKNESS
         layers_HU(i,j,kg,bi,bj) = 0. _d 0
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
         layers_VFlux(i,j,kg,bi,bj) = 0. _d 0
#ifdef LAYERS_THICKNESS
         layers_HV(i,j,kg,bi,bj) = 0. _d 0
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */
        ENDDO
       ENDDO
      ENDDO

C      _RL  theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
C     Sometimes it is done this way
C      DO j=1-Oly+1,sNy+Oly-1
C       DO i=1-Olx+1,sNx+Olx-1
      DO kk=1,NZZ
       k = MapIndex(kk)
       kci = CellIndex(kk)
       DO j = 1,sNy+1
        DO i = 1,sNx+1

#ifdef LAYERS_UFLUX
C ------ Find theta at the U point (west) on the fine Z grid
         TatU = MapFact(kk) *
     &    0.5 _d 0 * (theta(i-1,j,k,bi,bj)+theta(i,j,k,bi,bj)) +
     &    (1-MapFact(kk)) *
     &    0.5 _d 0 * (theta(i-1,j,k+1,bi,bj)+theta(i,j,k+1,bi,bj))

C ------ Now that we know T everywhere, determine the binning.

         IF (TatU .GE. layers_G(Nlayers)) THEN
C        the point is in the hottest bin or hotter
          kgu(i,j) = Nlayers
         ELSE IF (TatU .LT. layers_G(2)) THEN
C        the point is in the coldest bin or colder
          kgu(i,j) = 1
         ELSE IF ( (TatU .GE. layers_G(kgu(i,j)))
     &    .AND. (TatU .LT. layers_G(kgu(i,j)+1)) ) THEN
C        already on the right bin -- do nothing
         ELSE IF (TatU .GE. layers_G(kgu(i,j))) THEN
C        have to hunt for the right bin by getting hotter
          DO WHILE (TatU .GE. layers_G(kgu(i,j)+1))
           kgu(i,j) = kgu(i,j) + 1
          ENDDO
C         now layers_G(kgu(i,j)+1) < TatU <= layers_G(kgu(i,j)+1)
         ELSE IF (TatU .LT. layers_G(kgu(i,j)+1)) THEN
C        have to hunt for the right bin by getting colder
          DO WHILE (TatU .LT. layers_G(kgu(i,j)))
           kgu(i,j) = kgu(i,j) - 1
          ENDDO
C         now layers_G(kgu(i,j)+1) <= TatU < layers_G(kgu(i,j)+1)
         ELSE
C        that should have covered all the options
          WRITE(msgBuf,'(A,1E14.6)')
     &     'S/R LAYERS_CALC: Couldnt find a bin in layers_G for TatU=',
     &     TatU
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_INIT_FIXED'
         END IF

C ------ Augment the bin values
         layers_UFlux(i,j,kgu(i,j),bi,bj) =
     &    layers_UFlux(i,j,kgu(i,j),bi,bj) +
     &    dZZ * uVel(i,j,kci,bi,bj) * hFacW(i,j,kci,bi,bj)

#ifdef LAYERS_THICKNESS
         layers_HU(i,j,kgu(i,j),bi,bj) = layers_HU(i,j,kgu(i,j),bi,bj)
     &    + dZZ * hFacW(i,j,kci,bi,bj)
#endif /* LAYERS_THICKNESS */

#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
C ------ Find theta at the V point (south) on the fine Z grid
         TatV = MapFact(kk) *
     &    0.5 _d 0 * (theta(i,j-1,k,bi,bj)+theta(i,j,k,bi,bj)) +
     &    (1-MapFact(kk)) *
     &    0.5 _d 0 * (theta(i,j-1,k+1,bi,bj)+theta(i,j,k+1,bi,bj))

C ------ Now that we know T everywhere, determine the binning
         IF (TatV .GE. layers_G(Nlayers)) THEN
C         the point is in the hottest bin or hotter
          kgv(i,j) = Nlayers
         ELSE IF (TatV .LT. layers_G(2)) THEN
C         the point is in the coldest bin or colder
          kgv(i,j) = 1
         ELSE IF ( (TatV .GE. layers_G(kgv(i,j)))
     &    .AND. (TatV .LT. layers_G(kgv(i,j)+1)) ) THEN
C         already on the right bin -- do nothing
         ELSE IF (TatV .GE. layers_G(kgv(i,j))) THEN
C         have to hunt for the right bin by getting hotter
          DO WHILE (TatV .GE. layers_G(kgv(i,j)+1))
           kgv(i,j) = kgv(i,j) + 1
          ENDDO
C         now layers_G(kgv(i,j)+1) < TatV <= layers_G(kgv(i,j)+1)
         ELSE IF (TatV .LT. layers_G(kgv(i,j)+1)) THEN
C         have to hunt for the right bin by getting colder
          DO WHILE (TatV .LT. layers_G(kgv(i,j)))
           kgv(i,j) = kgv(i,j) - 1
          ENDDO
C         now layers_G(kgv(i,j)+1) <= TatV < layers_G(kgv(i,j)+1)
         ELSE
C         that should have covered all the options
          WRITE(msgBuf,'(A,1E14.6)')
     &     'S/R LAYERS_CALC: Couldnt find a bin in layers_G for TatV=',
     &     TatV
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_INIT_FIXED'
         END IF

C ------ Augment the bin values
         layers_VFlux(i,j,kgv(i,j),bi,bj) =
     &    layers_VFlux(i,j,kgv(i,j),bi,bj)
     &    + dZZ * vVel(i,j,kci,bi,bj) * hFacS(i,j,kci,bi,bj)

#ifdef LAYERS_THICKNESS
         layers_HV(i,j,kgv(i,j),bi,bj) = layers_HV(i,j,kgv(i,j),bi,bj)
     &    + dZZ * hFacS(i,j,kci,bi,bj)
#endif /* LAYERS_THICKNESS */

#endif /* LAYERS_VFLUX */

C       k loop
        ENDDO

       ENDDO
      ENDDO

#ifdef ALLOW_TIMEAVE
C--   Time-average
      IF ( taveFreq.GT.0. ) THEN

#ifdef LAYERS_UFLUX
         CALL TIMEAVE_CUMULATE( layers_UFlux_T, layers_UFlux, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
         CALL TIMEAVE_CUMULATE( layers_HU_T, layers_HU, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */
#ifdef LAYERS_VFLUX
         CALL TIMEAVE_CUMULATE( layers_VFlux_T, layers_VFlux, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#ifdef LAYERS_THICKNESS
         CALL TIMEAVE_CUMULATE( layers_HV_T, layers_HV, Nlayers,
     &                          deltaTclock, bi, bj, myThid )
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

       DO kg=1,Nlayers
         layers_TimeAve(kg,bi,bj)=layers_TimeAve(kg,bi,bj)+deltaTclock
       ENDDO

      ENDIF
#endif /* ALLOW_TIMEAVE */

C --- End bi,bj loop
      ENDDO
      ENDDO

#endif /* ALLOW_LAYERS */

      RETURN
      END
