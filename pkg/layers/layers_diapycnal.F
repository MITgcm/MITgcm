#include "LAYERS_OPTIONS.h"

CBOP 0
C     !ROUTINE: LAYERS_DIAPYCNAL
C     !INTERFACE:
      SUBROUTINE LAYERS_DIAPYCNAL(
     I                  tracer,iLa,
     O                  TtendSurf, TtendDiffh, TtendDiffr,
     O                  TtendAdvh, TtendAdvr, Ttendtot,
     O                  StendSurf, StendDiffh, StendDiffr,
     O                  StendAdvh, StendAdvr, Stendtot,
     O                  Hc, PIc,
     I                  myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAYERS_DIAPYCNAL
C     | Calculate the diapycnal velocity in isotracer layers, for a chosen
C     | tracer.
C     *==========================================================*
C     \ev
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

C !INPUT PARAMETERS:
C     myThid    :: my Thread Id number
C     tracer    :: potential temperature, salt or potential density prho
C     iLa       :: layer coordinate index
C     TtendSurf :: temperature tendency due to surface forcing times thickness
C     TtendDiffh:: temperature tendency due to horiz. diffusion times thickness
C     TtendDiffr:: temperature tendency due to vert. diffusion times thickness
C     TtendAdvh:: salinity tendency due to horiz. advection times thickness
C     TtendAdvr:: salinity tendency due to vert. advection times thickness
C     StendSurf :: salinity tendency due to surface forcing times thickness
C     StendDiffh:: salinity tendency due to horiz. diffusion times thickness
C     StendDiffr:: salinity tendency due to vert. diffusion times thickness
C     StendAdvh :: salinity tendency due to horiz. advection times thickness
C     StendAdvr :: salinity tendency due to vert. advection times thickness
C     Hc        :: Layer thickness at the tracer point (m)
C     PIw       :: 1 if layer exists, 0 otherwise (at tracer point)
      INTEGER iLa, myThid
      _RL tracer    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,     nSx,nSy)
      _RL Hc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL PIc       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL TtendSurf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      _RL TtendDiffh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      _RL TtendDiffr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      _RL TtendAdvh (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      _RL TtendAdvr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      _RL Ttendtot  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      _RL StendSurf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      _RL StendDiffh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      _RL StendDiffr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      _RL StendAdvh (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      _RL StendAdvr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      _RL Stendtot  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
CEOP

#ifdef LAYERS_THERMODYNAMICS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     i,j      :: horizontal indices
C     k        :: vertical index for model grid
C     kp1      :: vertical index for model grid next cell
C     kci      :: index from CellIndex
C     kg       :: index for looping though layers_bounds
C     kk       :: vertical index for ZZ (fine) grid
C     kloc     :: local copy of kgu/v to reduce accesses to index arrays
C     mSteps   :: maximum number of steps for bisection method
C     TatC     :: temperature at C point
      _RL Hcw       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers-1,nSx,nSy)
      INTEGER bi, bj
      INTEGER i,j,k,kk,kg,kci,kloc
      INTEGER mSteps
      INTEGER kgc(sNx+1,sNy+1)
      INTEGER kgcw(sNx+1,sNy+1)
      _RL TatC(sNx+1,sNy+1), dzfac, Tfac, Sfac
      LOGICAL errorFlag
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef LAYERS_FINEGRID_DIAPYCNAL
      INTEGER kp1
#endif

C --  constants for T and S forcing, gets reset later for rho
      Tfac = 1. _d 0
      Sfac = 1. _d 0

C     compute maximum number of steps for bisection method (approx.
C     log2(Nlayers)) as log2(Nlayers) + 1 for safety
      mSteps = int(log10(dble(Nlayers))/log10(2. _d 0))+1

C      STOP 'DEBUG END: S/R LAYERS_DIAPYCNAL'

C --- The tile loops
      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

C     Initialize the search indices
      DO j = 1,sNy+1
        DO i = 1,sNx+1
C       The temperature index (layer_G) goes from cold to warm.
C       The water column goes from warm (k=1) to cold (k=Nr).
C       So initialize the search with the warmest value.
          kgc(i,j) = Nlayers
          kgcw(i,j) = Nlayers-1
        ENDDO
      ENDDO

C     Reset the arrays
C --- These are at the w point
      DO kg=1,Nlayers-1
       DO j = 1-OLy,sNy+OLy
        DO i = 1-OLx,sNx+OLx
         TtendSurf (i,j,kg,bi,bj) = 0. _d 0
         TtendDiffh(i,j,kg,bi,bj) = 0. _d 0
         TtendDiffr(i,j,kg,bi,bj) = 0. _d 0
         TtendAdvh(i,j,kg,bi,bj)  = 0. _d 0
         TtendAdvr(i,j,kg,bi,bj)  = 0. _d 0
         Ttendtot(i,j,kg,bi,bj)   = 0. _d 0
         StendSurf (i,j,kg,bi,bj) = 0. _d 0
         StendDiffh(i,j,kg,bi,bj) = 0. _d 0
         StendDiffr(i,j,kg,bi,bj) = 0. _d 0
         StendAdvh(i,j,kg,bi,bj)  = 0. _d 0
         StendAdvr(i,j,kg,bi,bj)  = 0. _d 0
         Stendtot(i,j,kg,bi,bj)   = 0. _d 0
         Hcw(i,j,kg,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
C --- These are at the c point
      DO kg=1,Nlayers
       DO j = 1-OLy,sNy+OLy
        DO i = 1-OLx,sNx+OLx
         Hc(i,j,kg,bi,bj) = 0. _d 0
         PIc(i,j,kg,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

#ifdef LAYERS_FINEGRID_DIAPYCNAL
      DO kk=1,NZZ
       k = MapIndex(kk)
       kci = CellIndex(kk)
       DO j = 1,sNy+1
        DO i = 1,sNx+1
C ------ Find theta at the V point (south) on the fine Z grid
         kp1=k+1
         IF (maskC(i,j,kp1,bi,bj).EQ.zeroRS) kp1=k
         TatC(i,j) = MapFact(kk) * tracer(i,j,k,bi,bj) +
     &    (1. _d 0 -MapFact(kk)) * tracer(i,j,kp1,bi,bj)
        ENDDO
       ENDDO
#else
      DO kk=1,Nr
       k = kk
       kci = kk
       DO j = 1,sNy+1
        DO i = 1,sNx+1
         TatC(i,j) = tracer(i,j,k,bi,bj)
        ENDDO
       ENDDO
#endif /* LAYERS_FINEGRID_DIAPYCNAL */

C ------ debugging stuff
c         IF (i.EQ.38 .AND. j.EQ.4 .AND. bi.EQ.1 .AND. bj.EQ.1) THEN
c           i=38
c           j=4
c           WRITE(msgBuf,
c     &       '(A,I3,A,I3,A,I3,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F3.1)')
c     &          'LAYERS_DEBUG: iLa=', iLa,
c     &          ', kk=', kk,
c     &          ', k=', k,
c     &          ', tracer=', tracer(i,j,k,bi,bj),
c     &          ', TatC=',TatC(i,j),
c     &          ', hFacC=',hFacC(i,j,k,bi,bj)
c           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c     &                         SQUEEZE_RIGHT, myThid )
c         ENDIF
C ------ Now that we know T everywhere, determine the binning.
C        find the layer indices kgc for the center point
       CALL LAYERS_LOCATE(
     I      layers_bounds(1,iLa),Nlayers,mSteps,sNx,sNy,TatC,
     O      kgc,
     I      myThid )
#ifndef TARGET_NEC_SX
C     check for failures
       IF ( debugLevel .GE. debLevC ) THEN
        errorFlag = .FALSE.
        DO j = 1,sNy+1
         DO i = 1,sNx+1
          IF ( kgc(i,j) .LE. 0 ) THEN
           WRITE(msgBuf,'(2A,I3,A,I3,A,1E14.6)')
     &          'S/R LAYERS_LOCATE: Could not find a bin in ',
     &          'layers_bounds for TatC(',i,',',j,',)=',TatC(i,j)
           CALL PRINT_ERROR( msgBuf, myThid )
           errorFlag = .TRUE.
          ENDIF
         ENDDO
        ENDDO
        IF ( errorFlag ) STOP 'ABNORMAL END: S/R LAYERS_DIAPYCNAL'
       ENDIF
#endif /* ndef TARGET_NEC_SX */

C        find the layer indices kgcw for the w point
       CALL LAYERS_LOCATE(
     I      layers_bounds_w(1,iLa),Nlayers-1,mSteps,sNx,sNy,TatC,
     O      kgcw,
     I      myThid )
#ifndef TARGET_NEC_SX
C     check for failures
       IF ( debugLevel .GE. debLevC ) THEN
        errorFlag = .FALSE.
        DO j = 1,sNy+1
         DO i = 1,sNx+1
          IF ( kgcw(i,j) .LE. 0 ) THEN
           WRITE(msgBuf,'(2A,I3,A,I3,A,1E14.6)')
     &          'S/R LAYERS_LOCATE: Could not find a bin in ',
     &          'layers_bounds for TatC(',i,',',j,',)=',TatC(i,j)
           CALL PRINT_ERROR( msgBuf, myThid )
           errorFlag = .TRUE.
          ENDIF
         ENDDO
        ENDDO
        IF ( errorFlag ) STOP 'ABNORMAL END: S/R LAYERS_DIAPYCNAL'
       ENDIF
#endif /* ndef TARGET_NEC_SX */

C ------ Augment the bin values
       DO j = 1,sNy+1
        DO i = 1,sNx+1
#ifdef LAYERS_FINEGRID_DIAPYCNAL
         dzfac = dZZf(kk) * hFacC(i,j,kci,bi,bj)
#else
         dzfac = dRf(kci) * hFacC(i,j,kci,bi,bj)
#endif /* LAYERS_FINEGRID_DIAPYCNAL */
         kloc = kgcw(i,j)

C ------- Thickness at w point
         Hcw(i,j,kloc,bi,bj) = Hcw(i,j,kloc,bi,bj)
     &     + dzfac
C ------- Thickness at c point
         Hc(i,j,kgc(i,j),bi,bj) = Hc(i,j,kgc(i,j),bi,bj)
     &     + dzfac

C ------- Now rescale dzfac to include the layer coordinate spacing
         dzfac = dzfac * layers_recip_delta(kloc,iLa)

#ifdef LAYERS_PRHO_REF
         IF ( layers_num(iLa) .EQ. 3 ) THEN
           Tfac = layers_alpha(i,j,kci,bi,bj)
           Sfac = layers_beta(i,j,kci,bi,bj)
         ENDIF
#endif
         IF (kci.EQ.1) THEN
C ------- We are in the surface layer
          TtendSurf(i,j,kloc,bi,bj) =
     &     TtendSurf(i,j,kloc,bi,bj) +
     &     Tfac * dzfac * layers_surfflux(i,j,1,1,bi,bj)
          StendSurf(i,j,kloc,bi,bj) =
     &     StendSurf(i,j,kloc,bi,bj) +
     &     Sfac * dzfac * layers_surfflux(i,j,1,2,bi,bj)
         ENDIF

#ifdef SHORTWAVE_HEATING
          TtendSurf(i,j,kloc,bi,bj) =
     &     TtendSurf(i,j,kloc,bi,bj) +
     &     Tfac * dzfac * layers_sw(i,j,kci,1,bi,bj)
#endif /* SHORTWAVE_HEATING */

C ------- Diffusion
         TtendDiffh(i,j,kloc,bi,bj) =
     &     TtendDiffh(i,j,kloc,bi,bj) + dzfac * Tfac *
     &    (layers_dfx(i,j,kci,1,bi,bj)+
     &     layers_dfy(i,j,kci,1,bi,bj))
         TtendDiffr(i,j,kloc,bi,bj) =
     &     TtendDiffr(i,j,kloc,bi,bj) +
     &     dzfac * Tfac * layers_dfr(i,j,kci,1,bi,bj)
         StendDiffh(i,j,kloc,bi,bj) =
     &     StendDiffh(i,j,kloc,bi,bj) + dzfac * Sfac *
     &    (layers_dfx(i,j,kci,2,bi,bj)+
     &     layers_dfy(i,j,kci,2,bi,bj))
         StendDiffr(i,j,kloc,bi,bj) =
     &     StendDiffr(i,j,kloc,bi,bj) +
     &     dzfac * Sfac * layers_dfr(i,j,kci,2,bi,bj)
C ------- Advection
         TtendAdvh(i,j,kloc,bi,bj) =
     &     TtendAdvh(i,j,kloc,bi,bj) + dzfac * Tfac *
     &    (layers_afx(i,j,kci,1,bi,bj)+
     &     layers_afy(i,j,kci,1,bi,bj))
         TtendAdvr(i,j,kloc,bi,bj) =
     &     TtendAdvr(i,j,kloc,bi,bj) +
     &     dzfac * Tfac * layers_afr(i,j,kci,1,bi,bj)
         StendAdvh(i,j,kloc,bi,bj) =
     &     StendAdvh(i,j,kloc,bi,bj) + dzfac * Sfac *
     &    (layers_afx(i,j,kci,2,bi,bj)+
     &     layers_afy(i,j,kci,2,bi,bj))
         StendAdvr(i,j,kloc,bi,bj) =
     &     StendAdvr(i,j,kloc,bi,bj) +
     &     dzfac * Sfac * layers_afr(i,j,kci,2,bi,bj)
C -------- Total Tendency
         Ttendtot(i,j,kloc,bi,bj) =
     &     Ttendtot(i,j,kloc,bi,bj) +
     &     dzfac * Tfac * layers_tottend(i,j,kci,1,bi,bj)
         Stendtot(i,j,kloc,bi,bj) =
     &     Stendtot(i,j,kloc,bi,bj) +
     &     dzfac * Sfac * layers_tottend(i,j,kci,2,bi,bj)
        ENDDO
       ENDDO
      ENDDO

C--   Now that we know the thicknesses, compute the heaviside function
C--   (Needs another loop through Ng)
      DO kg=1,Nlayers
       DO j = 1,sNy+1
        DO i = 1,sNx+1
         IF (Hc(i,j,kg,bi,bj) .GT. 0.) THEN
          PIc(i,j,kg,bi,bj) = 1. _d 0
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C --- End bi,bj loop
      ENDDO
      ENDDO

#endif /* LAYERS_THERMODYNAMICS */

      RETURN
      END
