C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_init_variables.F,v 1.44 2012/11/15 20:46:52 dimitri Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_INIT_VARIABLES
C     !INTERFACE:
      SUBROUTINE OBCS_INIT_VARIABLES( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_INIT_VARIABLES
C     | o Initialise OBCs variable data
C     *==========================================================*
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "OBCS_PARAMS.h"
c#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#include "OBCS_SEAICE.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: my Thread Id Number
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi, bj
      INTEGER i, j, k
      INTEGER fp
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
#endif /* ALLOW_PTRACERS */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_INIT_VARIABLES',myThid)
#endif

      fp = readBinaryPrec

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_OBCS_PRESCRIBE
        OBCS_ldRec(bi,bj) = 0
#endif
        DO k=1,Nr
         DO i=1-Olx,sNx+Olx
#ifdef ALLOW_OBCS_NORTH
          OBNu(i,k,bi,bj)=0. _d 0
          OBNv(i,k,bi,bj)=0. _d 0
          OBNt(i,k,bi,bj)=0. _d 0
          OBNs(i,k,bi,bj)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
          OBNu0(i,k,bi,bj)=0. _d 0
          OBNv0(i,k,bi,bj)=0. _d 0
          OBNt0(i,k,bi,bj)=0. _d 0
          OBNs0(i,k,bi,bj)=0. _d 0
          OBNu1(i,k,bi,bj)=0. _d 0
          OBNv1(i,k,bi,bj)=0. _d 0
          OBNt1(i,k,bi,bj)=0. _d 0
          OBNs1(i,k,bi,bj)=0. _d 0
# endif
# ifdef ALLOW_OBCS_STEVENS
          OBNvStevens(i,k,bi,bj)=0. _d 0
          OBNtStevens(i,k,bi,bj)=0. _d 0
          OBNsStevens(i,k,bi,bj)=0. _d 0
# endif /* ALLOW_OBCS_STEVENS */
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
          OBSu(i,k,bi,bj)=0. _d 0
          OBSv(i,k,bi,bj)=0. _d 0
          OBSt(i,k,bi,bj)=0. _d 0
          OBSs(i,k,bi,bj)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
          OBSu0(i,k,bi,bj)=0. _d 0
          OBSv0(i,k,bi,bj)=0. _d 0
          OBSt0(i,k,bi,bj)=0. _d 0
          OBSs0(i,k,bi,bj)=0. _d 0
          OBSu1(i,k,bi,bj)=0. _d 0
          OBSv1(i,k,bi,bj)=0. _d 0
          OBSt1(i,k,bi,bj)=0. _d 0
          OBSs1(i,k,bi,bj)=0. _d 0
# endif
# ifdef ALLOW_OBCS_STEVENS
          OBSvStevens(i,k,bi,bj)=0. _d 0
          OBStStevens(i,k,bi,bj)=0. _d 0
          OBSsStevens(i,k,bi,bj)=0. _d 0
# endif /* ALLOW_OBCS_STEVENS */
#endif /* ALLOW_OBCS_SOUTH */
         ENDDO

         DO j=1-Oly,sNy+Oly
#ifdef ALLOW_OBCS_EAST
          OBEu(j,k,bi,bj)=0. _d 0
          OBEv(j,k,bi,bj)=0. _d 0
          OBEt(j,k,bi,bj)=0. _d 0
          OBEs(j,k,bi,bj)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
          OBEu0(j,k,bi,bj)=0. _d 0
          OBEv0(j,k,bi,bj)=0. _d 0
          OBEt0(j,k,bi,bj)=0. _d 0
          OBEs0(j,k,bi,bj)=0. _d 0
          OBEu1(j,k,bi,bj)=0. _d 0
          OBEv1(j,k,bi,bj)=0. _d 0
          OBEt1(j,k,bi,bj)=0. _d 0
          OBEs1(j,k,bi,bj)=0. _d 0
# endif
# ifdef ALLOW_OBCS_STEVENS
          OBEuStevens(j,k,bi,bj)=0. _d 0
          OBEtStevens(j,k,bi,bj)=0. _d 0
          OBEsStevens(j,k,bi,bj)=0. _d 0
# endif /* ALLOW_OBCS_STEVENS */
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
          OBWu(j,k,bi,bj)=0. _d 0
          OBWv(j,k,bi,bj)=0. _d 0
          OBWt(j,k,bi,bj)=0. _d 0
          OBWs(j,k,bi,bj)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
          OBWu0(j,k,bi,bj)=0. _d 0
          OBWv0(j,k,bi,bj)=0. _d 0
          OBWt0(j,k,bi,bj)=0. _d 0
          OBWs0(j,k,bi,bj)=0. _d 0
          OBWu1(j,k,bi,bj)=0. _d 0
          OBWv1(j,k,bi,bj)=0. _d 0
          OBWt1(j,k,bi,bj)=0. _d 0
          OBWs1(j,k,bi,bj)=0. _d 0
# endif
# ifdef ALLOW_OBCS_STEVENS
          OBWuStevens(j,k,bi,bj)=0. _d 0
          OBWtStevens(j,k,bi,bj)=0. _d 0
          OBWsStevens(j,k,bi,bj)=0. _d 0
# endif /* ALLOW_OBCS_STEVENS */
#endif /* ALLOW_OBCS_WEST */
         ENDDO
        ENDDO

#ifdef ALLOW_NONHYDROSTATIC
        DO k=1,Nr
         DO i=1-Olx,sNx+Olx
          OBNw (i,k,bi,bj) = 0. _d 0
          OBSw (i,k,bi,bj) = 0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
          OBNw0(i,k,bi,bj) = 0. _d 0
          OBSw0(i,k,bi,bj) = 0. _d 0
          OBNw1(i,k,bi,bj) = 0. _d 0
          OBSw1(i,k,bi,bj) = 0. _d 0
# endif
         ENDDO
         DO j=1-Oly,sNy+Oly
          OBEw (j,k,bi,bj) = 0. _d 0
          OBWw (j,k,bi,bj) = 0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
          OBEw0(j,k,bi,bj) = 0. _d 0
          OBWw0(j,k,bi,bj) = 0. _d 0
          OBEw1(j,k,bi,bj) = 0. _d 0
          OBWw1(j,k,bi,bj) = 0. _d 0
# endif
         ENDDO
        ENDDO
#endif /* ALLOW_NONHYDROSTATIC */

#ifdef NONLIN_FRSURF
        DO i=1-Olx,sNx+Olx
          OBNeta (i,bi,bj) = 0. _d 0
          OBSeta (i,bi,bj) = 0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
          OBNeta0(i,bi,bj) = 0. _d 0
          OBSeta0(i,bi,bj) = 0. _d 0
          OBNeta1(i,bi,bj) = 0. _d 0
          OBSeta1(i,bi,bj) = 0. _d 0
# endif
        ENDDO
        DO j=1-Oly,sNy+Oly
          OBEeta (j,bi,bj) = 0. _d 0
          OBWeta (j,bi,bj) = 0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
          OBEeta0(j,bi,bj) = 0. _d 0
          OBWeta0(j,bi,bj) = 0. _d 0
          OBEeta1(j,bi,bj) = 0. _d 0
          OBWeta1(j,bi,bj) = 0. _d 0
# endif
        ENDDO
#endif /* NONLIN_FRSURF */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_SEAICE
        DO i=1-Olx,sNx+Olx
#ifdef ALLOW_OBCS_NORTH
         OBNa (i,bi,bj)=0. _d 0
         OBNh (i,bi,bj)=0. _d 0
         OBNa0(i,bi,bj)=0. _d 0
         OBNh0(i,bi,bj)=0. _d 0
         OBNa1(i,bi,bj)=0. _d 0
         OBNh1(i,bi,bj)=0. _d 0
         OBNsl (i,bi,bj)=0. _d 0
         OBNsn (i,bi,bj)=0. _d 0
         OBNsl0(i,bi,bj)=0. _d 0
         OBNsn0(i,bi,bj)=0. _d 0
         OBNsl1(i,bi,bj)=0. _d 0
         OBNsn1(i,bi,bj)=0. _d 0
         OBNuice (i,bi,bj)=0. _d 0
         OBNvice (i,bi,bj)=0. _d 0
         OBNuice0(i,bi,bj)=0. _d 0
         OBNvice0(i,bi,bj)=0. _d 0
         OBNuice1(i,bi,bj)=0. _d 0
         OBNvice1(i,bi,bj)=0. _d 0
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
         OBSa (i,bi,bj)=0. _d 0
         OBSh (i,bi,bj)=0. _d 0
         OBSa0(i,bi,bj)=0. _d 0
         OBSh0(i,bi,bj)=0. _d 0
         OBSa1(i,bi,bj)=0. _d 0
         OBSh1(i,bi,bj)=0. _d 0
         OBSsl (i,bi,bj)=0. _d 0
         OBSsn (i,bi,bj)=0. _d 0
         OBSsl0(i,bi,bj)=0. _d 0
         OBSsn0(i,bi,bj)=0. _d 0
         OBSsl1(i,bi,bj)=0. _d 0
         OBSsn1(i,bi,bj)=0. _d 0
         OBSuice (i,bi,bj)=0. _d 0
         OBSvice (i,bi,bj)=0. _d 0
         OBSuice0(i,bi,bj)=0. _d 0
         OBSvice0(i,bi,bj)=0. _d 0
         OBSuice1(i,bi,bj)=0. _d 0
         OBSvice1(i,bi,bj)=0. _d 0
#endif /* ALLOW_OBCS_SOUTH */
        ENDDO
        DO j=1-Oly,sNy+Oly
#ifdef ALLOW_OBCS_EAST
         OBEa (j,bi,bj)=0. _d 0
         OBEh (j,bi,bj)=0. _d 0
         OBEa0(j,bi,bj)=0. _d 0
         OBEh0(j,bi,bj)=0. _d 0
         OBEa1(j,bi,bj)=0. _d 0
         OBEh1(j,bi,bj)=0. _d 0
         OBEsl (j,bi,bj)=0. _d 0
         OBEsn (j,bi,bj)=0. _d 0
         OBEsl0(j,bi,bj)=0. _d 0
         OBEsn0(j,bi,bj)=0. _d 0
         OBEsl1(j,bi,bj)=0. _d 0
         OBEsn1(j,bi,bj)=0. _d 0
         OBEuice (j,bi,bj)=0. _d 0
         OBEvice (j,bi,bj)=0. _d 0
         OBEuice0(j,bi,bj)=0. _d 0
         OBEvice0(j,bi,bj)=0. _d 0
         OBEuice1(j,bi,bj)=0. _d 0
         OBEvice1(j,bi,bj)=0. _d 0
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
         OBWa (j,bi,bj)=0. _d 0
         OBWh (j,bi,bj)=0. _d 0
         OBWa0(j,bi,bj)=0. _d 0
         OBWh0(j,bi,bj)=0. _d 0
         OBWa1(j,bi,bj)=0. _d 0
         OBWh1(j,bi,bj)=0. _d 0
         OBWsl (j,bi,bj)=0. _d 0
         OBWsn (j,bi,bj)=0. _d 0
         OBWsl0(j,bi,bj)=0. _d 0
         OBWsn0(j,bi,bj)=0. _d 0
         OBWsl1(j,bi,bj)=0. _d 0
         OBWsn1(j,bi,bj)=0. _d 0
         OBWuice (j,bi,bj)=0. _d 0
         OBWvice (j,bi,bj)=0. _d 0
         OBWuice0(j,bi,bj)=0. _d 0
         OBWvice0(j,bi,bj)=0. _d 0
         OBWuice1(j,bi,bj)=0. _d 0
         OBWvice1(j,bi,bj)=0. _d 0
#endif /* ALLOW_OBCS_WEST */
        ENDDO
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_PTRACERS
#ifndef ALLOW_AUTODIFF_TAMC
        IF ( usePTRACERS ) THEN
#endif
         DO iTracer=1,PTRACERS_numInUse
          DO k=1,Nr
           DO i=1-Olx,sNx+Olx
#ifdef ALLOW_OBCS_NORTH
            OBNptr (i,k,bi,bj,iTracer)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
            OBNptr0(i,k,bi,bj,iTracer)=0. _d 0
            OBNptr1(i,k,bi,bj,iTracer)=0. _d 0
# endif
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
            OBSptr (i,k,bi,bj,iTracer)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
            OBSptr0(i,k,bi,bj,iTracer)=0. _d 0
            OBSptr1(i,k,bi,bj,iTracer)=0. _d 0
# endif
#endif /* ALLOW_OBCS_SOUTH */
           ENDDO

           DO j=1-Oly,sNy+Oly
#ifdef ALLOW_OBCS_EAST
            OBEptr (j,k,bi,bj,iTracer)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
            OBEptr0(j,k,bi,bj,iTracer)=0. _d 0
            OBEptr1(j,k,bi,bj,iTracer)=0. _d 0
# endif
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
            OBWptr (j,k,bi,bj,iTracer)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
            OBWptr0(j,k,bi,bj,iTracer)=0. _d 0
            OBWptr1(j,k,bi,bj,iTracer)=0. _d 0
# endif
#endif /* ALLOW_OBCS_WEST */
           ENDDO
          ENDDO
         ENDDO
#ifndef ALLOW_AUTODIFF_TAMC
        ENDIF
#endif
#endif /* ALLOW_PTRACERS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_ORLANSKI
        IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &      useOrlanskiEast.OR.useOrlanskiWest) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('ORLANSKI_INIT',myThid)
#endif
          CALL ORLANSKI_INIT(bi, bj, myThid)
        ENDIF
#endif /* ALLOW_ORLANSKI */

       ENDDO
      ENDDO

#ifdef ALLOW_OBCS_TIDES
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,tidalComponents
         DO i=1-Olx,sNx+Olx
# ifdef ALLOW_OBCS_NORTH
          OBNam (i,k,bi,bj)=0. _d 0
          OBNph (i,k,bi,bj)=0. _d 0
# endif
# ifdef ALLOW_OBCS_SOUTH
          OBSam (i,k,bi,bj)=0. _d 0
          OBSph (i,k,bi,bj)=0. _d 0
# endif
         ENDDO
         DO j=1-Oly,sNy+Oly
# ifdef ALLOW_OBCS_EAST
          OBEam (j,k,bi,bj)=0. _d 0
          OBEph (j,k,bi,bj)=0. _d 0
# endif
# ifdef ALLOW_OBCS_WEST
          OBWam (j,k,bi,bj)=0. _d 0
          OBWph (j,k,bi,bj)=0. _d 0
# endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _BARRIER
# ifdef ALLOW_OBCS_NORTH
      IF ( OBNamFile .NE. ' '  ) CALL READ_REC_XZ_RL
     &     (OBNamFile,fp,tidalComponents,OBNam,1,nIter0,myThid )
      IF ( OBNphFile .NE. ' '  ) CALL READ_REC_XZ_RL
     &     (OBNphFile,fp,tidalComponents,OBNph,1,nIter0,myThid )
# endif
# ifdef ALLOW_OBCS_SOUTH
      IF ( OBSamFile .NE. ' '  ) CALL READ_REC_XZ_RL
     &     (OBSamFile,fp,tidalComponents,OBSam,1,nIter0,myThid )
      IF ( OBSphFile .NE. ' '  ) CALL READ_REC_XZ_RL
     &     (OBSphFile,fp,tidalComponents,OBSph,1,nIter0,myThid )
# endif
# ifdef ALLOW_OBCS_EAST
      IF ( OBEamFile .NE. ' '  ) CALL READ_REC_YZ_RL
     &     (OBEamFile,fp,tidalComponents,OBEam,1,nIter0,myThid )
      IF ( OBEphFile .NE. ' '  ) CALL READ_REC_YZ_RL
     &     (OBEphFile,fp,tidalComponents,OBEph,1,nIter0,myThid )
# endif
# ifdef ALLOW_OBCS_WEST
      IF ( OBWamFile .NE. ' '  ) CALL READ_REC_YZ_RL
     &     (OBWamFile,fp,tidalComponents,OBWam,1,nIter0,myThid )
      IF ( OBWphFile .NE. ' '  ) CALL READ_REC_YZ_RL
     &     (OBWphFile,fp,tidalComponents,OBWph,1,nIter0,myThid )
# endif
      _BARRIER
#endif /* ALLOW_OBCS_TIDES */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Only needed for Orlanski:
      IF ( nIter0.NE.0 .OR. pickupSuff.NE.' ' ) THEN
        CALL OBCS_READ_PICKUP( nIter0, myThid )
      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Load/compute OBCS values (initial conditions only)
      IF ( nIter0.EQ.0 .AND. pickupSuff.EQ.' ' ) THEN
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('OBCS_CALC',myThid)
#endif
       CALL OBCS_CALC( startTime, nIter0,
     &              uVel, vVel, wVel, theta, salt, myThid )

C--   Apply OBCS values to initial conditions for consistency
C      (but initial conditions only)
#ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('OBCS_APPLY_UV + OBCS_APPLY_TS',myThid)
#endif
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL OBCS_APPLY_UV( bi, bj, 0, uVel, vVel, myThid )
          CALL OBCS_APPLY_TS( bi, bj, 0, theta, salt, myThid )
        ENDDO
       ENDDO
       IF (useOBCSprescribe) THEN
C     After applying the boundary conditions exchange the 3D-fields.
C     This is only necessary of the boundary values have been read
C     from a file.
#ifdef ALLOW_DEBUG
        IF (debugMode)
     &    CALL DEBUG_CALL('EXCHANGES in OBCS_INIT_VARIABLES',myThid)
#endif
        CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
        _EXCH_XYZ_RL( theta, myThid )
        _EXCH_XYZ_RL( salt , myThid )
       ENDIF
C     endif start from rest
#ifdef ALLOW_OBCS_PRESCRIBE
      ELSEIF ( useOBCSprescribe ) THEN
C-- Need this call (although loading U,V,T,S OB values is not really necessary)
C    a) OB values needed for etaH (NonLinFreeSurf) and for wVel (nonHydrostatic)
C    b) OB values needed for ptracers (in case nIter0 = PTRACERS_Iter0 <> 0)
C    c) with present implementation, only do initialisation when called
C       with myTime=startTime (S/R EXF_GETFFIELDREC, setting "first")
C       or with myIter=nIter0 (S/R OBCS_FIELDS_LOAD). And this cannot be changed
C       because of above call to OBCS_CALC(startTime,nIter0).
        CALL OBCS_PRESCRIBE_READ( startTime, nIter0, myThid )
#endif
      ENDIF
C--   calls to S/R OBCS_COPY_TRACER on theta & salt are no longer needed
C     with maskInC,W,S in pkg/generic_advdiff: removed

#ifdef ALLOW_PTRACERS
C     repeat everything for passive tracers
      IF ( usePTRACERS ) THEN
C     catch the case when we do start from a pickup for dynamics variables
C     but initialise ptracers differently
       IF ( nIter0.EQ.PTRACERS_Iter0 ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('OBCS_APPLY_PTRACER',myThid)
#endif
        DO iTracer=1,PTRACERS_numInUse
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL OBCS_APPLY_PTRACER(
     I           bi, bj, 0, iTracer,
     U           ptracer(1-Olx,1-Oly,1,bi,bj,iTracer),
     I           myThid )
          ENDDO
         ENDDO
        ENDDO
C     endif start from rest
       ENDIF
       IF ( nIter0.EQ.PTRACERS_Iter0 .AND. useOBCSprescribe ) THEN
C     After applying the boundary conditions exchange the 3D-fields.
C     This is only necessary of the boundary values have been read
C     from a file.
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL(
     &        'PTRACERS EXCHANGES in OBCS_INIT_VARIABLES',myThid)
#endif
         CALL PTRACERS_FIELDS_BLOCKING_EXCH( myThid )
c      ELSE
C--   Calls to S/R OBCS_COPY_TRACER on pTracers are no longer needed
C     with maskInC,W,S in pkg/generic_advdiff: removed
C-    This call was part of ptracers exchange S/R but was needed in all cases
       ENDIF
C     endif usePTRACERS
      ENDIF
#endif /* ALLOW_PTRACERS */

#endif /* ALLOW_OBCS */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_INIT_VARIABLES',myThid)
#endif
      RETURN
      END
