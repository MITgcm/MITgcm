C $Header: /u/gcmpack/MITgcm/pkg/obcs/Attic/obcs_external_fields_load.F,v 1.10 2007/11/08 22:26:22 jmc Exp $
C $Name:  $
#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_EXTERNAL_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE OBCS_EXTERNAL_FIELDS_LOAD( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_EXTERNAL_FIELDS_LOAD
C     | o Control reading of fields from external source.
C     *==========================================================*
C     | External source field loading routine for open boundaries.
C     | This routine is called every time we want to
C     | load a a set of external open boundary fields.
C     | Only if there are fields available (file names are not empty)
C     | the open boundary fields are overwritten.
C     | The routine decides which fields to load and then reads them in.
C     | This routine needs to be customised for particular
C     | experiments.
C     | Notes
C     | =====
C     | Two-dimensional and three-dimensional I/O are handled in
C     | the following way under MITgcmUV. A master thread
C     | performs I/O using system calls. This threads reads data
C     | into a temporary buffer. At present the buffer is loaded
C     | with the entire model domain. This is probably OK for now
C     | Each thread then copies data from the buffer to the
C     | region of the proper array it is responsible for.
C     | =====
C     | This routine is the complete analogue to external_fields_load,
C     | except for exchanges of forcing fields. These are done in
C     | obcs_precribe_exchanges, which is called from dynamics.
C     | - Forcing period and cycle are the same as for other fields
C     |   in external forcing.
C     | - constant boundary values are also read here and not
C     |   directly in obcs_init_variables (which calls obcs_calc
C     |   which in turn calls this routine)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation time
C     myIter - Simulation timestep number
      INTEGER myThid
      _RL     myTime
      INTEGER myIter

C     if external forcing (exf) package is enabled, all loading of external
C     fields is done by exf
#if (defined ALLOW_OBCS && defined ALLOW_OBCS_PRESCRIBE && !defined ALLOW_EXF)
C
#include "OBCS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "OBCS_PTRACERS.h"
#include "PTRACERS_PARAMS.h"
c#include "PTRACERS_FIELDS.h"
#endif /* ALLOW_PTRACERS */

C     !LOCAL VARIABLES:
C     === Local arrays ===
C     aWght, bWght :: Interpolation weights
C     msgBuf       :: Informational/error meesage buffer
      INTEGER intime0,intime1,iTracer
      _RL aWght,bWght,rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF ( periodicExternalForcing ) THEN

C Now calculate whether it is time to update the forcing arrays
      rdt = 1. _d 0 / deltaTclock
      nForcingPeriods = NINT(externForcingCycle/externForcingPeriod)
      Imytm = NINT(myTime*rdt)
      Ifprd = NINT(externForcingPeriod*rdt)
      Ifcyc = NINT(externForcingCycle*rdt)
      Iftm  = MOD( Imytm+Ifcyc-Ifprd/2, Ifcyc)

      intime0 = 1 + INT(Iftm/Ifprd)
      intime1 = 1 + MOD(intime0,nForcingPeriods)
c     aWght = DFLOAT( Iftm-Ifprd*(intime0 - 1) ) / DFLOAT( Ifprd )
      aWght = FLOAT( Iftm-Ifprd*(intime0 - 1) )
      bWght = FLOAT( Ifprd )
      aWght =  aWght / bWght
      bWght = 1. _d 0 - aWght

      IF (
     &  Iftm-Ifprd*(intime0-1) .EQ. 0
     &  .OR. myIter .EQ. nIter0
     & ) THEN

       _BARRIER
       _BEGIN_MASTER(myThid)

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
       WRITE(msgBuf,'(1X,A,2I5,I10,1P1E20.12)')
     &  'OBCS_EXTERNAL_FIELDS_LOAD: Reading new data:',
     &  intime0, intime1, myIter, myTime
       CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)

#ifdef ALLOW_MDSIO
#ifdef ALLOW_OBCS_EAST
C     Eastern boundary
      IF ( OBEuFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBEuFile, readBinaryPrec,
     &        'RL', Nr, OBEu0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBEuFile, readBinaryPrec,
     &        'RL', Nr, OBEu1, intime1, myThid )
      ENDIF
      IF ( OBEvFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBEvFile, readBinaryPrec,
     &        'RL', Nr, OBEv0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBEvFile, readBinaryPrec,
     &        'RL', Nr, OBEv1, intime1, myThid )
      ENDIF
      IF ( OBEtFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBEtFile, readBinaryPrec,
     &        'RL', Nr, OBEt0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBEtFile, readBinaryPrec,
     &        'RL', Nr, OBEt1, intime1, myThid )
      ENDIF
      IF ( OBEsFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBEsFile, readBinaryPrec,
     &        'RL', Nr, OBEs0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBEsFile, readBinaryPrec,
     &        'RL', Nr, OBEs1, intime1, myThid )
      ENDIF
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_WEST
C     Western boundary
      IF ( OBWuFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBWuFile, readBinaryPrec,
     &        'RL', Nr, OBWu0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBWuFile, readBinaryPrec,
     &        'RL', Nr, OBWu1, intime1, myThid )
      ENDIF
      IF ( OBWvFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBWvFile, readBinaryPrec,
     &        'RL', Nr, OBWv0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBWvFile, readBinaryPrec,
     &        'RL', Nr, OBWv1, intime1, myThid )
      ENDIF
      IF ( OBWtFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBWtFile, readBinaryPrec,
     &        'RL', Nr, OBWt0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBWtFile, readBinaryPrec,
     &        'RL', Nr, OBWt1, intime1, myThid )
      ENDIF
      IF ( OBWsFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBWsFile, readBinaryPrec,
     &        'RL', Nr, OBWs0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBWsFile, readBinaryPrec,
     &        'RL', Nr, OBWs1, intime1, myThid )
      ENDIF
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_NORTH
C     Northern boundary
      IF ( OBNuFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBNuFile, readBinaryPrec,
     &        'RL', Nr, OBNu0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBNuFile, readBinaryPrec,
     &        'RL', Nr, OBNu1, intime1, myThid )
      ENDIF
      IF ( OBNvFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBNvFile, readBinaryPrec,
     &        'RL', Nr, OBNv0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBNvFile, readBinaryPrec,
     &        'RL', Nr, OBNv1, intime1, myThid )
      ENDIF
      IF ( OBNtFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBNtFile, readBinaryPrec,
     &        'RL', Nr, OBNt0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBNtFile, readBinaryPrec,
     &        'RL', Nr, OBNt1, intime1, myThid )
      ENDIF
      IF ( OBNsFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBNsFile, readBinaryPrec,
     &        'RL', Nr, OBNs0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBNsFile, readBinaryPrec,
     &        'RL', Nr, OBNs1, intime1, myThid )
      ENDIF
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
C     Southern boundary
      IF ( OBSuFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBSuFile, readBinaryPrec,
     &        'RL', Nr, OBSu0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBSuFile, readBinaryPrec,
     &        'RL', Nr, OBSu1, intime1, myThid )
      ENDIF
      IF ( OBSvFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBSvFile, readBinaryPrec,
     &        'RL', Nr, OBSv0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBSvFile, readBinaryPrec,
     &        'RL', Nr, OBSv1, intime1, myThid )
      ENDIF
      IF ( OBStFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBStFile, readBinaryPrec,
     &        'RL', Nr, OBSt0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBStFile, readBinaryPrec,
     &        'RL', Nr, OBSt1, intime1, myThid )
      ENDIF
      IF ( OBSsFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBSsFile, readBinaryPrec,
     &        'RL', Nr, OBSs0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBSsFile, readBinaryPrec,
     &        'RL', Nr, OBSs1, intime1, myThid )
      ENDIF
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_PTRACERS
      IF (usePTRACERS) THEN
C     read boundary values for passive tracers
       DO iTracer = 1, PTRACERS_numInUse
# ifdef ALLOW_OBCS_EAST
C     Eastern boundary
        IF ( OBEptrFile(iTracer) .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBEptrFile(itracer), readBinaryPrec,
     &        'RL', Nr, OBEptr0(1-Oly,1,1,1,iTracer),
     &        intime0, myThid )
         CALL MDSREADFIELDYZ ( OBEptrFile(itracer), readBinaryPrec,
     &        'RL', Nr, OBEptr1(1-Oly,1,1,1,iTracer),
     &        intime1, myThid )
        ENDIF
# endif /* ALLOW_OBCS_WEST */
# ifdef ALLOW_OBCS_WEST
C     Western boundary
        IF ( OBWptrFile(iTracer) .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBWptrFile(itracer), readBinaryPrec,
     &        'RL', Nr, OBWptr0(1-Oly,1,1,1,iTracer),
     &        intime0, myThid )
         CALL MDSREADFIELDYZ ( OBWptrFile(itracer), readBinaryPrec,
     &        'RL', Nr, OBWptr1(1-Oly,1,1,1,iTracer),
     &        intime1, myThid )
        ENDIF
# endif /* ALLOW_OBCS_WEST */
# ifdef ALLOW_OBCS_NORTH
C     Northern boundary
        IF ( OBNptrFile(iTracer) .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBNptrFile(itracer), readBinaryPrec,
     &        'RL', Nr, OBNptr0(1-Olx,1,1,1,iTracer),
     &        intime0, myThid )
         CALL MDSREADFIELDXZ ( OBNptrFile(itracer), readBinaryPrec,
     &        'RL', Nr, OBNptr1(1-Olx,1,1,1,iTracer),
     &        intime1, myThid )
        ENDIF
# endif /* ALLOW_OBCS_NORTH */
# ifdef ALLOW_OBCS_SOUTH
C     Southern boundary
        IF ( OBSptrFile(iTracer) .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBSptrFile(itracer), readBinaryPrec,
     &        'RL', Nr, OBSptr0(1-Olx,1,1,1,iTracer),
     &        intime0, myThid )
         CALL MDSREADFIELDXZ ( OBSptrFile(itracer), readBinaryPrec,
     &        'RL', Nr, OBSptr1(1-Olx,1,1,1,iTracer),
     &        intime1, myThid )
        ENDIF
# endif /* ALLOW_OBCS_SOUTH */
C     end do iTracer
       ENDDO
C     end if (usePTRACERS)
      ENDIF
#endif /* ALLOW_PTRACERS */
#else /* ALLOW_MDSIO */
      STOP 'ABNORMAL END: OBCS_EXTERNAL_FIELDS_LOAD: NEEDS MSDIO PKG'
#endif /* ALLOW_MDSIO */

       _END_MASTER(myThid)
C
C     At this point in external_fields_load the input fields are exchanged.
C     However, we do not have exchange routines for vertical
C     slices and they are not planned, either, so the approriate fields
C     are exchanged after the open boundary conditions have been
C     applied. (in DYNAMICS and DO_FIELDS_BLOCKING_EXCHANGES)
       _BARRIER

C     end if time to read new data
      ENDIF

C     if not periodicForcing
      ELSE
       aWght = 0. _d 0
       bWght = 1. _d 0
C     read boundary values once and for all
       IF ( myIter .EQ. nIter0 ) THEN
        _BARRIER
        _BEGIN_MASTER(myThid)
C      Read constant boundary conditions only for myIter = nIter0
        WRITE(msgBuf,'(1X,A,I10,1P1E20.12)')
     &       'OBCS_EXTERNAL_FIELDS_LOAD: Reading initial data:',
     &       myIter, myTime
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &       SQUEEZE_RIGHT,myThid)
        inTime0 = 1
#ifdef ALLOW_MDSIO
#ifdef ALLOW_OBCS_EAST
C     Eastern boundary
        IF ( OBEuFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBEuFile, readBinaryPrec,
     &        'RL', Nr, OBEu0, inTime0, myThid )
        ENDIF
        IF ( OBEvFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBEvFile, readBinaryPrec,
     &        'RL', Nr, OBEv0, inTime0, myThid )
        ENDIF
        IF ( OBEtFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBEtFile, readBinaryPrec,
     &        'RL', Nr, OBEt0, inTime0, myThid )
        ENDIF
        IF ( OBEsFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBEsFile, readBinaryPrec,
     &        'RL', Nr, OBEs0, inTime0, myThid )
        ENDIF
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_WEST
C     Western boundary
        IF ( OBWuFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBWuFile, readBinaryPrec,
     &        'RL', Nr, OBWu0, inTime0, myThid )
        ENDIF
        IF ( OBWvFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBWvFile, readBinaryPrec,
     &        'RL', Nr, OBWv0, inTime0, myThid )
        ENDIF
        IF ( OBWtFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBWtFile, readBinaryPrec,
     &        'RL', Nr, OBWt0, inTime0, myThid )
        ENDIF
        IF ( OBWsFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBWsFile, readBinaryPrec,
     &        'RL', Nr, OBWs0, inTime0, myThid )
        ENDIF
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_NORTH
C     Northern boundary
        IF ( OBNuFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXz ( OBNuFile, readBinaryPrec,
     &        'RL', Nr, OBNu0, inTime0, myThid )
        ENDIF
        IF ( OBNvFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBNvFile, readBinaryPrec,
     &        'RL', Nr, OBNv0, inTime0, myThid )
        ENDIF
        IF ( OBNtFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBNtFile, readBinaryPrec,
     &        'RL', Nr, OBNt0, inTime0, myThid )
        ENDIF
        IF ( OBNsFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBNsFile, readBinaryPrec,
     &        'RL', Nr, OBNs0, inTime0, myThid )
        ENDIF
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
C     Southern boundary
        IF ( OBSuFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXz ( OBSuFile, readBinaryPrec,
     &        'RL', Nr, OBSu0, inTime0, myThid )
        ENDIF
        IF ( OBSvFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBSvFile, readBinaryPrec,
     &        'RL', Nr, OBSv0, inTime0, myThid )
        ENDIF
        IF ( OBStFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBStFile, readBinaryPrec,
     &        'RL', Nr, OBSt0, inTime0, myThid )
        ENDIF
        IF ( OBSsFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBSsFile, readBinaryPrec,
     &        'RL', Nr, OBSs0, inTime0, myThid )
        ENDIF
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_PTRACERS
        IF (usePTRACERS) THEN
C     read passive tracer boundary values
         DO iTracer = 1, PTRACERS_numInUse
# ifdef ALLOW_OBCS_EAST
C     Eastern boundary
          IF ( OBEptrFile(iTracer) .NE. ' '  ) THEN
           CALL MDSREADFIELDYZ ( OBEptrFile(iTracer), readBinaryPrec,
     &          'RL', Nr, OBEptr0(1-Oly,1,1,1,iTracer),
     &          inTime0, myThid )
          ENDIF
# endif /* ALLOW_OBCS_WEST */
# ifdef ALLOW_OBCS_WEST
C     Western boundary
          IF ( OBWptrFile(iTracer) .NE. ' '  ) THEN
           CALL MDSREADFIELDYZ ( OBWptrFile(iTracer), readBinaryPrec,
     &          'RL', Nr, OBWptr0(1-Oly,1,1,1,iTracer),
     &          inTime0, myThid )
          ENDIF
# endif /* ALLOW_OBCS_WEST */
# ifdef ALLOW_OBCS_NORTH
C     Northern boundary
          IF ( OBNptrFile(iTracer) .NE. ' '  ) THEN
           CALL MDSREADFIELDXZ ( OBNptrFile(iTracer), readBinaryPrec,
     &          'RL', Nr, OBNptr0(1-Olx,1,1,1,iTracer),
     &          inTime0, myThid )
          ENDIF
# endif /* ALLOW_OBCS_NORTH */
# ifdef ALLOW_OBCS_SOUTH
C     Southern boundary
          IF ( OBSptrFile(iTracer) .NE. ' '  ) THEN
           CALL MDSREADFIELDXZ ( OBSptrFile(iTracer), readBinaryPrec,
     &          'RL', Nr, OBSptr0(1-Olx,1,1,1,iTracer),
     &          inTime0, myThid )
          ENDIF
# endif /* ALLOW_OBCS_SOUTH */
C     end do iTracer
         ENDDO
C     end if (usePTRACERS)
        ENDIF
#endif /* ALLOW_PTRACERS */
#else /* ALLOW_MDSIO */
      STOP 'ABNORMAL END: OBCS_EXTERNAL_FIELDS_LOAD: NEEDS MSDIO PKG'
#endif /* ALLOW_MDSIO */
        _END_MASTER(myThid)
        _BARRIER
C     endif myIter .EQ. nIter0
       ENDIF
C     endif for periodicForcing
      ENDIF

C--   Now interpolate OBSu, OBSv, OBSt, OBSs, OBSptr, etc.
C--   For periodicForcing, aWght = 0. and bWght = 1. so that the
C--   interpolation boilds down to copying the time-independent
C--   forcing field OBSu0 to OBSu
#ifdef ALLOW_OBCS_EAST
       IF ( OBEuFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     &      OBEu, OBEu0, OBEu1, aWght, bWght, myThid )
       IF ( OBEvFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     &      OBEv, OBEv0, OBEv1, aWght, bWght, myThid )
       IF ( OBEtFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     &      OBEt, OBEt0, OBEt1, aWght, bWght, myThid )
       IF ( OBEsFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     &      OBEs, OBEs0, OBEs1, aWght, bWght, myThid )
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
       IF ( OBWuFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     &      OBWu, OBWu0, OBWu1, aWght, bWght, myThid )
       IF ( OBWvFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     &      OBWv, OBWv0, OBWv1, aWght, bWght, myThid )
       IF ( OBWtFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     &      OBWt, OBWt0, OBWt1, aWght, bWght, myThid )
       IF ( OBWsFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     &      OBWs, OBWs0, OBWs1, aWght, bWght, myThid )
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_NORTH
       IF ( OBNuFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     &      OBNu, OBNu0, OBNu1, aWght, bWght, myThid )
       IF ( OBNvFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     &      OBNv, OBNv0, OBNv1, aWght, bWght, myThid )
       IF ( OBNtFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     &      OBNt, OBNt0, OBNt1, aWght, bWght, myThid )
       IF ( OBNsFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     &      OBNs, OBNs0, OBNs1, aWght, bWght, myThid )
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
       IF ( OBSuFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     &      OBSu, OBSu0, OBSu1, aWght, bWght, myThid )
       IF ( OBSvFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     &      OBSv, OBSv0, OBSv1, aWght, bWght, myThid )
       IF ( OBStFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     &      OBSt, OBSt0, OBSt1, aWght, bWght, myThid )
       IF ( OBSsFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     &      OBSs, OBSs0, OBSs1, aWght, bWght, myThid )
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_PTRACERS
      IF (usePTRACERS) THEN
C     "interpolate" passive tracer boundary values
       DO iTracer = 1, PTRACERS_numInUse
# ifdef ALLOW_OBCS_EAST
        IF ( OBEptrFile(iTracer) .NE. ' '  )
     &       CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     O       OBEptr (1-Oly,1,1,1,iTracer),
     I       OBEptr0(1-Oly,1,1,1,iTracer),
     I       OBEptr1(1-Oly,1,1,1,iTracer), aWght, bWght, myThid )
# endif /* ALLOW_OBCS_EAST */
# ifdef ALLOW_OBCS_WEST
        IF ( OBWptrFile(iTracer) .NE. ' '  )
     &       CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     O       OBWptr (1-Oly,1,1,1,iTracer),
     I       OBWptr0(1-Oly,1,1,1,iTracer),
     I       OBWptr1(1-Oly,1,1,1,iTracer), aWght, bWght, myThid )
# endif /* ALLOW_OBCS_WEST */
# ifdef ALLOW_OBCS_NORTH
        IF ( OBNptrFile(iTracer) .NE. ' '  )
     &       CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     O       OBNptr (1-Olx,1,1,1,iTracer),
     I       OBNptr0(1-Olx,1,1,1,iTracer),
     I       OBNptr1(1-Olx,1,1,1,iTracer), aWght, bWght, myThid )
# endif /* ALLOW_OBCS_NORTH */
# ifdef ALLOW_OBCS_SOUTH
        IF ( OBSptrFile(iTracer) .NE. ' '  )
     &       CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     O       OBSptr (1-Olx,1,1,1,iTracer),
     I       OBSptr0(1-Olx,1,1,1,iTracer),
     I       OBSptr1(1-Olx,1,1,1,iTracer), aWght, bWght, myThid )
# endif /* ALLOW_OBCS_SOUTH */
C     end do iTracer
       ENDDO
C     end if (usePTRACERS)
      ENDIF
#endif /* ALLOW_PTRACERS */
CMLC     endif for periodicForcing
CML      ENDIF

      RETURN
      END

CBOP
C     !ROUTINE: OBCS_EXTERNAL_FIELDS_INTERP_XZ
C     !INTERFACE:
      SUBROUTINE OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     O     fld,
     I     fld0, fld1, aWght, bWght, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_EXTERNAL_FIELDS_INTERP_XZ
C     | o Interpolate between to records
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     aWght, bWght :: Interpolation weights
      INTEGER myThid
      _RL aWght,bWght
      _RL fld (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL fld0(1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL fld1(1-Olx:sNx+Olx,Nr,nSx,nSy)

C     !LOCAL VARIABLES:
C     === Local arrays ===
C     bi,bj,i,j :: loop counters
      INTEGER bi,bj,i,k
CEOP
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K = 1, Nr
          DO i=1-Olx,sNx+Olx
           fld(i,k,bi,bj)   = bWght*fld0(i,k,bi,bj)
     &                       +aWght*fld1(i,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      RETURN
      END
CBOP
C     !ROUTINE: OBCS_EXTERNAL_FIELDS_INTERP_YZ
C     !INTERFACE:
      SUBROUTINE OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     O     fld,
     I     fld0, fld1, aWght, bWght, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_EXTERNAL_FIELDS_INTERP_YZ
C     | o Interpolate between to records
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     aWght, bWght :: Interpolation weights
      INTEGER myThid
      _RL aWght,bWght
      _RL fld (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL fld0(1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL fld1(1-Oly:sNy+Oly,Nr,nSx,nSy)

C     !LOCAL VARIABLES:
C     === Local arrays ===
C     bi,bj,i,j :: loop counters
      INTEGER bi,bj,j,k
CEOP
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K = 1, Nr
          DO j=1-Oly,sNy+Oly
           fld(j,k,bi,bj)   = bWght*fld0(j,k,bi,bj)
     &                       +aWght*fld1(j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO	

#endif /* ALLOW_OBCS AND ALLOW_OBCS_PRESCRIBE AND .NOT. ALLOW_EXF */

       RETURN
       END
