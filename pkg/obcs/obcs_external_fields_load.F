C $Header: /u/gcmpack/MITgcm/pkg/obcs/Attic/obcs_external_fields_load.F,v 1.16 2009/12/15 14:13:44 mlosch Exp $
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
#include "OBCS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "OBCS_PTRACERS.h"
#include "PTRACERS_PARAMS.h"
c#include "PTRACERS_FIELDS.h"
#endif /* ALLOW_PTRACERS */

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     if external forcing (exf) package is enabled (useEXF=T), all loading of
C     external fields is done by exf
#if (defined ALLOW_OBCS && defined ALLOW_OBCS_PRESCRIBE )

C     !LOCAL VARIABLES:
C     === Local arrays ===
C     aWght, bWght :: Interpolation weights
C     msgBuf       :: Informational/error message buffer
#ifdef NONLIN_FRSURF
      INTEGER i,j,bi,bj
#endif /* NONLIN_FRSURF */
      INTEGER fp
      INTEGER iRec0, iRec1, iTr
      _RL aWght,bWght,rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      fp = readBinaryPrec

      IF ( periodicExternalForcing ) THEN

C Now calculate whether it is time to update the forcing arrays
      rdt = 1. _d 0 / deltaTclock
      nForcingPeriods = NINT(externForcingCycle/externForcingPeriod)
      Imytm = NINT(myTime*rdt)
      Ifprd = NINT(externForcingPeriod*rdt)
      Ifcyc = NINT(externForcingCycle*rdt)
      Iftm  = MOD( Imytm+Ifcyc-Ifprd/2, Ifcyc)

      iRec0 = 1 + INT(Iftm/Ifprd)
      iRec1 = 1 + MOD(iRec0,nForcingPeriods)
c     aWght = DFLOAT( Iftm-Ifprd*(iRec0 - 1) ) / DFLOAT( Ifprd )
      aWght = FLOAT( Iftm-Ifprd*(iRec0 - 1) )
      bWght = FLOAT( Ifprd )
      aWght =  aWght / bWght
      bWght = 1. _d 0 - aWght

      IF (
     &  Iftm-Ifprd*(iRec0-1) .EQ. 0
     &  .OR. myIter .EQ. nIter0
     & ) THEN

#ifndef ALLOW_MDSIO
       STOP 'ABNORMAL END: OBCS_EXTERNAL_FIELDS_LOAD: NEEDS MSDIO PKG'
#endif /* ALLOW_MDSIO */

       _BARRIER

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
       WRITE(msgBuf,'(1X,A,2I5,I10,1P1E20.12)')
     &  'OBCS_EXTERNAL_FIELDS_LOAD: Reading new data:',
     &  iRec0, iRec1, myIter, myTime
       CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)

#ifdef ALLOW_OBCS_EAST
C     Eastern boundary
      IF ( OBEuFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL( OBEuFile, fp,Nr,OBEu0,iRec0,myIter,myThid )
       CALL READ_REC_YZ_RL( OBEuFile, fp,Nr,OBEu1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBEvFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL( OBEvFile, fp,Nr,OBEv0,iRec0,myIter,myThid )
       CALL READ_REC_YZ_RL( OBEvFile, fp,Nr,OBEv1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBEtFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL( OBEtFile, fp,Nr,OBEt0,iRec0,myIter,myThid )
       CALL READ_REC_YZ_RL( OBEtFile, fp,Nr,OBEt1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBEsFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL( OBEsFile, fp,Nr,OBEs0,iRec0,myIter,myThid )
       CALL READ_REC_YZ_RL( OBEsFile, fp,Nr,OBEs1,iRec1,myIter,myThid )
      ENDIF
# ifdef ALLOW_NONHYDROSTATIC
      IF ( OBEwFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL( OBEwFile, fp,Nr,OBEw0,iRec0,myIter,myThid )
       CALL READ_REC_YZ_RL( OBEwFile, fp,Nr,OBEw1,iRec1,myIter,myThid )
      ENDIF
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
      IF ( OBEetaFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL(OBEetaFile,fp,1,OBEeta0,iRec0,myIter,myThid)
       CALL READ_REC_YZ_RL(OBEetaFile,fp,1,OBEeta1,iRec1,myIter,myThid)
      ENDIF
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
C     Western boundary
      IF ( OBWuFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL( OBWuFile, fp,Nr,OBWu0,iRec0,myIter,myThid )
       CALL READ_REC_YZ_RL( OBWuFile, fp,Nr,OBWu1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBWvFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL( OBWvFile, fp,Nr,OBWv0,iRec0,myIter,myThid )
       CALL READ_REC_YZ_RL( OBWvFile, fp,Nr,OBWv1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBWtFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL( OBWtFile, fp,Nr,OBWt0,iRec0,myIter,myThid )
       CALL READ_REC_YZ_RL( OBWtFile, fp,Nr,OBWt1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBWsFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL( OBWsFile, fp,Nr,OBWs0,iRec0,myIter,myThid )
       CALL READ_REC_YZ_RL( OBWsFile, fp,Nr,OBWs1,iRec1,myIter,myThid )
      ENDIF
# ifdef ALLOW_NONHYDROSTATIC
      IF ( OBWwFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL( OBWwFile, fp,Nr,OBWw0,iRec0,myIter,myThid )
       CALL READ_REC_YZ_RL( OBWwFile, fp,Nr,OBWw1,iRec1,myIter,myThid )
      ENDIF
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
      IF ( OBWetaFile .NE. ' '  ) THEN
       CALL READ_REC_YZ_RL(OBWetaFile,fp,1,OBWeta0,iRec0,myIter,myThid)
       CALL READ_REC_YZ_RL(OBWetaFile,fp,1,OBWeta1,iRec1,myIter,myThid)
      ENDIF
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_NORTH
C     Northern boundary
      IF ( OBNuFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL( OBNuFile, fp,Nr,OBNu0,iRec0,myIter,myThid )
       CALL READ_REC_XZ_RL( OBNuFile, fp,Nr,OBNu1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBNvFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL( OBNvFile, fp,Nr,OBNv0,iRec0,myIter,myThid )
       CALL READ_REC_XZ_RL( OBNvFile, fp,Nr,OBNv1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBNtFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL( OBNtFile, fp,Nr,OBNt0,iRec0,myIter,myThid )
       CALL READ_REC_XZ_RL( OBNtFile, fp,Nr,OBNt1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBNsFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL( OBNsFile, fp,Nr,OBNs0,iRec0,myIter,myThid )
       CALL READ_REC_XZ_RL( OBNsFile, fp,Nr,OBNs1,iRec1,myIter,myThid )
      ENDIF
# ifdef ALLOW_NONHYDROSTATIC
      IF ( OBNwFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL( OBNwFile, fp,Nr,OBNw0,iRec0,myIter,myThid )
       CALL READ_REC_XZ_RL( OBNwFile, fp,Nr,OBNw1,iRec1,myIter,myThid )
      ENDIF
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
      IF ( OBNetaFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL(OBNetaFile,fp,1,OBNeta0,iRec0,myIter,myThid)
       CALL READ_REC_XZ_RL(OBNetaFile,fp,1,OBNeta1,iRec1,myIter,myThid)
      ENDIF
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
C     Southern boundary
      IF ( OBSuFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL( OBSuFile, fp,Nr,OBSu0,iRec0,myIter,myThid )
       CALL READ_REC_XZ_RL( OBSuFile, fp,Nr,OBSu1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBSvFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL( OBSvFile, fp,Nr,OBSv0,iRec0,myIter,myThid )
       CALL READ_REC_XZ_RL( OBSvFile, fp,Nr,OBSv1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBStFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL( OBStFile, fp,Nr,OBSt0,iRec0,myIter,myThid )
       CALL READ_REC_XZ_RL( OBStFile, fp,Nr,OBSt1,iRec1,myIter,myThid )
      ENDIF
      IF ( OBSsFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL( OBSsFile, fp,Nr,OBSs0,iRec0,myIter,myThid )
       CALL READ_REC_XZ_RL( OBSsFile, fp,Nr,OBSs1,iRec1,myIter,myThid )
      ENDIF
# ifdef ALLOW_NONHYDROSTATIC
      IF ( OBSwFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL( OBSwFile, fp,Nr,OBSw0,iRec0,myIter,myThid )
       CALL READ_REC_XZ_RL( OBSwFile, fp,Nr,OBSw1,iRec1,myIter,myThid )
      ENDIF
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
      IF ( OBSetaFile .NE. ' '  ) THEN
       CALL READ_REC_XZ_RL(OBSetaFile,fp,1,OBSeta0,iRec0,myIter,myThid)
       CALL READ_REC_XZ_RL(OBSetaFile,fp,1,OBSeta1,iRec1,myIter,myThid)
      ENDIF
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_PTRACERS
      IF (usePTRACERS) THEN
C     read boundary values for passive tracers
       DO iTr = 1, PTRACERS_numInUse
# ifdef ALLOW_OBCS_EAST
C     Eastern boundary
        IF ( OBEptrFile(iTr) .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBEptrFile(iTr), fp, Nr,
     &                OBEptr0(1-Oly,1,1,1,iTr), iRec0, myIter, myThid )
         CALL READ_REC_YZ_RL( OBEptrFile(iTr), fp, Nr,
     &                OBEptr1(1-Oly,1,1,1,iTr), iRec1, myIter, myThid )
        ENDIF
# endif /* ALLOW_OBCS_EAST */
# ifdef ALLOW_OBCS_WEST
C     Western boundary
        IF ( OBWptrFile(iTr) .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBWptrFile(iTr), fp, Nr,
     &                OBWptr0(1-Oly,1,1,1,iTr), iRec0, myIter, myThid )
         CALL READ_REC_YZ_RL( OBWptrFile(iTr), fp, Nr,
     &                OBWptr1(1-Oly,1,1,1,iTr), iRec1, myIter, myThid )
        ENDIF
# endif /* ALLOW_OBCS_WEST */
# ifdef ALLOW_OBCS_NORTH
C     Northern boundary
        IF ( OBNptrFile(iTr) .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBNptrFile(iTr), fp, Nr,
     &                OBNptr0(1-Oly,1,1,1,iTr), iRec0, myIter, myThid )
         CALL READ_REC_XZ_RL( OBNptrFile(iTr), fp, Nr,
     &                OBNptr1(1-Oly,1,1,1,iTr), iRec1, myIter, myThid )
        ENDIF
# endif /* ALLOW_OBCS_NORTH */
# ifdef ALLOW_OBCS_SOUTH
C     Southern boundary
        IF ( OBSptrFile(iTr) .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBSptrFile(iTr), fp, Nr,
     &                OBSptr0(1-Oly,1,1,1,iTr), iRec0, myIter, myThid )
         CALL READ_REC_XZ_RL( OBSptrFile(iTr), fp, Nr,
     &                OBSptr1(1-Oly,1,1,1,iTr), iRec1, myIter, myThid )
        ENDIF
# endif /* ALLOW_OBCS_SOUTH */
C     end do iTr
       ENDDO
C     end if (usePTRACERS)
      ENDIF
#endif /* ALLOW_PTRACERS */

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
#ifndef ALLOW_MDSIO
         STOP 'ABNORMAL END: OBCS_EXTERNAL_FIELDS_LOAD: NEEDS MSDIO PKG'
#endif /* ALLOW_MDSIO */
        _BARRIER
C      Read constant boundary conditions only for myIter = nIter0
        WRITE(msgBuf,'(1X,A,I10,1P1E20.12)')
     &       'OBCS_EXTERNAL_FIELDS_LOAD: Reading initial data:',
     &       myIter, myTime
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &       SQUEEZE_RIGHT,myThid)
        iRec0 = 1

#ifdef ALLOW_OBCS_EAST
C     Eastern boundary
        IF ( OBEuFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBEuFile,fp,Nr,OBEu0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBEvFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBEvFile,fp,Nr,OBEv0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBEtFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBEtFile,fp,Nr,OBEt0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBEsFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBEsFile,fp,Nr,OBEs0,iRec0,myIter,myThid )
        ENDIF
# ifdef ALLOW_NONHYDROSTATIC
        IF ( OBEwFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBEwFile,fp,Nr,OBEw0,iRec0,myIter,myThid )
        ENDIF
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
        IF ( OBEetaFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBEetaFile, fp, 1, OBEeta0, iRec0,
     &                        myIter, myThid )
        ENDIF
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
C     Western boundary
        IF ( OBWuFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBWuFile,fp,Nr,OBWu0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBWvFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBWvFile,fp,Nr,OBWv0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBWtFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBWtFile,fp,Nr,OBWt0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBWsFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBWsFile,fp,Nr,OBWs0,iRec0,myIter,myThid )
        ENDIF
# ifdef ALLOW_NONHYDROSTATIC
        IF ( OBWwFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBWwFile,fp,Nr,OBWw0,iRec0,myIter,myThid )
        ENDIF
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
        IF ( OBWetaFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBWetaFile, fp, 1, OBWeta0, iRec0,
     &                        myIter, myThid )
        ENDIF
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_NORTH
C     Northern boundary
        IF ( OBNuFile .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBNuFile,fp,Nr,OBNu0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBNvFile .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBNvFile,fp,Nr,OBNv0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBNtFile .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBNtFile,fp,Nr,OBNt0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBNsFile .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBNsFile,fp,Nr,OBNs0,iRec0,myIter,myThid )
        ENDIF
# ifdef ALLOW_NONHYDROSTATIC
        IF ( OBNwFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBNwFile,fp,Nr,OBNw0,iRec0,myIter,myThid )
        ENDIF
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
        IF ( OBNetaFile .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBNetaFile, fp, 1, OBNeta0, iRec0,
     &                        myIter, myThid )
        ENDIF
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
C     Southern boundary
        IF ( OBSuFile .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBSuFile,fp,Nr,OBSu0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBSvFile .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBSvFile,fp,Nr,OBSv0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBStFile .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBStFile,fp,Nr,OBSt0,iRec0,myIter,myThid )
        ENDIF
        IF ( OBSsFile .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBSsFile,fp,Nr,OBSs0,iRec0,myIter,myThid )
        ENDIF
# ifdef ALLOW_NONHYDROSTATIC
        IF ( OBSwFile .NE. ' '  ) THEN
         CALL READ_REC_YZ_RL( OBSwFile,fp,Nr,OBSw0,iRec0,myIter,myThid )
        ENDIF
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
        IF ( OBSetaFile .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBSetaFile, fp, 1, OBSeta0, iRec0,
     &                        myIter, myThid )
        ENDIF
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_PTRACERS
        IF (usePTRACERS) THEN
C     read passive tracer boundary values
         DO iTr = 1, PTRACERS_numInUse
# ifdef ALLOW_OBCS_EAST
C     Eastern boundary
          IF ( OBEptrFile(iTr) .NE. ' '  ) THEN
           CALL READ_REC_YZ_RL( OBEptrFile(iTr), fp, Nr,
     &               OBEptr0(1-Oly,1,1,1,iTr), iRec0,myIter, myThid )
          ENDIF
# endif /* ALLOW_OBCS_EAST */
# ifdef ALLOW_OBCS_WEST
C     Western boundary
          IF ( OBWptrFile(iTr) .NE. ' '  ) THEN
           CALL READ_REC_YZ_RL( OBWptrFile(iTr), fp, Nr,
     &               OBWptr0(1-Oly,1,1,1,iTr), iRec0, myIter, myThid )
          ENDIF
# endif /* ALLOW_OBCS_WEST */
# ifdef ALLOW_OBCS_NORTH
C     Northern boundary
          IF ( OBNptrFile(iTr) .NE. ' '  ) THEN
           CALL READ_REC_XZ_RL( OBNptrFile(iTr), fp, Nr,
     &               OBNptr0(1-Oly,1,1,1,iTr), iRec0, myIter, myThid )
          ENDIF
# endif /* ALLOW_OBCS_NORTH */
# ifdef ALLOW_OBCS_SOUTH
C     Southern boundary
          IF ( OBSptrFile(iTr) .NE. ' '  ) THEN
           CALL READ_REC_XZ_RL( OBSptrFile(iTr), fp, Nr,
     &               OBSptr0(1-Oly,1,1,1,iTr), iRec0, myIter, myThid )
          ENDIF
# endif /* ALLOW_OBCS_SOUTH */
C     end do iTr
         ENDDO
C     end if (usePTRACERS)
        ENDIF
#endif /* ALLOW_PTRACERS */
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
# ifdef ALLOW_NONHYDROSTATIC
       IF ( OBEwFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     &      OBEw, OBEw0, OBEw1, aWght, bWght, myThid )
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
       IF ( OBEetaFile .NE. ' ' ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-Oly,sNy+Oly
           OBEeta(j,bi,bj) = bWght*OBEeta0(j,bi,bj)
     &                      +aWght*OBEeta1(j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDIF
# endif /* NONLIN_FRSURF */
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
# ifdef ALLOW_NONHYDROSTATIC
       IF ( OBWwFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     &      OBWw, OBWw0, OBWw1, aWght, bWght, myThid )
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
       IF ( OBWetaFile .NE. ' ' ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-Oly,sNy+Oly
           OBWeta(j,bi,bj) = bWght*OBWeta0(j,bi,bj)
     &                      +aWght*OBWeta1(j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDIF
# endif /* NONLIN_FRSURF */
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
# ifdef ALLOW_NONHYDROSTATIC
       IF ( OBNwFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     &      OBNw, OBNw0, OBNw1, aWght, bWght, myThid )
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
       IF ( OBNetaFile .NE. ' ' ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO i=1-Olx,sNx+Olx
           OBNeta(i,bi,bj) = bWght*OBNeta0(i,bi,bj)
     &                      +aWght*OBNeta1(i,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDIF
# endif /* NONLIN_FRSURF */
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
# ifdef ALLOW_NONHYDROSTATIC
       IF ( OBSwFile .NE. ' '  ) CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     &      OBSw, OBSw0, OBSw1, aWght, bWght, myThid )
# endif /* ALLOW_NONHYDROSTATIC */
# ifdef NONLIN_FRSURF
       IF ( OBSetaFile .NE. ' ' ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO i=1-Olx,sNx+Olx
           OBSeta(i,bi,bj) = bWght*OBSeta0(i,bi,bj)
     &                      +aWght*OBSeta1(i,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDIF
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_PTRACERS
      IF (usePTRACERS) THEN
C     "interpolate" passive tracer boundary values
       DO iTr = 1, PTRACERS_numInUse
# ifdef ALLOW_OBCS_EAST
        IF ( OBEptrFile(iTr) .NE. ' '  )
     &       CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     O       OBEptr (1-Oly,1,1,1,iTr),
     I       OBEptr0(1-Oly,1,1,1,iTr),
     I       OBEptr1(1-Oly,1,1,1,iTr), aWght, bWght, myThid )
# endif /* ALLOW_OBCS_EAST */
# ifdef ALLOW_OBCS_WEST
        IF ( OBWptrFile(iTr) .NE. ' '  )
     &       CALL OBCS_EXTERNAL_FIELDS_INTERP_YZ(
     O       OBWptr (1-Oly,1,1,1,iTr),
     I       OBWptr0(1-Oly,1,1,1,iTr),
     I       OBWptr1(1-Oly,1,1,1,iTr), aWght, bWght, myThid )
# endif /* ALLOW_OBCS_WEST */
# ifdef ALLOW_OBCS_NORTH
        IF ( OBNptrFile(iTr) .NE. ' '  )
     &       CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     O       OBNptr (1-Olx,1,1,1,iTr),
     I       OBNptr0(1-Olx,1,1,1,iTr),
     I       OBNptr1(1-Olx,1,1,1,iTr), aWght, bWght, myThid )
# endif /* ALLOW_OBCS_NORTH */
# ifdef ALLOW_OBCS_SOUTH
        IF ( OBSptrFile(iTr) .NE. ' '  )
     &       CALL OBCS_EXTERNAL_FIELDS_INTERP_XZ(
     O       OBSptr (1-Olx,1,1,1,iTr),
     I       OBSptr0(1-Olx,1,1,1,iTr),
     I       OBSptr1(1-Olx,1,1,1,iTr), aWght, bWght, myThid )
# endif /* ALLOW_OBCS_SOUTH */
C     end do iTr
       ENDDO
C     end if (usePTRACERS)
      ENDIF
#endif /* ALLOW_PTRACERS */

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

#endif /* ALLOW_OBCS AND ALLOW_OBCS_PRESCRIBE */

       RETURN
       END
