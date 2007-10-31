C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_readparms.F,v 1.17 2007/10/31 21:53:49 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_READPARMS                                |
C     | o Routine to initialize OBCS variables and constants.    |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"
#ifdef ALLOW_ORLANSKI
#include "ORLANSKI.h"
#endif
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_OBCS

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER I,J,iTracer

      NAMELIST /OBCS_PARM01/
     &          OB_Jnorth,OB_Jsouth,OB_Ieast,OB_Iwest,
     &          useOrlanskiNorth,useOrlanskiSouth,
     &          useOrlanskiEast,useOrlanskiWest,
     &          OBNuFile,OBNvFile,OBNtFile,OBNsFile,OBNaFile,OBNhFile,
     &          OBSuFile,OBSvFile,OBStFile,OBSsFile,OBSaFile,OBShFile,
     &          OBEuFile,OBEvFile,OBEtFile,OBEsFile,OBEaFile,OBEhFile,
     &          OBWuFile,OBWvFile,OBWtFile,OBWsFile,OBWaFile,OBWhFile,
     &          OBNslFile,OBSslFile,OBEslFile,OBWslFile,
     &          OBNsnFile,OBSsnFile,OBEsnFile,OBWsnFile,
     &          OBNuiceFile,OBSuiceFile,OBEuiceFile,OBWuiceFile,
     &          OBNviceFile,OBSviceFile,OBEviceFile,OBWviceFile,
     &          useOBCSsponge, useOBCSbalance, useOBCSprescribe,
     &          OBCSprintDiags
#ifdef ALLOW_PTRACERS
     &        , OBNptrFile,OBSptrFile,OBEptrFile,OBWptrFile
#endif

#ifdef ALLOW_ORLANSKI
      NAMELIST /OBCS_PARM02/
     & CMAX, cvelTimeScale, CFIX, useFixedCEast, useFixedCWest
#endif

#ifdef ALLOW_OBCS_SPONGE
      NAMELIST /OBCS_PARM03/
     &          Urelaxobcsinner,Urelaxobcsbound,
     &          Vrelaxobcsinner,Vrelaxobcsbound,
     &          spongeThickness
#endif

      _BEGIN_MASTER(myThid)

C--   OBCS_READPARMS has been called so we know that
C     the package is active.
      OBCSIsOn=.TRUE.

      WRITE(msgBuf,'(A)') ' OBCS_READPARMS: opening data.obcs'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.obcs', 'OBCS_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Default flags and values for OBCS
      DO I=1,Nx
       OB_Jnorth(I)=0
       OB_Jsouth(I)=0
      ENDDO
      DO J=1,Ny
       OB_Ieast(J)=0
       OB_Iwest(J)=0
      ENDDO
      useOrlanskiNorth=.FALSE.
      useOrlanskiSouth=.FALSE.
      useOrlanskiEast =.FALSE.
      useOrlanskiWest =.FALSE.
      useOBCSsponge   =.FALSE.
      useOBCSbalance  =.FALSE.
      useOBCSprescribe=.FALSE.
      OBCSprintDiags  =.TRUE.

      OBNuFile = ' '
      OBNvFile = ' '
      OBNtFile = ' '
      OBNsFile = ' '
      OBNaFile = ' '
      OBNslFile = ' '
      OBNsnFile = ' '
      OBNuiceFile = ' '
      OBNviceFile = ' '
      OBNhFile = ' '
      OBSuFile = ' '
      OBSvFile = ' '
      OBStFile = ' '
      OBSsFile = ' '
      OBSaFile = ' '
      OBShFile = ' '
      OBSslFile = ' '
      OBSsnFile = ' '
      OBSuiceFile = ' '
      OBSviceFile = ' '
      OBEuFile = ' '
      OBEvFile = ' '
      OBEtFile = ' '
      OBEsFile = ' '
      OBEaFile = ' '
      OBEhFile = ' '
      OBEslFile = ' '
      OBEsnFile = ' '
      OBEuiceFile = ' '
      OBEviceFile = ' '
      OBWuFile = ' '
      OBWvFile = ' '
      OBWtFile = ' '
      OBWsFile = ' '
      OBWaFile = ' '
      OBWhFile = ' '
      OBWslFile = ' '
      OBWsnFile = ' '
      OBWuiceFile = ' '
      OBWviceFile = ' '
#ifdef ALLOW_PTRACERS
      DO iTracer = 1, PTRACERS_num
       OBNptrFile(iTracer) = ' '
       OBSptrFile(iTracer) = ' '
       OBEptrFile(iTracer) = ' '
       OBWptrFile(iTracer) = ' '
      ENDDO
#endif

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=OBCS_PARM01)

C     Account for periodicity if negative indices were supplied
      DO J=1,Ny
       IF (OB_Ieast(J).lt.0) OB_Ieast(J)=OB_Ieast(J)+Nx+1
      ENDDO
      DO I=1,Nx
       IF (OB_Jnorth(I).lt.0) OB_Jnorth(I)=OB_Jnorth(I)+Ny+1
      ENDDO
      write(*,*) 'OB Jn =',OB_Jnorth
      write(*,*) 'OB Js =',OB_Jsouth
      write(*,*) 'OB Ie =',OB_Ieast
      write(*,*) 'OB Iw =',OB_Iwest

#ifdef ALLOW_ORLANSKI
C     Default Orlanski radiation parameters
      CMAX = 0.45 _d 0 /* maximum allowable phase speed-CFL for AB-II */
      cvelTimeScale = 2000.0 _d 0 /* Averaging period for phase speed in sec. */
      CFIX = 0.8 _d 0 /* Fixed boundary phase speed in m/s */
      useFixedCEast=.FALSE.
      useFixedCWest=.FALSE.
      IF (useOrlanskiNorth.OR.
     &    useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.
     &    useOrlanskiWest)
     & READ(UNIT=iUnit,NML=OBCS_PARM02)
#endif

#ifdef ALLOW_OBCS_SPONGE
C     Default sponge layer parameters:
C     sponge layer is turned off by default
      spongeThickness = 0
      Urelaxobcsinner = 0. _d 0
      Urelaxobcsbound = 0. _d 0
      Vrelaxobcsinner = 0. _d 0
      Vrelaxobcsbound = 0. _d 0
CML this was the previous default in units of days
CML      spongeThickness = 2
CML      Urelaxobcsinner = 5. _d 0
CML      Urelaxobcsbound = 1. _d 0
CML      Vrelaxobcsinner = 5. _d 0
CML      Vrelaxobcsbound = 1. _d 0
      IF (useOBCSsponge)
     & READ(UNIT=iUnit,NML=OBCS_PARM03)
#endif

      WRITE(msgBuf,'(A)') ' OBCS_READPARMS: finished reading data.obcs'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_OBCS */
      RETURN
      END
