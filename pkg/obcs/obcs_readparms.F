C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_readparms.F,v 1.5 2001/05/14 21:36:45 heimbach Exp $
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

C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_OBCS

      NAMELIST /OBCS_PARM01/
     &          OB_Jnorth,OB_Jsouth,OB_Ieast,OB_Iwest,
     &          useOrlanskiNorth,useOrlanskiSouth,
     &          useOrlanskiEast,useOrlanskiWest,
     &          OBNuFile,OBNvFile,OBNtFile,OBNsFile,
     &          OBSuFile,OBSvFile,OBStFile,OBSsFile,
     &          OBEuFile,OBEvFile,OBEtFile,OBEsFile,
     &          OBWuFile,OBWvFile,OBWtFile,OBWsFile

#ifdef ALLOW_ORLANSKI
      NAMELIST /OBCS_PARM02/
     & CMAX, cvelTimeScale
#endif

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER I,J

C--   OBCS_READPARMS has been called so we know that
C     the package is active.
      OBCSIsOn=.TRUE.

      _BEGIN_MASTER(myThid)

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

      OBNuFile = ' '
      OBNvFile = ' '
      OBNtFile = ' '
      OBNsFile = ' '
      OBSuFile = ' '
      OBSvFile = ' '
      OBStFile = ' '
      OBSsFile = ' '
      OBEuFile = ' '
      OBEvFile = ' '
      OBEtFile = ' '
      OBEsFile = ' '
      OBWuFile = ' '
      OBWvFile = ' '
      OBWtFile = ' '
      OBWsFile = ' '

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
      IF (useOrlanskiNorth.OR.
     &    useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.
     &    useOrlanskiWest) 
     & READ(UNIT=iUnit,NML=OBCS_PARM02)
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
