C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_readparms.F,v 1.2 2001/02/02 21:36:30 adcroft Exp $
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

      _RL Orlanski_Cmax,Orlanski_TimeScale
      NAMELIST /OBCS_PARM01/
     &          OB_Jnorth,OB_Jsouth,OB_Ieast,OB_Iwest,
     &          useOrlanskiNorth,useOrlanskiSouth,
     &          useOrlanskiEast,useOrlanskiWest,
     &          Orlanski_Cmax,Orlanski_TimeScale

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

C--   Defaults for the Orlanksi package
      Orlanski_Cmax=0.
      Orlanski_TimeScale=0.

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=OBCS_PARM01)

C     Account for periodicity if negative indices were supplied
      DO J=1,Ny
       IF (OB_Ieast(J).lt.0) OB_Ieast(J)=OB_Ieast(J)+Nx+1
      ENDDO
      DO I=1,Nx
       IF (OB_Jnorth(I).lt.0) OB_Jnorth(I)=OB_Jnorth(I)+Ny+1
      ENDDO
      write(0,*) 'OB Jn =',OB_Jnorth
      write(0,*) 'OB Js =',OB_Jsouth
      write(0,*) 'OB Ie =',OB_Ieast
      write(0,*) 'OB Iw =',OB_Iwest

#ifndef ALLOW_ORLANSKI
      IF (
     &    Orlanski_Cmax.NE.0. .OR.
     &    Orlanski_TimeScale.NE.0.) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_READPARMS: Orlanski parameters were set!'
        CALL PRINT_ERROR( msgBuf, 1)
        STOP 'OBCS_READPARMS: Inconsistent CPP options and parameters'
      ENDIF
#else
      Cmax=Orlanski_Cmax
      cVelTimeScale=Orlanski_TimeScale
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
