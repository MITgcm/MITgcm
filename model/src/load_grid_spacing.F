C $Header: /u/gcmpack/MITgcm/model/src/load_grid_spacing.F,v 1.1 2005/07/31 22:07:48 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: LOAD_GRID_SPACING
C     !INTERFACE:
      SUBROUTINE LOAD_GRID_SPACING( myThid )

C     !DESCRIPTION:
C     load grid-spacing (vector array) delX, delY or delR from file.
C     notes: loading of delR from file not yet implemented
      
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid - Number of this instance of INI_PARMS
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     tmp4delX  :: temporary arrays to read in delX
C     tmp8delX  :: temporary arrays to read in delX
C     tmp4delY  :: temporary arrays to read in delY
C     tmp8delY  :: temporary arrays to read in delY
C     msgBuf    :: Informational/error meesage buffer
C     iUnit     :: Work variable for IO unit number
C     i, j      :: Loop counters
      REAL*4 tmp4delX(Nx), tmp4delY(Ny)
      REAL*8 tmp8delX(Nx), tmp8delY(Ny)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i, j, iLen, iUnit
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     X coordinate
      IF ( delXFile .NE. ' ' ) THEN
        _BEGIN_MASTER( myThid )
        CALL MDSFINDUNIT( iUnit, myThid )
        iLen = ILNBLNK(delXFile)
        IF (readBinaryPrec.EQ.precFloat32) THEN
         OPEN(iUnit, FILE=delXFile(1:iLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=WORDLENGTH*Nx)
         READ(iUnit,rec=1) tmp4delX
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR4( Nx, tmp4delX )
#endif
         DO i=1,Nx
           delX(i) = tmp4delX(i)
         ENDDO
        ELSEIF (readBinaryPrec.EQ.precFloat64) THEN
         OPEN(iUnit, FILE=delXFile(1:iLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=WORDLENGTH*2*Nx)
         READ(iUnit,rec=1) tmp8delX
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR8( Nx, tmp8delX )
#endif
         DO i=1,Nx
           delX(i) = tmp8delX(i)
         ENDDO
        ENDIF
        WRITE(msgBuf,'(3A)') 'S/R LOAD_GRID_SPACING:',
     &    ' delX loaded from file: ', delXFile(1:iLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        _END_MASTER(myThid)
      ENDIF

C     Y coordinate
      IF ( delYFile .NE. ' ' ) THEN
        _BEGIN_MASTER( myThid )
        CALL MDSFINDUNIT( iUnit, myThid )
        iLen = ILNBLNK(delYFile)
        IF (readBinaryPrec.EQ.precFloat32) THEN
         OPEN(iUnit, FILE=delYFile(1:iLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=WORDLENGTH*Ny)
         READ(iUnit,rec=1) tmp4delY
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR4( Ny, tmp4delY )
#endif
         DO j=1,Ny
           delY(j) = tmp4delY(j)
         ENDDO
        ELSEIF (readBinaryPrec.EQ.precFloat64) THEN
         OPEN(iUnit, FILE=delYFile(1:iLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=WORDLENGTH*2*Ny)
         READ(iUnit,rec=1) tmp8delY
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR8( Ny, tmp8delY )
#endif
         DO j=1,Ny
           delY(j) = tmp8delY(j)
         ENDDO
        ENDIF
        WRITE(msgBuf,'(3A)') 'S/R LOAD_GRID_SPACING:',
     &    ' delY loaded from file: ', delYFile(1:iLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        _END_MASTER(myThid)
      ENDIF

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
