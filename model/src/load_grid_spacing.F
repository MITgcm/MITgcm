C $Header: /u/gcmpack/MITgcm/model/src/load_grid_spacing.F,v 1.2 2006/11/29 04:39:06 jmc Exp $
C $Name:  $

c #include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: LOAD_GRID_SPACING
C     !INTERFACE:
      SUBROUTINE LOAD_GRID_SPACING( myThid )

C     !DESCRIPTION:
C     load grid-spacing (vector array) delX, delY, delR or delRc from file.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid    :: my Thread Id. number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     tmp4delX  :: temporary arrays to read in delX
C     tmp8delX  :: temporary arrays to read in delX
C     tmp4delY  :: temporary arrays to read in delY
C     tmp8delY  :: temporary arrays to read in delY
C     tmp4delR  :: temporary arrays to read in delR
C     tmp8delR  :: temporary arrays to read in delR
C     tmp4delRc :: temporary arrays to read in delRc
C     tmp8delRc :: temporary arrays to read in delRc
C     msgBuf    :: Informational/error meesage buffer
C     iUnit     :: Work variable for IO unit number
C     rcLen1    :: record length of 1 element to read
C     i, j, k   :: Loop counters
      REAL*4 tmp4delX(Nx), tmp4delY(Ny), tmp4delR(Nr), tmp4delRc(Nr+1)
      REAL*8 tmp8delX(Nx), tmp8delY(Ny), tmp8delR(Nr), tmp8delRc(Nr+1)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i, j, k, iLen, iUnit, rcLen1
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER( myThid )
      rcLen1 = WORDLENGTH
      IF (readBinaryPrec.EQ.precFloat64) rcLen1 = WORDLENGTH*2

C     X coordinate
      IF ( delXFile .NE. ' ' ) THEN
        CALL MDSFINDUNIT( iUnit, myThid )
        iLen = ILNBLNK(delXFile)
        IF (readBinaryPrec.EQ.precFloat32) THEN
         OPEN(iUnit, FILE=delXFile(1:iLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=rcLen1*Nx)
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
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=rcLen1*Nx)
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
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF

C     Y coordinate
      IF ( delYFile .NE. ' ' ) THEN
        CALL MDSFINDUNIT( iUnit, myThid )
        iLen = ILNBLNK(delYFile)
        IF (readBinaryPrec.EQ.precFloat32) THEN
         OPEN(iUnit, FILE=delYFile(1:iLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=rcLen1*Ny)
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
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=rcLen1*Ny)
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
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF

C     vertical coordinate "R"
      IF ( delRFile .NE. ' ' ) THEN
        CALL MDSFINDUNIT( iUnit, myThid )
        iLen = ILNBLNK(delRFile)
        IF (readBinaryPrec.EQ.precFloat32) THEN
         OPEN(iUnit, FILE=delRFile(1:iLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=rcLen1*Nr)
         READ(iUnit,rec=1) tmp4delR
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR4( Nr, tmp4delR )
#endif
         DO k=1,Nr
           delR(k) = tmp4delR(k)
         ENDDO
        ELSEIF (readBinaryPrec.EQ.precFloat64) THEN
         OPEN(iUnit, FILE=delRFile(1:iLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=rcLen1*Nr)
         READ(iUnit,rec=1) tmp8delR
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR8( Nr, tmp8delR )
#endif
         DO k=1,Nr
           delR(k) = tmp8delR(k)
         ENDDO
        ENDIF
        WRITE(msgBuf,'(3A)') 'S/R LOAD_GRID_SPACING:',
     &    ' delR loaded from file: ', delRFile(1:iLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF

      IF ( delRcFile .NE. ' ' ) THEN
        CALL MDSFINDUNIT( iUnit, myThid )
        iLen = ILNBLNK(delRcFile)
        IF (readBinaryPrec.EQ.precFloat32) THEN
         OPEN(iUnit, FILE=delRcFile(1:iLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=rcLen1*(Nr+1))
         READ(iUnit,rec=1) tmp4delRc
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR4( Nr+1, tmp4delRc )
#endif
         DO k=1,Nr+1
           delRc(k) = tmp4delRc(k)
         ENDDO
        ELSEIF (readBinaryPrec.EQ.precFloat64) THEN
         OPEN(iUnit, FILE=delRcFile(1:iLen), STATUS='OLD',
     &        FORM='UNFORMATTED',ACCESS='DIRECT',RECL=rcLen1*(Nr+1))
         READ(iUnit,rec=1) tmp8delRc
         CLOSE(iUnit)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR8( Nr+1, tmp8delRc )
#endif
         DO k=1,Nr+1
           delRc(k) = tmp8delRc(k)
         ENDDO
        ENDIF
        WRITE(msgBuf,'(3A)') 'S/R LOAD_GRID_SPACING:',
     &    ' delRc loaded from file: ', delRcFile(1:iLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF

      _END_MASTER(myThid)
C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
