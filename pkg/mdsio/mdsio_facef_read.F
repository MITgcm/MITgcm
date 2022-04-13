#include "MDSIO_OPTIONS.h"

C--  File mdsio_read_facefile.F:
C--   Contents
C--   o MDS_FACEF_READ_RS
C--   o MDS_FACEF_READ_RL  <- not yet coded

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE:  MDS_FACEF_READ_RS
C     !INTERFACE:
      SUBROUTINE MDS_FACEF_READ_RS(
     I                    fName, fPrec, irec,
     U                    array,
     I                    bi,bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE MDS_FACEF_READ_RS
C     *==========================================================*
C     | Read 1 field from a file which contains all the data from
C     |  1 "face" (= piece of domain with rectangular topology)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      CHARACTER*(*) fName
      INTEGER fPrec
      INTEGER irec
      _RS array(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      INTEGER bi,bj, myThid
CEOP

C     !FUNCTIONS:
      INTEGER  MDS_RECLEN
      EXTERNAL MDS_RECLEN
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j, dUnit, iLen
      INTEGER length_of_rec
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_EXCH2
      INTEGER tN, dNx, dNy, tBx, tBy, tNx, tNY, jj, jBase
      Real*4 ioBuf4(1:sNx*nSx*nPx+1)
      Real*8 ioBuf8(1:sNx*nSx*nPx+1)
#else
      Real*4 ioBuf4(1:sNx+1,1:sNy+1)
      Real*8 ioBuf8(1:sNx+1,1:sNy+1)
#endif /* ALLOW_EXCH2 */

      iLen = ILNBLNK(fName)
#ifdef ALLOW_EXCH2
C     Figure out offset of tile within face
      tN  = W2_myTileList(bi,bj)
      dNx = exch2_mydnx(tN)
      dNy = exch2_mydny(tN)
      tBx = exch2_tbasex(tN)
      tBy = exch2_tbasey(tN)
      tNx = exch2_tnx(tN)
      tNy = exch2_tny(tN)

      CALL MDSFINDUNIT( dUnit, myThid )
      length_of_rec = MDS_RECLEN( fPrec, (dNx+1), myThid )
      OPEN( dUnit, file=fName(1:iLen), status='old', _READONLY_ACTION
     &             access='direct', recl=length_of_rec )
      j = 0
      jBase=(irec-1)*(dNy+1)
      IF ( fPrec.EQ.precFloat32 ) THEN
        DO jj=1+tBy,sNy+1+tBy
         READ(dUnit,rec=jj+jBase) (ioBuf4(i),i=1,dNx+1)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR4( (dNx+1), ioBuf4 )
#endif
         j = j+1
         DO i=1,sNx+1
          array(i,j,bi,bj) = ioBuf4(i+tBx)
         ENDDO
        ENDDO
      ELSEIF ( fPrec.EQ.precFloat64 ) THEN
        DO jj=1+tBy,sNy+1+tBy
         READ(dUnit,rec=jj+jBase) (ioBuf8(i),i=1,dNx+1)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR8( (dNx+1), ioBuf8 )
#endif
         j = j+1
         DO i=1,sNx+1
          array(i,j,bi,bj) = ioBuf8(i+tBx)
         ENDDO
        ENDDO
      ELSE
        WRITE(msgBuf,'(A,I8,A)')  ' MDS_FACEF_READ_RS:',
     &             fPrec, ' = illegal value for fPrec'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R MDS_FACEF_READ_RS'
      ENDIF
      CLOSE( dUnit )

#else /* ALLOW_EXCH2 */

      CALL MDSFINDUNIT( dUnit, myThid )
      length_of_rec = MDS_RECLEN( fPrec, (sNx+1)*(sNy+1), myThid )
      OPEN( dUnit, file=fName(1:iLen), status='old', _READONLY_ACTION
     &             access='direct', recl=length_of_rec )
      IF ( fPrec.EQ.precFloat32 ) THEN
        READ(dUnit, rec=irec) ioBuf4
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR4( (sNx+1)*(sNy+1), ioBuf4 )
#endif
        DO j=1,sNy+1
         DO i=1,sNx+1
          array(i,j,bi,bj) = ioBuf4(i,j)
         ENDDO
        ENDDO
      ELSEIF ( fPrec.EQ.precFloat64 ) THEN
        READ(dUnit, rec=irec) ioBuf8
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( (sNx+1)*(sNy+1), ioBuf8 )
#endif
        DO j=1,sNy+1
         DO i=1,sNx+1
          array(i,j,bi,bj) = ioBuf8(i,j)
         ENDDO
        ENDDO
      ELSE
        WRITE(msgBuf,'(A,I8,A)')  ' MDS_FACEF_READ_RS:',
     &             fPrec, ' = illegal value for fPrec'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R MDS_FACEF_READ_RS'
      ENDIF
      CLOSE( dUnit )

#endif /* ALLOW_EXCH2 */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
