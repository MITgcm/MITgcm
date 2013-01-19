C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_wr_rec_rl.F,v 1.5 2013/01/19 23:47:01 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MDS_WR_REC_RL

C !INTERFACE:
      SUBROUTINE MDS_WR_REC_RL(
     I                          arr,
     O                          r4Buf, r8Buf,
     I                          fPrec, dUnit, iRec, nArr, myThid )

C !DESCRIPTION:
C Write one reccord to already opened io-unit "dUnit", from RL array "arr"

C !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

C !INPUT PARAMETERS:
C   arr     RL     :: vector array to write
C   fPrec  integer :: file precision
C   dUnit  integer :: 'Opened' I/O channel
C   iRec   integer :: record number to WRITE
C   nArr   integer :: dimension off array "arr"
C   myThid integer :: my Thread Id number
C !OUTPUT PARAMETERS:
C   r4Buf  real*4  :: buffer array
C   r8Buf  real*8  :: buffer array
      INTEGER fPrec
      INTEGER dUnit
      INTEGER iRec
      INTEGER nArr
      INTEGER myThid
      _RL    arr(nArr)
      Real*4 r4Buf(nArr)
      Real*8 r8Buf(nArr)
CEOP

C !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER k

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( debugLevel.GE.debLevD ) THEN
        WRITE(msgBuf,'(A,I9,2x,I9)')
     &      ' MDS_WR_REC_RL: iRec,Dim = ', iRec, nArr
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF

      IF ( fPrec.EQ.precFloat32 ) THEN
        DO k=1,nArr
          r4Buf(k) = arr(k)
        ENDDO
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR4( nArr, r4Buf )
#endif
        WRITE( dUnit, rec=iRec ) r4Buf
      ELSEIF ( fPrec.EQ.precFloat64 ) THEN
        DO k=1,nArr
          r8Buf(k) = arr(k)
        ENDDO
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( nArr, r8Buf )
#endif
        WRITE( dUnit, rec=iRec ) r8Buf
      ELSE
        WRITE(msgBuf,'(A,I9)')
     &        ' MDS_WR_REC_RL: illegal value for fPrec=',fPrec
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R MDS_WR_REC_RL'
      ENDIF

      RETURN
      END
