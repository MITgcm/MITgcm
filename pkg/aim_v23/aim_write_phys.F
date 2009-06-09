C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_write_phys.F,v 1.2 2009/06/09 22:44:02 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_WRITE_PHYS
C     !INTERFACE:
      SUBROUTINE AIM_WRITE_PHYS(
     I               pref, suff, nNr, field,
     I               kLev, bi, bj, iRec, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE AIM_WRITE_PHYS
C     | o Write variable from AIM physics common block
C     |   (=> no overlap & nThreads) and reverse K index.
C     *==========================================================*
C     | Note: assume symetry in tiles per thread treatment
C     *==========================================================*
C     !USES
      IMPLICIT NONE

C     == Global variables ===
#include "AIM_SIZE.h"

#include "EEPARAMS.h"
c #include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     pref   :: Prefix of the output file name
C     suff   :: Suffix of the output file name
C     nNr    :: 3rd dim. of the input field
C     field  :: Field (from aim-physics) to write
C     kLev   :: level index to write (0 = write all levels)
C     bi,bj  :: Tile index
C     iRec   :: reccord number in the output file
C     myIter :: Current iteration number in simulation
C     myThid :: my Thread Id number
      CHARACTER*(*) pref, suff
      INTEGER nNr
      _RL field(sNx,sNy,nNr,MAX_NO_THREADS)
      INTEGER kLev, bi, bj, iRec, myIter, myThid

#ifdef ALLOW_AIM

C Functions
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER iL
      INTEGER i, j, k, Katm, nLoc
      INTEGER ith, biLoc, bjLoc
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef LOCBIN_IO_THREAD_SAFE
C-    safe for any thread to do IO
      ith = myThid
      biLoc = bi
      bjLoc = bj
#else /* LOCBIN_IO_THREAD_SAFE */
C-    master-thread does IO for all threads
      _BARRIER
      _BEGIN_MASTER( myThid )
      DO ith=1,nThreads
        biLoc = bi + myBxLo(ith) - 1
        bjLoc = bj + myByLo(ith) - 1
#endif /* LOCBIN_IO_THREAD_SAFE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Check for argument list consistency
        IF ( nNr.LT.1 .OR. nNr.GT.Nr ) THEN
          iL = ILNBLNK( pref )
          WRITE(msgBuf,'(A,I10,A,2I5,A,I4,2A)')
     &      'AIM_WRITE_PHYS (it=', myIter, ' bi,bj=', bi,bj,
     &      ' iRec=', iRec, ' ): try to write: ', pref(1:iL)
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I4,A,I4)')
     &      'AIM_WRITE_PHYS: 3rd dim.(field)=',nNr,' has to be <',Nr
          CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL END: S/R AIM_WRITE_PHYS'
        ELSEIF ( kLev.NE.0 .AND. kLev.GT.nNr ) THEN
          iL = ILNBLNK( pref )
          WRITE(msgBuf,'(A,I10,A,2I5,A,I4,2A)')
     &      'AIM_WRITE_PHYS (it=', myIter, ' bi,bj=', bi,bj,
     &      ' iRec=', iRec, ' ): try to write: ', pref(1:iL)
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I4,A,I4)')
     &     'AIM_WRITE_PHYS: kLev=', kLev,
     &     ' out of bounds (dim=', nNr,' )'
          CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL END: S/R AIM_WRITE_PHYS'
        ENDIF

C--   Copy the input field into tempo. array:
        nLoc = nNr
        IF ( kLev.GE.1 .AND. kLev.LE.nNr ) THEN
          nLoc = 1
          DO j=1,sNy
           DO i=1,sNx
            tmpFld(i,j,1) = field(i,j,kLev,ith)
           ENDDO
          ENDDO
        ELSEIF (nNr.EQ.Nr) THEN
C-     Reverse K index:
         DO k=1,Nr
          Katm = _KD2KA( k )
          DO j=1,sNy
           DO i=1,sNx
            tmpFld(i,j,k) = field(i,j,Katm,ith)
           ENDDO
          ENDDO
         ENDDO
        ELSE
C-     Do simple copy
         DO k=1,nNr
          DO j=1,sNy
           DO i=1,sNx
            tmpFld(i,j,k) = field(i,j,k,ith)
           ENDDO
          ENDDO
         ENDDO
        ENDIF

C--   Write to file: note: call with myThArg=0 => single thread job
        CALL WRITE_LOCAL_RL( pref, suff, nLoc, tmpFld,
     &                       biLoc, bjLoc, iRec, myIter, 0 )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifndef LOCBIN_IO_THREAD_SAFE
      ENDDO
      _END_MASTER( myThid )
      _BARRIER
#endif /* ndef LOCBIN_IO_THREAD_SAFE */

#endif /* ALLOW_AIM */
      RETURN
      END
