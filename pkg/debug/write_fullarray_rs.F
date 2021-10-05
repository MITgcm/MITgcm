#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: WRITE_FULLARRAY_RS
C     !INTERFACE:
      SUBROUTINE WRITE_FULLARRAY_RS( fnam, fld, kSize,
     I                               biArg, bjArg,
     I                               iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE WRITE_FULLARRAY
C     | write full array (including the overlap) to binary files
C     *==========================================================*
C     | Only used for debugging purpose.
C     |  can write local array (with no bi,bj) corresponding to
C     |      tile biArg,bjArg
C     |  or global array (with bi,bj) (called with biArg=bjArg=0)
C     | Warning:
C     |   1) does not explicitly do the byte-swapping unless
C     |      specified by compiler option.
C     |   2) ignores writeBinaryPrec and just write output with
C     |      same precision as input array (float32/float64)
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      CHARACTER*(*) fnam
      INTEGER kSize
      INTEGER biArg, bjArg
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid
      _RS fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)

C     !FUNCTIONS:
C     ==  Functions ==
      INTEGER  ILNBLNK, IFNBLNK, MDS_RECLEN
      EXTERNAL ILNBLNK, IFNBLNK, MDS_RECLEN

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k,bi,bj,iG,jG
      INTEGER s1Lo,s1Hi, dUnit, filePrec, length_of_rec, kRec
      CHARACTER*(MAX_LEN_FNAM) fullName
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C--   to Build file name
      s1Lo = IFNBLNK(fnam)
      s1Hi = ILNBLNK(fnam)
      CALL MDSFINDUNIT( dUnit, myThid )

C--   file precision has to match array type (no copy to buffer)
#ifdef RS_IS_REAL4
      filePrec = precFloat32
#else
      filePrec = precFloat64
#endif

      IF ( biArg.EQ.0 .AND. bjArg.EQ.0 ) THEN
C--   Write full global array:
       DO bj = 1,nSy
        DO bi = 1,nSx
         iG=bi+(myXGlobalLo-1)/sNx
         jG=bj+(myYGlobalLo-1)/sNy

         IF ( myIter.GE.0 ) THEN
           WRITE( fullName, '(2A,I10.10,2(A,I3.3),A)' )
     &      fnam(s1Lo:s1Hi),'.',myIter,'.',iG,'.',jG,'.data'
         ELSE
           WRITE( fullName, '(A,2(A,I3.3),A)' )
     &      fnam(s1Lo:s1Hi),'.',iG,'.',jG,'.data'
         ENDIF

c        OPEN( dUnit, file=fullName, status='unknown',
c    &         form='unformatted')
c        WRITE(dUnit) ((( fld(i,j,k,bi,bj),
c    &                        i=1-OLx,sNx+OLx),
c    &                        j=1-OLy,sNy+OLy),
c    &                        k=1,kSize)
         length_of_rec = MDS_RECLEN(
     &                   filePrec, (sNx+2*OLx)*(sNy+2*OLy), myThid )
         OPEN( dUnit, file=fullName, status='unknown',
     &         access='direct', recl=length_of_rec )
         DO k = 1,kSize
           kRec = k + (iRec-1)*kSize
           WRITE(dUnit,rec=kRec) (( fld(i,j,k,bi,bj),
     &                                i=1-OLx,sNx+OLx),
     &                                j=1-OLy,sNy+OLy )
         ENDDO
         CLOSE(dUnit)

        ENDDO
       ENDDO

      ELSE
C--   Write local array:
         iG=biArg+(myXGlobalLo-1)/sNx
         jG=bjArg+(myYGlobalLo-1)/sNy

         IF ( myIter.GE.0 ) THEN
           WRITE( fullName, '(2A,I10.10,2(A,I3.3),A)' )
     &      fnam(s1Lo:s1Hi),'.',myIter,'.',iG,'.',jG,'.data'
         ELSE
           WRITE( fullName, '(A,2(A,I3.3),A)' )
     &      fnam(s1Lo:s1Hi),'.',iG,'.',jG,'.data'
         ENDIF

c        OPEN( dUnit, file=fullName, status='unknown',
c    &         form='unformatted')
c        WRITE(dUnit) ((( fld(i,j,k,1,1),
c    &                       i=1-OLx,sNx+OLx),
c    &                       j=1-OLy,sNy+OLy),
c    &                       k=1,kSize)
         length_of_rec = MDS_RECLEN(
     &                   filePrec, (sNx+2*OLx)*(sNy+2*OLy), myThid )
         OPEN( dUnit, file=fullName, status='unknown',
     &         access='direct', recl=length_of_rec )
         DO k = 1,kSize
           kRec = k + (iRec-1)*kSize
           WRITE(dUnit,rec=kRec) (( fld(i,j,k,1,1),
     &                                i=1-OLx,sNx+OLx),
     &                                j=1-OLy,sNy+OLy )
         ENDDO
         CLOSE(dUnit)

      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
