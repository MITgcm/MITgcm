C $Header: /u/gcmpack/MITgcm/pkg/debug/write_fullarray_rl.F,v 1.2 2004/09/23 21:21:02 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE WRITE_FULLARRAY_RL(fnam, fld, kSize, 
     I                              biArg, bjArg, myIter, myThid)
C     *==========================================================*
C     | SUBROUTINE WRITE_FULLARRAY
C     | write full array (including the overlap) to binary files
C     *==========================================================*
C     | Only used for debugging purpose.
C     |  can write local array (tile biArg,bjArg) or global
C     |  array (with biArg=bjArg=0)
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
      CHARACTER*(*) fnam
      INTEGER kSize
      INTEGER biArg, bjArg
      INTEGER myIter
      INTEGER myThid
      _RL fld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,kSize,nSx,nSy)
C     ==  Functions ==
      INTEGER ILNBLNK,IFNBLNK

CEndofinterface
C     == Local variables ==
      CHARACTER*(2) fType
      INTEGER i,j,k,bi,bj,iG,jG
      INTEGER s1Lo,s1Hi, dUnit
      CHARACTER*(80) fullName

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C--   to Build file name
      s1Lo = IFNBLNK(fnam)
      s1Hi = ILNBLNK(fnam)
      CALL MDSFINDUNIT( dUnit, mythid )

      fType='RL'
      
      IF ( biArg.EQ.0 .AND. bjArg.EQ.0 ) THEN
C--   Write full global array:
       DO bj = 1,nSy
        DO bi = 1,nSx
         iG=bi+(myXGlobalLo-1)/sNx
         jG=bj+(myYGlobalLo-1)/sNy

         WRITE( fullName, '(2A,I10.10,2(A,I3.3),A)' ) 
     &    fnam(s1Lo:s1Hi),'.',myIter,'.',iG,'.',jG,'.bin'

c        length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
         OPEN(dUnit, file=fullName, status='unknown', 
     &        form='unformatted')
c    &        access='direct', recl=length_of_rec )
         WRITE(dUnit) ((( fld(i,j,k,bi,bj),
     &                       i=1-Olx,sNx+Olx),
     &                       j=1-Oly,sNy+Oly),
     &                       k=1,kSize)
         CLOSE(dUnit)

        ENDDO
       ENDDO

      ELSE
C--   Write local array:
         iG=biArg+(myXGlobalLo-1)/sNx
         jG=bjArg+(myYGlobalLo-1)/sNy

         WRITE( fullName, '(2A,I10.10,2(A,I3.3),A)' ) 
     &    fnam(s1Lo:s1Hi),'.',myIter,'.',iG,'.',jG,'.bin'

c        length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
         OPEN(dUnit, file=fullName, status='unknown', 
     &        form='unformatted')
c    &        access='direct', recl=length_of_rec )
         WRITE(dUnit) ((( fld(i,j,k,1,1),
     &                       i=1-Olx,sNx+Olx),
     &                       j=1-Oly,sNy+Oly),
     &                       k=1,kSize)
         CLOSE(dUnit)

      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
