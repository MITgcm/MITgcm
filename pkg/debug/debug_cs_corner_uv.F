C $Header: /u/gcmpack/MITgcm/pkg/debug/debug_cs_corner_uv.F,v 1.2 2004/09/24 17:01:09 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
c #include "DEBUG_OPTIONS.h"

      SUBROUTINE DEBUG_CS_CORNER_UV( 
     I        word2print,
     I        uFld, vFld,
     I        k, ioUnit, bi,bj, myThid)
C     *==========================================================*
C     | S/R DEBUG_CS_CORNER_UV                                   |
C     | o check UV fields at Egdes of CS grid, near corners.     |
C     *==========================================================*
C     | Values of U,V fields at the Edges of the CS grid         |
C     |  are common to 2 faces, and are stored + used in 2       |
C     |  places (2 tiles): one in the interior of the 1rst tile, |
C     |  the other in the halo of the 2nd one.                   | 
C     | This S/R print the 2 values and  check that they are     | 
C     |  identical (print the difference).                       |
C     | This is specially usefull for checking that gU,gV are    |
C     |  correct before entering solve_for_pressure.             |
C     *==========================================================*
C     | Note: only works on a 1.cpu set up with 6 tiles          |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
c #include "PARAMS.h"
c #include "GRID.h"

C     == Routine arguments ==
C     word2print - a string to print
C     uFld   - u component of 2D vector 
C     vFld   - v component of 2D vector 
C     k      - current level
C     ioUnit - I/O unit number
C     bi,bj  - tile indices
C     myThid - Instance number for this invocation of 
      CHARACTER*(*) word2print
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER ic, k, ioUnit
      INTEGER bi,bj
      INTEGER myThid

c #ifdef ALLOW_DEBUG

C     == Local variables in common block :
      COMMON / DEBUG_CS_CORNER_UV_LOCAL / tmpU, tmpV
      _RL tmpU(4,Nr,nSx,nSy)
      _RL tmpV(4,Nr,nSx,nSy)

C     == Local variables ==
      INTEGER n1,n2,n3

      EXTERNAL ILNBLNK
      INTEGER  ILNBLNK

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

 1010 FORMAT(2A,I2,1PE12.4,I3,1P2E12.4)
c     IF (k.EQ.4 .AND. myIter.EQ.nIter0 ) THEN
       tmpU(1,k,bi,bj)= uFld(1,1)
       tmpU(2,k,bi,bj)= uFld(1,sNy)
       tmpU(3,k,bi,bj)= uFld(sNx+1,1)
       tmpU(4,k,bi,bj)= uFld(sNx+1,sNy)
       tmpV(1,k,bi,bj)= vFld(1,1)
       tmpV(2,k,bi,bj)= vFld(sNx,1)
       tmpV(3,k,bi,bj)= vFld(1,sNy+1)
       tmpV(4,k,bi,bj)= vFld(sNx,sNy+1)
       _BARRIER
       IF (bi.EQ.nSx .AND. nSx.EQ.6) THEN
        WRITE(ioUnit,'(2A)') 'DEBUG_CS_CORNER_UV: ',
     &   '------------------------------------------------------------'
        ic = MAX(1,ILNBLNK(word2print))
        WRITE(ioUnit,'(2A)') 'DEBUG_CS_CORNER_UV: ',
     &        word2print(1:ic)
        WRITE(ioUnit,'(2A,I4)') 'DEBUG_CS_CORNER_UV: ',
     &       ' Edges values near a corner, lev=',k 
        WRITE(ioUnit,'(2A)') 'DEBUG_CS_CORNER_UV: ',
     &     '      face_1, value_1, face_2, value_2, difference v1-v2:'
        DO n1=1,6
         IF (MOD(n1,2).EQ.1 ) THEN
c         n1=1 n2=5,+v,-
c         n1=1 n3=6,+v,+
c         n1=3 n2=1,+v,-
c         n1=3 n3=2,+v,+
c         n1=5 n2=3,+v,-
c         n1=5 n3=4,+v,+
          n2=1+mod(n1-2+5,6)
          n3=1+mod(n1-1+5,6)
          WRITE(ioUnit,1010) 'DEBUG_CS_CORNER_UV: ', 'W edge, S:',
     &      n1,tmpU(1,k,n1,1), n2,tmpV(4,k,n2,1),
     &         tmpU(1,k,n1,1)  -  tmpV(4,k,n2,1)
          WRITE(ioUnit,1010) 'DEBUG_CS_CORNER_UV: ', 'W edge, N:',
     &      n1,tmpU(2,k,n1,1), n2,tmpV(3,k,n2,1),
     &         tmpU(2,k,n1,1)  -  tmpV(3,k,n2,1)
          WRITE(ioUnit,1010) 'DEBUG_CS_CORNER_UV: ', 'S edge, W:',
     &      n1,tmpV(1,k,n1,1), n3,tmpV(3,k,n3,1),
     &         tmpV(1,k,n1,1)  -  tmpV(3,k,n3,1)
          WRITE(ioUnit,1010) 'DEBUG_CS_CORNER_UV: ', 'S edge, E:',
     &      n1,tmpV(2,k,n1,1), n3,tmpV(4,k,n3,1),
     &         tmpV(2,k,n1,1)  -  tmpV(4,k,n3,1)
         ELSE
c         n1=2 n2=1,+u,+
c         n1=2 n3=6,+u,-
c         n1=4 n2=3,+u,+
c         n1=4 n3=2,+u,-
c         n1=6 n2=5,+u,+
c         n1=6 n3=4,+u,-
          n2=1+mod(n1-1+5,6)
          n3=1+mod(n1-2+5,6)
          WRITE(ioUnit,1010) 'DEBUG_CS_CORNER_UV: ', 'W edge, S:',
     &      n1,tmpU(1,k,n1,1), n2,tmpU(3,k,n2,1),
     &         tmpU(1,k,n1,1)  -  tmpU(3,k,n2,1)
          WRITE(ioUnit,1010) 'DEBUG_CS_CORNER_UV: ', 'W edge, N:',
     &      n1,tmpU(2,k,n1,1), n2,tmpU(4,k,n2,1),
     &         tmpU(2,k,n1,1)  -  tmpU(4,k,n2,1)
          WRITE(ioUnit,1010) 'DEBUG_CS_CORNER_UV: ', 'S edge, W:',
     &      n1,tmpV(1,k,n1,1), n3,tmpU(4,k,n3,1),
     &         tmpV(1,k,n1,1)  -  tmpU(4,k,n3,1)
          WRITE(ioUnit,1010) 'DEBUG_CS_CORNER_UV: ', 'S edge, E:',
     &      n1,tmpV(2,k,n1,1), n3,tmpU(3,k,n3,1),
     &         tmpV(2,k,n1,1)  -  tmpU(3,k,n3,1)
         ENDIF
        ENDDO
        WRITE(ioUnit,'(2A)') 'DEBUG_CS_CORNER_UV: ',
     &   '------------------------------------------------------------'
       ENDIF
c     ENDIF

c #endif /* ALLOW_DEBUG */

      RETURN
      END
