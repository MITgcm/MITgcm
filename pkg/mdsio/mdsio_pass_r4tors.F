C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_pass_r4tors.F,v 1.4 2010/12/23 02:40:42 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C- modification: no need to edit the 4 scr files mdsio_pass_r{4,8}tor{l,s}.F :
C        from the 1rst src file (mdsio_pass_r4torl.F), can update the 3 others
C        using the script "derive_other_types".
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: MDS_PASS_R4toRS
C !INTERFACE:
      SUBROUTINE MDS_PASS_R4toRS(
     U                            buffer, arrFld,
     I                            oLi, oLj, nNz, kLo, kSize,
     I                            biArg, bjArg, copyTo, myThid )

C !DESCRIPTION:
C     Transfert 3-D real*4 buffer to 3-D RS model array, or the reverse,
C      depending on "copyTo" value. Apply transfert to tile biArg,bjArg
C      only or to all myThid tiles if called with biArg=bjArg=0.

C     !USES:
      IMPLICIT NONE

C Global variables / common blocks
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C Routine arguments
C buffer  (real*4) :: buffer 3-D array (Input/Output if copyTo=T/F)
C arrFld   ( RS )  :: model 3-D tiled array (Output/Input if copyTo=T/F)
C oLi     (integer):: Overlap size (dim-1) of buffer to copy - to/from - arrFld
C oLj     (integer):: Overlap size (dim-2) of buffer to copy - to/from - arrFld
C nNz     (integer):: Number of levels to - fill in / extract from - arrFld
C kLo     (integer):: 1rst level to - fill in / extract from - arrFld
C kSize   (integer):: third dimension of 3-D array "arrFld"
C biArg   (integer):: tile X-index to - fill in / extract from - tiled buffer
C bjArg   (integer):: tile Y-index to - fill in / extract from - tiled buffer
C copyTo  (logical):: if =T, copy 2-D -> 3-D ; if =F: copy 2-D <- 3-D
C myThid  (integer):: my Thread Id number
      INTEGER oLi, oLj
      INTEGER nNz, kSize
      Real*4 buffer(1-oLi:sNx+oLi,1-oLj:sNy+oLj,nNz,nSx,nSy)
      _RS    arrFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      INTEGER kLo
      INTEGER biArg
      INTEGER bjArg
      LOGICAL copyTo
      INTEGER myThid

C !LOCAL VARIABLES:
C   i,j,k :: loop indices
C   bi,bj :: tile indices
      INTEGER i,j,k,bi,bj
      INTEGER kLev
CEOP
      IF ( oLi.LT.0 .OR. oLi.GT.OLx .OR.
     &     oLj.LT.0 .OR. oLj.GT.OLy ) THEN
        STOP 'ABNORMAL END: MDS_PASS_R4toRS invalid oLi,oLj Arg'
      ENDIF

      IF ( biArg.EQ.0 .AND. bjArg.EQ.0 ) THEN
        IF ( copyTo ) THEN
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k=1,nNz
             kLev = kLo+k-1
             DO j=1-oLj,sNy+oLj
              DO i=1-oLi,sNx+oLi
                arrFld(i,j,kLev,bi,bj) = buffer(i,j,k,bi,bj)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
          ENDDO
        ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k=1,nNz
             kLev = kLo+k-1
             DO j=1-oLj,sNy+oLj
              DO i=1-oLi,sNx+oLi
                buffer(i,j,k,bi,bj) = arrFld(i,j,kLev,bi,bj)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
          ENDDO
        ENDIF
      ELSEIF ( biArg.GE.1 .AND. biArg.LE.nSx
     &   .AND. bjArg.GE.1 .AND. bjArg.LE.nSy ) THEN
        bi = biArg
        bj = bjArg
        IF ( copyTo ) THEN
          DO k=1,nNz
            kLev = kLo+k-1
            DO j=1-oLj,sNy+oLj
              DO i=1-oLi,sNx+oLi
                arrFld(i,j,kLev,1,1) = buffer(i,j,k,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ELSE
          DO k=1,nNz
            kLev = kLo+k-1
            DO j=1-oLj,sNy+oLj
              DO i=1-oLi,sNx+oLi
                buffer(i,j,k,bi,bj) = arrFld(i,j,kLev,1,1)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ELSE
        STOP 'ABNORMAL END: MDS_PASS_R4toRS invalid bi,bj Arg'
      ENDIF

      RETURN
      END
