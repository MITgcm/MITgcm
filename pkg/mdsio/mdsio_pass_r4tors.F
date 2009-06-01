C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_pass_r4tors.F,v 1.2 2009/06/01 14:20:31 jmc Exp $
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
      SUBROUTINE MDS_PASS_R4toRS( local, arr, k, nNz,
     I                            biArg, bjArg, copyTo, myThid )

C !DESCRIPTION:
C     Transfert 2-D real*4 array to 3-D RS array, or the reverse,
C      depending on "copyTo" value. Apply transfert to tile biArg,bjArg
C      only or to all myThid tiles if called with biArg=bjArg=0.

C     !USES:
      IMPLICIT NONE

C Global variables / common blocks
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C Routine arguments
C local   (real*4) :: local 2-D array (Input/Output if copyTo=T/F)
C arr     ( RS )   :: model 3-D tiled array (Output/Input if copyTo=T/F)
C k       (integer):: level index to - fill in / extract from - 3-D array
C nNz     (integer):: size of third dimension of 3-D array "arr"
C biArg   (integer):: tile X-index to - fill in / extract from - tiled array
C bjArg   (integer):: tile Y-index to - fill in / extract from - tiled array
C copyTo  (logical):: if =T, copy 2-D -> 3-D ; if =F: copy 2-D <- 3-D
C myThid  (integer):: my Thread Id number
      INTEGER nNz
      Real*4 local(1:sNx,1:sNy,nSx,nSy)
      _RS    arr  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      INTEGER k
      INTEGER biArg
      INTEGER bjArg
      LOGICAL copyTo
      INTEGER myThid

C !LOCAL VARIABLES:
C   i,j   :: loop indices
C   bi,bj :: tile indices
      INTEGER i,j,bi,bj
CEOP

      IF ( biArg.EQ.0 .AND. bjArg.EQ.0 ) THEN
        IF ( copyTo ) THEN
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
             DO j=1,sNy
              DO i=1,sNx
                arr(i,j,k,bi,bj) = local(i,j,bi,bj)
              ENDDO
             ENDDO
           ENDDO
          ENDDO
        ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
             DO j=1,sNy
              DO i=1,sNx
                local(i,j,bi,bj) = arr(i,j,k,bi,bj)
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
            DO j=1,sNy
              DO i=1,sNx
                arr(i,j,k,bi,bj) = local(i,j,1,1)
              ENDDO
            ENDDO
        ELSE
            DO j=1,sNy
              DO i=1,sNx
                local(i,j,1,1) = arr(i,j,k,bi,bj)
              ENDDO
            ENDDO
        ENDIF
      ELSE
        STOP 'ABNORMAL END: MDS_PASS_R4toRS invalid bi,bj Arg'
      ENDIF

      RETURN
      END
