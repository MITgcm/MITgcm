C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_pass_r4tors.F,v 1.1 2009/05/11 02:20:49 jmc Exp $
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
      SUBROUTINE MDS_PASS_R4toRS(local,arr,k,nNz,copyTo,myThid)

C !DESCRIPTION:
C     Transfert 2-D real*4 array to 3-D RS array,
C     or the reverse, depending on "copyTo" value.

C     !USES:
      IMPLICIT NONE

C Global variables / common blocks
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C Routine arguments
C local   (real*4) :: local 2-D array (Input/Output if copyTo=T/F)
C arr     ( RS )   :: model 3-D tiled array (Output/Input if copyTo=T/F)
C k       (integer):: level index to fill in / to extract from 3-D array
C nNz     (integer):: size of third dimension of 3-D array "arr"
C copyTo  (logical):: if =T, copy 2-D -> 3-D ; if =F: copy 2-D <- 3-D
C myThid  (integer):: my Thread Id number
      INTEGER nNz
      Real*4 local(1:sNx,1:sNy,nSx,nSy)
      _RS    arr  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      INTEGER k
      LOGICAL copyTo
      INTEGER myThid

C !LOCAL VARIABLES:
C   i,j   :: loop indices
C   bi,bj :: tile indices
      INTEGER i,j,bi,bj
CEOP

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

      RETURN
      END
