C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_pass_wh_r4torl.F,v 1.1 2010/09/24 18:39:35 gforget Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

CBOP
C !ROUTINE: MDS_PASS_WH_R4toRL
C !INTERFACE:
      SUBROUTINE MDS_PASS_WH_R4toRL( buffer, arrFld, nNz, kLo, kSize,
     I                            biArg, bjArg, copyTo, myThid )

C !DESCRIPTION:
C     Transfert 3-D real*4 buffer to 3-D RL model array, or the reverse,
C      depending on "copyTo" value. Apply transfert to tile biArg,bjArg
C      only or to all myThid tiles if called with biArg=bjArg=0.

C     !USES:
      IMPLICIT NONE

C Global variables / common blocks
#include "EEPARAMS.h"
#include "SIZE.h"

C     sNxWh :: x tile size with halo included
C     sNyWh :: y tile size with halo included
      INTEGER sNxWh
      INTEGER sNyWh
      PARAMETER ( sNxWh = sNx+2*Olx )
      PARAMETER ( sNyWh = sNy+2*Oly )

C     !INPUT/OUTPUT PARAMETERS:
C Routine arguments
C buffer  (real*4) :: buffer 3-D array (Input/Output if copyTo=T/F)
C arrFld   ( RL )  :: model 3-D tiled array (Output/Input if copyTo=T/F)
C nNz     (integer):: Number of levels to - fill in / extract from - arrFld
C kLo     (integer):: 1rst level to - fill in / extract from - arrFld
C kSize   (integer):: third dimension of 3-D array "arrFld"
C biArg   (integer):: tile X-index to - fill in / extract from - tiled buffer
C bjArg   (integer):: tile Y-index to - fill in / extract from - tiled buffer
C copyTo  (logical):: if =T, copy 2-D -> 3-D ; if =F: copy 2-D <- 3-D
C myThid  (integer):: my Thread Id number
      INTEGER nNz, kSize
      Real*4 buffer(1:sNxWh,1:sNyWh,nNz,nSx,nSy)
      _RL    arrFld(1:sNxWh,1:sNyWh,kSize,nSx,nSy)
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

      IF ( biArg.EQ.0 .AND. bjArg.EQ.0 ) THEN
        IF ( copyTo ) THEN
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k=1,nNz
             kLev = kLo+k-1
             DO j=1,sNyWh
              DO i=1,sNxWh
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
             DO j=1,sNyWh
              DO i=1,sNxWh
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
            DO j=1,sNyWh
              DO i=1,sNxWh
                arrFld(i,j,kLev,1,1) = buffer(i,j,k,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ELSE
          DO k=1,nNz
            kLev = kLo+k-1
            DO j=1,sNyWh
              DO i=1,sNxWh
                buffer(i,j,k,bi,bj) = arrFld(i,j,kLev,1,1)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ELSE
        STOP 'ABNORMAL END: MDS_PASS_WH_R4toRL invalid bi,bj Arg'
      ENDIF

      RETURN
      END


