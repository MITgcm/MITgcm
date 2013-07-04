C $Header: /u/gcmpack/MITgcm/eesupp/src/reset_halo.F,v 1.1 2013/07/04 22:49:16 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

C--  File reset_halo.F: to fill-up halo region with constant value
C--   Contents
C--   o RESET_HALO_RL
C--   o RESET_HALO_RS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: RESET_HALO_RL
C     !INTERFACE:
      SUBROUTINE RESET_HALO_RL(
     U                          glFld,
     I                          fixVal, kSize, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R RESET_HALO_RL
C     *==========================================================*
C     | o Reset to constant value the halo region of a global
C     |   3-D array. Note: used for debugging only .
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     glFld    :: field global array to update in halo-region
C     fixVal   :: constant value to fill-up the halo region with
C     kSize    :: size of 3-D field-array
C     myThid   :: my Thread Id number
      INTEGER kSize
      _RL glFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL fixVal
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi, bj, k
      INTEGER ii, i, j
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO k=1,kSize

         DO j=1-OLy,sNy+OLy
          IF ( j.LT.1 .OR. j.GT.sNy ) THEN
           DO i=1-OLx,sNx+OLx
             glFld(i,j,k,bi,bj) = fixVal
           ENDDO
          ELSE
           DO ii=1-OLx,OLx
             i = ii
             IF ( ii.GE.1 ) i= ii+sNx
             glFld(i,j,k,bi,bj) = fixVal
           ENDDO
          ENDIF
         ENDDO

        ENDDO

C-   end bi,bj loops
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: RESET_HALO_RS
C     !INTERFACE:
      SUBROUTINE RESET_HALO_RS(
     U                          glFld,
     I                          fixVal, kSize, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R RESET_HALO_RS
C     *==========================================================*
C     | o Reset to constant value the halo region of a global
C     |   3-D array. Note: used for debugging only .
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     glFld    :: field global array to update in halo-region
C     fixVal   :: constant value to fill-up the halo region with
C     kSize    :: size of 3-D field-array
C     myThid   :: my Thread Id number
      INTEGER kSize
      _RS glFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RS fixVal
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi, bj, k
      INTEGER ii, i, j
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO k=1,kSize

         DO j=1-OLy,sNy+OLy
          IF ( j.LT.1 .OR. j.GT.sNy ) THEN
           DO i=1-OLx,sNx+OLx
             glFld(i,j,k,bi,bj) = fixVal
           ENDDO
          ELSE
           DO ii=1-OLx,OLx
             i = ii
             IF ( ii.GE.1 ) i= ii+sNx
             glFld(i,j,k,bi,bj) = fixVal
           ENDDO
          ENDIF
         ENDDO

        ENDDO

C-   end bi,bj loops
       ENDDO
      ENDDO

      RETURN
      END
