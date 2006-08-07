C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_init_varia.F,v 1.2 2006/08/07 21:13:52 edhill Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_INIT_VARIA

C !INTERFACE:
      SUBROUTINE MYPACKAGE_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Initialize MYPACKAGE variables

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "MYPACKAGE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_MYPACKAGE

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j,k,bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     Initialize arrays in common blocks :
C     3-dim.
        DO k=1,Nr
         DO j=1-Oly,sNy+OLy
          DO i=1-Olx,sNx+Olx
           mypaStatScal1(i,j,k,bi,bj) = 0. _d 0
           mypaStatScal2(i,j,k,bi,bj) = 0. _d 0
           mypaStatUvel(i,j,k,bi,bj) = 0. _d 0
           mypaStatVvel(i,j,k,bi,bj) = 0. _d 0
           mypaGScal1(i,j,k,bi,bj) = 0. _d 0
           mypaGScal2(i,j,k,bi,bj) = 0. _d 0
           mypaGUvel(i,j,k,bi,bj) = 0. _d 0
           mypaGVvel(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

C     2-dim.
        DO j=1-Oly,sNy+OLy
         DO i=1-Olx,sNx+Olx
          mypaSurf1(i,j,bi,bj) = 0. _d 0
          mypaSurf2(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

C- end bi,bj loops
       ENDDO
      ENDDO

      IF ( mypaStatScal1File .NE. ' ' ) THEN
       _BEGIN_MASTER( myThid )
        CALL READ_FLD_XYZ_RL(mypaStatScal1File,' ',
     &   mypaStatScal1,0,myThid)
       _END_MASTER(myThid)
       _EXCH_XYZ_R8(mypaStatScal1,myThid)
      ENDIF

      IF ( mypaStatScal2File .NE. ' ' ) THEN
       _BEGIN_MASTER( myThid )
        CALL READ_FLD_XYZ_RL(mypaStatScal2File,' ',
     &   mypaStatScal2,0,myThid)
       _END_MASTER(myThid)
       _EXCH_XYZ_R8(mypaStatScal2,myThid)
      ENDIF

      IF ( mypaStatUvelFile .NE. ' ' ) THEN
       _BEGIN_MASTER( myThid )
        CALL READ_FLD_XYZ_RL(mypaStatUvelFile,' ',
     &   mypaStatUvel,0,myThid)
       _END_MASTER(myThid)
       _EXCH_XYZ_R8(mypaStatUvel,myThid)
      ENDIF

      IF ( mypaStatVvelFile .NE. ' ' ) THEN
       _BEGIN_MASTER( myThid )
        CALL READ_FLD_XYZ_RL(mypaStatVvelFile,' ',
     &   mypaStatVvel,0,myThid)
       _END_MASTER(myThid)
       _EXCH_XYZ_R8(mypaStatVvel,myThid)
      ENDIF

      IF ( mypaSurf1File .NE. ' ' ) THEN
       _BEGIN_MASTER( myThid )
        CALL READ_FLD_XY_RL(mypaSurf1File,' ',
     &   mypaSurf1,0,myThid)
       _END_MASTER(myThid)
       _EXCH_XY_R8(mypaSurf1,myThid)
      ENDIF

      IF ( mypaSurf2File .NE. ' ' ) THEN
       _BEGIN_MASTER( myThid )
        CALL READ_FLD_XY_RL(mypaSurf2File,' ',
     &   mypaSurf2,0,myThid)
       _END_MASTER(myThid)
       _EXCH_XY_R8(mypaSurf2,myThid)
      ENDIF

#endif /* ALLOW_MYPACKAGE */

      RETURN
      END
