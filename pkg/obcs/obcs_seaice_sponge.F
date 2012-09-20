C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_seaice_sponge.F,v 1.1 2012/09/20 19:04:46 dimitri Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C--   File obcs_seaice_sponge.F:
C--    Contents:
C--    o OBCS_SEAICE_SPONGE_A
C--    o OBCS_SEAICE_SPONGE_H

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
      SUBROUTINE OBCS_SEAICE_SPONGE_A( myThid )
C     *==========================================================*
C     | S/R OBCS_SEAICE_SPONGE_A
C     | o Contains problem specific forcing for salinity.
C     *==========================================================*
C     | Adds a relaxation term to AREA near Open-Boundaries
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#include "OBCS_SEAICE.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE_PARAMS.h"
# include "SEAICE.h"
#endif

C     == Routine arguments ==
      INTEGER myThid
CEndOfInterface

#if (defined(ALLOW_OBCS) && defined(ALLOW_SEAICE) && defined(ALLOW_OBCS_SEAICE_SPONGE))
C     == Local variables ==
C     Loop counters
      INTEGER bi, bj, i, j, isl, jsl
      _RL lambda_obcs

      IF ( useSeaiceSponge .AND. seaiceSpongeThickness.NE.0 ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

C Northern Open Boundary
# ifdef ALLOW_OBCS_NORTH
         IF ( tileHasOBN(bi,bj) ) THEN
          DO i=1,sNx
           IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
            DO jsl= 1,seaiceSpongeThickness
             j=OB_Jn(i,bi,bj)-jsl
             IF ((j.ge.1).and.(j.le.sNy)) THEN
              lambda_obcs = (
     &           float(seaiceSpongeThickness-jsl)*Arelaxobcsbound
     &           + float(jsl)*Arelaxobcsinner)
     &           / float(seaiceSpongeThickness)
              IF (lambda_obcs.ne.0.) THEN
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              ELSE
               lambda_obcs = 0. _d 0
              ENDIF
              AREA(i,j,bi,bj) =  AREA(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( AREA(i,j,bi,bj) - OBNa(i,bi,bj) )
             ENDIF
            ENDDO
           ENDIF
          ENDDO
         ENDIF
# endif

C Southern Open Boundary
# ifdef ALLOW_OBCS_SOUTH
         IF ( tileHasOBS(bi,bj) ) THEN
          DO i=1,sNx
           IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
            DO jsl= 1,seaiceSpongeThickness
             j=OB_Js(i,bi,bj)+jsl
             IF ((j.ge.1).and.(j.le.sNy)) THEN
              lambda_obcs = (
     &           float(seaiceSpongeThickness)*Arelaxobcsbound
     &           + float(jsl)*Arelaxobcsinner)
     &           / float(seaiceSpongeThickness)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              AREA(i,j,bi,bj) = AREA(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( AREA(i,j,bi,bj) - OBSa(i,bi,bj) )
             ENDIF
            ENDDO
           ENDIF
          ENDDO
         ENDIF
# endif

C Eastern Open Boundary
# ifdef ALLOW_OBCS_EAST
         IF ( tileHasOBE(bi,bj) ) THEN
          DO j=1,sNy
           IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
            DO isl= 1,seaiceSpongeThickness
             i=OB_Ie(j,bi,bj)-isl
             IF ((i.ge.1).and.(i.le.sNx)) THEN
              lambda_obcs = (
     &           float(seaiceSpongeThickness-isl)*Arelaxobcsbound
     &           + float(isl)*Arelaxobcsinner)
     &           / float(seaiceSpongeThickness)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              AREA(i,j,bi,bj) = AREA(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( AREA(i,j,bi,bj) - OBEa(j,bi,bj) )
             ENDIF
            ENDDO
           ENDIF
          ENDDO
         ENDIF
# endif

C Western Open Boundary
# ifdef ALLOW_OBCS_WEST
         IF ( tileHasOBW(bi,bj) ) THEN
          DO j=1,sNy
           IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
            DO isl= 1,seaiceSpongeThickness
             i=OB_Iw(j,bi,bj)+isl
             IF ((i.ge.1).and.(i.le.sNx)) THEN
              lambda_obcs= (
     &           float(seaiceSpongeThickness-isl)*Arelaxobcsbound
     &           + float(isl)*Arelaxobcsinner)
     &           / float(seaiceSpongeThickness)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              AREA(i,j,bi,bj) =  AREA(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( AREA(i,j,bi,bj) - OBWa(j,bi,bj) )
             ENDIF
            ENDDO
           ENDIF
          ENDDO
         ENDIF
# endif

        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_OBCS & ALLOW_SEAICE & ALLOW_OBCS_SEAICE_SPONGE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
      SUBROUTINE OBCS_SEAICE_SPONGE_H( myThid )
C     *==========================================================*
C     | S/R OBCS_SEAICE_SPONGE_H
C     | o Contains problem specific forcing for salinity.
C     *==========================================================*
C     | Adds a relaxation term to HEFF near Open-Boundaries
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#include "OBCS_SEAICE.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE_PARAMS.h"
# include "SEAICE.h"
#endif

C     == Routine arguments ==
      INTEGER myThid
CEndOfInterface

#if (defined(ALLOW_OBCS) && defined(ALLOW_SEAICE) && defined(ALLOW_OBCS_SEAICE_SPONGE))
C     == Local variables ==
C     Loop counters
      INTEGER bi, bj, i, j, isl, jsl
      _RL lambda_obcs

      IF ( useSeaiceSponge .AND. seaiceSpongeThickness.NE.0 ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

C Northern Open Boundary
# ifdef ALLOW_OBCS_NORTH
         IF ( tileHasOBN(bi,bj) ) THEN
          DO i=1,sNx
           IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
            DO jsl= 1,seaiceSpongeThickness
             j=OB_Jn(i,bi,bj)-jsl
             IF ((j.ge.1).and.(j.le.sNy)) THEN
              lambda_obcs = (
     &           float(seaiceSpongeThickness-jsl)*Hrelaxobcsbound
     &           + float(jsl)*Hrelaxobcsinner)
     &           / float(seaiceSpongeThickness)
              IF (lambda_obcs.ne.0.) THEN
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              ELSE
               lambda_obcs = 0. _d 0
              ENDIF
              HEFF(i,j,bi,bj) =  HEFF(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HEFF(i,j,bi,bj) - OBNh(i,bi,bj) )
             ENDIF
            ENDDO
           ENDIF
          ENDDO
         ENDIF
# endif

C Southern Open Boundary
# ifdef ALLOW_OBCS_SOUTH
         IF ( tileHasOBS(bi,bj) ) THEN
          DO i=1,sNx
           IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
            DO jsl= 1,seaiceSpongeThickness
             j=OB_Js(i,bi,bj)+jsl
             IF ((j.ge.1).and.(j.le.sNy)) THEN
              lambda_obcs = (
     &           float(seaiceSpongeThickness)*Hrelaxobcsbound
     &           + float(jsl)*Hrelaxobcsinner)
     &           / float(seaiceSpongeThickness)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              HEFF(i,j,bi,bj) = HEFF(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HEFF(i,j,bi,bj) - OBSh(i,bi,bj) )
             ENDIF
            ENDDO
           ENDIF
          ENDDO
         ENDIF
# endif

C Eastern Open Boundary
# ifdef ALLOW_OBCS_EAST
         IF ( tileHasOBE(bi,bj) ) THEN
          DO j=1,sNy
           IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
            DO isl= 1,seaiceSpongeThickness
             i=OB_Ie(j,bi,bj)-isl
             IF ((i.ge.1).and.(i.le.sNx)) THEN
              lambda_obcs = (
     &           float(seaiceSpongeThickness-isl)*Hrelaxobcsbound
     &           + float(isl)*Hrelaxobcsinner)
     &           / float(seaiceSpongeThickness)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              HEFF(i,j,bi,bj) = HEFF(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HEFF(i,j,bi,bj) - OBEh(i,bi,bj) )
             ENDIF
            ENDDO
           ENDIF
          ENDDO
         ENDIF
# endif

C Western Open Boundary
# ifdef ALLOW_OBCS_WEST
         IF ( tileHasOBW(bi,bj) ) THEN
          DO j=1,sNy
           IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
            DO isl= 1,seaiceSpongeThickness
             i=OB_Iw(j,bi,bj)+isl
             IF ((i.ge.1).and.(i.le.sNx)) THEN
              lambda_obcs= (
     &           float(seaiceSpongeThickness-isl)*Hrelaxobcsbound
     &           + float(isl)*Hrelaxobcsinner)
     &           / float(seaiceSpongeThickness)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              HEFF(i,j,bi,bj) =  HEFF(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HEFF(i,j,bi,bj) - OBWh(i,bi,bj) )
             ENDIF
            ENDDO
           ENDIF
          ENDDO
         ENDIF
# endif

        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_OBCS & ALLOW_SEAICE & ALLOW_OBCS_SEAICE_SPONGE */

      RETURN
      END
