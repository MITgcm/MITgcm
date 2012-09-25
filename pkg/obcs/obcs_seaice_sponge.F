C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_seaice_sponge.F,v 1.2 2012/09/25 16:39:20 dimitri Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C--   File obcs_seaice_sponge.F:
C--    Contents:
C--    o OBCS_SEAICE_SPONGE_A
C--    o OBCS_SEAICE_SPONGE_H
C--    o OBCS_SEAICE_SPONGE_SL
C--    o OBCS_SEAICE_SPONGE_SN

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
      SUBROUTINE OBCS_SEAICE_SPONGE_A( myThid )
C     *==========================================================*
C     | S/R OBCS_SEAICE_SPONGE_A
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
     &           + float(jsl-1)*Arelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              IF (lambda_obcs.ne.0.) THEN
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              ELSE
               lambda_obcs = 0. _d 0
              ENDIF
              AREA(i,j,bi,bj) = AREA(i,j,bi,bj)
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
     &           float(seaiceSpongeThickness-jsl)*Arelaxobcsbound
     &           + float(jsl-1)*Arelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
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
     &           + float(isl-1)*Arelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
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
     &           + float(isl-1)*Arelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              AREA(i,j,bi,bj) = AREA(i,j,bi,bj)
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
     &           + float(jsl-1)*Hrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              IF (lambda_obcs.ne.0.) THEN
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              ELSE
               lambda_obcs = 0. _d 0
              ENDIF
              HEFF(i,j,bi,bj) = HEFF(i,j,bi,bj)
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
     &           float(seaiceSpongeThickness-jsl)*Hrelaxobcsbound
     &           + float(jsl-1)*Hrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
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
     &           + float(isl-1)*Hrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              HEFF(i,j,bi,bj) = HEFF(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HEFF(i,j,bi,bj) - OBEh(j,bi,bj) )
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
     &           + float(isl-1)*Hrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              HEFF(i,j,bi,bj) = HEFF(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HEFF(i,j,bi,bj) - OBWh(j,bi,bj) )
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
      SUBROUTINE OBCS_SEAICE_SPONGE_SL( myThid )
C     *==========================================================*
C     | S/R OBCS_SEAICE_SPONGE_SL
C     | Adds a relaxation term to HSALT near Open-Boundaries
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

#if (defined(ALLOW_OBCS) && defined(ALLOW_SEAICE) && defined(ALLOW_OBCS_SEAICE_SPONGE) && defined(SEAICE_VARIABLE_SALINITY))
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
     &           float(seaiceSpongeThickness-jsl)*SLrelaxobcsbound
     &           + float(jsl-1)*SLrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              IF (lambda_obcs.ne.0.) THEN
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              ELSE
               lambda_obcs = 0. _d 0
              ENDIF
              HSALT(i,j,bi,bj) = HSALT(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HSALT(i,j,bi,bj) - OBNsl(i,bi,bj) )
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
     &           float(seaiceSpongeThickness-jsl)*SLrelaxobcsbound
     &           + float(jsl-1)*SLrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              HSALT(i,j,bi,bj) = HSALT(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HSALT(i,j,bi,bj) - OBSsl(i,bi,bj) )
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
     &           float(seaiceSpongeThickness-isl)*SLrelaxobcsbound
     &           + float(isl-1)*SLrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              HSALT(i,j,bi,bj) = HSALT(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HSALT(i,j,bi,bj) - OBEsl(j,bi,bj) )
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
     &           float(seaiceSpongeThickness-isl)*SLrelaxobcsbound
     &           + float(isl-1)*SLrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              HSALT(i,j,bi,bj) = HSALT(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HSALT(i,j,bi,bj) - OBWsl(j,bi,bj) )
             ENDIF
            ENDDO
           ENDIF
          ENDDO
         ENDIF
# endif

        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_OBCS & ALLOW_SEAICE & ALLOW_OBCS_SEAICE_SPONGE & SEAICE_VARIABLE_SALINITY */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
      SUBROUTINE OBCS_SEAICE_SPONGE_SN( myThid )
C     *==========================================================*
C     | S/R OBCS_SEAICE_SPONGE_SN
C     | Adds a relaxation term to HSNOW near Open-Boundaries
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
     &           float(seaiceSpongeThickness-jsl)*SNrelaxobcsbound
     &           + float(jsl-1)*SNrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              IF (lambda_obcs.ne.0.) THEN
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              ELSE
               lambda_obcs = 0. _d 0
              ENDIF
              HSNOW(i,j,bi,bj) = HSNOW(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HSNOW(i,j,bi,bj) - OBNsn(i,bi,bj) )
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
     &           float(seaiceSpongeThickness-jsl)*SNrelaxobcsbound
     &           + float(jsl-1)*SNrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              HSNOW(i,j,bi,bj) = HSNOW(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HSNOW(i,j,bi,bj) - OBSsn(i,bi,bj) )
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
     &           float(seaiceSpongeThickness-isl)*SNrelaxobcsbound
     &           + float(isl-1)*SNrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              HSNOW(i,j,bi,bj) = HSNOW(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HSNOW(i,j,bi,bj) - OBEsn(j,bi,bj) )
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
     &           float(seaiceSpongeThickness-isl)*SNrelaxobcsbound
     &           + float(isl-1)*SNrelaxobcsinner)
     &           / float(seaiceSpongeThickness-1)
              if (lambda_obcs.ne.0.) then
               lambda_obcs = SEAICE_deltaTtherm / lambda_obcs
              else
               lambda_obcs = 0. _d 0
              endif
              HSNOW(i,j,bi,bj) = HSNOW(i,j,bi,bj)
     &           - maskC(i,j,1,bi,bj) * lambda_obcs
     &           * ( HSNOW(i,j,bi,bj) - OBWsn(j,bi,bj) )
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
