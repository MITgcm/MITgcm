C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_seaice.F,v 1.6 2010/11/14 23:26:39 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_APPLY_SEAICE
C     !INTERFACE:

      SUBROUTINE OBCS_APPLY_SEAICE( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R OBCS_APPLY_SEAICE
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
# include "SEAICE.h"
# include "SEAICE_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C    myThid   :: my Thread Id number
      INTEGER myThid
CEOP

#if (defined (ALLOW_OBCS) && defined (ALLOW_SEAICE))

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi, bj
      INTEGER i, j
      INTEGER Iobc, Jobc
#ifdef OBCS_SEAICE_SMOOTH_EDGE
      INTEGER k
      _RL edge_val
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_OBCS_NORTH
        IF ( tileHasOBN(bi,bj) ) THEN
C     Set model variables to OB values on Northern Boundary
         DO i=1-Olx,sNx+Olx
          Jobc = OB_Jn(I,bi,bj)
          IF ( Jobc.NE.0 ) THEN
            DO j = Jobc, Jobc+Oly
              HEFF(i,j,bi,bj)  = OBNh (i,bi,bj)
              AREA(i,j,bi,bj)  = OBNa (i,bi,bj)
              HSNOW(i,j,bi,bj) = OBNsn(i,bi,bj)
# ifdef SEAICE_SALINITY
              HSALT(i,j,bi,bj) = OBNsl(i,bi,bj)
# endif
            ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
        IF ( tileHasOBS(bi,bj) ) THEN
C     Set model variables to OB values on Southern Boundary
         DO i=1-Olx,sNx+Olx
          Jobc = OB_Js(I,bi,bj)
          IF ( Jobc.NE.0 ) THEN
            DO j = Jobc-Oly, Jobc
              HEFF(i,j,bi,bj)  = OBSh (i,bi,bj)
              AREA(i,j,bi,bj)  = OBSa (i,bi,bj)
              HSNOW(i,j,bi,bj) = OBSsn(i,bi,bj)
# ifdef SEAICE_SALINITY
              HSALT(i,j,bi,bj) = OBSsl(i,bi,bj)
# endif
            ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_SOUTH */

#ifdef ALLOW_OBCS_EAST
        IF ( tileHasOBE(bi,bj) ) THEN
C     Set model variables to OB values on Eastern Boundary
         DO j=1-Oly,sNy+Oly
          Iobc = OB_Ie(J,bi,bj)
          IF (Iobc.NE.0) THEN
            DO i = Iobc, Iobc+Olx
              HEFF(i,j,bi,bj)  = OBEh (j,bi,bj)
              AREA(i,j,bi,bj)  = OBEa (j,bi,bj)
              HSNOW(i,j,bi,bj) = OBEsn(j,bi,bj)
# ifdef SEAICE_SALINITY
              HSALT(i,j,bi,bj) = OBEsl(j,bi,bj)
# endif
            ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
        IF ( tileHasOBW(bi,bj) ) THEN
C     Set model variables to OB values on Western Boundary
         DO j=1-Oly,sNy+Oly
          Iobc=OB_Iw(j,bi,bj)
          IF (Iobc.NE.0) THEN
            DO i = Iobc-Olx, Iobc
              HEFF(i,j,bi,bj)  = OBWh (j,bi,bj)
              AREA(i,j,bi,bj)  = OBWa (j,bi,bj)
              HSNOW(i,j,bi,bj) = OBWsn(j,bi,bj)
# ifdef SEAICE_SALINITY
              HSALT(i,j,bi,bj) = OBWsl(j,bi,bj)
# endif
            ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_WEST */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef OBCS_SEAICE_SMOOTH_EDGE
        k = 1

#ifdef ALLOW_OBCS_NORTH
        IF ( tileHasOBN(bi,bj) ) THEN
C     Northern Boundary
         DO i=1-Olx,sNx+Olx
          Jobc = OB_Jn(I,bi,bj)
          IF ( Jobc.NE.0 ) THEN
           IF ( _maskS(i,Jobc-1,k,bi,bj) .NE. 0. ) THEN
            IF ( SEAICEadvHeff .AND. OBNhfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HEFF(i,Jobc-1,bi,bj)+HEFF(i,Jobc-2,bi,bj))
             HEFF(i,Jobc-1,bi,bj) = edge_val
             HEFF(i,Jobc-2,bi,bj) = edge_val
            ENDIF
            IF ( SEAICEadvArea .AND. OBNafile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (AREA(i,Jobc-1,bi,bj)+AREA(i,Jobc-2,bi,bj))
             AREA(i,Jobc-1,bi,bj) = edge_val
             AREA(i,Jobc-2,bi,bj) = edge_val
            ENDIF
            IF ( SEAICEadvSNOW .AND. OBNsnfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HSNOW(i,Jobc-1,bi,bj)+HSNOW(i,Jobc-2,bi,bj))
             HSNOW(i,Jobc-1,bi,bj) = edge_val
             HSNOW(i,Jobc-2,bi,bj) = edge_val
            ENDIF
#  ifdef SEAICE_SALINITY
            IF ( SEAICEadvSALT .AND. OBNslfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HSALT(i,Jobc-1,bi,bj)+HSALT(i,Jobc-2,bi,bj))
             HSALT(i,Jobc-1,bi,bj) = edge_val
             HSALT(i,Jobc-2,bi,bj) = edge_val
            ENDIF
#  endif
           ENDIF
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
        IF ( tileHasOBS(bi,bj) ) THEN
C     Southern Boundary
         DO i=1-Olx,sNx+Olx
          Jobc = OB_Js(I,bi,bj)
          IF ( Jobc.NE.0 ) THEN
           IF ( _maskS(i,Jobc+2,k,bi,bj) .NE. 0. ) THEN
            IF ( SEAICEadvHeff .AND. OBNhfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HEFF(i,Jobc+1,bi,bj)+HEFF(i,Jobc+2,bi,bj))
             HEFF(i,Jobc+1,bi,bj) = edge_val
             HEFF(i,Jobc+2,bi,bj) = edge_val
            ENDIF
            IF ( SEAICEadvArea .AND. OBNafile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (AREA(i,Jobc+1,bi,bj)+AREA(i,Jobc+2,bi,bj))
             AREA(i,Jobc+1,bi,bj) = edge_val
             AREA(i,Jobc+2,bi,bj) = edge_val
            ENDIF
            IF ( SEAICEadvSNOW .AND. OBNsnfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HSNOW(i,Jobc+1,bi,bj)+HSNOW(i,Jobc+2,bi,bj))
             HSNOW(i,Jobc+1,bi,bj) = edge_val
             HSNOW(i,Jobc+2,bi,bj) = edge_val
            ENDIF
#  ifdef SEAICE_SALINITY
            IF ( SEAICEadvSALT .AND. OBNslfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HSALT(i,Jobc+1,bi,bj)+HSALT(i,Jobc+2,bi,bj))
             HSALT(i,Jobc+1,bi,bj) = edge_val
             HSALT(i,Jobc+2,bi,bj) = edge_val
            ENDIF
#  endif
           ENDIF
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_SOUTH */

#ifdef ALLOW_OBCS_EAST
        IF ( tileHasOBE(bi,bj) ) THEN
C     Eastern Boundary
         DO j=1-Oly,sNy+Oly
          Iobc = OB_Ie(J,bi,bj)
          IF (Iobc.NE.0) THEN
           IF ( _maskW(Iobc-1,j,k,bi,bj) .NE. 0. ) THEN
            IF ( SEAICEadvHeff .AND. OBNhfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HEFF(Iobc-1,j,bi,bj)+HEFF(Iobc-2,j,bi,bj))
             HEFF(Iobc-1,j,bi,bj) = edge_val
             HEFF(Iobc-2,j,bi,bj) = edge_val
            ENDIF
            IF ( SEAICEadvArea .AND. OBNafile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (AREA(Iobc-1,j,bi,bj)+AREA(Iobc-2,j,bi,bj))
             AREA(Iobc-1,j,bi,bj) = edge_val
             AREA(Iobc-2,j,bi,bj) = edge_val
            ENDIF
            IF ( SEAICEadvSNOW .AND. OBNsnfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HSNOW(Iobc-1,j,bi,bj)+HSNOW(Iobc-2,j,bi,bj))
             HSNOW(Iobc-1,j,bi,bj) = edge_val
             HSNOW(Iobc-2,j,bi,bj) = edge_val
            ENDIF
#  ifdef SEAICE_SALINITY
            IF ( SEAICEadvSALT .AND. OBNslfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HSALT(Iobc-1,j,bi,bj)+HSALT(Iobc-2,j,bi,bj))
             HSALT(Iobc-1,j,bi,bj) = edge_val
             HSALT(Iobc-2,j,bi,bj) = edge_val
            ENDIF
#  endif
           ENDIF
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
        IF ( tileHasOBW(bi,bj) ) THEN
C     Western Boundary
         DO j=1-Oly,sNy+Oly
          Iobc=OB_Iw(j,bi,bj)
          IF (Iobc.NE.0) THEN
           IF ( _maskW(Iobc+2,j,k,bi,bj) .NE. 0. ) THEN
            IF ( SEAICEadvHeff .AND. OBNhfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HEFF(Iobc+1,j,bi,bj)+HEFF(Iobc+2,j,bi,bj))
             HEFF(Iobc+1,j,bi,bj) = edge_val
             HEFF(Iobc+2,j,bi,bj) = edge_val
            ENDIF
            IF ( SEAICEadvArea .AND. OBNafile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (AREA(Iobc+1,j,bi,bj)+AREA(Iobc+2,j,bi,bj))
             AREA(Iobc+1,j,bi,bj) = edge_val
             AREA(Iobc+2,j,bi,bj) = edge_val
            ENDIF
            IF ( SEAICEadvSNOW .AND. OBNsnfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HSNOW(Iobc+1,j,bi,bj)+HSNOW(Iobc+2,j,bi,bj))
             HSNOW(Iobc+1,j,bi,bj) = edge_val
             HSNOW(Iobc+2,j,bi,bj) = edge_val
            ENDIF
#  ifdef SEAICE_SALINITY
            IF ( SEAICEadvSALT .AND. OBNslfile .NE. ' ' ) THEN
             edge_val = 0.5 _d 0 *
     &           (HSALT(Iobc+1,j,bi,bj)+HSALT(Iobc+2,j,bi,bj))
             HSALT(Iobc+1,j,bi,bj) = edge_val
             HSALT(Iobc+2,j,bi,bj) = edge_val
            ENDIF
#  endif
           ENDIF
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_WEST */

#endif /* OBCS_SEAICE_SMOOTH_EDGE */

       ENDDO
      ENDDO

#endif /* defined (ALLOW_OBCS) && defined (ALLOW_SEAICE) */

      RETURN
      END
