C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_seaice.F,v 1.2 2008/01/19 01:57:11 dimitri Exp $
C $Name:  $

#undef OBCS_SEAICE_SMOOTH_EDGE

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_SEAICE( myThid )
C     /==========================================================\
C     | S/R OBCS_APPLY_SEAICE                                    |
C     \==========================================================/
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

C     == Routine Arguments ==
      INTEGER myThid

#if (defined (ALLOW_OBCS) && defined (ALLOW_SEAICE))

C     == Local variables ==
      INTEGER I, J, bi, bj, I_obc, J_obc
      _RL edge_val

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C     Set model variables to OB values on Northern Boundary
#ifdef ALLOW_OBCS_NORTH
        DO I=1-Olx,sNx+Olx
         J_obc = OB_Jn(I,bi,bj)
         IF ( J_obc.NE.0 ) THEN
          DO J = J_obc, J_obc+Oly
           IF ( SEAICEadvHeff .AND. OBNhfile .NE. ' ' )
     &          HEFF(I,J,1,bi,bj) = OBNh (I,bi,bj)
           IF ( SEAICEadvArea .AND. OBNafile .NE. ' ' )
     &          AREA(I,J,1,bi,bj) = OBNa (I,bi,bj)
           IF ( SEAICEadvSNOW .AND. OBNsnfile .NE. ' ' )
     &          HSNOW(I,J,bi,bj)  = OBNsn(I,bi,bj)
# ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBNslfile .NE. ' ' )
     &          HSALT(I,J,bi,bj)  = OBNsl(I,bi,bj)
# endif
          ENDDO
# ifdef OBCS_SEAICE_SMOOTH_EDGE
          IF ( _maskS(I,J_obc-1,1,bi,bj) .NE. 0. ) THEN
           IF ( SEAICEadvHeff .AND. OBNhfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HEFF(I,J_obc-1,1,bi,bj)+HEFF(I,J_obc-2,1,bi,bj))
            HEFF(I,J_obc-1,1,bi,bj) = edge_val
            HEFF(I,J_obc-2,1,bi,bj) = edge_val
           ENDIF
           IF ( SEAICEadvArea .AND. OBNafile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (AREA(I,J_obc-1,1,bi,bj)+AREA(I,J_obc-2,1,bi,bj))
            AREA(I,J_obc-1,1,bi,bj) = edge_val
            AREA(I,J_obc-2,1,bi,bj) = edge_val
           ENDIF
           IF ( SEAICEadvSNOW .AND. OBNsnfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HSNOW(I,J_obc-1,bi,bj)+HSNOW(I,J_obc-2,bi,bj))
            HSNOW(I,J_obc-1,bi,bj) = edge_val
            HSNOW(I,J_obc-2,bi,bj) = edge_val
           ENDIF
#  ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBNslfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HSALT(I,J_obc-1,bi,bj)+HSALT(I,J_obc-2,bi,bj))
            HSALT(I,J_obc-1,bi,bj) = edge_val
            HSALT(I,J_obc-2,bi,bj) = edge_val
           ENDIF
#  endif
          ENDIF
# endif /* OBCS_SEAICE_SMOOTH_EDGE */
         ENDIF
        ENDDO
#endif /* ALLOW_OBCS_NORTH */

C     Set model variables to OB values on Southern Boundary
#ifdef ALLOW_OBCS_SOUTH
        DO I=1-Olx,sNx+Olx
         J_obc = OB_Js(I,bi,bj)
         IF ( J_obc.NE.0 ) THEN
          DO J = J_obc-Oly, J_obc
           IF ( SEAICEadvHeff .AND. OBShfile .NE. ' ' )
     &          HEFF(I,J,1,bi,bj) = OBSh (I,bi,bj)
           IF ( SEAICEadvArea .AND. OBSafile .NE. ' ' )
     &          AREA(I,J,1,bi,bj) = OBSa (I,bi,bj)
           IF ( SEAICEadvSNOW .AND. OBSsnfile .NE. ' ' )
     &          HSNOW(I,J,bi,bj)  = OBSsn(I,bi,bj)
# ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBSslfile .NE. ' ' )
     &          HSALT(I,J,bi,bj)  = OBSsl(I,bi,bj)
# endif
          ENDDO
# ifdef OBCS_SEAICE_SMOOTH_EDGE
          IF ( _maskS(I,J_obc+2,1,bi,bj) .NE. 0. ) THEN
           IF ( SEAICEadvHeff .AND. OBNhfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HEFF(I,J_obc+1,1,bi,bj)+HEFF(I,J_obc+2,1,bi,bj))
            HEFF(I,J_obc+1,1,bi,bj) = edge_val
            HEFF(I,J_obc+2,1,bi,bj) = edge_val
           ENDIF
           IF ( SEAICEadvArea .AND. OBNafile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (AREA(I,J_obc+1,1,bi,bj)+AREA(I,J_obc+2,1,bi,bj))
            AREA(I,J_obc+1,1,bi,bj) = edge_val
            AREA(I,J_obc+2,1,bi,bj) = edge_val
           ENDIF
           IF ( SEAICEadvSNOW .AND. OBNsnfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HSNOW(I,J_obc+1,bi,bj)+HSNOW(I,J_obc+2,bi,bj))
            HSNOW(I,J_obc+1,bi,bj) = edge_val
            HSNOW(I,J_obc+2,bi,bj) = edge_val
           ENDIF
#  ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBNslfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HSALT(I,J_obc+1,bi,bj)+HSALT(I,J_obc+2,bi,bj))
            HSALT(I,J_obc+1,bi,bj) = edge_val
            HSALT(I,J_obc+2,bi,bj) = edge_val
           ENDIF
#  endif
          ENDIF
# endif /* OBCS_SEAICE_SMOOTH_EDGE */
         ENDIF
        ENDDO
#endif /* ALLOW_OBCS_SOUTH */

C     Set model variables to OB values on Eastern Boundary
#ifdef ALLOW_OBCS_EAST
        DO J=1-Oly,sNy+Oly
         I_obc = OB_Ie(J,bi,bj)
         IF (I_obc.NE.0) THEN
          DO I = I_obc, I_obc+Olx
           IF ( SEAICEadvHeff .AND. OBEhfile .NE. ' ' )
     &          HEFF(I,J,1,bi,bj) = OBEh (J,bi,bj)
           IF ( SEAICEadvArea .AND. OBEafile .NE. ' ' )
     &          AREA(I,J,1,bi,bj) = OBEa (J,bi,bj)
           IF ( SEAICEadvSNOW .AND. OBEsnfile .NE. ' ' )
     &          HSNOW(I,J,bi,bj)  = OBEsn(J,bi,bj)
# ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBEslfile .NE. ' ' )
     &          HSALT(I,J,bi,bj)  = OBEsl(J,bi,bj)
# endif
          ENDDO
# ifdef OBCS_SEAICE_SMOOTH_EDGE
          IF ( _maskW(I_obc-1,J,1,bi,bj) .NE. 0. ) THEN
           IF ( SEAICEadvHeff .AND. OBNhfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HEFF(I_obc-1,J,1,bi,bj)+HEFF(I_obc-2,J,1,bi,bj))
            HEFF(I_obc-1,J,1,bi,bj) = edge_val
            HEFF(I_obc-2,J,1,bi,bj) = edge_val
           ENDIF
           IF ( SEAICEadvArea .AND. OBNafile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (AREA(I_obc-1,J,1,bi,bj)+AREA(I_obc-2,J,1,bi,bj))
            AREA(I_obc-1,J,1,bi,bj) = edge_val
            AREA(I_obc-2,J,1,bi,bj) = edge_val
           ENDIF
           IF ( SEAICEadvSNOW .AND. OBNsnfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HSNOW(I_obc-1,J,bi,bj)+HSNOW(I_obc-2,J,bi,bj))
            HSNOW(I_obc-1,J,bi,bj) = edge_val
            HSNOW(I_obc-2,J,bi,bj) = edge_val
           ENDIF
#  ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBNslfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HSALT(I_obc-1,J,bi,bj)+HSALT(I_obc-2,J,bi,bj))
            HSALT(I_obc-1,J,bi,bj) = edge_val
            HSALT(I_obc-2,J,bi,bj) = edge_val
           ENDIF
#  endif
          ENDIF
# endif /* OBCS_SEAICE_SMOOTH_EDGE */
         ENDIF
        ENDDO
#endif /* ALLOW_OBCS_EAST */

C     Set model variables to OB values on Western Boundary
#ifdef ALLOW_OBCS_WEST
        DO J=1-Oly,sNy+Oly
         I_obc=OB_Iw(J,bi,bj)
         IF (I_obc.NE.0) THEN
          DO I = I_obc-Olx, I_obc
           IF ( SEAICEadvHeff .AND. OBWhfile .NE. ' ' )
     &          HEFF(I,J,1,bi,bj) = OBWh (J,bi,bj)
           IF ( SEAICEadvArea .AND. OBWafile .NE. ' ' )
     &          AREA(I,J,1,bi,bj) = OBWa (J,bi,bj)
           IF ( SEAICEadvSNOW .AND. OBWsnfile .NE. ' ' )
     &          HSNOW(I,J,bi,bj)  = OBWsn(J,bi,bj)
# ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBWslfile .NE. ' ' )
     &          HSALT(I,J,bi,bj)  = OBWsl(J,bi,bj)
# endif
          ENDDO
# ifdef OBCS_SEAICE_SMOOTH_EDGE
          IF ( _maskW(I_obc+2,J,1,bi,bj) .NE. 0. ) THEN
           IF ( SEAICEadvHeff .AND. OBNhfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HEFF(I_obc+1,J,1,bi,bj)+HEFF(I_obc+2,J,1,bi,bj))
            HEFF(I_obc+1,J,1,bi,bj) = edge_val
            HEFF(I_obc+2,J,1,bi,bj) = edge_val
           ENDIF
           IF ( SEAICEadvArea .AND. OBNafile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (AREA(I_obc+1,J,1,bi,bj)+AREA(I_obc+2,J,1,bi,bj))
            AREA(I_obc+1,J,1,bi,bj) = edge_val
            AREA(I_obc+2,J,1,bi,bj) = edge_val
           ENDIF
           IF ( SEAICEadvSNOW .AND. OBNsnfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HSNOW(I_obc+1,J,bi,bj)+HSNOW(I_obc+2,J,bi,bj))
            HSNOW(I_obc+1,J,bi,bj) = edge_val
            HSNOW(I_obc+2,J,bi,bj) = edge_val
           ENDIF
#  ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBNslfile .NE. ' ' ) THEN
            edge_val = 0.5 _d 0 *
     &           (HSALT(I_obc+1,J,bi,bj)+HSALT(I_obc+2,J,bi,bj))
            HSALT(I_obc+1,J,bi,bj) = edge_val
            HSALT(I_obc+2,J,bi,bj) = edge_val
           ENDIF
#  endif
          ENDIF
# endif /* OBCS_SEAICE_SMOOTH_EDGE */
         ENDIF
        ENDDO
#endif /* ALLOW_OBCS_WEST */

       ENDDO
      ENDDO

#endif /* defined (ALLOW_OBCS) && defined (ALLOW_SEAICE) */

      RETURN
      END
