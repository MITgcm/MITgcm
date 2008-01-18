C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_seaice.F,v 1.1 2008/01/18 01:38:34 dimitri Exp $
C $Name:  $

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
      INTEGER I,J,K,bi,bj,I_obc,J_obc
      _RL obc_mask

      K = 1

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C     Set model variables to OB values on Northern Boundary
#ifdef ALLOW_OBCS_NORTH
        DO I=1-Olx,sNx+Olx
         J_obc = OB_Jn(I,bi,bj)
         IF (J_obc.NE.0) THEN
          obc_mask = _maskS(I,J_obc,K,bi,bj)
          DO J = J_obc, J_obc+Oly
           IF ( SEAICEadvHeff .AND. OBNhfile .NE. ' ' )
     &          HEFF(i,j,1,bi,bj) = OBNh (I,bi,bj) * obc_mask
           IF ( SEAICEadvArea .AND. OBNafile .NE. ' ' )
     &          AREA(i,j,1,bi,bj) = OBNa (I,bi,bj) * obc_mask
           IF ( SEAICEadvSNOW .AND. OBNsnfile .NE. ' ' )
     &          HSNOW(i,j,bi,bj)  = OBNsn(I,bi,bj) * obc_mask
# ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBNslfile .NE. ' ' )
     &          HSALT(i,j,bi,bj)  = OBNsl(I,bi,bj) * obc_mask
# endif
          ENDDO
         ENDIF
        ENDDO
#endif /* ALLOW_OBCS_NORTH */

C     Set model variables to OB values on Southern Boundary
#ifdef ALLOW_OBCS_SOUTH
        DO I=1-Olx,sNx+Olx
         J_obc = OB_Js(I,bi,bj)
         IF (J_obc.NE.0) THEN
          obc_mask = _maskS(I,J_obc+1,K,bi,bj)
          DO J = J_obc-Oly, J_obc
           IF ( SEAICEadvHeff .AND. OBShfile .NE. ' ' )
     &          HEFF(i,j,1,bi,bj) = OBSh (I,bi,bj) * obc_mask
           IF ( SEAICEadvArea .AND. OBSafile .NE. ' ' )
     &          AREA(i,j,1,bi,bj) = OBSa (I,bi,bj) * obc_mask
           IF ( SEAICEadvSNOW .AND. OBSsnfile .NE. ' ' )
     &          HSNOW(i,j,bi,bj)  = OBSsn(I,bi,bj) * obc_mask
# ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBSslfile .NE. ' ' )
     &          HSALT(i,j,bi,bj)  = OBSsl(I,bi,bj) * obc_mask
# endif
          ENDDO
         ENDIF
        ENDDO
#endif /* ALLOW_OBCS_SOUTH */

C     Set model variables to OB values on Eastern Boundary
#ifdef ALLOW_OBCS_EAST
        DO J=1-Oly,sNy+Oly
         I_obc = OB_Ie(J,bi,bj)
         IF (I_obc.NE.0) THEN
          obc_mask = _maskW(I_obc,J,K,bi,bj)
          DO I = I_obc, I_obc+Olx
           IF ( SEAICEadvHeff .AND. OBEhfile .NE. ' ' )
     &          HEFF(i,j,1,bi,bj) = OBEh (J,bi,bj) * obc_mask
           IF ( SEAICEadvArea .AND. OBEafile .NE. ' ' )
     &          AREA(i,j,1,bi,bj) = OBEa (J,bi,bj) * obc_mask
           IF ( SEAICEadvSNOW .AND. OBEsnfile .NE. ' ' )
     &          HSNOW(i,j,bi,bj)  = OBEsn(J,bi,bj) * obc_mask
# ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBEslfile .NE. ' ' )
     &          HSALT(i,j,bi,bj)  = OBEsl(J,bi,bj) * obc_mask
# endif
          ENDDO
         ENDIF
        ENDDO
#endif /* ALLOW_OBCS_EAST */

C     Set model variables to OB values on Western Boundary
#ifdef ALLOW_OBCS_WEST
        DO J=1-Oly,sNy+Oly
         I_obc=OB_Iw(J,bi,bj)
         IF (I_obc.NE.0) THEN
          obc_mask = _maskW(I_obc+1,J,K,bi,bj)
          DO I = I_obc-Olx, I_obc
           IF ( SEAICEadvHeff .AND. OBWhfile .NE. ' ' )
     &          HEFF(i,j,1,bi,bj) = OBWh (J,bi,bj) * obc_mask
           IF ( SEAICEadvArea .AND. OBWafile .NE. ' ' )
     &          AREA(i,j,1,bi,bj) = OBWa (J,bi,bj) * obc_mask
           IF ( SEAICEadvSNOW .AND. OBWsnfile .NE. ' ' )
     &          HSNOW(i,j,bi,bj)  = OBWsn(J,bi,bj) * obc_mask
# ifdef SEAICE_SALINITY
           IF ( SEAICEadvSALT .AND. OBWslfile .NE. ' ' )
     &          HSALT(i,j,bi,bj)  = OBWsl(J,bi,bj) * obc_mask
# endif
          ENDDO
         ENDIF
        ENDDO
#endif /* ALLOW_OBCS_WEST */

       ENDDO
      ENDDO

#endif /* defined (ALLOW_OBCS) && defined (ALLOW_SEAICE) */

      RETURN
      END
