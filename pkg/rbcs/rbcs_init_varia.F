C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_init_varia.F,v 1.8 2011/05/14 19:52:12 jmc Exp $
C $Name:  $

#include "RBCS_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE RBCS_INIT_VARIA( myThid )

C !DESCRIPTION:
C calls subroutines that initialized variables for relaxed
c boundary conditions

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif
#include "RBCS_SIZE.h"
c#include "RBCS_PARAMS.h"
#include "RBCS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_RBCS

C     !LOCAL VARIABLES:
C     i,j,k,bi,bj,iTracer  :: loop indices
      INTEGER i,j,k,bi,bj
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
#endif

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          rbcsLdRec(bi,bj) = 0
          DO k=1,Nr
            DO j=1-Oly,sNy+OLy
              DO i=1-Olx,sNx+Olx
#ifndef DISABLE_RBCS_MOM
                rbcu0(i,j,k,bi,bj) = 0. _d 0
                rbcv0(i,j,k,bi,bj) = 0. _d 0
                rbcu1(i,j,k,bi,bj) = 0. _d 0
                rbcv1(i,j,k,bi,bj) = 0. _d 0
                RBCuVel(i,j,k,bi,bj) = 0. _d 0
                RBCvVel(i,j,k,bi,bj) = 0. _d 0
#endif
                rbct0(i,j,k,bi,bj) = 0. _d 0
                rbcs0(i,j,k,bi,bj) = 0. _d 0
                rbct1(i,j,k,bi,bj) = 0. _d 0
                rbcs1(i,j,k,bi,bj) = 0. _d 0
                RBCtemp(i,j,k,bi,bj) = 0. _d 0
                RBCsalt(i,j,k,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

#ifdef ALLOW_PTRACERS
C     Loop over tracers
      DO iTracer = 1, PTRACERS_num
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
             DO j=1-Oly,sNy+OLy
               DO i=1-Olx,sNx+Olx
                rbcptr0(i,j,k,bi,bj,iTracer) = 0. _d 0
                rbcptr1(i,j,k,bi,bj,iTracer) = 0. _d 0
                RBC_ptracers(i,j,k,bi,bj,iTracer) = 0. _d 0
               ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
C       end of Tracer loop
      ENDDO
#endif /* ALLOW_PTRACERS */

#endif /* ALLOW_RBCS */

      RETURN
      END
