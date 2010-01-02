C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_init_varia.F,v 1.3 2010/01/02 23:16:22 jmc Exp $
C $Name:  $

#include "KPP_OPTIONS.h"
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: KPP_INIT_VARIA

C     !INTERFACE:
      SUBROUTINE KPP_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Routine to initialize KPP parameters and variables.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#include "KPP.h"
#include "KPP_PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_KPP
C     !LOCAL VARIABLES:
C     i,j,k,bi,bj - Loop counters
      INTEGER i, j, k, bi, bj

C-----------------------------------------------------------------------
C     calculate mask pMask for pressure/tracer cells
C        (0 => land, 1 => water)
C     compute maximum number of wet levels in each column
C-----------------------------------------------------------------------

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

             DO j = 1-OLy, sNy+OLy
               DO i = 1-OLx, sNx+OLx
                  nzmax(i,j,bi,bj) = kLowC(I,J,bi,bj)
CML                  DO k = 1, Nr
CMLc                    IF (_hFacC(i,j,k,bi,bj).EQ.0.) THEN
CMLc                       pMask (i,j,k,bi,bj) = 0.
CMLc                    ELSE
CMLc                       pMask (i,j,k,bi,bj) = 1.
CMLc                    ENDIF
CML                     nzmax(i,j,bi,bj) = nzmax(i,j,bi,bj)
CML     &                                + INT(maskC(i,j,k,bi,bj))
CMLc    &                                + INT(pMask(i,j,k,bi,bj))
CML                  ENDDO
               ENDDO
            ENDDO

C-----------------------------------------------------------------------
C Initialize KPP variables KPPhbl, KPPghat, KPPviscAz,
C    KPPdiffKzT, and KPPdiffKzS
C-----------------------------------------------------------------------

            DO j = 1-OLy, sNy+OLy
               DO i = 1-OLx, sNx+OLx
                  KPPhbl(i,j,bi,bj) = 0.
               ENDDO
            ENDDO
            DO k = 1, Nr
               DO j = 1-OLy, sNy+OLy
                  DO i = 1-OLx, sNx+OLx
                     KPPghat       (i,j,k,bi,bj) = 0.
                     KPPviscAz  (i,j,k,bi,bj) = viscArNr(1)
                     KPPdiffKzS (i,j,k,bi,bj) = 0.
                     KPPdiffKzT (i,j,k,bi,bj) = 0.
                  ENDDO
               ENDDO
            ENDDO

cph Not needed here. Do this in kpp_calc.
cph            CALL CALC_3D_DIFFUSIVITY(
cph     I           bi,bj,1-Olx,sNx+OLx,1-Oly,sNy+OLy,
cph     I           GAD_SALINITY, .FALSE., .FALSE.,
cph     O           KPPdiffKzS(1-Olx,1-Oly,1,bi,bj),
cph     I           myThid)
cph            CALL CALC_3D_DIFFUSIVITY(
cph     I           bi,bj,1-Olx,sNx+OLx,1-Oly,sNy+OLy,
cph     I           GAD_TEMPERATURE, .FALSE., .FALSE.,
cph     O           KPPdiffKzT(1-Olx,1-Oly,1,bi,bj),
cph     I           myThid)

       ENDDO
      ENDDO

#endif /* ALLOW_KPP */

      RETURN
      END
