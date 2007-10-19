C $Header: /u/gcmpack/MITgcm/pkg/kpp/Attic/kpp_init.F,v 1.18 2007/10/19 19:11:17 jmc Exp $
C $Name:  $

#include "KPP_OPTIONS.h"
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: KPP_INIT

C     !INTERFACE:
      SUBROUTINE KPP_INIT( myThid )

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
C     zehat       - zeta * ustar**3
C     zeta        - Stability parameter d/l
      INTEGER i, j, k, bi, bj
      _RL zehat
      _RL zeta
      _RL usta
      _RL p25, p33

      _BEGIN_MASTER(myThid)

      p25 = 0.25 _d 0
      p33 = 1. _d 0 / 3. _d 0

C-----------------------------------------------------------------------
C Initialize constants that depend on parameters in data.kpp
C-----------------------------------------------------------------------

      Vtc     = concv * SQRT(0.2 _d 0 /concs/epsilon) / vonk**2 / Ricr
      cg      = cstar * vonk * (concs * vonk * epsilon)**p33

c-----------------------------------------------------------------------
c construct the wm and ws lookup tables
c-----------------------------------------------------------------------

      deltaz = (zmax - zmin)/(nni + 1)
      deltau = (umax - umin)/(nnj + 1)

      DO i = 0, nni + 1
         zehat = deltaz*i + zmin
         DO j = 0, nnj + 1
            usta = deltau*j + umin
            zeta = zehat / max(phepsi,usta**3)
            IF (zehat .GE. 0.) THEN
               wmt(i,j) = vonk*usta/(1. + conc1*zeta)
               wst(i,j) = wmt(i,j)
            ELSE
               IF (zeta .GT. zetam) THEN
                  wmt(i,j) = vonk*usta*(1. - conc2*zeta)**p25
               ELSE
                  wmt(i,j) = vonk*(conam*usta**3 - concm*zehat)**p33
               ENDIF
               IF (zeta .GT. zetas) THEN
                  wst(i,j) = vonk*usta*SQRT(1. _d 0 - conc3*zeta)
               ELSE
                  wst(i,j) = vonk*(conas*usta**3 - concs*zehat)**p33
               ENDIF
            ENDIF
         ENDDO
      ENDDO

C-----------------------------------------------------------------------
C     vertical grid
C-----------------------------------------------------------------------

      zgrid(0)  =  phepsi
      hwide(0)  =  phepsi
c     zgrid(1)  = -drF(1)*0.5
c     hwide(1)  =  drF(1)
c     DO k = 2, Nr
c        zgrid(k) = zgrid(k-1) - (drF(k-1)+drF(k))*0.5
c        hwide(k) = drF(k)
c     ENDDO
C- jmc : use the model vertical grid :
      DO k = 1, Nr
         zgrid(k) = rC(k)
         hwide(k) = drF(k)
      ENDDO

      zgrid(Nrp1) = zgrid(Nr) * 100.

      hwide(Nrp1) = phepsi

      _END_MASTER(myThid)
      _BARRIER

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
                     KPPviscAz  (i,j,k,bi,bj) = viscAr
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
