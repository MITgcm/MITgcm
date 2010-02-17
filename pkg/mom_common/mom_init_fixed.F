C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_init_fixed.F,v 1.9 2010/02/17 23:39:37 gforget Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MOM_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE MOM_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize fixed quantities
C      for momentum (common to fluxform & vecinv) packages

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "MOM_VISC.h"
#if defined (ALLOW_3D_VISCAH) || defined (ALLOW_3D_VISCA4)
#include "DYNVARS.h"
#endif

C     !INPUT PARAMETERS:
C     myThid               :: thread number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j,k,bi,bj  :: loop counter
      INTEGER i,j,bi,bj
#if (defined (NONLIN_FRSURF) || \
     defined (ALLOW_3D_VISCAH) || \
     defined (ALLOW_3D_VISCA4))
      INTEGER k
#endif

#ifdef NONLIN_FRSURF
C-    used for side-drag:
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            h0FacC(i,j,k,bi,bj) = _hFacC(i,j,k,bi,bj)
            h0FacW(i,j,k,bi,bj) = _hFacW(i,j,k,bi,bj)
            h0FacS(i,j,k,bi,bj) = _hFacS(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
       ENDDO
      ENDDO
#endif /* NONLIN_FRSURF */

#if defined (ALLOW_3D_VISCAH) || defined (ALLOW_3D_VISCA4)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
#ifdef ALLOW_3D_VISCAH
           viscAhDfld(i,j,k,bi,bj) = 0. _d 0
           viscAhZfld(i,j,k,bi,bj) = 0. _d 0
#endif
#ifdef ALLOW_3D_VISCA4           
           viscA4Dfld(i,j,k,bi,bj) = 0. _d 0
           viscA4Zfld(i,j,k,bi,bj) = 0. _d 0
#endif          
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_3D_VISCAH */

       IF (deltaTmom.NE.0.) THEN
        recip_dt = 1. _d 0/deltaTmom
       ELSE
        recip_dt = 0. _d 0
       ENDIF

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            L2_D(i,j,bi,bj) = rA(i,j,bi,bj)
          ENDDO
         ENDDO
         IF ( .NOT.useAreaViscLength ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            IF ( recip_dxF(i,j,bi,bj).NE.0. .OR.
     &           recip_dyF(i,j,bi,bj).NE.0. ) THEN
              L2_D(i,j,bi,bj) = 2. _d 0
     &           /((recip_dxF(i,j,bi,bj)**2+recip_dyF(i,j,bi,bj)**2))
            ENDIF
           ENDDO
          ENDDO
         ENDIF
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            L3_D(i,j,bi,bj) = L2_D(i,j,bi,bj)**1.5
            L4rdt_D(i,j,bi,bj) = 0.03125 _d 0*recip_dt
     &                       *L2_D(i,j,bi,bj)**2
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            L2_Z(i,j,bi,bj) = rAz(i,j,bi,bj)
          ENDDO
         ENDDO
         IF ( .NOT.useAreaViscLength ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            IF ( recip_dxV(i,j,bi,bj).NE.0. .OR.
     &           recip_dyU(i,j,bi,bj).NE.0. ) THEN
              L2_Z(i,j,bi,bj) = 2. _d 0
     &           /((recip_dxV(i,j,bi,bj)**2+recip_dyU(i,j,bi,bj)**2))
            ENDIF
           ENDDO
          ENDDO
         ENDIF
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            L3_Z(i,j,bi,bj) = L2_Z(i,j,bi,bj)**1.5
            L4rdt_Z(i,j,bi,bj) = 0.03125 _d 0*recip_dt
     &                       *L2_Z(i,j,bi,bj)**2
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#ifdef ALLOW_3D_VISCAH
       IF ( viscAhDfile .NE. ' ' ) THEN
          CALL READ_FLD_XYZ_RL(viscAhDfile,' ',viscAhDfld,0,myThid)
          CALL EXCH_3D_RL( viscAhDfld, Nr, myThid )
       ENDIF
       IF ( viscAhZfile .NE. ' ' ) THEN
          CALL READ_FLD_XYZ_RL(viscAhZfile,' ',viscAhZfld,0,myThid)
          CALL EXCH_Z_3D_RL( viscAhZfld, Nr, myThid )
       ENDIF
#endif
#ifdef ALLOW_3D_VISCA4
       IF ( viscA4Dfile .NE. ' ' ) THEN
          CALL READ_FLD_XYZ_RL(viscA4Dfile,' ',viscA4Dfld,0,myThid)
          CALL EXCH_3D_RL( viscA4Dfld, Nr, myThid )
       ENDIF
       IF ( viscA4Zfile .NE. ' ' ) THEN
          CALL READ_FLD_XYZ_RL(viscA4Zfile,' ',viscA4Zfld,0,myThid)
          CALL EXCH_Z_3D_RL( viscA4Zfld, Nr, myThid )
       ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL MOM_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
