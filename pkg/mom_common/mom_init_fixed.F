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

C     !INPUT PARAMETERS:
C     myThid               :: my thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j,k,bi,bj  :: loop counter
      INTEGER i,j,k,bi,bj
      _RL recip_dt
      _RL twoThird
#ifdef ALLOW_BOTTOMDRAG_ROUGHNESS
C     msgBuf :: Informational/error message buffer
      INTEGER kLow
      _RL hLoc
      _RL recip_zRoughBot
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif

      twoThird = 2. _d 0 / 3. _d 0
      recip_dt = 1. _d 0
      IF ( deltaTMom.NE.0. ) recip_dt = 1. _d 0/deltaTMom

      _BEGIN_MASTER(myThid)
      DO k=1,Nr
        deepFacAdv(k) = 1. _d 0
      ENDDO
#ifndef MOM_USE_OLD_DEEP_VERT_ADV
      IF ( useNHMTerms ) THEN
       DO k=1,Nr
        deepFacAdv(k) = deepFacC(k)
       ENDDO
      ENDIF
#endif
      _END_MASTER(myThid)

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_3D_VISCAH
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            viscAhDfld(i,j,k,bi,bj) = 0. _d 0
            viscAhZfld(i,j,k,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
#endif
#ifdef ALLOW_3D_VISCA4
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            viscA4Dfld(i,j,k,bi,bj) = 0. _d 0
            viscA4Zfld(i,j,k,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
#endif
#ifdef ALLOW_NONHYDROSTATIC
C--   Set the horizontal viscosities to default value.
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            viscAh_W(i,j,k,bi,bj) = viscAhW
            viscA4_W(i,j,k,bi,bj) = viscA4W
           ENDDO
          ENDDO
         ENDDO
#endif /* ALLOW_NONHYDROSTATIC */

C--   Calculate length-scale factor (used in S/R MOM_CALC_VISC)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            L2_D(i,j,bi,bj) = rA(i,j,bi,bj)
          ENDDO
         ENDDO
         IF ( .NOT.useAreaViscLength ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF ( recip_dxF(i,j,bi,bj).NE.0. .OR.
     &           recip_dyF(i,j,bi,bj).NE.0. ) THEN
              L2_D(i,j,bi,bj) = 2. _d 0
     &           /((recip_dxF(i,j,bi,bj)**2+recip_dyF(i,j,bi,bj)**2))
            ENDIF
           ENDDO
          ENDDO
         ENDIF
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            L3_D(i,j,bi,bj) = L2_D(i,j,bi,bj)**1.5
            L4rdt_D(i,j,bi,bj) = 0.03125 _d 0*recip_dt
     &                       *L2_D(i,j,bi,bj)**2
          ENDDO
         ENDDO

         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            L2_Z(i,j,bi,bj) = rAz(i,j,bi,bj)
          ENDDO
         ENDDO
         IF ( .NOT.useAreaViscLength ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF ( recip_dxV(i,j,bi,bj).NE.0. .OR.
     &           recip_dyU(i,j,bi,bj).NE.0. ) THEN
              L2_Z(i,j,bi,bj) = 2. _d 0
     &           /((recip_dxV(i,j,bi,bj)**2+recip_dyU(i,j,bi,bj)**2))
            ENDIF
           ENDDO
          ENDDO
         ENDIF
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            L3_Z(i,j,bi,bj) = L2_Z(i,j,bi,bj)**1.5
            L4rdt_Z(i,j,bi,bj) = 0.03125 _d 0*recip_dt
     &                       *L2_Z(i,j,bi,bj)**2
          ENDDO
         ENDDO

#ifdef ALLOW_SMAG_3D
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           smag3D_hLsC(i,j,bi,bj) = rA (i,j,bi,bj)**twoThird
           smag3D_hLsW(i,j,bi,bj) = rAw(i,j,bi,bj)**twoThird
           smag3D_hLsS(i,j,bi,bj) = rAs(i,j,bi,bj)**twoThird
           smag3D_hLsZ(i,j,bi,bj) = rAz(i,j,bi,bj)**twoThird
          ENDDO
         ENDDO
#endif /* ALLOW_SMAG_3D */

C-     end bi,bj loops
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
#endif /* ALLOW_3D_VISCAH */
#ifdef ALLOW_3D_VISCA4
      IF ( viscA4Dfile .NE. ' ' ) THEN
         CALL READ_FLD_XYZ_RL(viscA4Dfile,' ',viscA4Dfld,0,myThid)
         CALL EXCH_3D_RL( viscA4Dfld, Nr, myThid )
      ENDIF
      IF ( viscA4Zfile .NE. ' ' ) THEN
         CALL READ_FLD_XYZ_RL(viscA4Zfile,' ',viscA4Zfld,0,myThid)
         CALL EXCH_Z_3D_RL( viscA4Zfld, Nr, myThid )
      ENDIF
#endif /* ALLOW_3D_VISCA4 */

#ifdef ALLOW_BOTTOMDRAG_ROUGHNESS
C--   Compute bottom drag coefficents as a function of grid cell thickness
C     and roughness length, assuming von Karman constant = 0.4
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          bottomDragCoeffW(i,j,bi,bj) = bottomDragQuadratic
          bottomDragCoeffS(i,j,bi,bj) = bottomDragQuadratic
         ENDDO
        ENDDO
        IF ( zRoughBot .GT. 0. _d 0 .AND. usingZCoords ) THEN
         recip_zRoughBot = 1. _d 0 / zRoughBot
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx+1,sNx+OLx
           kLow = MIN( kLowC(i-1,j,bi,bj), kLowC(i,j,bi,bj)  )
           IF ( kLow .GE. 1 ) THEN
            hLoc = ( oneRL +
     &           0.5*drF(kLow)*hFacW(i,j,kLow,bi,bj)*recip_zRoughBot )
            hLoc = 0.4 _d 0/LOG(hLoc)
            bottomDragCoeffW(i,j,bi,bj) = hLoc*hLoc
           ELSE
            bottomDragCoeffW(i,j,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
         DO j=1-OLy+1,sNy+OLy
          DO i=1-OLx,sNx+OLx
           kLow = MIN( kLowC(i,j-1,bi,bj), kLowC(i,j,bi,bj)  )
           IF ( kLow .GE. 1 ) THEN
            hLoc = ( oneRL +
     &           0.5*drF(kLow)*hFacS(i,j,kLow,bi,bj)*recip_zRoughBot )
            hLoc = 0.4 _d 0/LOG(hLoc)
            bottomDragCoeffS(i,j,bi,bj) = hLoc*hLoc
           ELSE
            bottomDragCoeffS(i,j,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ELSEIF ( zRoughBot .GT. 0. _d 0 ) THEN
C-    Roughness dependent drag not yet coded for P-Coordinates
         WRITE(msgBuf,'(2A)') 'MOM_INIT_FIXED: zRoughBot > 0.',
     &                ' not yet implemented for P-Coordinate'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R MOM_INIT_FIXED'
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_BOTTOMDRAG_ROUGHNESS */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL MOM_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
