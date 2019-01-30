#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SHELFICE_REMESH_C_MASK
C     !INTERFACE:
      SUBROUTINE SHELFICE_REMESH_C_MASK(
!     I     rF, recip_drF, drF, kLowC,
!     U     hFacC,
     I     mytime, myiter,myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_REMESH_C_MASK
C     | o Loops through top level cells and determines those where
C     | o hfac is too large and hence splits into two cells,
C     | o and cells where hfac is too small, and merges cell with
C     | o below.
C     | o
C     | o Affects all dynamic arrays, as well as R_shelfice
C     | o Recomputes hFacC using formula from initialisation
C     | o based on new R_shelfice
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SURFACE.h"
#include "GRID.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     rF        :: R-coordinate of face of cell (units of r).
C     recip_drF :: Recipricol of cell face separation along Z axis ( units of r ).
C     hFacC     :: Fraction of cell in vertical which is open (see GRID.h)
C     myThid    :: Number of this instance of SHELFICE_UPDATE_MASKS
!      _RS rF        (1:Nr+1)
!      _RS recip_drF (1:Nr)
!      _RS drF (1:Nr)
!      _RS hFacC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
!      INTEGER kLowC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER myiter, mytime, myThid

#ifdef ALLOW_SHELFICE
#ifdef ALLOW_SHELFICE_REMESHING
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: tile indices
C     I,J,K   :: Loop counters
      INTEGER bi, bj
      INTEGER I, J, K
      _RL hFacCtmp
      _RL hFacMnSz
      _RL adjust
      _RL stag_fac
      _RL salt_int, theta_int, newvol, eta_old, vel_int
      _RL tmpVar
      _RS maskS_old (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS maskW_old (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RL hFacS_old (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RL hFacW_old (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)

      stag_fac = 0.0
      if (staggerTimestep) stag_fac = 1.0

C- Update etaN
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1,sNy
         DO I = 1,sNx

! SPLIT CELLS

          IF ( R_shelfice(I,J,bi,bj) .LT. 0.0) THEN
           K  = MAX(1,kTopC(I,J,bi,bj))
           IF (K.gt.1) THEN
            IF (hfac_surfc(i,j,bi,bj)
     &      .GT. SHELFICESplitThreshold ) THEN
             IF ((hfac_surfc(I,J,bi,bj)-1)*drf(K)*recip_drF(K-1)
     &       .GT. SHELFICEMergeThreshold ) THEN

              adjust = rF(k-1) - R_shelfice(I,J,BI,BJ)
              eta_old = etah(I,J,BI,BJ)

! decrement eta and increment R_shelfice and Ro_surf
              etaN(I,J,bi,bj) = etaN(I,J,bi,bj)- adjust
              etaH(I,J,bi,bj) = etaH(I,J,bi,bj)- adjust
              etaHnm1(I,J,bi,bj) = etaHnm1(I,J,bi,bj)- adjust
              R_shelfIce(I,J,bi,bj) = R_shelfIce(I,J,bi,bj)+adjust
              Ro_surf(I,J,bi,bj) = Ro_surf(I,J,bi,bj)+adjust

! initialise new cell-centered variables from values below

              salt(I,J,K-1,bi,bj)=salt(I,J,K,bi,bj)
              theta(I,J,K-1,bi,bj)=theta(I,J,K,bi,bj)

! also need to increment geometric factors and masks
! maskC, hFacC, hFac_surfC, kSurfC, kTopC, recip_hFacC
              if (K .lt. klowc(i,j,bi,bj)) THEN
               h0FacC(I,J,K,bi,bj) = 1.
              else
               h0FacC(I,J,K,bi,bj) = (rF(K)-r_low(I,J,BI,BJ)) * 
     &          recip_drF(K)
              endif
               h0FacC(I,J,K-1,bi,bj) = 1.
             ENDIF
            ENDIF
           ENDIF
          ENDIF

! MERGE CELLS

          IF (kTopC(i,j,bi,bj) .LT.kLowC (i,j,bi,bj))THEN
           K = MAX(1,kTopC(I,J,bi,bj))
           IF (hfac_surfc(i,j,bi,bj) .LT. 
     &      SHELFICEMergeThreshold ) THEN
            IF ((hfac_surfc(i,j,bi,bj)*drF(k)*recip_drF(k+1)+1) .LT.
     &       SHELFICESplitThreshold ) THEN

             adjust = R_shelfice(i,j,bi,bj)-rF(k+1)
             eta_old = etah(I,J,BI,BJ)

! increment eta and decrement R_shelfice and Ro_sur
             etaN(I,J,bi,bj) = etaN(I,J,bi,bj) + adjust
             etaH(I,J,bi,bj) = etaH(I,J,bi,bj) + adjust
             etaHnm1(I,J,bi,bj) = etaHnm1(I,J,bi,bj) + adjust
             R_shelfice(I,J,bi,bj) = R_shelfice(I,J,bi,bj)-adjust
             Ro_surf(I,J,bi,bj) = Ro_surf(I,J,bi,bj)-adjust

! FIRST FIND THE VOLUME OF NEW CELL -- TAKING BATHY INTO ACCOUNT
             if (K+1 .lt. klowc(i,j,bi,bj)) THEN
              newvol = drF(k+1) + etaH(I,J,bi,bj)
             else
              newvol = rF(k+1)-r_low(I,J,BI,BJ) + etaH(I,J,bi,bj)
             endif
             salt_int = salt(I,J,K,bi,bj) * 
     &         (stag_fac * hFac_surfC(I,J,BI,BJ) + 
     &          (1-stag_fac) * hFacC(I,J,K,BI,BJ)) * drF(K) + 
     &          salt(I,J,K+1,bi,bj) *
     &          hFacC(I,J,K+1,BI,BJ) * drF(K+1)

             salt(I,J,K+1,bi,bj)=
     &         salt_int / newvol
             salt(I,J,K,bi,bj)= 0.0
              
! cells below become weighted avg of cell centered values
             theta_int = theta(I,J,K,bi,bj) * 
     &         (stag_fac * hFac_surfC(I,J,BI,BJ) + 
     &          (1-stag_fac) * hFacC(I,J,K,BI,BJ)) * drF(K) + 
     &          theta(I,J,K+1,bi,bj) *
     &          hFacC(I,J,K+1,BI,BJ) * drF(K+1)

             theta(I,J,K+1,bi,bj)=
     &         theta_int / newvol
             theta(I,J,K,bi,bj)= 0.0

! also need to increment geometric factors and masks
! maskC, hFacC, hFac_surfC, kSurfC, kTopC, recip_hFacC
             h0FacC(I,J,K,bi,bj) = 0.
             if (K+1 .lt. klowc(i,j,bi,bj)) THEN
              h0FacC(I,J,K+1,bi,bj) = (drF(k+1) + 0*eta_old) *
     &          recip_drF(K+1)
             else
               h0FacC(I,J,K+1,bi,bj) = (rF(K+1)-r_low(I,J,BI,BJ)+
     &          0*eta_old) * recip_drF(K+1)
             endif

            ENDIF  
           ENDIF 
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XYZ_RL(h0FacC,myThid)

      DO k=1,Nr
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1-oly,sNy+oly
          DO I = 1-olx,sNx+olx
           hfacc(i,j,k,bi,bj) = h0facc(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
     
!      DO bj = myByLo(myThid), myByHi(myThid)
!       DO bi = myBxLo(myThid), myBxHi(myThid)
!        DO J = 1,sNy
!         DO I = 1,sNx
!           etaHnm1(I,J,bi,bj)=etaH(I,J,bi,bj)
!         ENDDO
!        ENDDO
!       ENDDO
!      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C--   Calculate quantities derived from XY depth map
        DO j=1,sNy
         DO i=1,sNx
C         Total fluid column thickness (r_unit) :
           tmpVar = Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
C          Inverse of fluid column thickness (1/r_unit)
           IF ( tmpVar .LE. zeroRL ) THEN
            recip_Rcol(i,j,bi,bj) = 0.
           ELSE
            recip_Rcol(i,j,bi,bj) = 1. _d 0 / tmpVar
           ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
    
C- fill in the overlap (+ BARRIER):
      CALL EXCH_XYZ_RL(salt,myThid)
      CALL EXCH_XYZ_RL(theta,myThid)
      CALL EXCH_XYZ_RS(maskc,myThid)
      CALL EXCH_XY_RL(EtaHnm1,myThid)
      CALL EXCH_XY_RL(EtaN,myThid)
      CALL EXCH_XY_RL(EtaH,myThid)
      _EXCH_XY_RS(R_shelfIce, myThid )
      _EXCH_XY_RS(Ro_Surf, myThid )
      CALL EXCH_XY_RL(recip_Rcol,myThid)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
           rSurfW(i,j,bi,bj) =
     &           MIN( Ro_surf(i-1,j,bi,bj), Ro_surf(i,j,bi,bj) )
           rSurfW(i,j,bi,bj) =
     &           MAX( rSurfW(i,j,bi,bj), rLowW(i,j,bi,bj) )
         ENDDO
        ENDDO
      
        DO j=1,sNy
         DO i=1,sNx
           rSurfS(i,j,bi,bj) =
     &           MIN( Ro_surf(i,j-1,bi,bj), Ro_surf(i,j,bi,bj) )
           rSurfS(i,j,bi,bj) =
     &           MAX( rSurfS(i,j,bi,bj), rLowS(i,j,bi,bj) )
         ENDDO
        ENDDO
       ENDDO
      ENDDO


      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          kSurfC(i,j,bi,bj) = Nr+1
         ENDDO
        ENDDO
        DO k=Nr,1,-1
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF ( hFacC(i,j,k,bi,bj).NE.zeroRS ) kSurfC(i,j,bi,bj) = k
          ENDDO
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
!          maskInC(i,j,bi,bj) = 0.
!          IF ( kSurfC(i,j,bi,bj).LE.Nr ) maskInC(i,j,bi,bj) = 1.
! DNG do not need to update these for vertical remeshing
! DNG but will need to update for horizontal remeshing
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          IF ( kSurfC(i,j,bi,bj).LE.Nr .AND.
     &         R_shelfIce(i,j,bi,bj).LT.zeroRS ) THEN
            kTopC(i,j,bi,bj) = kSurfC(i,j,bi,bj)
          ELSE
            kTopC(i,j,bi,bj) = 0
          ENDIF
          
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO k=1,Nr
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx

           hFacW_old (i,j,k,bi,bj) = hFacW(i,j,k,bi,bj) 
           hFacS_old (i,j,k,bi,bj) = hFacS(i,j,k,bi,bj) 
           maskW_old (i,j,k,bi,bj) = maskW(i,j,k,bi,bj) 
           maskS_old (i,j,k,bi,bj) = maskS(i,j,k,bi,bj) 
           
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL SHELFICE_REMESH_UVMASKS (mythid)
#ifdef ALLOW_OBCS
         if (useobcs) then
! DNG call will be needed for horizontal remeshing
!         CALL OBCS_UPDATE_REMESH( myThid )
         endif
#endif
      CALL CALC_SURF_DR( etaHnm1, myTime, myIter, myThid )
      CALL UPDATE_SURF_DR(.TRUE., myTime, myIter, myThid)
      CALL CALC_SURF_DR( etaH, myTime, myIter, myThid )

      DO k=1,Nr
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx

            IF (maskS(i,j,k,bi,bj).eq.0.0 .AND.
     &         maskS_old(i,j,k,bi,bj).eq.1.0) THEN
             
             newvol = hFacS(i,j,k+1,bi,bj) * drF(k+1)
             vel_int = hFacS_old (i,j,k,bi,bj) * drF(k) * 
     &         vVel(i,j,k,bi,bj) +  
     &        hFacS_old (i,j,k+1,bi,bj) * drF(k+1) *
     &         vVel(i,j,k+1,bi,bj)
             vVel(i,j,k,bi,bj) = 0.0
             vVel(i,j,k+1,bi,bj) = vel_int / newvol
             vel_int = hFacS_old (i,j,k,bi,bj) * drF(k) * 
     &         gvNm1(i,j,k,bi,bj) +  
     &        hFacS_old (i,j,k+1,bi,bj) * drF(k+1) *
     &         gvNm1(i,j,k+1,bi,bj)
             gvNm1(i,j,k,bi,bj) = 0.0
             gvNm1(i,j,k+1,bi,bj) = vel_int / newvol
            
            ENDIF

            IF (maskW(i,j,k,bi,bj).eq.0.0 .AND.
     &         maskW_old (i,j,k,bi,bj).eq.1.0) THEN

             newvol = hFacW(i,j,k+1,bi,bj) * drF(k+1)
             vel_int = hFacW_old (i,j,k,bi,bj) * drF(k) *
     &         uVel(i,j,k,bi,bj) +
     &        hFacW_old (i,j,k+1,bi,bj) * drF(k+1) *
     &         uVel(i,j,k+1,bi,bj)
             uVel(i,j,k,bi,bj) = 0.0
             uVel(i,j,k+1,bi,bj) = vel_int / newvol
             vel_int = hFacW_old (i,j,k,bi,bj) * drF(k) *
     &         guNm1(i,j,k,bi,bj) +
     &        hFacW_old (i,j,k+1,bi,bj) * drF(k+1) *
     &         guNm1(i,j,k+1,bi,bj)
             guNm1(i,j,k,bi,bj) = 0.0
             guNm1(i,j,k+1,bi,bj) = vel_int / newvol

            ENDIF
       
           IF (k .lt. Nr) THEN
            IF (maskS(i,j,k,bi,bj).eq.1.0 .AND.
     &         maskS_old (i,j,k,bi,bj).eq.0.0) THEN
             vVel(i,j,k,bi,bj) = vVel(i,j,k+1,bi,bj)
             gVNm1(i,j,k,bi,bj) = gvNm1(i,j,k+1,bi,bj)
            ENDIF
            IF (maskW(i,j,k,bi,bj).eq.1.0 .AND.
     &         maskW_old (i,j,k,bi,bj).eq.0.0) THEN
             uVel(i,j,k,bi,bj) = uVel(i,j,k+1,bi,bj)
             guNm1(i,j,k,bi,bj) = guNm1(i,j,k+1,bi,bj)
            ENDIF
           ENDIF

          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XYZ_RL(uvel,myThid)
      CALL EXCH_XYZ_RL(vvel,myThid)
      CALL EXCH_XYZ_RL(guNm1,myThid)
      CALL EXCH_XYZ_RL(gvNm1,myThid)
      CALL EXCH_XYZ_RL(hfacs,myThid)
      CALL EXCH_XYZ_RL(hfacw,myThid)
      CALL EXCH_XYZ_RL(masks,myThid)
      CALL EXCH_XYZ_RL(maskw,myThid)
      CALL EXCH_XYZ_RL(hfacc,myThid)
 

#endif /* ALLOW_SHELFICE */
#endif
      RETURN
      END
