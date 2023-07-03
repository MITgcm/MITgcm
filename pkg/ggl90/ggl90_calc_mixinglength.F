#include "GGL90_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: GGL90_CALC_MIXINGLENGTH

C !INTERFACE: ======================================================
      SUBROUTINE GGL90_CALC_MIXINGLENGTH(
     U     GGL90mixingLength,
#ifdef ALLOW_GGL90_LANGMUIR
     O     LCmixingLength,
#endif
     O     rMixingLength,
     I     iMin ,iMax ,jMin ,jMax,
     I     bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GGL90_CALC_MIXINGLENGTH                       |
C     | o Compute GGL90mixingLength (and LCmixingLength)         |
C     *==========================================================*
C     | Equation numbers refer to                                |
C     |  Gaspar et al. (1990), JGR 95 (C9), pp 16,179            |
C     | Some parts of the implementation follow Blanke and       |
C     |  Delecuse (1993), JPO, and OPA code, in particular the   |
C     |  computation of the                                      |
C     |  mixing length = max(min(lk,depth),lkmin)                |
C     | Note: Only call this S/R if Nr > 1 (no use if Nr=1)      |
C     *==========================================================*

C \ev

C !USES: ============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GGL90.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C Routine arguments
C     iMin,iMax,jMin,jMax :: index boundaries of computation domain
C     bi, bj :: Current tile indices
C     GGL90mixingLength :: mixing length (m)
C     myTime :: Current time in simulation
C     myIter :: Current time-step number
C     myThid :: My Thread Id number
      INTEGER iMin ,iMax ,jMin ,jMax
      INTEGER bi, bj
      _RL     GGL90mixingLength(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_GGL90_LANGMUIR
      _RL     LCmixingLength   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
      _RL     rMixingLength    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GGL90
C !LOCAL VARIABLES: ====================================================
C     i, j, k          :: array computation indices
C     kSrf             :: vertical index of surface level
C     kTop             :: index of top interface (just below surf. level)
C
C     In general, all 3D variables are defined at W-points (i.e.,
C     between k and k-1), all 2D variables are also defined at W-points
C     or at the very surface level (like uStarSquare)
C
C     GGL90mixingLength:: mixing length of scheme following Banke+Delecuse
C         rMixingLength:: inverse of mixing length
      INTEGER i, j, k
      INTEGER kSrf, kTop
      _RL MaxLength
      _RL mxLength_Dn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL MLtmp
C     This mixed layer model is not invariant under coordinate
C     transformation to pressure coordinates, so we need these
C     factors to scale the vertical (pressure) coordinates
      _RL coordFac, recip_coordFac
#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (depends on tiles)
C     kkey :: tape key (depends on levels and tiles)
      INTEGER tkey, kkey
#endif
CEOP

      IF ( usingPCoords ) THEN
       kSrf = Nr
       kTop = Nr
      ELSE
       kSrf =  1
       kTop =  2
      ENDIF

      coordFac = 1. _d 0
      IF ( usingPCoords) coordFac = gravity * rhoConst
      recip_coordFac = 1./coordFac

#ifdef ALLOW_AUTODIFF_TAMC
      tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
#endif /* ALLOW_AUTODIFF_TAMC */

C     Initialize local fields
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         mxLength_Dn(i,j,k)       = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
c       rMixingLength(i,j,1)  = 0. _d 0
        mxLength_Dn(i,j,1) = GGL90mixingLengthMin
       ENDDO
      ENDDO

#ifdef ALLOW_GGL90_LANGMUIR
      IF (useLANGMUIR) THEN
       DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          LCmixingLength(i,j,k) = GGL90mixingLengthMin
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif

C- ensure mixing between first and second level
      IF (mxlSurfFlag) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
#ifdef ALLOW_SHELFICE
         IF ( useShelfIce ) THEN
          kSrf = MAX(1,kTopC(i,j,bi,bj))
          kTop = MIN(kSrf+1,Nr)
         ENDIF
#endif
         GGL90mixingLength(i,j,kTop)=drF(kSrf)*recip_coordFac
        ENDDO
       ENDDO
      ENDIF

C--   Impose upper and lower bound for mixing length
#ifdef ALLOW_AUTODIFF
CADJ STORE GGL90mixingLength = comlev1_bibj, key=tkey, kind=isbyte
#endif
      IF ( mxlMaxFlag .EQ. 0 ) THEN

       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
C     Use thickness of water column (inverse of recip_Rcol) as the
C     maximum length.
          MaxLength =
     &         ( Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj) )*recip_coordFac
          GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &                                   MaxLength)
         ENDDO
        ENDDO
       ENDDO

      ELSEIF ( mxlMaxFlag .EQ. 1 ) THEN

       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          MaxLength=MIN(Ro_surf(i,j,bi,bj)-rF(k),rF(k)-R_low(i,j,bi,bj))
     &         * recip_coordFac
c         MaxLength=MAX(MaxLength,20. _d 0)
          GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &                                   MaxLength)
         ENDDO
        ENDDO
       ENDDO

      ELSEIF ( mxlMaxFlag .EQ. 2 ) THEN

       IF ( usingPcoords ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90mixingLength(:,:,Nr)
CADJ &     = comlev1_bibj, key = tkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C     Downward sweep, extra treatment of k=Nr for p-coordinates
C     because level Nr+1 is not available
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90mixingLength(i,j,Nr) = MIN(GGL90mixingLength(i,j,Nr),
     &         GGL90mixingLengthMin+drF(Nr)*recip_coordFac)
         ENDDO
        ENDDO
        DO k=Nr-1,2,-1
#ifdef ALLOW_AUTODIFF_TAMC
         kkey = k + (tkey-1)*Nr
CADJ STORE GGL90mixingLength(:,:,k+1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax
           GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &          GGL90mixingLength(i,j,k+1)+drF(k)*recip_coordFac)
          ENDDO
         ENDDO
        ENDDO
C     Upward sweep
        DO k=2,Nr
#ifdef ALLOW_AUTODIFF_TAMC
         kkey = k + (tkey-1)*Nr
CADJ STORE GGL90mixingLength(:,:,k-1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax
           GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &          GGL90mixingLength(i,j,k-1)+drF(k-1)*recip_coordFac)
          ENDDO
         ENDDO
        ENDDO
       ELSE
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90mixingLength(:,:,Nr)
CADJ &     = comlev1_bibj, key = tkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C     Downward sweep
        DO k=2,Nr
#ifdef ALLOW_AUTODIFF_TAMC
         kkey = k + (tkey-1)*Nr
CADJ STORE GGL90mixingLength(:,:,k-1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax
           GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &          GGL90mixingLength(i,j,k-1)+drF(k-1)*recip_coordFac)
          ENDDO
         ENDDO
        ENDDO
C     Upward sweep, extra treatment of k=Nr for z-coordinates
C     because level Nr+1 is not available
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90mixingLength(i,j,Nr) = MIN(GGL90mixingLength(i,j,Nr),
     &         GGL90mixingLengthMin+drF(Nr)*recip_coordFac)
         ENDDO
        ENDDO
        DO k=Nr-1, 2,-1
#ifdef ALLOW_AUTODIFF_TAMC
         kkey = k + (tkey-1)*Nr
CADJ STORE GGL90mixingLength(:,:,k+1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax
           GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &          GGL90mixingLength(i,j,k+1)+drF(k)*recip_coordFac)
          ENDDO
         ENDDO
        ENDDO
       ENDIF

      ELSEIF ( mxlMaxFlag .EQ. 3 ) THEN

       IF ( usingPcoords ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90mixingLength(:,:,Nr)
CADJ &     = comlev1_bibj, key = tkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C     Downward sweep, extra treatment of k=Nr for p-coordinates
C     because level Nr+1 is not available
        DO j=jMin,jMax
         DO i=iMin,iMax
          mxLength_Dn(i,j,Nr) = MIN(GGL90mixingLength(i,j,Nr),
     &         GGL90mixingLengthMin+drF(Nr)*recip_coordFac)
         ENDDO
        ENDDO
        DO k=Nr-1,2,-1
#ifdef ALLOW_AUTODIFF_TAMC
         kkey = k + (tkey-1)*Nr
CADJ STORE mxLength_Dn(:,:,k+1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax
           mxLength_Dn(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &          mxLength_Dn(i,j,k+1)+drF(k)*recip_coordFac)
          ENDDO
         ENDDO
        ENDDO
C     Upward sweep
        DO k=2,Nr
#ifdef ALLOW_AUTODIFF_TAMC
         kkey = k + (tkey-1)*Nr
CADJ STORE GGL90mixingLength(:,:,k-1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax
           GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &          GGL90mixingLength(i,j,k-1)+drF(k-1)*recip_coordFac)
          ENDDO
         ENDDO
        ENDDO
       ELSE
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90mixingLength(:,:,Nr)
CADJ &     = comlev1_bibj, key = tkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C     Downward sweep
        DO k=2,Nr
#ifdef ALLOW_AUTODIFF_TAMC
         kkey = k + (tkey-1)*Nr
CADJ STORE mxLength_Dn(:,:,k-1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax
           mxLength_Dn(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &          mxLength_Dn(i,j,k-1)+drF(k-1)*recip_coordFac)
          ENDDO
         ENDDO
        ENDDO
C     Upward sweep, extra treatment of k=Nr for z-coordinates
C     because level Nr+1 is not available
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90mixingLength(i,j,Nr) = MIN(GGL90mixingLength(i,j,Nr),
     &         GGL90mixingLengthMin+drF(Nr)*recip_coordFac)
         ENDDO
        ENDDO
        DO k=Nr-1,2,-1
#ifdef ALLOW_AUTODIFF_TAMC
         kkey = k + (tkey-1)*Nr
CADJ STORE GGL90mixingLength(:,:,k-1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax
           GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &          GGL90mixingLength(i,j,k+1)+drF(k)*recip_coordFac)
          ENDDO
         ENDDO
        ENDDO
       ENDIF

      ELSE
       WRITE(msgBuf,'(A,I5,A)')
     &   'GGL90_CALC_MIXINGLENGTH: mxlMaxFlag=',
     &   mxlMaxFlag,' not implemented'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GGL90_CALC_MIXINGLENGTH'
      ENDIF

C--   Impose minimum mixing length to avoid division by zero
C     and compute inverse
      IF ( mxlMaxFlag.EQ.3 ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE mxLength_Dn       = comlev1_bibj, key = tkey, kind=isbyte
CADJ STORE GGL90mixingLength = comlev1_bibj, key = tkey, kind=isbyte
#endif
       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &                                  mxLength_Dn(i,j,k))
#ifdef ALLOW_AUTODIFF
         ENDDO
        ENDDO
       ENDDO
       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          MLtmp = SQRT( GGL90mixingLength(i,j,k)*mxLength_Dn(i,j,k)
     &         + GGL90mixingLengthMin**2 )
#else
          MLtmp = SQRT( GGL90mixingLength(i,j,k)*mxLength_Dn(i,j,k) )
          MLtmp = MAX( MLtmp, GGL90mixingLengthMin )
#endif
          rMixingLength(i,j,k) = 1. _d 0 / MLtmp
         ENDDO
        ENDDO
       ENDDO
      ELSE
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90mixingLength = comlev1_bibj, key=tkey, kind=isbyte
#endif
       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
#ifdef ALLOW_AUTODIFF
          GGL90mixingLength(i,j,k) = SQRT( GGL90mixingLength(i,j,k)**2
     &         + GGL90mixingLengthMin**2 )
         ENDDO
        ENDDO
       ENDDO
       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
#else
          GGL90mixingLength(i,j,k) = MAX(GGL90mixingLength(i,j,k),
     &                                   GGL90mixingLengthMin)
#endif
          rMixingLength(i,j,k) = 1. _d 0 / GGL90mixingLength(i,j,k)
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_GGL90_LANGMUIR
      IF (useLANGMUIR) THEN
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90mixingLength = comlev1_bibj, key = tkey, kind=isbyte
# endif
C      Compute the mixing length scale with Langmuir circulation.
C      The calculation closely follows that for GGL90mixingLength.
       IF ( mxlMaxFlag .EQ. 1 ) THEN

        DO k=2,Nr
         DO j=jMin,jMax
          DO i=iMin,iMax
           MLtmp = (Ro_surf(i,j,bi,bj)-rF(k))*recip_coordFac
           IF (GGL90mixingLength(i,j,k) .EQ. MLtmp) THEN
            LCmixingLength(i,j,k) = LC_Gamma
     &                              * GGL90mixingLength(i,j,k)
           ELSE
            LCmixingLength(i,j,k) = GGL90mixingLength(i,j,k)
           ENDIF
          ENDDO
         ENDDO
        ENDDO

       ELSEIF ( mxlMaxFlag .EQ. 2 ) THEN

        IF ( usingPcoords ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90mixingLength(:,:,Nr)
CADJ &     = comlev1_bibj, key = tkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C     Upward sweep
         DO k=2,Nr
#ifdef ALLOW_AUTODIFF_TAMC
          kkey = k + (tkey-1)*Nr
CADJ STORE GGL90mixingLength(:,:,k-1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          DO j=jMin,jMax
           DO i=iMin,iMax
            MLtmp = GGL90mixingLength(i,j,k-1)+drF(k-1)*recip_coordFac
            IF (GGL90mixingLength(i,j,k) .EQ. MLtmp) THEN
             LCmixingLength(i,j,k) = LC_Gamma
     &                               * GGL90mixingLength(i,j,k)
            ELSE
             LCmixingLength(i,j,k) = GGL90mixingLength(i,j,k)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ELSE
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90mixingLength(:,:,Nr)
CADJ &     = comlev1_bibj, key = tkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C     Downward sweep
         DO k=2,Nr
#ifdef ALLOW_AUTODIFF_TAMC
          kkey = k + (tkey-1)*Nr
CADJ STORE GGL90mixingLength(:,:,k-1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          DO j=jMin,jMax
           DO i=iMin,iMax
            MLtmp = GGL90mixingLength(i,j,k-1)+drF(k-1)*recip_coordFac
            IF (GGL90mixingLength(i,j,k) .EQ. MLtmp) THEN
             LCmixingLength(i,j,k) = LC_Gamma
     &                               * GGL90mixingLength(i,j,k)
            ELSE
             LCmixingLength(i,j,k) = GGL90mixingLength(i,j,k)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
         DO k=Nr-1, 2,-1
#ifdef ALLOW_AUTODIFF_TAMC
          kkey = k + (tkey-1)*Nr
CADJ STORE GGL90mixingLength(:,:,k+1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          DO j=jMin,jMax
           DO i=iMin,iMax
            LCmixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &          GGL90mixingLength(i,j,k+1)+drF(k)*recip_coordFac)
           ENDDO
          ENDDO
         ENDDO

        ENDIF

       ELSEIF ( mxlMaxFlag .EQ. 3 ) THEN

        IF ( usingPcoords ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90mixingLength(:,:,Nr)
CADJ &     = comlev1_bibj, key = tkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C     Upward sweep
         DO k=2,Nr
#ifdef ALLOW_AUTODIFF_TAMC
          kkey = k + (tkey-1)*Nr
CADJ STORE GGL90mixingLength(:,:,k-1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          DO j=jMin,jMax
           DO i=iMin,iMax
            MLtmp = GGL90mixingLength(i,j,k-1)+drF(k-1)*recip_coordFac
            IF (GGL90mixingLength(i,j,k) .EQ. MLtmp) THEN
             LCmixingLength(i,j,k) = LC_Gamma
     &                               * GGL90mixingLength(i,j,k)
            ELSE
             LCmixingLength(i,j,k) = GGL90mixingLength(i,j,k)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ELSE
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90mixingLength(:,:,Nr)
CADJ &     = comlev1_bibj, key = tkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C     Downward sweep
         DO k=2,Nr
#ifdef ALLOW_AUTODIFF_TAMC
          kkey = k + (tkey-1)*Nr
CADJ STORE mxLength_Dn(:,:,k-1)
CADJ &     = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          DO j=jMin,jMax
           DO i=iMin,iMax
            MLtmp = mxLength_Dn(i,j,k-1)+drF(k-1)*recip_coordFac
            IF (GGL90mixingLength(i,j,k) .EQ. MLtmp) THEN
             LCmixingLength(i,j,k) = LC_Gamma
     &                              * GGL90mixingLength(i,j,k)
            ELSE
             LCmixingLength(i,j,k) = GGL90mixingLength(i,j,k)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDIF

       ELSE
        WRITE(msgBuf,'(2A,I5,A)')
     &       'GGL90_CALC_MIXINGLENGTH: ',
     &       'Langmuir Circ. Parameterization with mxlMaxFlag=',
     &   mxlMaxFlag,' not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GGL90_CALC_MIXINGLENGTH'
       ENDIF
      ENDIF
#endif /* ALLOW_GGL90_LANGMUIR */

#endif /* ALLOW_GGL90 */

      RETURN
      END
