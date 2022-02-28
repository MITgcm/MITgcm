#include "GMREDI_OPTIONS.h"

C     !ROUTINE: EIGENVAL
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_EIGS(
     I     iMin, iMax, jMin, jMax,
     I     bi, bj, N2, myThid, kLow,
     I     mask, hfac, recip_hfac,
     I     rlow, nmodes, writediag,
     O     Rmid, vec)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_CALC_URMS
C     | o Calculate the vertical structure of the rms eddy
C     |   velocity based on baroclinic modal decomposition
C     *==========================================================*
C     \ev

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj    :: tile indices
      LOGICAL writediag
      INTEGER iMin,iMax,jMin,jMax
      INTEGER bi, bj
      INTEGER myThid
      INTEGER nmodes
      INTEGER kLow(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL hfac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL recip_hfac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rlow(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL N2(  1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Rmid(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vec(nmodes,1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef GM_BATES_K3D
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k,kk,m
# ifdef HAVE_LAPACK
C     info     :: error code from LAPACK
C     idx      :: index used for sorting the eigenvalues
C     a3d      :: lower diagonal of eigenvalue problem
C     b3d      :: diagonal of eigenvalue problem
C     c3d      :: upper diagonal of eigenvalue problem
C     val      :: Eigenvalue (wavenumber) of the first baroclinic mode
C     eigR     :: Real component of all the eigenvalues in a water column
C     eigI     :: Imaginary component of all the eigenvalues in a water column
C     vecs     :: All the eigenvectors of a water column
C     dummy    :: Redundant variable for calling lapack
C     work     :: Work array for lapack
C     array    :: Array containing the matrix with a,b,c
C     eigval   :: Vector containing the eigenvalues
      INTEGER info
      INTEGER idx
      _RL a3d(   1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL b3d(   1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL c3d(   1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL val(   1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL eigR(Nr),eigI(Nr),vecs(Nr,Nr),dummy(1,Nr),work(Nr*Nr)
      _RL array(Nr,Nr)
      _RL eigval(0:nmodes)
# else
C     drNr     :: distance from bottom of cell to cell centre at Nr
C     BuoyFreq :: buoyancy frequency, SQRT(N2)
C     intN     :: Vertical integral of BuoyFreq to each grid cell centre
C     intN0    :: Vertical integral of BuoyFreq to z=0
C     c1       :: intN0/pi
C     nEigs    :: number of eigenvalues/vectors to calculate
      _RL drNr
      _RL BuoyFreq(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL intN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL intN0(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL c1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER nEigs(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
# endif
C     small    :: a small number (used to avoid floating point exceptions)
C     vecint   :: vertical integral of eigenvector and/or square of eigenvector
C     fCori2   :: square of the Coriolis parameter
      _RL small
      _RL vecint(nmodes,1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fCori2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      small = TINY(zeroRL)

C     Square of the Coriolis parameter
      DO i=1-OLx,sNx+OLx
       DO j=1-OLy,sNy+OLy
        fCori2(i,j) = fCori(i,j,bi,bj)*fCori(i,j,bi,bj)
       ENDDO
      ENDDO

      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         DO m=1,nmodes
          vec(m,i,j,k) = zeroRL
         ENDDO
        ENDDO
       ENDDO
      ENDDO

# ifdef HAVE_LAPACK
C     Calculate the tridiagonal operator matrix for
C     f^2 d/dz 1/N^2 d/dz
C     a3d is the lower off-diagonal, b3d is the diagonal
C     and c3d is the upper off-diagonal
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         IF (kLow(i,j) .GT. 0) THEN
           IF (k.EQ.1) THEN
             a3d(i,j,k) = zeroRL
             c3d(i,j,k) = fCori2(i,j)*recip_hFac(i,j,k)
     &                    *recip_drC(k+1)*recip_drF(k)/N2(i,j,k+1)
             b3d(i,j,k) = -c3d(i,j,k)

           ELSEIF (k.LT.kLow(i,j)) THEN
             a3d(i,j,k) = fCori2(i,j)*recip_hFac(i,j,k)
     &                    *recip_drF(k)*recip_drC(k)/N2(i,j,k)
             c3d(i,j,k) = fCori2(i,j)*recip_hFac(i,j,k)
     &                    *recip_drF(k)*recip_drC(k+1)/N2(i,j,k+1)
             b3d(i,j,k) = -a3d(i,j,k)-c3d(i,j,k)

           ELSEIF (k.EQ.kLow(i,j)) THEN
             a3d(i,j,k) = fCori2(i,j)*recip_hFac(i,j,k)
     &                    *recip_drF(k)*recip_drC(k)/N2(i,j,k)
             c3d(i,j,k) = zeroRL
             b3d(i,j,k) = -a3d(i,j,k)

           ELSE
             a3d(i,j,k) = zeroRL
             b3d(i,j,k) = zeroRL
             c3d(i,j,k) = zeroRL
           ENDIF

         ELSE
           a3d(i,j,k) = zeroRL
           b3d(i,j,k) = zeroRL
           c3d(i,j,k) = zeroRL
         ENDIF
        ENDDO
       ENDDO
      ENDDO

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        IF (kLow(i,j).GT.0) THEN
          DO kk=1,Nr
           DO k=1,Nr
            array(k,kk) = zeroRL
           ENDDO
          ENDDO

          k=1
          array(k,k)   = b3d(i,j,k)
          array(k,k+1) = c3d(i,j,k)
          DO k=2,Nr-1
           array(k,k-1) = a3d(i,j,k)
           array(k,k)   = b3d(i,j,k)
           array(k,k+1) = c3d(i,j,k)
          ENDDO
          k=Nr
          array(k,k-1) = a3d(i,j,k)
          array(k,k)   = b3d(i,j,k)

          CALL DGEEV('N','V',Nr,array,Nr,eigR,eigI,dummy,1,
     &         vecs,Nr,work,Nr*Nr,info)
          IF( info.LT.0 ) THEN
            WRITE(msgBuf,'(A,x,2(A1,I2),A1,x,A,I4)')
     &           'GMREDI_CALC_EIGS problem with arguments for DGEEV at',
     &           '(',i,',',j,')', 'error code =',info
            CALL PRINT_ERROR( msgBuf , myThid )

          ELSEIF(info.GT.0 ) THEN
            WRITE(msgBuf,'(A,x,2(A1,I2),A1,x,A,I4)')
     &           'GMREDI_CALC_EIGS problems with eigensolver DGEEV at',
     &           '(',i,',',j,')', 'error code =',info
            CALL PRINT_ERROR( msgBuf , myThid )

          ENDIF

C         Find the second largest eigenvalue (the Rossby radius)
C         and the first M baroclinic modes (eigenvectors)
          DO m=0,nmodes
           eigval(m) = -HUGE(zeroRL)
          ENDDO

          DO k=1,kLow(i,j)
           eigval(0) = MAX(eigval(0),eigR(k))
          ENDDO
          DO m=1,MIN(nmodes,klow(i,j)-1)
           DO k=1,kLow(i,j)
            IF (eigR(k).LT.eigval(m-1)) THEN
              eigval(m) = MAX(eigval(m),eigR(k))
              IF (eigval(m).EQ.eigR(k)) idx=k
            ENDIF
           ENDDO
           IF(vecs(1,idx).LT.zeroRL) THEN
             DO k=1,Nr
              vec(m,i,j,k) = -vecs(k,idx)
             ENDDO
           ELSE
             DO k=1,Nr
              vec(m,i,j,k) = vecs(k,idx)
             ENDDO
           ENDIF
          ENDDO
          val(i,j) = eigval(1)
        ELSE
          val(i,j)=zeroRL
          DO k=1,Nr
           DO m=1,nmodes
            vec(m,i,j,k)=zeroRL
           ENDDO
          ENDDO

        ENDIF
       ENDDO
      ENDDO

       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         IF (kLow(i,j).GT.2 .AND. val(i,j).NE.zeroRL) THEN
           Rmid(i,j) = 1.0/(SQRT(ABS(val(i,j)))+small)
         ELSE
           Rmid(i,j) = zeroRL
         ENDIF
        ENDDO
       ENDDO

# else
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         BuoyFreq(i,j,k) = mask(i,j,k)*SQRT(N2(i,j,k))
        ENDDO
       ENDDO
      ENDDO
      k=Nr+1
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        BuoyFreq(i,j,k) = zeroRL
       ENDDO
      ENDDO

C     integrate N using something like Simpson s rule (but not quite)
C     drC*( (N(k+1)+N(k+2))/2 + (N(k)+N(k+1))/2 )/2
C     when k=Nr, say that N(k+2)=0 and N(k+1)=0
      k=Nr
C     drNr is like drC(Nr+1)/2
      drNr = rC(Nr)-rF(Nr+1)
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        intN(i,j,k) = op5*BuoyFreq(i,j,k)*drNr
       ENDDO
      ENDDO
      DO k=Nr-1,1,-1
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         intN(i,j,k) = intN(i,j,k+1)
     &        + drC(k)*( op25*BuoyFreq(i,j,k+2) + op5*BuoyFreq(i,j,k)
     &                 + op25*BuoyFreq(i,j,k+1) )
        ENDDO
       ENDDO
      ENDDO

C     intN integrates to z=rC(1).  We want to integrate to z=0.
C     Assume that N(0)=0 and N(1)=0.
C     drC(1) is like half a grid cell.
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        intN0(i,j) = intN(i,j,1)
     &       + drC(1)*op5*BuoyFreq(i,j,2)
       ENDDO
      ENDDO

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        c1(i,j) = intN0(i,j)/pi
        Rmid(i,j) = c1(i,j)/ABS(fCori(i,j,bi,bj))
       ENDDO
      ENDDO

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        nEigs(i,j) = MIN(klow(i,j),nmodes)
       ENDDO
      ENDDO
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         IF (mask(i,j,k).NE.0.0) THEN
           DO m=1,nEigs(i,j)
            vec(m,i,j,k) = -COS(intN(i,j,k)/(m*c1(i,j)))
           ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C     The WKB approximation for the baroclinic mode does not always
C     integrate to zero so we adjust it.
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        DO m=1,nEigs(i,j)
         vecint(m,i,j) = zeroRL
        ENDDO
       ENDDO
      ENDDO
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         DO m=1,nEigs(i,j)
         vecint(m,i,j) = vecint(m,i,j) + hfac(i,j,k)*vec(m,i,j,k)*drF(k)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        DO m=1,nEigs(i,j)
         vecint(m,i,j) = vecint(m,i,j)/(-rlow(i,j)+small)
        ENDDO
       ENDDO
      ENDDO
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         DO m=1,nEigs(i,j)
          vec(m,i,j,k) = vec(m,i,j,k) - vecint(m,i,j)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
# endif

C     Normalise the eigenvectors
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        DO m=1,nmodes
         vecint(m,i,j) = zeroRL
        ENDDO
       ENDDO
      ENDDO

      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         DO m=1,nmodes
          vecint(m,i,j) = vecint(m,i,j) +
     &         mask(i,j,k)*drF(k)*hfac(i,j,k)
     &         *vec(m,i,j,k)*vec(m,i,j,k)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        DO m=1,nmodes
         vecint(m,i,j) = SQRT(vecint(m,i,j))
        ENDDO
       ENDDO
      ENDDO

      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         DO m=1,nmodes
          vec(m,i,j,k) = vec(m,i,j,k)/(vecint(m,i,j)+small)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

# ifdef ALLOW_DIAGNOSTICS
C     Diagnostics
      IF ( useDiagnostics.AND.writediag ) THEN
#  ifdef HAVE_LAPACK
        CALL DIAGNOSTICS_FILL(a3d, 'GM_A3D  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(b3d, 'GM_B3D  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(c3d, 'GM_C3D  ',0,Nr,0,1,1,myThid)
#  endif
      ENDIF
# endif

#endif /* GM_BATES_K3D */

      RETURN
      END
