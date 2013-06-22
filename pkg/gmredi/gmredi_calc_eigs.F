C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_calc_eigs.F,v 1.2 2013/06/22 14:44:54 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

C     !ROUTINE: EIGENVAL
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_EIGS(
     I     iMin, iMax, jMin, jMax,
     I     bi, bj, N2, myThid, kLow,
     I     mask, hfac, recip_hfac,
     I     writediag,
     O     Kdef, vec)

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
      INTEGER kLow(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL hfac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL recip_hfac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N2(  1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Kdef(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vec(GM_K3D_NModes,1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef GM_K3D
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k,kk,m,info
      _RL small
      _RL a3d(   1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL b3d(   1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL c3d(   1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vecint(GM_K3D_NModes,1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL val(   1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fCori2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL eigR(Nr),eigI(Nr),vecs(Nr,Nr),dummy(1,Nr),work(Nr*Nr)
      _RL array(Nr,Nr)
      _RL eigval(0:GM_K3D_NModes)
      INTEGER idx
#ifdef ALLOW_DIAGNOSTICS
      _RL vec_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
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
         DO m=1,GM_K3D_NModes
          vec(m,i,j,k) = zeroRL
         ENDDO
        ENDDO
       ENDDO
      ENDDO

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
             IF (k+1.GT.Nr) THEN
               PRINT '(a4,x,3(a1,i2),a1,3(x,i2))', 'kp1:',
     &              '(',i,',',j,',',k,')',
     &              kLow(i,j), k+1, Nr
             ENDIF
             a3d(i,j,k) = fCori2(i,j)*recip_hFac(i,j,k)
     &                    *recip_drF(k)*recip_drC(k)/N2(i,j,k)
             c3d(i,j,k) = fCori2(i,j)*recip_hFac(i,j,k)
     &                    *recip_drF(k)*recip_drC(k+1)/N2(i,j,k+1)
             b3d(i,j,k) = -a3d(i,j,k)-c3d(i,j,k)

           ELSEIF (k.EQ.kLow(i,j)) THEN
             IF (k.GT.Nr) THEN
               PRINT '(a4,x,3(a1,i2),a1,3(x,i2))', 'k  :',
     &              '(',i,',',j,',',k,')',
     &              kLow(i,j), k, Nr
             ENDIF
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

#ifdef use_lapack

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
C         Find the second largest eigenvalue (the Rossby radius)
C         and the first M baroclinic modes (eigenvectors)
          DO m=0,GM_K3D_NModes
           eigval(m) = -HUGE(zeroRL)
          ENDDO

          DO k=1,kLow(i,j)
           eigval(0) = MAX(eigval(0),eigR(k))
          ENDDO
          DO m=1,MIN(GM_K3D_NModes,klow(i,j))
C          DO m=1,GM_K3D_NModes
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
           DO m=1,GM_K3D_NModes
            vec(m,i,j,k)=zeroRL
           ENDDO
          ENDDO

        ENDIF
       ENDDO
      ENDDO

C     Normalise the eigenvectors
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        DO m=1,GM_K3D_NModes
         vecint(m,i,j) = zeroRL
        ENDDO
       ENDDO
      ENDDO

      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         DO m=1,GM_K3D_NModes
          vecint(m,i,j) = vecint(m,i,j) +
     &         mask(i,j,k)*drF(k)*hfac(i,j,k)
     &         *vec(m,i,j,k)*vec(m,i,j,k)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        DO m=1,GM_K3D_NModes
         vecint(m,i,j) = SQRT(vecint(m,i,j))
        ENDDO
       ENDDO
      ENDDO

      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         DO m=1,GM_K3D_NModes
          vec(m,i,j,k) = vec(m,i,j,k)/(vecint(m,i,j)+small)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#else
C      CALL EIGENVAL(a3d,b3d,c3d,bi,bj,val)
C      CALL EIGENVEC(a3d,b3d,c3d,val,1,bi,bj,vec)
#endif

      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         IF (kLow(i,j).GT.2 .AND. val(i,j).NE.zeroRL) THEN
           Kdef(i,j) = SQRT(ABS(val(i,j)))
         ELSE
           Kdef(i,j) = zeroRL
         ENDIF
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
C     Diagnostics
      IF ( useDiagnostics.AND.writediag ) THEN
        CALL DIAGNOSTICS_FILL(a3d, 'GM_A3D  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(b3d, 'GM_B3D  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(c3d, 'GM_C3D  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(val, 'GM_VAL  ',0, 1,0,1,1,myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           vec_diag(i,j,k) = vec(1,i,j,k)
          ENDDO
         ENDDO
        ENDDO

        CALL DIAGNOSTICS_FILL(vec_diag, 'GM_VEC  ',0,Nr,0,1,1,myThid)
      ENDIF
#endif

#endif /* GM_K3D */

      RETURN
      END
