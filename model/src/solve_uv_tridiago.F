C $Header: /u/gcmpack/MITgcm/model/src/solve_uv_tridiago.F,v 1.1 2012/12/16 19:11:53 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: SOLVE_UV_TRIDIAGO
C     !INTERFACE:
      SUBROUTINE SOLVE_UV_TRIDIAGO(
     I                     kSize, ols, solve4u, solve4v,
     I                     aU, bU, cU, rhsU,
     I                     aV, bV, cV, rhsV,
     O                     uFld, vFld,
     O                     errCode,
     I                     subIter, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SOLVE_UV_TRIDIAGO
C     | o Solve a pair of tri-diagonal system along X and Y lines
C     |   (in X-dir for uFld and in Y-dir for vFld)
C     *==========================================================*
C     | o Used, e.g., in linear part of seaice LSR solver
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     kSize    :: size in 3rd dimension
C     ols      :: size of overlap (of input arg array)
C     solve4u  :: logical flag, do solve for u-component if true
C     solve4v  :: logical flag, do solve for v-component if true
C     aU,bU,cU :: u-matrix (lower diagonal, main diagonal & upper diagonal)
C     rhsU     :: RHS vector (u-component)
C     aV,bV,cV :: v-matrix (lower diagonal, main diagonal & upper diagonal)
C     rhsV     :: RHS vector (v-component)
C     uFld     :: X = solution of: A_u * X = rhsU
C     vFld     :: X = solution of: A_v * X = rhsV
C     errCode  :: > 0 if singular matrix
C     subIter  :: current sub-iteration number
C     myIter   :: current iteration number
C     myThid   :: my Thread Id number
      INTEGER kSize, ols
      LOGICAL solve4u, solve4v
      _RL  aU (1-ols:sNx+ols,1-ols:sNy+ols,kSize,nSx,nSy)
      _RL  bU (1-ols:sNx+ols,1-ols:sNy+ols,kSize,nSx,nSy)
      _RL  cU (1-ols:sNx+ols,1-ols:sNy+ols,kSize,nSx,nSy)
      _RL rhsU(1-ols:sNx+ols,1-ols:sNy+ols,kSize,nSx,nSy)
      _RL  aV (1-ols:sNx+ols,1-ols:sNy+ols,kSize,nSx,nSy)
      _RL  bV (1-ols:sNx+ols,1-ols:sNy+ols,kSize,nSx,nSy)
      _RL  cV (1-ols:sNx+ols,1-ols:sNy+ols,kSize,nSx,nSy)
      _RL rhsV(1-ols:sNx+ols,1-ols:sNy+ols,kSize,nSx,nSy)
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      INTEGER errCode
      INTEGER subIter, myIter, myThid

C     !SHARED LOCAL VARIABLES:
C     aTu, cTu, yTu :: tile edges coeff and RHS for u-component
C     aTv, cTv, yTv :: tile edges coeff and RHS for v-component
      COMMON /SOLVE_UV_3DIAG_LOCAL/
     &  aTu, cTu, yTu, aTv, cTv, yTv
      _RL aTu(2,1:sNy,nSx,nSy)
      _RL cTu(2,1:sNy,nSx,nSy)
      _RL yTu(2,1:sNy,nSx,nSy)
      _RL aTv(2,1:sNx,nSx,nSy)
      _RL cTv(2,1:sNx,nSx,nSy)
      _RL yTv(2,1:sNx,nSx,nSy)

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi, bj, bm, bp
      INTEGER i,j,k
      INTEGER ii, im, ip
      INTEGER jj, jm, jp
      _RL tmpVar
      _RL uTmp1, uTmp2, vTmp1, vTmp2
      _RL alpU(1:sNx,1:sNy,nSx,nSy)
      _RL gamU(1:sNx,1:sNy,nSx,nSy)
      _RL yy_U(1:sNx,1:sNy,nSx,nSy)
      _RL alpV(1:sNx,1:sNy,nSx,nSy)
      _RL gamV(1:sNx,1:sNy,nSx,nSy)
      _RL yy_V(1:sNx,1:sNy,nSx,nSy)
CEOP

      errCode = 0
      IF ( .NOT.solve4u .AND. .NOT.solve4v ) RETURN

C--   outside loop on level number k
      DO k = 1,kSize

       IF ( solve4u ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

C--   work on local copy:
          DO j= 1,sNy
           DO i= 1,sNx
            alpU(i,j,bi,bj) = aU(i,j,k,bi,bj)
            gamU(i,j,bi,bj) = cU(i,j,k,bi,bj)
            yy_U(i,j,bi,bj) = rhsU(i,j,k,bi,bj)
           ENDDO
          ENDDO

C--   Beginning of forward sweep (i=1)
          i = 1
          DO j= 1,sNy
C-    normalise row [1] ( 1 on main diagonal)
            tmpVar = bU(i,j,k,bi,bj)
            IF ( tmpVar.NE.0. _d 0 ) THEN
             tmpVar = 1. _d 0 / tmpVar
            ELSE
             tmpVar = 0. _d 0
             errCode = 1
            ENDIF
            gamU(i,j,bi,bj) = gamU(i,j,bi,bj)*tmpVar
            alpU(i,j,bi,bj) = alpU(i,j,bi,bj)*tmpVar
            yy_U(i,j,bi,bj) = yy_U(i,j,bi,bj)*tmpVar
          ENDDO

C--   Middle of forward sweep (i=2:sNx)
          DO j= 1,sNy
           DO i= 2,sNx
            im = i-1
C-    update row [i] <-- [i] - alp_i * [i-1] and normalise (main diagonal = 1)
            tmpVar = bU(i,j,k,bi,bj) - alpU(i,j,bi,bj)*gamU(im,j,bi,bj)
            IF ( tmpVar.NE.0. _d 0 ) THEN
             tmpVar = 1. _d 0 / tmpVar
            ELSE
             tmpVar = 0. _d 0
             errCode = 1
            ENDIF
            yy_U(i,j,bi,bj) = ( yy_U(i,j,bi,bj)
     &                        - alpU(i,j,bi,bj)*yy_U(im,j,bi,bj)
     &                        )*tmpVar
            gamU(i,j,bi,bj) =   gamU(i,j,bi,bj)*tmpVar
            alpU(i,j,bi,bj) = - alpU(i,j,bi,bj)*alpU(im,j,bi,bj)*tmpVar
           ENDDO
          ENDDO

C--   Backward sweep (i=sNx-1:-1:1)
          DO j= 1,sNy
           DO ii= 1,sNx-1
            i = sNx - ii
            ip = i+1
C-    update row [i] <-- [i] - gam_i * [i+1]
            yy_U(i,j,bi,bj) =  yy_U(i,j,bi,bj)
     &                       - gamU(i,j,bi,bj)*yy_U(ip,j,bi,bj)
            alpU(i,j,bi,bj) =  alpU(i,j,bi,bj)
     &                       - gamU(i,j,bi,bj)*alpU(ip,j,bi,bj)
            gamU(i,j,bi,bj) = -gamU(i,j,bi,bj)*gamU(ip,j,bi,bj)
           ENDDO
          ENDDO

C--    At this stage, the 3-diagonal system is reduced to Identity with two
C      more columns (alp & gam) corresponding to unknow X(i=0) and X(i=sNx+1):
C                                       X_0
C         alp  1 0    ...    0 0 gam    X_1        Y_1
C         alp  0 1    ...    0 0 gam    X_2        Y_2
C
C          .   . .    ...    . .  .      .          .
C       (  .   . .    ...    . .  .  )(  .   ) = (  .   )
C          .   . .    ...    . .  .      .          .
C
C         alp  0 0    ...    1 0 gam    X_n-1      Y_n-1
C         alp  0 0    ...    0 1 gam    X_n        Y_n
C                                       X_n+1
C-----

C--   Store tile edges coeff: (1) <--> i=1 ; (2) <--> i=sNx
          DO j= 1,sNy
            aTu(1,j,bi,bj) = alpU( 1, j,bi,bj)
            cTu(1,j,bi,bj) = gamU( 1, j,bi,bj)
            yTu(1,j,bi,bj) = yy_U( 1, j,bi,bj)
            aTu(2,j,bi,bj) = alpU(sNx,j,bi,bj)
            cTu(2,j,bi,bj) = gamU(sNx,j,bi,bj)
            yTu(2,j,bi,bj) = yy_U(sNx,j,bi,bj)
          ENDDO

C     end bi,bj-loops
         ENDDO
        ENDDO

C--   Solve for tile edges values
        IF ( nPx*nPy.GT.1 .OR. useCubedSphereExchange ) THEN
          STOP 'ABNORMAL END: S/R SOLVE_UV_TRIDIAGO: missing code'
        ENDIF
        _BARRIER
        _BEGIN_MASTER(myThid)
        DO bj=1,nSy
         DO j=1,sNy

          DO bi=2,nSx
           bm = bi-1
C-    update row [1,bi] <- [1,bi] - a(1,bi)*[2,bi-1] (& normalise diag)
            tmpVar = oneRL - aTu(1,j,bi,bj)*cTu(2,j,bm,bj)
            IF ( tmpVar.NE.0. _d 0 ) THEN
             tmpVar = 1. _d 0 / tmpVar
            ELSE
             tmpVar = 0. _d 0
             errCode = 1
            ENDIF
            yTu(1,j,bi,bj) = ( yTu(1,j,bi,bj)
     &                       - aTu(1,j,bi,bj)*yTu(2,j,bm,bj)
     &                       )*tmpVar
            cTu(1,j,bi,bj) =   cTu(1,j,bi,bj)*tmpVar
            aTu(1,j,bi,bj) = - aTu(1,j,bi,bj)*aTu(2,j,bm,bj)*tmpVar

C-    update row [2,bi] <- [2,bi] - a(2,bi)*[2,bi-1] + a(2,bi)*c(2,bi-1)*[1,bi]
            tmpVar = aTu(2,j,bi,bj)*cTu(2,j,bm,bj)
            yTu(2,j,bi,bj) =  yTu(2,j,bi,bj)
     &                      - aTu(2,j,bi,bj)*yTu(2,j,bm,bj)
     &                      + tmpVar*yTu(1,j,bi,bj)
            cTu(2,j,bi,bj) =  cTu(2,j,bi,bj)
     &                      + tmpVar*cTu(1,j,bi,bj)
            aTu(2,j,bi,bj) = -aTu(2,j,bi,bj)*aTu(2,j,bm,bj)
     &                      + tmpVar*aTu(1,j,bi,bj)
          ENDDO

          DO bi=nSx-1,1,-1
           bp = bi+1
           DO i=1,2
C-    update row [1,bi] <- [1,bi] - c(1,bi)*[1,bi+1]
C-    update row [2,bi] <- [2,bi] - c(2,bi)*[1,bi+1]
            aTu(i,j,bi,bj) =  aTu(i,j,bi,bj)
     &                      - cTu(i,j,bi,bj)*aTu(1,j,bp,bj)
            yTu(i,j,bi,bj) =  yTu(i,j,bi,bj)
     &                      - cTu(i,j,bi,bj)*yTu(1,j,bp,bj)
            cTu(i,j,bi,bj) = -cTu(i,j,bi,bj)*cTu(1,j,bp,bj)
           ENDDO
          ENDDO

C--  periodic in X:  X_0 <=> X_Nx and X_(N+1) <=> X_1 ;
C    find the value at the 2 opposite location (i=1 and i=Nx)
          bm = 1
          bp = nSx
          cTu(1,j,bm,bj) = oneRL + cTu(1,j,bm,bj)
          aTu(2,j,bp,bj) = oneRL + aTu(2,j,bp,bj)
          tmpVar = cTu(1,j,bm,bj) * aTu(2,j,bp,bj)
     &           - aTu(1,j,bm,bj) * cTu(2,j,bp,bj)
          IF ( tmpVar.NE.0. _d 0 ) THEN
             tmpVar = 1. _d 0 / tmpVar
          ELSE
             tmpVar = 0. _d 0
             errCode = 1
          ENDIF
          uTmp1 = ( aTu(2,j,bp,bj) * yTu(1,j,bm,bj)
     &            - aTu(1,j,bm,bj) * yTu(2,j,bp,bj)
     &            )*tmpVar
          uTmp2 = ( cTu(1,j,bm,bj) * yTu(2,j,bp,bj)
     &            - cTu(2,j,bp,bj) * yTu(1,j,bm,bj)
     &            )*tmpVar

C-    finalise tile-edges solution (put into RHS "yTu"):
          DO bi=1,nSx
           DO i=1,2
            IF ( bi+i .EQ.2 ) THEN
             yTu(i,j,bi,bj) = uTmp1
            ELSEIF ( bi+i .EQ. nSx+2 ) THEN
             yTu(i,j,bi,bj) = uTmp2
            ELSE
             yTu(i,j,bi,bj) = yTu(i,j,bi,bj)
     &                      - aTu(i,j,bi,bj) * uTmp2
     &                      - cTu(i,j,bi,bj) * uTmp1
            ENDIF
           ENDDO
          ENDDO

         ENDDO
        ENDDO
        _END_MASTER(myThid)
        _BARRIER

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          bm = 1 + MOD(bi-2+nSx,nSx)
          bp = 1 + MOD(bi-0+nSx,nSx)
          DO j= 1,sNy
           DO i= 1,sNx
            uFld(i,j,k,bi,bj) = yy_U(i,j,bi,bj)
     &                      - alpU(i,j,bi,bj) * yTu(2,j,bm,bj)
     &                      - gamU(i,j,bi,bj) * yTu(1,j,bp,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C     end solve for uFld
       ENDIF

       IF ( solve4v ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

C--   work on local copy:
          DO j= 1,sNy
           DO i= 1,sNx
            alpV(i,j,bi,bj) = aV(i,j,k,bi,bj)
            gamV(i,j,bi,bj) = cV(i,j,k,bi,bj)
            yy_V(i,j,bi,bj) = rhsV(i,j,k,bi,bj)
           ENDDO
          ENDDO

C--   Beginning of forward sweep (j=1)
          j = 1
          DO i= 1,sNx
C-    normalise row [1] ( 1 on main diagonal)
            tmpVar = bV(i,j,k,bi,bj)
            IF ( tmpVar.NE.0. _d 0 ) THEN
             tmpVar = 1. _d 0 / tmpVar
            ELSE
             tmpVar = 0. _d 0
             errCode = 1
            ENDIF
            gamV(i,j,bi,bj) = gamV(i,j,bi,bj)*tmpVar
            alpV(i,j,bi,bj) = alpV(i,j,bi,bj)*tmpVar
            yy_V(i,j,bi,bj) = yy_V(i,j,bi,bj)*tmpVar
          ENDDO

C--   Middle of forward sweep (j=2:sNy)
          DO i= 1,sNx
           DO j= 2,sNy
            jm = j-1
C-    update row [j] <-- [j] - alp_j * [j-1] and normalise (main diagonal = 1)
            tmpVar = bV(i,j,k,bi,bj) - alpV(i,j,bi,bj)*gamV(i,jm,bi,bj)
            IF ( tmpVar.NE.0. _d 0 ) THEN
             tmpVar = 1. _d 0 / tmpVar
            ELSE
             tmpVar = 0. _d 0
             errCode = 1
            ENDIF
            yy_V(i,j,bi,bj) = ( yy_V(i,j,bi,bj)
     &                        - alpV(i,j,bi,bj)*yy_V(i,jm,bi,bj)
     &                        )*tmpVar
            gamV(i,j,bi,bj) =   gamV(i,j,bi,bj)*tmpVar
            alpV(i,j,bi,bj) = - alpV(i,j,bi,bj)*alpV(i,jm,bi,bj)*tmpVar
           ENDDO
          ENDDO

C--   Backward sweep (j=sNy-1:-1:1)
          DO i= 1,sNx
           DO jj= 1,sNy-1
            j = sNy - jj
            jp = j+1
C-    update row [j] <-- [j] - gam_j * [j+1]
            yy_V(i,j,bi,bj) =  yy_V(i,j,bi,bj)
     &                       - gamV(i,j,bi,bj)*yy_V(i,jp,bi,bj)
            alpV(i,j,bi,bj) =  alpV(i,j,bi,bj)
     &                       - gamV(i,j,bi,bj)*alpV(i,jp,bi,bj)
            gamV(i,j,bi,bj) = -gamV(i,j,bi,bj)*gamV(i,jp,bi,bj)
           ENDDO
          ENDDO

C--    At this stage, the 3-diagonal system is reduced to Identity with two
C      more columns (alp & gam) corresponding to unknow X(j=0) and X(j=sNy+1)

C--   Store tile edges coeff: (1) <--> j=1 ; (2) <--> j=sNy
          DO i= 1,sNx
            aTv(1,i,bi,bj) = alpV(i, 1, bi,bj)
            cTv(1,i,bi,bj) = gamV(i, 1, bi,bj)
            yTv(1,i,bi,bj) = yy_V(i, 1, bi,bj)
            aTv(2,i,bi,bj) = alpV(i,sNy,bi,bj)
            cTv(2,i,bi,bj) = gamV(i,sNy,bi,bj)
            yTv(2,i,bi,bj) = yy_V(i,sNy,bi,bj)
          ENDDO

C     end bi,bj-loops
         ENDDO
        ENDDO

C--   Solve for tile edges values
        IF ( nPx*nPy.GT.1 .OR. useCubedSphereExchange ) THEN
         STOP 'ABNORMAL END: S/R SOLVE_UV_TRIDIAGO: missing code'
        ENDIF
        _BARRIER
        _BEGIN_MASTER(myThid)
        DO bi=1,nSx
         DO i=1,sNx

          DO bj=2,nSy
           bm = bj-1
C-    update row [1,bj] <- [1,bj] - a(1,bj)*[2,bj-1] (& normalise diag)
            tmpVar = oneRL - aTv(1,i,bi,bj)*cTv(2,i,bi,bm)
            IF ( tmpVar.NE.0. _d 0 ) THEN
             tmpVar = 1. _d 0 / tmpVar
            ELSE
             tmpVar = 0. _d 0
             errCode = 1
            ENDIF
            yTv(1,i,bi,bj) = ( yTv(1,i,bi,bj)
     &                       - aTv(1,i,bi,bj)*yTv(2,i,bi,bm)
     &                       )*tmpVar
            cTv(1,i,bi,bj) =   cTv(1,i,bi,bj)*tmpVar
            aTv(1,i,bi,bj) = - aTv(1,i,bi,bj)*aTv(2,i,bi,bm)*tmpVar

C-    update row [2,bj] <- [2,bj] - a(2,bj)*[2,bj-1] + a(2,bj)*c(2,bj-1)*[1,bj]
            tmpVar = aTv(2,i,bi,bj)*cTv(2,i,bi,bm)
            yTv(2,i,bi,bj) =  yTv(2,i,bi,bj)
     &                      - aTv(2,i,bi,bj)*yTv(2,i,bi,bm)
     &                      + tmpVar*yTv(1,i,bi,bj)
            cTv(2,i,bi,bj) =  cTv(2,i,bi,bj)
     &                      + tmpVar*cTv(1,i,bi,bj)
            aTv(2,i,bi,bj) = -aTv(2,i,bi,bj)*aTv(2,i,bi,bm)
     &                      + tmpVar*aTv(1,i,bi,bj)
          ENDDO

          DO bj=nSy-1,1,-1
           bp = bj+1
           DO j=1,2
C-    update row [1,bj] <- [1,bj] - c(1,bj)*[1,bj+1]
C-    update row [2,bj] <- [2,bj] - c(2,bj)*[1,bj+1]
            aTv(j,i,bi,bj) =  aTv(j,i,bi,bj)
     &                      - cTv(j,i,bi,bj)*aTv(1,i,bi,bp)
            yTv(j,i,bi,bj) =  yTv(j,i,bi,bj)
     &                      - cTv(j,i,bi,bj)*yTv(1,i,bi,bp)
            cTv(j,i,bi,bj) = -cTv(j,i,bi,bj)*cTv(1,i,bi,bp)
           ENDDO
          ENDDO

C--  periodic in Y:  X_0 <=> X_Ny and X_(N+1) <=> X_1 ;
C    find the value at the 2 opposite location (j=1 and j=Ny)
          bm = 1
          bp = nSy
          cTv(1,i,bi,bm) = oneRL + cTv(1,i,bi,bm)
          aTv(2,i,bi,bp) = oneRL + aTv(2,i,bi,bp)
          tmpVar = cTv(1,i,bi,bm) * aTv(2,i,bi,bp)
     &           - aTv(1,i,bi,bm) * cTv(2,i,bi,bp)
          IF ( tmpVar.NE.0. _d 0 ) THEN
             tmpVar = 1. _d 0 / tmpVar
          ELSE
             tmpVar = 0. _d 0
             errCode = 1
          ENDIF
          vTmp1 = ( aTv(2,i,bi,bp) * yTv(1,i,bi,bm)
     &            - aTv(1,i,bi,bm) * yTv(2,i,bi,bp)
     &            )*tmpVar
          vTmp2 = ( cTv(1,i,bi,bm) * yTv(2,i,bi,bp)
     &            - cTv(2,i,bi,bp) * yTv(1,i,bi,bm)
     &            )*tmpVar

C-    finalise tile-edges solution (put into RHS "yTv"):
          DO bj=1,nSy
           DO j=1,2
            IF ( bj+j .EQ.2 ) THEN
             yTv(j,i,bi,bj) = vTmp1
            ELSEIF ( bj+j .EQ. nSy+2 ) THEN
             yTv(j,i,bi,bj) = vTmp2
            ELSE
             yTv(j,i,bi,bj) = yTv(j,i,bi,bj)
     &                      - aTv(j,i,bi,bj) * vTmp2
     &                      - cTv(j,i,bi,bj) * vTmp1
            ENDIF
           ENDDO
          ENDDO

         ENDDO
        ENDDO
        _END_MASTER(myThid)
        _BARRIER

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          bm = 1 + MOD(bj-2+nSy,nSy)
          bp = 1 + MOD(bj-0+nSy,nSy)
          DO j= 1,sNy
           DO i= 1,sNx
            vFld(i,j,k,bi,bj) = yy_V(i,j,bi,bj)
     &                      - alpV(i,j,bi,bj) * yTv(2,i,bi,bm)
     &                      - gamV(i,j,bi,bj) * yTv(1,i,bi,bp)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C     end solve for vFld
       ENDIF

C     end k-loop
      ENDDO

      RETURN
      END
