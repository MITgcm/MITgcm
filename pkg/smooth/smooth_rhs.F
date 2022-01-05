#include "SMOOTH_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE SMOOTH_RHS( fld_in, gt_in, myThid )

C     *==========================================================*
C     | SUBROUTINE smooth_rhs
C     | o As part of smooth_diff3D, this routine computes the
C     |   right hand side of the tendency equation (see below).
C     |   It is made of bits from model/src and pkg/generic_advdiff
C     |   pieced togheter.
C     *==========================================================*

C !DESCRIPTION:
C Calculates the tendency of a tracer due to advection and diffusion.
C It calculates the fluxes in each direction indepentently and then
C sets the tendency to the divergence of these fluxes. The advective
C fluxes are only calculated here when using the linear advection schemes
C otherwise only the diffusive and parameterized fluxes are calculated.
C
C Contributions to the flux are calculated and added:
C \begin{equation*}
C {\bf F} = {\bf F}_{adv} + {\bf F}_{diff} +{\bf F}_{GM} + {\bf F}_{KPP}
C \end{equation*}
C
C The tendency is the divergence of the fluxes:
C \begin{equation*}
C G_\theta = G_\theta + \nabla \cdot {\bf F}
C \end{equation*}
C
C The tendency is assumed to contain data on entry.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SMOOTH.h"

C !INPUT PARAMETERS: ===================================================
      _RL fld_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gt_in (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C local variables:
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dTdz  (nSx,nSy)
      _RL dTdx  (nSx,nSy)
      _RL dTdy  (nSx,nSy)
      INTEGER i,j,k
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fVerT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

c 1rst k loop: initialization
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           fZon(i,j,k,bi,bj)  = 0. _d 0
           fMer(i,j,k,bi,bj)  = 0. _d 0
           fVerT(i,j,k,bi,bj) = 0. _d 0
           gt_in(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

        iMin = 1-OLx+1
        iMax = sNx+OLx-1
        jMin = 1-OLy+1
        jMax = sNy+OLy-1

c 2nd k loop: flux computation
        DO k=1,Nr

         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           df(i,j,bi,bj) = 0. _d 0
           xA(i,j,bi,bj) = _dyG(i,j,bi,bj)
     &      *drF(k)*smooth_hFacW(i,j,k,bi,bj)
           yA(i,j,bi,bj) = _dxG(i,j,bi,bj)
     &      *drF(k)*smooth_hFacS(i,j,k,bi,bj)
           IF (k .EQ. 1) THEN
             maskUp(i,j,bi,bj) = 0.
           ELSE
            maskUp(i,j,bi,bj) =
     &        maskC(i,j,k-1,bi,bj)*maskC(i,j,k,bi,bj)
           ENDIF
          ENDDO
         ENDDO

c      ///gmredi_xtr///

         DO j=jMin,jMax
          DO i=iMin,iMax
           df(i,j,bi,bj) = df(i,j,bi,bj)
     &       -xA(i,j,bi,bj)
     &       *smooth3D_Kux(i,j,k,bi,bj)
     &       *recip_dxC(i,j,bi,bj)
     &       *(fld_in(i,j,k,bi,bj)-fld_in(i-1,j,k,bi,bj))
          ENDDO
         ENDDO

         DO j=jMin,jMax
          DO i=iMin,iMax
           dTdz(bi,bj) =  0.5*(
     &      +0.5*recip_drC(k)*
     &       ( maskC(i-1,j,k,bi,bj)*
     &       (fld_in(i-1,j, MAX(k-1,1) ,bi,bj)-fld_in(i-1,j,k,bi,bj))
     &       +maskC( i ,j,k,bi,bj)*
     &       (fld_in( i ,j, MAX(k-1,1) ,bi,bj)-fld_in( i ,j,k,bi,bj))
     &       )
     &      +0.5*recip_drC(MIN(k+1,Nr))*
     &       ( maskC(i-1,j,MIN(k+1,Nr),bi,bj)*
     &       (fld_in(i-1,j,k,bi,bj)-fld_in(i-1,j,MIN(k+1,Nr),bi,bj))
     &       +maskC( i ,j,MIN(k+1,Nr),bi,bj)*
     &       (fld_in( i ,j,k,bi,bj)-fld_in( i ,j,MIN(k+1,Nr),bi,bj))
     &       )                )
           df(i,j,bi,bj) = df(i,j,bi,bj)
     &      - xA(i,j,bi,bj)*smooth3D_Kuz(i,j,k,bi,bj)*dTdz(bi,bj)
          ENDDO
         ENDDO

         DO j=jMin,jMax
          DO i=iMin,iMax
           dTdy(bi,bj) = 0.5*(
     &      +0.5*(maskS(i,j,k,bi,bj)
     &         *recip_dyC(i,j,bi,bj)*
     &         (fld_in(i,j,k,bi,bj)-fld_in(i,j-1,k,bi,bj))
     &        +maskS(i,j+1,k,bi,bj)
     &        *recip_dyC(i,j+1,bi,bj)*
     &        (fld_in(i,j+1,k,bi,bj)-fld_in(i,j,k,bi,bj)))
     &      +0.5*(maskS(i-1,j,k,bi,bj)
     &        *recip_dyC(i,j,bi,bj)*
     &        (fld_in(i-1,j,k,bi,bj)-fld_in(i-1,j-1,k,bi,bj))
     &        +maskS(i-1,j+1,k,bi,bj)
     &        *recip_dyC(i,j+1,bi,bj)*
     &        (fld_in(i-1,j+1,k,bi,bj)-fld_in(i-1,j,k,bi,bj)))
     &       )
           df(i,j,bi,bj) = df(i,j,bi,bj)
     &      - xA(i,j,bi,bj)*smooth3D_Kuy(i,j,k,bi,bj)*dTdy(bi,bj)
          ENDDO
         ENDDO

c      /// end for x ///

         DO j=jMin,jMax
          DO i=iMin,iMax
           fZon(i,j,k,bi,bj) = fZon(i,j,k,bi,bj) + df(i,j,bi,bj)
          ENDDO
         ENDDO

         DO j=jMin,jMax
          DO i=iMin,iMax
           df(i,j,bi,bj) = 0.
          ENDDO
         ENDDO

c      ///gmredi_ytr///

         DO j=jMin,jMax
          DO i=iMin,iMax
           df(i,j,bi,bj) = df(i,j,bi,bj)
     &      -yA(i,j,bi,bj)
     &       *smooth3D_Kvy(i,j,k,bi,bj)
     &       *recip_dyC(i,j,bi,bj)
     &       *(fld_in(i,j,k,bi,bj)-fld_in(i,j-1,k,bi,bj))
          ENDDO
         ENDDO

         DO j=jMin,jMax
          DO i=iMin,iMax
           dTdz(bi,bj) =  0.5*(
     &      +0.5*recip_drC(k)*
     &       ( maskC(i,j-1,k,bi,bj)*
     &          (fld_in(i,j-1,MAX(k-1,1),bi,bj)-fld_in(i,j-1,k,bi,bj))
     &         +maskC(i, j ,k,bi,bj)*
     &          (fld_in(i, j ,MAX(k-1,1),bi,bj)-fld_in(i, j ,k,bi,bj))
     &       )
     &      +0.5*recip_drC(MIN(k+1,Nr))*
     &       ( maskC(i,j-1,MIN(k+1,Nr),bi,bj)*
     &         (fld_in(i,j-1,k,bi,bj)-fld_in(i,j-1,MIN(k+1,Nr),bi,bj))
     &         +maskC(i, j ,MIN(k+1,Nr),bi,bj)*
     &         (fld_in(i, j ,k,bi,bj)-fld_in(i, j ,MIN(k+1,Nr),bi,bj))
     &       )      )
           df(i,j,bi,bj) = df(i,j,bi,bj)
     &      - yA(i,j,bi,bj)*smooth3D_Kvz(i,j,k,bi,bj)*dTdz(bi,bj)
          ENDDO
         ENDDO

         DO j=jMin,jMax
          DO i=iMin,iMax
           dTdx(bi,bj) = 0.5*(
     &      +0.5*(maskW(i+1,j,k,bi,bj)
     &            *recip_dxC(i+1,j,bi,bj)*
     &            (fld_in(i+1,j,k,bi,bj)-fld_in(i,j,k,bi,bj))
     &            +maskW(i,j,k,bi,bj)
     &            *recip_dxC(i,j,bi,bj)*
     &            (fld_in(i,j,k,bi,bj)-fld_in(i-1,j,k,bi,bj)))
     &      +0.5*(maskW(i+1,j-1,k,bi,bj)
     &            *recip_dxC(i+1,j,bi,bj)*
     &            (fld_in(i+1,j-1,k,bi,bj)-fld_in(i,j-1,k,bi,bj))
     &            +maskW(i,j-1,k,bi,bj)
     &            *recip_dxC(i,j,bi,bj)*
     &            (fld_in(i,j-1,k,bi,bj)-fld_in(i-1,j-1,k,bi,bj)))
     &       )
           df(i,j,bi,bj) = df(i,j,bi,bj)
     &      - yA(i,j,bi,bj)*smooth3D_Kvx(i,j,k,bi,bj)*dTdx(bi,bj)
          ENDDO
         ENDDO

c      /// end for y ///

         DO j=jMin,jMax
          DO i=iMin,iMax
           fMer(i,j,k,bi,bj) = fMer(i,j,k,bi,bj) + df(i,j,bi,bj)
          ENDDO
         ENDDO

         DO j=jMin,jMax
          DO i=iMin,iMax
           df(i,j,bi,bj) = 0.
          ENDDO
         ENDDO

c       /// GAD_DIFF_R ///

         IF ( k.GT.1 .AND. .NOT.smooth3DdoImpldiff ) THEN
          DO j=jMin,jMax
           DO i=iMin,iMax
            df(i,j,bi,bj) =
     &        -_rA(i,j,bi,bj)
     &        *smooth3D_kappaR(i,j,k,bi,bj)*recip_drC(k)
     &        *(fld_in(i,j,k,bi,bj)
     &        -fld_in(i,j,k-1,bi,bj))*rkSign
           ENDDO
          ENDDO
         ENDIF

c      ///gmredi rtrans///

         IF (k.GT.1) THEN
          DO j=jMin,jMax
           DO i=iMin,iMax
            dTdx(bi,bj) = 0.5*(
     &       +0.5*(maskW(i+1,j,k,bi,bj)
     &             *recip_dxC(i+1,j,bi,bj)*
     &             (fld_in(i+1,j,k,bi,bj)-fld_in(i,j,k,bi,bj))
     &            +maskW(i,j,k,bi,bj)
     &             *recip_dxC(i,j,bi,bj)*
     &             (fld_in(i,j,k,bi,bj)-fld_in(i-1,j,k,bi,bj)))
     &       +0.5*(maskW(i+1,j,k-1,bi,bj)
     &             *recip_dxC(i+1,j,bi,bj)*
     &             (fld_in(i+1,j,k-1,bi,bj)-fld_in(i,j,k-1,bi,bj))
     &            +maskW(i,j,k-1,bi,bj)
     &             *recip_dxC(i,j,bi,bj)*
     &             (fld_in(i,j,k-1,bi,bj)-fld_in(i-1,j,k-1,bi,bj)))
     &                        )

            dTdy(bi,bj) = 0.5*(
     &       +0.5*(maskS(i,j,k,bi,bj)
     &             *recip_dyC(i,j,bi,bj)*
     &             (fld_in(i,j,k,bi,bj)-fld_in(i,j-1,k,bi,bj))
     &            +maskS(i,j+1,k,bi,bj)
     &             *recip_dyC(i,j+1,bi,bj)*
     &             (fld_in(i,j+1,k,bi,bj)-fld_in(i,j,k,bi,bj)))
     &       +0.5*(maskS(i,j,k-1,bi,bj)
     &             *recip_dyC(i,j,bi,bj)*
     &             (fld_in(i,j,k-1,bi,bj)-fld_in(i,j-1,k-1,bi,bj))
     &            +maskS(i,j+1,k-1,bi,bj)
     &             *recip_dyC(i,j+1,bi,bj)*
     &             (fld_in(i,j+1,k-1,bi,bj)-fld_in(i,j,k-1,bi,bj)))
     &                        )

            df(i,j,bi,bj) = df(i,j,bi,bj)
     &           - rA(i,j,bi,bj)
     &           *( smooth3D_Kwx(i,j,k,bi,bj)*dTdx(bi,bj)
     &             +smooth3D_Kwy(i,j,k,bi,bj)*dTdy(bi,bj) )

           ENDDO
          ENDDO
         ENDIF

c     /// end for r ///

         IF (k.GT.1) THEN
          DO j=jMin,jMax
           DO i=iMin,iMax
            fVerT(i,j,k-1,bi,bj) = fVerT(i,j,k-1,bi,bj)
     &       + df(i,j,bi,bj)*maskUp(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF

         DO j=jMin,jMax
          DO i=iMin,iMax
           df(i,j,bi,bj) = 0.
          ENDDO
         ENDDO

        ENDDO !k
       ENDDO !bi
      ENDDO !bj

c these exchanges are crucial:
      CALL EXCH_UV_XYZ_RL(fZon,fMer,.TRUE.,myThid)
      CALL EXCH_XYZ_RL ( fVerT, myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
c 3rd k loop: Divergence of fluxes
        DO k=1,Nr
         IF (k.GT.1) THEN
          DO j=jMin,jMax
           DO i=iMin,iMax
            gt_in(i,j,k,bi,bj) = gt_in(i,j,k,bi,bj)
     &       -smooth_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &       *recip_rA(i,j,bi,bj)
     &       *( (fZon(i+1,j,k,bi,bj)-fZon(i,j,k,bi,bj))
     &         +(fMer(i,j+1,k,bi,bj)-fMer(i,j,k,bi,bj))
     &         +(fVerT(i,j,k,bi,bj)-fVerT(i,j,k-1,bi,bj))*rkSign
     &        )
           ENDDO
          ENDDO
         ELSE
          DO j=jMin,jMax
           DO i=iMin,iMax
            gt_in(i,j,k,bi,bj) = gt_in(i,j,k,bi,bj)
     &       -smooth_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &       *recip_rA(i,j,bi,bj)
     &       *( (fZon(i+1,j,k,bi,bj)-fZon(i,j,k,bi,bj))
     &         +(fMer(i,j+1,k,bi,bj)-fMer(i,j,k,bi,bj))
     &         +(fVerT(i,j,k,bi,bj))*rkSign
     &        )
           ENDDO
          ENDDO
         ENDIF
        ENDDO

       ENDDO
      ENDDO

      CALL EXCH_XYZ_RL( gt_in, myThid )

      RETURN
      END

