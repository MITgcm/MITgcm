#include "KL10_OPTIONS.h"

CBOP
C !ROUTINE: KL10_CALC

C !INTERFACE: =======================================================
      SUBROUTINE KL10_CALC(
     I                bi, bj, sigmaR, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE KL10_CALC                                     |
C     | o Compute all KL10 fields defined in KL10.h              |
C     *==========================================================*
C     | This subroutine is based on SPEM code                    |
C     *==========================================================*
C \ev

C--------------------------------------------------------------------

C JMK
C global parameters updated by kl_calc
C     KLviscAz  :: KL eddy viscosity coefficient              (m^2/s)
C     KLdiffKzT :: KL diffusion coefficient for temperature   (m^2/s)

C !USES: ============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "KL10.h"
c#ifdef ALLOW_AUTODIFF_TAMC
c# include "tamc.h"
c#endif /* ALLOW_AUTODIFF_TAMC */

C !INPUT PARAMETERS: ===================================================
c Routine arguments
C     bi, bj :: Current tile indices
C     sigmaR :: Vertical gradient of iso-neutral density
C     myTime :: Current time in simulation
C     myIter :: Current time-step number
C     myThid :: My Thread Id number
      INTEGER bi, bj
      _RL     sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_KL10
C !LOCAL VARIABLES: ====================================================
c Local constants
C     iMin, iMax, jMin, jMax :: array computation indices
      INTEGER I, J, K, Km1, JJ
      INTEGER   iMin ,iMax ,jMin ,jMax,di
      _RL     KLviscTmp, rhot, tempu
      _RL     b0, buoyFreqf, buoyFreqc, KLviscold,zsum,zsums
      _RL     rhoS(1:Nr),RS(1:Nr)
      _RL     dzp,ec,ep,es,epss(-1:0),epsw(-1:0),dz,KTemp
c     _RL     bF(1:Nr)
c     _RL     theta_mcb(1:Nr),theta_mcb3(1:Nr)
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      iMin = 2-OLx
      iMax = sNx+OLx-1
      jMin = 2-OLy
      jMax = sNy+OLy-1

      DO J=jMin,jMax
         DO I=iMin,iMax
            K=1
            CALL FIND_RHO_SCALAR(theta(I,J,K,bi,bj), salt(I,J,K,bi,bj),
     &           totPhiHyd(I,J,K,bi,bj),rhot,myThid )
            rhoS(1)=rhot
            RS(1)=rC(1)

            KLeps(I-1,J-1,1,bi,bj)=0.0
c           eps(k-1) = (dz(k-1)*eps0(k-1) +dz(k)*eps0(k))/(dz(k-1)+dz(k))
            ep = 0.0
            dzp = 0.0

            KLviscAr(I,J,1,bi,bj) = viscArNr(1)
            KLviscold = KLviscAr(I,J,1,bi,bj) ! at previous cell center
C get sorted densities rhoS, and the array with the depths from which
C the density came from, RS.
            DO K=2,Nr
               CALL FIND_RHO_SCALAR(theta(I,J,K,bi,bj), salt(I,J,K,bi
     &              ,bj),totPhiHyd(I,J,K,bi,bj),rhot,myThid )
               rhoS(K)=rhot
               RS(K)=rC(K)
c$$$               WRITE(msgBuf, '(A,I10.10,A,E10.4,A,E10.4)') 'Hellok ', K
c$$$     &              -1,' ',theta(I,J,K,bi,bj),' ',rhot
c$$$               CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c$$$     &                             SQUEEZE_RIGHT, myThid )
               IF ( (rhoS(K).LT.rhoS(K-1)).AND.(maskC(I,J,K,bi
     &              ,bj).GT.0)) THEN
                  JJ=K-1
                  DO WHILE ( (JJ.GT.0).AND.(rhoS(K).LT.rhoS(JJ)) )
c                    write(*,*) K,JJ,rhoS(K),rhoS(JJ)
                     JJ=JJ-1
                  ENDDO
                  rhoS(JJ+1:K)=cshift(rhoS(JJ+1:K),-1)
                  RS(JJ+1:K)=cshift(RS(JJ+1:K),-1)
               ENDIF
            ENDDO

C RS-R is dz....
C recip_drC=inverse distanance between centers,
C first is between surface and first center
C diffKrNrS(K) = viscArNr(K) = background value

            KLdiffKr(I,J,1,bi,bj) = MAX(KLviscAr(I,J,1,bi,bj),
#ifdef ALLOW_3D_DIFFKR
     &                                  diffKr(I,J,1,bi,bj) )
#else
     &                                  diffKrNrS(1) )
#endif
C N at surface = zero or uses gradient
            b0 = MAX(-gravity*mass2rUnit*
     &              (rhoS(1) - rhoS(2))*recip_drC(2),0. _d 0)
c           b0 = 0.
            DO di=-1,0
               epss(di)=0.0
               epsw(di)=0.0
            ENDDO

            DO K=1,Nr
               IF (K.LT.Nr) THEN
                  buoyFreqf = -gravity*mass2rUnit*
     &              (rhoS(K) - rhoS(K+1))*recip_drC(K+1)
               ELSE
C N zero OR not zero near bottom (at the end of array)
                  buoyFreqf = -gravity*mass2rUnit*
     &              (rhoS(K-1) - rhoS(K))*recip_drC(K)
C                  buoyFreqf = 0.
               ENDIF
               buoyFreqf = MAX(buoyFreqf,0. _d 0) ! not < 0
               buoyFreqc = (buoyFreqf + b0)*0.5   ! mean at cell center

C viscosity at cell center at K
C = 0.2*dz^2*N.  0.2 is mixing efficiency.
C to derive, note K = 0.2\epsilon/N^2.  Then note that
C \epsilon = dz^2N^3 (Ozmidov scale)
               KLviscTmp = MAX( viscArNr(K), 0.2*(RS(K)-rC(K))*
     &                        (RS(K)-rC(K))*sqrt(buoyFreqc))

               IF (K.GT.1) THEN
                  Km1=K-1

C viscosity at cell face above center at K
                  KTemp = 0.5*(KLviscTmp+KLviscold)
C Put an upper limit on viscosity to prevent instability when
C explicit viscosity is C used (e.g. for nonhydrostatic case) SAL
                  KTemp = MIN(KLviscMax,KTemp)
                  KLviscAr(I,J,K,bi,bj) = MAX(KTemp,viscArNr(K))
                  KLdiffKr(I,J,K,bi,bj) = MAX(KTemp,
#ifdef ALLOW_3D_DIFFKR
     &                                        diffKr(I,J,K,bi,bj) )
#else
     &                                        diffKrNrS(K) )
#endif

C Compute Epsilon for diagnostics:
C
C need to caclulate Im1 and Jm1 epsilon unfortunately...  Here at
C i-1,j-1 we average the west nu(du/dz)^2 at i-1 and i, and the south
C nu(dv/dv)^2 at j-1 and j, and then add them
C
C dz is calculated from the face distances, with the cells assumed to be
C half way.  Note the use of hfacW and hfacS to make these correct near
C bathy.
                  zsum=0.
                  ec=0.0
                  zsums=0.
                  es=0.
                  DO di=-1,0
                     IF (hfacW(I+di,J-1,K,bi,bj).GT.0.000001) THEN
                        dz = 0.5*(drF(K)*hfacW(I+di,J-1,K,bi,bj)
     &                       +drF(Km1)*hfacW(I+di,J-1,Km1,bi,bj))
                        IF (dz.GT.0.00001) THEN
                           tempu = (uVel(I+di,J-1,Km1,bi,bj)-uVel(I+di,J
     &                          -1,K,bi,bj))/dz
                           epsw(di)=tempu*tempu*KLviscAr(I+di,J-1,K,bi
     &                          ,bj)
                           ec=ec+epsw(di)*dz
                           zsum = zsum+dz
                        ENDIF
                     ELSE
C                       This face is on the seafloor.  set epsilon=the
C                       previous and dz = half the face.
                        dz=0.5*(drF(Km1)*hfacW(I+di,J-1,Km1,bi ,bj))
                        ec=ec+epsw(di)*dz
                        zsum = zsum+dz
                     ENDIF
C Now do the v-component
                     IF (hfacS(I-1,J+di,K,bi,bj).GT.0.000001) THEN
                        dz = 0.5*(drF(K)*hfacS(I-1,J+di,K,bi,bj)
     &                       +drF(Km1)*hfacS(I-1,J+di,Km1,bi,bj))
                        IF (dz.GT.0.00001) THEN
                           tempu = (vVel(I-1,J+di,Km1,bi,bj)-vVel(I-1,J
     &                          +di,K,bi,bj))/dz
                           epss(di)=tempu*tempu*KLviscAr(I-1,J+di,K,bi
     &                          ,bj)
                           es = es+epss(di)*dz
                           zsums = zsums+dz
                        ENDIF
                     ELSE
C                       This face is on the seafloor.  set epsilon=the
C                       previous and dz = half the face.
                        dz=+0.5*(drF(Km1)*hfacS(I-1,J+di,Km1 ,bi,bj))
                        es = es+epss(di)*dz
                        zsums = zsums+dz
                     ENDIF
                  ENDDO
C                 take the average of the du/dz terms
                  IF (zsum.GT.0.00001) THEN
                     ec=ec/zsum
                  ENDIF
C                 take the average of the dv/dz terms
                  IF (zsums.GT.0.00001) THEN
                     es=es/zsums
                  ENDIF
C add the u and v dissipations:
                  ec=es+ec

C Note this ec is defined on cell faces K=2..NR at the center of the
C cells (i.e. at XC), so its above the density variables.
C
C So to get at the center of the cells, just average this one and the previous one.
C  And its a true average because the

                  KLeps(I-1,J-1,Km1,bi,bj) = 0.5*(ep+ec)
                  IF (Km1.EQ.1) THEN
                     KLeps(I-1,J-1,Km1,bi,bj) = ec
                  ENDIF
                  ep=ec
               ENDIF
c$$$               WRITE(msgBuf, '(A,I10.10,A,E10.4,A,E10.4)') 'Hellok ', K
c$$$     &              -1,' ',theta(I,J,K,bi,bj),' ',KLeps(I-1,J-1,Km1,bi
c$$$     &              ,bj)
c$$$               CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c$$$     &                             SQUEEZE_RIGHT, myThid )

               b0        = buoyFreqf ! at previous cell face
               KLviscold = KLviscTmp ! at previous cell center
            ENDDO
C           ENDDO K
C     set on K=Nr
            KLeps(I-1,J-1,Nr,bi,bj) =ep

         ENDDO
C           ENDDO J
      ENDDO
C           ENDDO I

#endif /* ALLOW_KL10 */

      RETURN
      END
