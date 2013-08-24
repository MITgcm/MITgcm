C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_cg_functions.F,v 1.2 2013/08/24 20:35:17 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_CG_ACTION( myThid, 
     O    uret, 
     O    vret, 
     I    u, 
     I    v, 
     I    is, ie, js, je )
C     /============================================================\
C     | SUBROUTINE                                                 |   
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "STREAMICE.h"      
#include "STREAMICE_CG.h"      

C     !INPUT/OUTPUT ARGUMENTS
C     uret, vret - result of matrix operating on u, v
C     is, ie, js, je - starting and ending cells
      INTEGER myThid
      _RL uret (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vret (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL u (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER is, ie, js, je

#ifdef ALLOW_STREAMICE

C the linear action of the matrix on (u,v) with triangular finite elements
C as of now everything is passed in so no grid pointers or anything of the sort have to be dereferenced,
C but this may change pursuant to conversations with others
C
C is & ie are the cells over which the iteration is done; this may change between calls to this subroutine
C     in order to make less frequent halo updates
C isym = 1 if grid is symmetric, 0 o.w.

C the linear action of the matrix on (u,v) with triangular finite elements
C Phi has the form
C Phi (i,j,k,q) - applies to cell i,j

C      3 - 4
C      |   |
C      1 - 2

C Phi (i,j,2*k-1,q) gives d(Phi_k)/dx at quadrature point q 
C Phi (i,j,2*k,q) gives d(Phi_k)/dy at quadrature point q
C Phi_k is equal to 1 at vertex k, and 0 at vertex l .ne. k, and bilinear

C     !LOCAL VARIABLES:
C     == Local variables == 
      INTEGER iq, jq, inode, jnode, i, j, bi, bj, ilq, jlq, m, n,Gi,Gj
      _RL ux, vx, uy, vy, uq, vq, exx, eyy, exy
      _RL Ucell (2,2)
      _RL Vcell (2,2)
      _RL Hcell (2,2)
      _RL phival(2,2)

      uret(1,1,1,1) = uret(1,1,1,1)
      vret(1,1,1,1) = vret(1,1,1,1)

      DO j = js, je
       DO i = is, ie
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)

         Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
         Gj = (myYGlobalLo-1)+(bj-1)*sNy+j

          IF (STREAMICE_hmask (i,j,bi,bj) .eq. 1.0) THEN
           DO iq = 1,2 
            DO jq = 1,2

            n = 2*(jq-1)+iq


            uq = u(i,j,bi,bj) * Xquad(3-iq) * Xquad(3-jq) + 
     &       u(i+1,j,bi,bj) * Xquad(iq) * Xquad(3-jq) + 
     &       u(i,j+1,bi,bj) * Xquad(3-iq) * Xquad(jq) + 
     &       u(i+1,j+1,bi,bj) * Xquad(iq) * Xquad(jq)
            vq = v(i,j,bi,bj) * Xquad(3-iq) * Xquad(3-jq) + 
     &       v(i+1,j,bi,bj) * Xquad(iq) * Xquad(3-jq) + 
     &       v(i,j+1,bi,bj) * Xquad(3-iq) * Xquad(jq) + 
     &       v(i+1,j+1,bi,bj) * Xquad(iq) * Xquad(jq)
            ux = u(i,j,bi,bj) * DPhi(i,j,bi,bj,1,n,1) + 
     &       u(i+1,j,bi,bj) * DPhi(i,j,bi,bj,2,n,1) + 
     &       u(i,j+1,bi,bj) * DPhi(i,j,bi,bj,3,n,1) + 
     &       u(i+1,j+1,bi,bj) * DPhi(i,j,bi,bj,4,n,1)
            uy = u(i,j,bi,bj) * DPhi(i,j,bi,bj,1,n,2) + 
     &       u(i+1,j,bi,bj) * DPhi(i,j,bi,bj,2,n,2) + 
     &       u(i,j+1,bi,bj) * DPhi(i,j,bi,bj,3,n,2) + 
     &       u(i+1,j+1,bi,bj) * DPhi(i,j,bi,bj,4,n,2)
            vx = v(i,j,bi,bj) * DPhi(i,j,bi,bj,1,n,1) + 
     &       v(i+1,j,bi,bj) * DPhi(i,j,bi,bj,2,n,1) + 
     &       v(i,j+1,bi,bj) * DPhi(i,j,bi,bj,3,n,1) + 
     &       v(i+1,j+1,bi,bj) * DPhi(i,j,bi,bj,4,n,1)
            vy = v(i,j,bi,bj) * DPhi(i,j,bi,bj,1,n,2) + 
     &       v(i+1,j,bi,bj) * DPhi(i,j,bi,bj,2,n,2) + 
     &       v(i,j+1,bi,bj) * DPhi(i,j,bi,bj,3,n,2) + 
     &       v(i+1,j+1,bi,bj) * DPhi(i,j,bi,bj,4,n,2)
            exx = ux + k1AtC_str(i,j,bi,bj)*vq
            eyy = vy + k2AtC_str(i,j,bi,bj)*uq
            exy = .5*(uy+vx) + 
     &       k1AtC_str(i,j,bi,bj)*uq + k2AtC_str(i,j,bi,bj)*vq

            do inode = 1,2 
             do jnode = 1,2

             m = 2*(jnode-1)+inode
             ilq = 1 
             jlq = 1
             if (inode.eq.iq) ilq = 2
             if (jnode.eq.jq) jlq = 2 
             phival(inode,jnode) = Xquad(ilq)*Xquad(jlq)

             if (STREAMICE_umask(i-1+inode,j-1+jnode,bi,bj).eq.1.0) then            

              uret(i-1+inode,j-1+jnode,bi,bj) = 
     &         uret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * (
     &          DPhi(i,j,bi,bj,m,n,1)*(4*exx+2*eyy) + 
     &          DPhi(i,j,bi,bj,m,n,2)*(2*exy))


              uret(i-1+inode,j-1+jnode,bi,bj) = 
     &         uret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * phival(inode,jnode) *
     &         (4*k2AtC_str(i,j,bi,bj)*eyy+2*k2AtC_str(i,j,bi,bj)*exx+
     &          4*0.5*k1AtC_str(i,j,bi,bj)*exy)


              uret(i-1+inode,j-1+jnode,bi,bj) = 
     &         uret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         phival(inode,jnode) * 
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         tau_beta_eff_streamice (i,j,bi,bj) * uq


             endif
    
             if (STREAMICE_vmask(i-1+inode,j-1+jnode,bi,bj).eq.1.0) then
              vret(i-1+inode,j-1+jnode,bi,bj) = 
     &         vret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * (
     &          DPhi(i,j,bi,bj,m,n,2)*(4*eyy+2*exx) + 
     &          DPhi(i,j,bi,bj,m,n,1)*(2*exy))
              vret(i-1+inode,j-1+jnode,bi,bj) = 
     &         vret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * phival(inode,jnode) *
     &         (4*k1AtC_str(i,j,bi,bj)*exx+2*k1AtC_str(i,j,bi,bj)*eyy+
     &          4*0.5*k2AtC_str(i,j,bi,bj)*exy)
              vret(i-1+inode,j-1+jnode,bi,bj) = 
     &         vret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         phival(inode,jnode) * 
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         tau_beta_eff_streamice (i,j,bi,bj) * vq
              
             endif
            enddo 
            enddo

           enddo            
           enddo
c-- STREAMICE_hmask
          endif

         enddo
        enddo
       enddo
      enddo

#endif
      RETURN
      END SUBROUTINE

      SUBROUTINE STREAMICE_CG_MAKE_A( myThid )
C     /============================================================\
C     | SUBROUTINE                                                 |   
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "STREAMICE.h"      
#include "STREAMICE_CG.h"      

C     !INPUT/OUTPUT ARGUMENTS
C     uret, vret - result of matrix operating on u, v
C     is, ie, js, je - starting and ending cells
      INTEGER myThid

#ifdef ALLOW_STREAMICE

#ifdef STREAMICE_CONSTRUCT_MATRIX

C the linear action of the matrix on (u,v) with triangular finite elements
C as of now everything is passed in so no grid pointers or anything of the sort have to be dereferenced,
C but this may change pursuant to conversations with others
C
C is & ie are the cells over which the iteration is done; this may change between calls to this subroutine
C     in order to make less frequent halo updates
C isym = 1 if grid is symmetric, 0 o.w.

C the linear action of the matrix on (u,v) with triangular finite elements
C Phi has the form
C Phi (i,j,k,q) - applies to cell i,j

C      3 - 4
C      |   |
C      1 - 2

C Phi (i,j,2*k-1,q) gives d(Phi_k)/dx at quadrature point q 
C Phi (i,j,2*k,q) gives d(Phi_k)/dy at quadrature point q
C Phi_k is equal to 1 at vertex k, and 0 at vertex l .ne. k, and bilinear

C     !LOCAL VARIABLES:
C     == Local variables == 
      INTEGER iq, jq, inodx, inody, i, j, bi, bj, ilqx, ilqy, m_i, n
      INTEGER jlqx, jlqy, jnodx,jnody, m_j, col_y, col_x, cg_halo, k
      INTEGER colx_rev, coly_rev
      _RL ux, vx, uy, vy, uq, vq, exx, eyy, exy, tmpval
      _RL phival(2,2)

!       do i=1,3
!        do j=0,2
!         col_index_a = i + j*3
!        enddo
!       enddo

      cg_halo = min(OLx-1,OLy-1)

      DO j = 1-cg_halo, sNy+cg_halo
       DO i = 1-cg_halo, sNx+cg_halo
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
cc          DO k=1,4
           DO col_x=-1,1
            DO col_y=-1,1
             streamice_cg_A1(i,j,bi,bj,col_x,col_y)=0.0
             streamice_cg_A2(i,j,bi,bj,col_x,col_y)=0.0
             streamice_cg_A3(i,j,bi,bj,col_x,col_y)=0.0
             streamice_cg_A4(i,j,bi,bj,col_x,col_y)=0.0
            ENDDO
           ENDDO
cc          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO j = 1-cg_halo, sNy+cg_halo
       DO i = 1-cg_halo, sNx+cg_halo
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          IF (STREAMICE_hmask (i,j,bi,bj) .eq. 1.0) THEN
           DO iq=1,2 
            DO jq = 1,2
            
             n = 2*(jq-1)+iq
            
             DO inodx = 1,2 
              DO inody = 1,2

               if (STREAMICE_umask(i-1+inodx,j-1+inody,bi,bj)
     &          .eq.1.0 .or. 
     &             streamice_vmask(i-1+inodx,j-1+inody,bi,bj).eq.1.0)
     &          then            
             
                m_i = 2*(inody-1)+inodx
                ilqx = 1 
                ilqy = 1
            
                if (inodx.eq.iq) ilqx = 2
                if (inody.eq.jq) ilqy = 2  
                phival(inodx,inody) = Xquad(ilqx)*Xquad(ilqy)

                DO jnodx = 1,2
                 DO jnody = 1,2 
                  if (STREAMICE_umask(i-1+jnodx,j-1+jnody,bi,bj)
     &             .eq.1.0 .or.
     &             STREAMICE_vmask(i-1+jnodx,j-1+jnody,bi,bj).eq.1.0) 
     &             then            

                   m_j = 2*(jnody-1)+jnodx
                   ilqx = 1 
                   ilqy = 1
                   if (jnodx.eq.iq) ilqx = 2
                   if (jnody.eq.jq) ilqy = 2  

!                    col_j = col_index_a (
!      &              jnodx+mod(inodx,2),
!      &              jnody+mod(inody,2) )

                   col_x = mod(inodx,2)+jnodx-2
                   colx_rev = mod(jnodx,2)+inodx-2
                   col_y = mod(inody,2)+jnody-2
                   coly_rev = mod(jnody,2)+inody-2
c


                   IF ( (inodx.eq.jnodx .and. inody.eq.jnody) .or.
     &                  (inodx.eq.1 .and. inody.eq.1) .or.
     &                  (jnody.eq.2 .and. inody.eq.1) .or.
     &                  (jnody.eq.2 .and. jnodx.eq.2)) THEN



                   ux = DPhi (i,j,bi,bj,m_j,n,1)
                   uy = DPhi (i,j,bi,bj,m_j,n,2)
                   vx = 0  
                   vy = 0
                   uq = Xquad(ilqx) * Xquad(ilqy) 
                   vq = 0

                   exx = ux + k1AtC_str(i,j,bi,bj)*vq
                   eyy = vy + k2AtC_str(i,j,bi,bj)*uq
                   exy = .5*(uy+vx) + 
     &              k1AtC_str(i,j,bi,bj)*uq + k2AtC_str(i,j,bi,bj)*vq
                        
                    tmpval = .25 *
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              visc_streamice(i,j,bi,bj) * (
     &              DPhi(i,j,bi,bj,m_i,n,1)*(4*exx+2*eyy) + 
     &              DPhi(i,j,bi,bj,m_i,n,2)*(2*exy))

                   streamice_cg_A1
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A1
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval

                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN
                    streamice_cg_A1
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A1
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF

!!!

                    tmpval = .25 *
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              visc_streamice(i,j,bi,bj) * (
     &              DPhi(i,j,bi,bj,m_i,n,2)*(4*eyy+2*exx) + 
     &              DPhi(i,j,bi,bj,m_i,n,1)*(2*exy))

                   streamice_cg_A3
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A3
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval
    
                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN 
                    streamice_cg_A2
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A2
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF

!!!

                    tmpval = .25 *
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              visc_streamice(i,j,bi,bj) * phival(inodx,inody) *
     &             (4*k2AtC_str(i,j,bi,bj)*eyy+2*k2AtC_str(i,j,bi,bj)*
     &              exx+4*0.5*k1AtC_str(i,j,bi,bj)*exy)    

                   streamice_cg_A1
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A1
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval
     
                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN
                    streamice_cg_A1
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A1
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF

!!!

                   tmpval = .25 * 
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              visc_streamice(i,j,bi,bj) * phival(inodx,inody) *
     &             (4*k1AtC_str(i,j,bi,bj)*exx+2*k1AtC_str(i,j,bi,bj)*
     &              eyy+4*0.5*k2AtC_str(i,j,bi,bj)*exy)

                   streamice_cg_A3
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A3
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval

                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN
                    streamice_cg_A2
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A2
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF


!!!

                    tmpval = .25*phival(inodx,inody) * 
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              tau_beta_eff_streamice (i,j,bi,bj) * uq

                   streamice_cg_A1
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A1
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval
                   
                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN
                    streamice_cg_A1
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A1
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF


!!!
                    tmpval = .25*phival(inodx,inody) * 
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              tau_beta_eff_streamice (i,j,bi,bj) * vq

                   streamice_cg_A3
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A3
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval
                  
                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN 
                    streamice_cg_A2
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A2
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF


              
!!!

                   vx = DPhi (i,j,bi,bj,m_j,n,1)
                   vy = DPhi (i,j,bi,bj,m_j,n,2)
                   ux = 0 
                   uy = 0
                   vq = Xquad(ilqx) * Xquad(ilqy) 
                   uq = 0

                   exx = ux + k1AtC_str(i,j,bi,bj)*vq
                   eyy = vy + k2AtC_str(i,j,bi,bj)*uq
                   exy = .5*(uy+vx) + 
     &              k1AtC_str(i,j,bi,bj)*uq + k2AtC_str(i,j,bi,bj)*vq

                    tmpval = .25 *
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              visc_streamice(i,j,bi,bj) * (
     &              DPhi(i,j,bi,bj,m_i,n,1)*(4*exx+2*eyy) + 
     &              DPhi(i,j,bi,bj,m_i,n,2)*(2*exy))

                   streamice_cg_A2
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A2
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval
                   
                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN
                    streamice_cg_A3
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A3
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF


                    tmpval = .25 *
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              visc_streamice(i,j,bi,bj) * (
     &              DPhi(i,j,bi,bj,m_i,n,2)*(4*eyy+2*exx) + 
     &              DPhi(i,j,bi,bj,m_i,n,1)*(2*exy))

                   streamice_cg_A4
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A4
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval
                   
                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN
                    streamice_cg_A4
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A4
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF


                   tmpval = .25 *
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              visc_streamice(i,j,bi,bj) * phival(inodx,inody) *
     &             (4*k2AtC_str(i,j,bi,bj)*eyy+2*k2AtC_str(i,j,bi,bj)*
     &              exx+4*0.5*k1AtC_str(i,j,bi,bj)*exy)   

                   streamice_cg_A2
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A2
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval
                   
                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN
                    streamice_cg_A3
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A3
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF


                   tmpval = .25 * 
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              visc_streamice(i,j,bi,bj) * phival(inodx,inody) *
     &             (4*k1AtC_str(i,j,bi,bj)*exx+2*k1AtC_str(i,j,bi,bj)*
     &              eyy+4*0.5*k2AtC_str(i,j,bi,bj)*exy)

                   streamice_cg_A4
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A4
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval
                   
                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN
                    streamice_cg_A4
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A4
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF


                    tmpval = .25*phival(inodx,inody) * 
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              tau_beta_eff_streamice (i,j,bi,bj) * uq

                   streamice_cg_A2
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A2
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval
                   
                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN
                    streamice_cg_A3
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A3
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF


                    tmpval = .25*phival(inodx,inody) * 
     &              grid_jacq_streamice(i,j,bi,bj,n) * 
     &              tau_beta_eff_streamice (i,j,bi,bj) * vq

                   streamice_cg_A4
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)=               
     &             streamice_cg_A4
     &                 (i-1+inodx,j-1+inody,bi,bj,col_x,col_y)+tmpval
                   
                   IF (.not. (inodx.eq.jnodx .and. inody.eq.jnody)) THEN
                    streamice_cg_A4
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)=               
     &              streamice_cg_A4
     &               (i-1+jnodx,j-1+jnody,bi,bj,colx_rev,coly_rev)+
     &               tmpval
                   ENDIF


                   endif
                  endif 
                 enddo
                enddo
               endif
              enddo
             enddo
            enddo
           enddo
          endif
         enddo
        enddo
       enddo
      enddo



#endif
#endif
      RETURN
      END SUBROUTINE

      SUBROUTINE STREAMICE_CG_ADIAG( myThid, 
     O      uret, 
     O      vret)
    
C     /============================================================\
C     | SUBROUTINE                                                 |   
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"            

C     !INPUT/OUTPUT ARGUMENTS
C     uret, vret - result of matrix operating on u, v
C     is, ie, js, je - starting and ending cells
      INTEGER myThid
      _RL uret (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vret (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)


#ifdef ALLOW_STREAMICE

C the linear action of the matrix on (u,v) with triangular finite elements
C as of now everything is passed in so no grid pointers or anything of the sort have to be dereferenced,
C but this may change pursuant to conversations with others
C
C is & ie are the cells over which the iteration is done; this may change between calls to this subroutine
C     in order to make less frequent halo updates
C isym = 1 if grid is symmetric, 0 o.w.

C the linear action of the matrix on (u,v) with triangular finite elements
C Phi has the form
C Phi (i,j,k,q) - applies to cell i,j

C      3 - 4
C      |   |
C      1 - 2

C Phi (i,j,2*k-1,q) gives d(Phi_k)/dx at quadrature point q 
C Phi (i,j,2*k,q) gives d(Phi_k)/dy at quadrature point q
C Phi_k is equal to 1 at vertex k, and 0 at vertex l .ne. k, and bilinear

C     !LOCAL VARIABLES:
C     == Local variables == 
      INTEGER iq, jq, inode, jnode, i, j, bi, bj, ilq, jlq, m, n
      _RL ux, vx, uy, vy, uq, vq, exx, eyy, exy
      _RL Ucell (2,2)
      _RL Vcell (2,2)
      _RL Hcell (2,2)
      _RL phival(2,2)

      uret(1,1,1,1) = uret(1,1,1,1)
      vret(1,1,1,1) = vret(1,1,1,1)

      DO j = 0, sNy+1
       DO i = 0, sNx+1
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          IF (STREAMICE_hmask (i,j,bi,bj) .eq. 1.0) THEN
           DO iq=1,2 
            DO jq = 1,2
            
            n = 2*(jq-1)+iq
            
            DO inode = 1,2 
             DO jnode = 1,2
             
             m = 2*(jnode-1)+inode

             if (STREAMICE_umask(i-1+inode,j-1+jnode,bi,bj).eq.1.0 .or.
     &           STREAMICE_vmask(i-1+inode,j-1+jnode,bi,bj).eq.1.0) 
     &           then

              ilq = 1 
              jlq = 1
            
              if (inode.eq.iq) ilq = 2
              if (jnode.eq.jq) jlq = 2  
              phival(inode,jnode) = Xquad(ilq)*Xquad(jlq)

              ux = DPhi (i,j,bi,bj,m,n,1)
              uy = DPhi (i,j,bi,bj,m,n,2)
              vx = 0  
              vy = 0
              uq = Xquad(ilq) * Xquad(jlq) 
              vq = 0

              exx = ux + k1AtC_str(i,j,bi,bj)*vq
              eyy = vy + k2AtC_str(i,j,bi,bj)*uq
              exy = .5*(uy+vx) + 
     &         k1AtC_str(i,j,bi,bj)*uq + k2AtC_str(i,j,bi,bj)*vq

              uret(i-1+inode,j-1+jnode,bi,bj) = 
     &         uret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * (
     &          DPhi(i,j,bi,bj,m,n,1)*(4*exx+2*eyy) + 
     &          DPhi(i,j,bi,bj,m,n,2)*(2*exy))

              uret(i-1+inode,j-1+jnode,bi,bj) = 
     &         uret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * phival(inode,jnode) *
     &         (4*k2AtC_str(i,j,bi,bj)*eyy+2*k2AtC_str(i,j,bi,bj)*exx+
     &          4*0.5*k1AtC_str(i,j,bi,bj)*exy)    

              
              uret(i-1+inode,j-1+jnode,bi,bj) = 
     &         uret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         phival(inode,jnode) * grid_jacq_streamice(i,j,bi,bj,n) * 
     &         tau_beta_eff_streamice (i,j,bi,bj) * uq
              

              vx = DPhi (i,j,bi,bj,m,n,1)
              vy = DPhi (i,j,bi,bj,m,n,2)
              ux = 0 
              uy = 0
              vq = Xquad(ilq) * Xquad(jlq) 
              uq = 0

              exx = ux + k1AtC_str(i,j,bi,bj)*vq
              eyy = vy + k2AtC_str(i,j,bi,bj)*uq
              exy = .5*(uy+vx) + 
     &         k1AtC_str(i,j,bi,bj)*uq + k2AtC_str(i,j,bi,bj)*vq

              vret(i-1+inode,j-1+jnode,bi,bj) = 
     &         vret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * (
     &          DPhi(i,j,bi,bj,m,n,2)*(4*eyy+2*exx) + 
     &          DPhi(i,j,bi,bj,m,n,1)*(2*exy))
              vret(i-1+inode,j-1+jnode,bi,bj) = 
     &         vret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * phival(inode,jnode) *
     &         (4*k1AtC_str(i,j,bi,bj)*exx+2*k1AtC_str(i,j,bi,bj)*eyy+
     &          4*0.5*k2AtC_str(i,j,bi,bj)*exy)

              
              vret(i-1+inode,j-1+jnode,bi,bj) = 
     &         vret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         phival(inode,jnode) * grid_jacq_streamice(i,j,bi,bj,n) * 
     &         tau_beta_eff_streamice (i,j,bi,bj) * vq
              
             endif

            enddo 
            enddo
           enddo 
           enddo
          endif
         enddo
        enddo
       enddo
      enddo

#endif
      RETURN
      END SUBROUTINE



      SUBROUTINE STREAMICE_CG_BOUND_VALS( myThid, 
     O    uret, 
     O    vret)
C     /============================================================\
C     | SUBROUTINE                                                 |   
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "STREAMICE.h" 
#include "STREAMICE_CG.h"

C     !INPUT/OUTPUT ARGUMENTS
C     uret, vret - result of matrix operating on u, v
C     is, ie, js, je - starting and ending cells
      INTEGER myThid
      _RL uret (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vret (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
    
#ifdef ALLOW_STREAMICE

C the linear action of the matrix on (u,v) with triangular finite elements
C as of now everything is passed in so no grid pointers or anything of the sort have to be dereferenced,
C but this may change pursuant to conversations with others
C
C is & ie are the cells over which the iteration is done; this may change between calls to this subroutine
C     in order to make less frequent halo updates
C isym = 1 if grid is symmetric, 0 o.w.

C the linear action of the matrix on (u,v) with triangular finite elements
C Phi has the form
C Phi (i,j,k,q) - applies to cell i,j

C      3 - 4
C      |   |
C      1 - 2

C Phi (i,j,2*k-1,q) gives d(Phi_k)/dx at quadrature point q 
C Phi (i,j,2*k,q) gives d(Phi_k)/dy at quadrature point q
C Phi_k is equal to 1 at vertex k, and 0 at vertex l .ne. k, and bilinear

C     !LOCAL VARIABLES:
C     == Local variables == 
      INTEGER iq, jq, inode, jnode, i, j, bi, bj, ilq, jlq, m, n
      _RL ux, vx, uy, vy, uq, vq, exx, eyy, exy
      _RL Ucell (2,2)
      _RL Vcell (2,2)
      _RL Hcell (2,2)
      _RL phival(2,2)

      uret(1,1,1,1) = uret(1,1,1,1)
      vret(1,1,1,1) = vret(1,1,1,1)

      DO j = 0, sNy+1
       DO i = 0, sNx+1
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          IF ((STREAMICE_hmask (i,j,bi,bj) .eq. 1.0) .AND.
     &     ((STREAMICE_umask(i,j,bi,bj).eq.3.0) .OR.
     &      (STREAMICE_umask(i,j+1,bi,bj).eq.3.0) .OR.
     &      (STREAMICE_umask(i+1,j,bi,bj).eq.3.0) .OR.
     &      (STREAMICE_umask(i+1,j+1,bi,bj).eq.3.0) .OR.
     &      (STREAMICE_vmask(i,j,bi,bj).eq.3.0) .OR.
     &      (STREAMICE_vmask(i,j+1,bi,bj).eq.3.0) .OR.
     &      (STREAMICE_vmask(i+1,j,bi,bj).eq.3.0) .OR.
     &      (STREAMICE_vmask(i+1,j+1,bi,bj).eq.3.0))) THEN
           
           DO iq=1,2 
            DO jq = 1,2

            n = 2*(jq-1)+iq

            uq = u_bdry_values_SI(i,j,bi,bj)*Xquad(3-iq)*Xquad(3-jq)+ 
     &       u_bdry_values_SI(i+1,j,bi,bj)*Xquad(iq)*Xquad(3-jq)+ 
     &       u_bdry_values_SI(i,j+1,bi,bj)*Xquad(3-iq)*Xquad(jq)+ 
     &       u_bdry_values_SI(i+1,j+1,bi,bj)*Xquad(iq)*Xquad(jq)
            vq = v_bdry_values_SI(i,j,bi,bj)*Xquad(3-iq)*Xquad(3-jq)+ 
     &       v_bdry_values_SI(i+1,j,bi,bj)*Xquad(iq)*Xquad(3-jq)+ 
     &       v_bdry_values_SI(i,j+1,bi,bj)*Xquad(3-iq)*Xquad(jq)+ 
     &       v_bdry_values_SI(i+1,j+1,bi,bj)*Xquad(iq)*Xquad(jq)
            ux = u_bdry_values_SI(i,j,bi,bj) * DPhi(i,j,bi,bj,1,n,1) + 
     &       u_bdry_values_SI(i+1,j,bi,bj) * DPhi(i,j,bi,bj,2,n,1) + 
     &       u_bdry_values_SI(i,j+1,bi,bj) * DPhi(i,j,bi,bj,3,n,1) + 
     &       u_bdry_values_SI(i+1,j+1,bi,bj) * DPhi(i,j,bi,bj,4,n,1)
            uy = u_bdry_values_SI(i,j,bi,bj) * DPhi(i,j,bi,bj,1,n,2) +
     &       u_bdry_values_SI(i+1,j,bi,bj) * DPhi(i,j,bi,bj,2,n,2) + 
     &       u_bdry_values_SI(i,j+1,bi,bj) * DPhi(i,j,bi,bj,3,n,2) + 
     &       u_bdry_values_SI(i+1,j+1,bi,bj) * DPhi(i,j,bi,bj,4,n,2)
            vx = v_bdry_values_SI(i,j,bi,bj) * DPhi(i,j,bi,bj,1,n,1) + 
     &       v_bdry_values_SI(i+1,j,bi,bj) * DPhi(i,j,bi,bj,2,n,1) + 
     &       v_bdry_values_SI(i,j+1,bi,bj) * DPhi(i,j,bi,bj,3,n,1) + 
     &       v_bdry_values_SI(i+1,j+1,bi,bj) * DPhi(i,j,bi,bj,4,n,1)
            vy = v_bdry_values_SI(i,j,bi,bj) * DPhi(i,j,bi,bj,1,n,2) + 
     &       v_bdry_values_SI(i+1,j,bi,bj) * DPhi(i,j,bi,bj,2,n,2) + 
     &       v_bdry_values_SI(i,j+1,bi,bj) * DPhi(i,j,bi,bj,3,n,2) + 
     &       v_bdry_values_SI(i+1,j+1,bi,bj) * DPhi(i,j,bi,bj,4,n,2)
            exx = ux + k1AtC_str(i,j,bi,bj)*vq
            eyy = vy + k2AtC_str(i,j,bi,bj)*uq
            exy = .5*(uy+vx) + 
     &       k1AtC_str(i,j,bi,bj)*uq + k2AtC_str(i,j,bi,bj)*vq


            do inode = 1,2 
             do jnode = 1,2

             m = 2*(jnode-1)+inode
             ilq = 1 
             jlq = 1
             if (inode.eq.iq) ilq = 2
             if (jnode.eq.jq) jlq = 2  
             phival(inode,jnode) = Xquad(ilq)*Xquad(jlq)

             if (STREAMICE_umask(i-1+inode,j-1+jnode,bi,bj).eq.1.0) then          

              
              uret(i-1+inode,j-1+jnode,bi,bj) = 
     &         uret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * (
     &          DPhi(i,j,bi,bj,m,n,1)*(4*exx+2*eyy) + 
     &          DPhi(i,j,bi,bj,m,n,2)*(2*exy))


              uret(i-1+inode,j-1+jnode,bi,bj) = 
     &         uret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * phival(inode,jnode) *
     &         (4*k2AtC_str(i,j,bi,bj)*eyy+2*k2AtC_str(i,j,bi,bj)*exx+
     &          4*0.5*k1AtC_str(i,j,bi,bj)*exy)    
             

!               if (STREAMICE_float_cond(i,j,bi,bj) .eq. 1) then
              uret(i-1+inode,j-1+jnode,bi,bj) = 
     &         uret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         phival(inode,jnode) * grid_jacq_streamice(i,j,bi,bj,n) * 
     &         tau_beta_eff_streamice (i,j,bi,bj) * uq


!               endif
             endif
             if (STREAMICE_vmask(i-1+inode,j-1+jnode,bi,bj).eq.1.0) then 
              vret(i-1+inode,j-1+jnode,bi,bj) = 
     &         vret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * (
     &          DPhi(i,j,bi,bj,m,n,2)*(4*eyy+2*exx) + 
     &          DPhi(i,j,bi,bj,m,n,1)*(2*exy))
              vret(i-1+inode,j-1+jnode,bi,bj) = 
     &         vret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         grid_jacq_streamice(i,j,bi,bj,n) * 
     &         visc_streamice(i,j,bi,bj) * phival(inode,jnode) *
     &         (4*k1AtC_str(i,j,bi,bj)*exx+2*k1AtC_str(i,j,bi,bj)*eyy+
     &          4*0.5*k2AtC_str(i,j,bi,bj)*exy)
              vret(i-1+inode,j-1+jnode,bi,bj) = 
     &         vret(i-1+inode,j-1+jnode,bi,bj) + .25 *
     &         phival(inode,jnode) * grid_jacq_streamice(i,j,bi,bj,n) * 
     &         tau_beta_eff_streamice (i,j,bi,bj) * vq
             endif
            enddo 
            enddo
           enddo 
           enddo
          endif
         enddo
        enddo
       enddo
      enddo

#endif
      RETURN
      END SUBROUTINE
