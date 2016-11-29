#include "STREAMICE_OPTIONS.h"

      subroutine template()
      use OAD_cp
      use OAD_tape
      use OAD_rev

!$TEMPLATE_PRAGMA_DECLARATIONS

      integer myi
! Temporaries to hold the stack pointers
      integer temp_double_tape_pointer, temp_integer_tape_pointer, temp_logical_tape_pointer, temp_character_tape_pointer, temp_string_tape_pointer 
      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr
!<------------------Begin user declarations ---------------------->!
! Insert declarations of dummy variables for calling adjoint computation 
! without side effects, and storing adjoint variable iterates

#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP))

#ifdef STREAMICE_ALLOW_FRIC_CONTROL
      Real*8 C_basal_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef STREAMICE_ALLOW_BGLEN_CONTROL
      Real*8 B_glen_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef STREAMICE_ALLOW_DEPTH_CONTROL
      Real*8 R_low_si_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
      Real*8 H_streamice_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 taudx_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 taudy_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 u_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 v_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 u_new_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 v_new_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef STREAMICE_HYBRID_STRESS
      Real*8 taubx_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 tauby_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 visc_full_dummy_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 taubx_new_dummy (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 tauby_new_dummy (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 visc_new_dummy (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

!<------------------End user declarations ------------------------>!
      if (our_rev_mode%plain) then
      our_orig_mode=our_rev_mode
      IF(ISINLOOP .eq. 0) THEN
        CONVERGED = .FALSE.
        ADJ_CONVERGED = .FALSE.
        !ERR_LAST_CHANGE = 10.
      end if
      IF(ISINLOOP .ne. 0) THEN
        IF(.NOT. CONVERGED) THEN
          NL_ITER = (NL_ITER + 1)
          CALL OpenAD_streamice_vel_phi(MYTHID,MAXNLITER,MAXCGITER,CGTOL
     +,CG_ITERS,err_max)

          WRITE(MSGBUF,'(A,I5,A,I4,A)') 'streamice linear solve number',
     +NL_ITER,' ',CG_ITERS,' iterations '

          CALL print_message(MSGBUF,STANDARDMESSAGEUNIT,'R',1)

!!!!!!!!!!!! conv check

          if (STREAMICE_chkresidconvergence) then


           WRITE(msgBuf,'(A,E15.7)') 'err/err_init',
     &                       err_max/err_init
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

           IF (err_max .LE. streamice_nonlin_tol * err_init) THEN
            CONVERGED = .true.
           ENDIF

           IF (err_max<err_last_change*1.e-2 .and.
     &      STREAMICE_lower_cg_tol) THEN
            cgtol = cgtol * 5.e-2
            err_last_change = err_max
            WRITE(msgBuf,'(A,E15.7)') 'new cg tol: ',
     &                       cgtol
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
           ENDIF

          endif

          if (STREAMICE_chkfixedptconvergence) then

           CALL openad_STREAMICE_GET_FP_ERR_OAD ( err_max_fp, myThid )

           WRITE(msgBuf,'(A,1PE22.14)') 'STREAMICE_FP_ERROR =',
     &                       err_max_fp
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

           IF (err_max_fp .LE. streamice_nonlin_tol_fp) THEN
            CONVERGED = .true.
           ENDIF


          endif


          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              U_streamice (i,j,bi,bj)=u_new_SI (i,j,bi,bj)
              V_streamice (i,j,bi,bj)=v_new_SI (i,j,bi,bj)
#ifdef STREAMICE_HYBRID_STRESS
              streamice_taubx(i,j,bi,bj)=taubx_new_si(i,j,bi,bj)
              streamice_tauby(i,j,bi,bj)=tauby_new_si(i,j,bi,bj)
              DO m=Nr,1,-1
               visc_streamice_full(i,j,m,bi,bj)= 
     &          visc_full_new_si(i,j,m,bi,bj)
              ENDDO
#endif
             ENDDO
            ENDDO
           ENDDO
          ENDDO

!!!!!!!!!!!! end conv check


        end if        
      end if
      
      our_rev_mode=our_orig_mode
      end if


!!!!!!!!!!!! TAPE MODE !!!!!!!!!!!!!!





      if (our_rev_mode%tape) then
      our_orig_mode=our_rev_mode
      if(isinloop.eq.0) then
        CONVERGED = .false.
        nl_iter = 0
      end if

      if(isinloop.eq.1) then

        CALL TIMER_START('STREAMICE TAPE FIXED POINT LOOP',myThid)

        IF (.not. (CONVERGED).AND. nl_iter.lt.MAXNLITER) THEN
          NL_ITER = (NL_ITER+1)
          !Run in plain mode while not converged
          our_rev_mode%plain=.true.
          our_rev_mode%tape=.false.
          our_rev_mode%adjoint=.false.
          CALL OpenAD_streamice_vel_phi(MYTHID,MAXNLITER,MAXCGITER,CGTOL
     +,CG_ITERS,err_max)

!---- write out number of cg iters

          WRITE(MSGBUF,'(A,I5,A,I4,A)') 'streamice linear solve number',
     +NL_ITER,' ',CG_ITERS,' iterations '
!          OAD_CTMP0 = 1
          CALL print_message(MSGBUF,STANDARDMESSAGEUNIT,'R',1)

!---- conv check
          
          if (STREAMICE_chkresidconvergence) then


           WRITE(msgBuf,'(A,E15.7)') 'err/err_init',
     &                       err_max/err_init
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

           IF (err_max .LE. streamice_nonlin_tol * err_init) THEN
            CONVERGED = .true.
           ENDIF

           IF (err_max<err_last_change*1.e-2 .and.
     &      STREAMICE_lower_cg_tol) THEN
            cgtol = cgtol * 5.e-2
            err_last_change = err_max
            WRITE(msgBuf,'(A,E15.7)') 'new cg tol: ',
     &                       cgtol
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
           ENDIF

          endif

          if (STREAMICE_chkfixedptconvergence) then

           CALL openad_STREAMICE_GET_FP_ERR_OAD ( err_max_fp, myThid )

           WRITE(msgBuf,'(A,1PE22.14)') 'STREAMICE_FP_ERROR =',
     &                       err_max_fp
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

           IF (err_max_fp .LE. streamice_nonlin_tol_fp) THEN
            CONVERGED = .true.
           ENDIF

          endif

          DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
              DO j=1-OLy,sNy+OLy
                DO i=1-OLx,sNx+OLx
                  U_streamice (i,j,bi,bj)%v=u_new_SI (i,j,bi,bj)%v
                  V_streamice (i,j,bi,bj)%v=v_new_SI (i,j,bi,bj)%v
#ifdef STREAMICE_HYBRID_STRESS
                  streamice_taubx(i,j,bi,bj)%v=
     &             taubx_new_si(i,j,bi,bj)%v
                  streamice_tauby(i,j,bi,bj)%v=
     &             tauby_new_si(i,j,bi,bj)%v
                  DO m=Nr,1,-1
                   visc_streamice_full(i,j,m,bi,bj)%v= 
     &              visc_full_new_si(i,j,m,bi,bj)%v
                  ENDDO
#endif
                ENDDO
              ENDDO
            ENDDO
          ENDDO





!--------conv check done

          if (converged .OR. nl_iter.eq.MAXNLITER) then
            !Run once in tape mode if this is the last time 
            our_rev_mode%plain=.false.
            our_rev_mode%tape=.true.
            our_rev_mode%adjoint=.false.
            CALL OpenAD_streamice_vel_phi(MYTHID,MAXNLITER,MAXCGITER,CG
     +TOL,CG_ITERS,err_max)
          end if 
        end if
        CALL TIMER_STOP('STREAMICE TAPE FIXED POINT LOOP',myThid)
      end if 
      if(isinloop.eq.2 ) then

        CALL OpenAD_streamice_vel_phi(MYTHID,MAXNLITER,MAXCGITER,CGTOL
     +,CG_ITERS,err_max)

        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-OLy,sNy+OLy
              DO i=1-OLx,sNx+OLx
                U_streamice (i,j,bi,bj)%v=u_new_SI (i,j,bi,bj)%v
                V_streamice (i,j,bi,bj)%v=v_new_SI (i,j,bi,bj)%v 
#ifdef STREAMICE_HYBRID_STRESS
                  streamice_taubx(i,j,bi,bj)%v=
     &             taubx_new_si(i,j,bi,bj)%v
                  streamice_tauby(i,j,bi,bj)%v=
     &             tauby_new_si(i,j,bi,bj)%v
                  DO m=Nr,1,-1
                   visc_streamice_full(i,j,m,bi,bj)%v= 
     &              visc_full_new_si(i,j,m,bi,bj)%v
                  ENDDO
#endif
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      end if 
      our_rev_mode=our_orig_mode
      end if


!!!!!!!!!!!! ADJOINT MODE !!!!!!!!!!!!!!



      if (our_rev_mode%adjoint) then  
      our_orig_mode=our_rev_mode

      if(isinloop.eq.2) then

        ADJ_CONVERGED = .false.
        adj_iter = 0

        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-OLy,sNy+OLy
              DO i=1-OLx,sNx+OLx
                v_new_SI (i,j,bi,bj)%d= V_streamice(i,j,bi,bj)%d 
                V_streamice  (i,j,bi,bj)%d = 0.0
                u_new_SI (i,j,bi,bj)%d= U_streamice(i,j,bi,bj)%d 
                U_streamice  (i,j,bi,bj)%d = 0.0
#ifdef STREAMICE_HYBRID_STRESS
                taubx_new_si(i,j,bi,bj)%d=
     &           streamice_taubx(i,j,bi,bj)%d
                streamice_taubx(i,j,bi,bj)%d = 0.0
                tauby_new_si(i,j,bi,bj)%d=
     &           streamice_tauby(i,j,bi,bj)%d
                streamice_tauby(i,j,bi,bj)%d = 0.0
                DO m=Nr,1,-1
                 visc_full_new_si(i,j,m,bi,bj)%d= 
     &            visc_streamice_full(i,j,m,bi,bj)%d
                 visc_streamice_full(i,j,m,bi,bj)%d = 0.0
                ENDDO
#endif
              ENDDO
            ENDDO
          ENDDO
        ENDDO

        CALL OpenAD_streamice_vel_phi(MYTHID,MAXNLITER,MAXCGITER,CGTOL
     +,CG_ITERS,err_max)

        DO BJ = MYBYLO(MYTHID), MYBYHI(MYTHID), 1
          DO BI = MYBXLO(MYTHID), MYBXHI(MYTHID), 1
            DO J = 1-OLy,sNy+OLy
              DO I = 1-OLx,sNx+OLx

                U_streamice_dvals(I,J,BI,BJ) = 
     +U_STREAMICE(INT(I),INT(J),INT(BI),INT(BJ))%d
                V_streamice_dvals(I,J,BI,BJ) = 
     +V_STREAMICE(INT(I),INT(J),INT(BI),INT(BJ))%d
#ifdef STREAMICE_HYBRID_STRESS
                taubx_dvals(I,J,BI,BJ) =
     +streamice_taubx(INT(I),INT(J),INT(BI),INT(BJ))%d
                tauby_dvals(I,J,BI,BJ) =
     +streamice_tauby(INT(I),INT(J),INT(BI),INT(BJ))%d
                DO m=Nr,1,-1
                visc_full_dvals(I,J,m,BI,BJ) =
     +VISC_STREAMICE_FULL(INT(I),INT(J),m,INT(BI),INT(BJ))%d
                ENDDO
#endif

                U_new_si(I,J,BI,BJ)%d = 
     +U_STREAMICE(INT(I),INT(J),INT(BI),INT(BJ))%d
                V_new_si(I,J,BI,BJ)%d = 
     +V_STREAMICE(INT(I),INT(J),INT(BI),INT(BJ))%d
#ifdef STREAMICE_HYBRID_STRESS
                taubx_new_si(I,J,BI,BJ)%d =
     +streamice_taubx(INT(I),INT(J),INT(BI),INT(BJ))%d
                tauby_new_si(I,J,BI,BJ)%d =
     +streamice_tauby(INT(I),INT(J),INT(BI),INT(BJ))%d
                DO m=Nr,1,-1
                visc_full_new_si(I,J,m,BI,BJ)%d =
     +VISC_STREAMICE_FULL(INT(I),INT(J),m,INT(BI),INT(BJ))%d
                ENDDO
#endif

              END DO
            END DO
          END DO
        END DO

      end if



      if(isinloop.eq.1) then
        if((.NOT.ADJ_CONVERGED).AND.(adj_iter.lt.MAXNLITER)) then

          adj_iter = adj_iter + 1
          if (adj_iter.eq.1) then
           CALL TIMER_START('STREAMICE ADJ FIXED POINT LOOP0',myThid)
          else
           CALL TIMER_START('STREAMICE ADJ FIXED POINT LOOP',myThid)
          endif

          DO BJ = MYBYLO(MYTHID), MYBYHI(MYTHID), 1
            DO BI = MYBXLO(MYTHID), MYBXHI(MYTHID), 1
              DO J = 1-OLy,sNy+OLy
                DO I = 1-OLx,sNx+OLx



                  U_DUMMY_D(INT(I),INT(J),INT(BI),INT(BJ)) =
     +U_streamice(INT(I),INT(J),INT(BI),INT(BJ))%d
                  V_DUMMY_D(INT(I),INT(J),INT(BI),INT(BJ)) =
     +V_streamice(INT(I),INT(J),INT(BI),INT(BJ))%d
#ifdef STREAMICE_HYBRID_STRESS
                  TAUBX_DUMMY_D(INT(I),INT(J),INT(BI),INT(BJ)) =
     +streamice_taubx(INT(I),INT(J),INT(BI),INT(BJ))%d
                  TAUBY_DUMMY_D(INT(I),INT(J),INT(BI),INT(BJ)) =
     +streamice_tauby(INT(I),INT(J),INT(BI),INT(BJ))%d
                  DO m=Nr,1,-1
                   VISC_FULL_DUMMY_D(INT(I),INT(J),m,INT(BI),INT(BJ))=
     +VISC_STREAMICE_FULL(INT(I),INT(J),m,INT(BI),INT(BJ))%d
                  ENDDO
#endif


                  U_NEW_DUMMY_D(INT(I),INT(J),INT(BI),INT(BJ)) =
     +U_NEW_SI(INT(I),INT(J),INT(BI),INT(BJ))%d
                  V_NEW_DUMMY_D(INT(I),INT(J),INT(BI),INT(BJ)) =
     +V_NEW_SI(INT(I),INT(J),INT(BI),INT(BJ))%d
#ifdef STREAMICE_HYBRID_STRESS
                  TAUBX_new_DUMMY(INT(I),INT(J),INT(BI),INT(BJ)) =
     +TAUBX_new_si(INT(I),INT(J),INT(BI),INT(BJ))%d
                  TAUBY_new_DUMMY(INT(I),INT(J),INT(BI),INT(BJ)) =
     +TAUBY_new_si(INT(I),INT(J),INT(BI),INT(BJ))%d
                  DO m=Nr,1,-1
                   VISC_new_DUMMY(INT(I),INT(J),m,INT(BI),INT(BJ))=
     +visc_full_new_si(INT(I),INT(J),m,INT(BI),INT(BJ))%d
                  ENDDO
#endif


       
                  U_STREAMICE(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +U_streamice_dvals(I,J,BI,BJ)
                  V_STREAMICE(INT(I),INT(J),INT(BI),INT(BJ))%d = 
     +V_STREAMICE_dvals(I,J,BI,BJ)
#ifdef STREAMICE_HYBRID_STRESS
                  streamice_taubx(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +taubx_dvals(I,J,BI,BJ)
                  streamice_tauby(INT(I),INT(J),INT(BI),INT(BJ))%d = 
     +tauby_dvals(I,J,BI,BJ)
                  DO m=Nr,1,-1
              VISC_STREAMICE_FULL(INT(I),INT(J),m,INT(BI),INT(BJ))%d
     +=visc_full_dvals(I,J,m,BI,BJ)                  
                  ENDDO
#endif



#ifdef STREAMICE_ALLOW_FRIC_CONTROL
                  C_basal_dummy_d(INT(I),INT(J),INT(BI),INT(BJ)) =
     +C_basal_friction(INT(I),INT(J),INT(BI),INT(BJ))%d
#endif
#ifdef STREAMICE_ALLOW_BGLEN_CONTROL
                  b_glen_dummy_d(INT(I),INT(J),INT(BI),INT(BJ)) =
     +B_glen(INT(I),INT(J),INT(BI),INT(BJ))%d
#endif
#ifdef STREAMICE_ALLOW_DEPTH_CONTROL
                  R_low_si_dummy_d(INT(I),INT(J),INT(BI),INT(BJ)) =
     +R_low_si(INT(I),INT(J),INT(BI),INT(BJ))%d
#endif
                  H_streamice_dummy_d(INT(I),INT(J),INT(BI),INT(BJ)) =
     +H_streamice(INT(I),INT(J),INT(BI),INT(BJ))%d
                  taudx_dummy_d(INT(I),INT(J),INT(BI),INT(BJ)) =
     +taudx_si(INT(I),INT(J),INT(BI),INT(BJ))%d
                  taudy_dummy_d(INT(I),INT(J),INT(BI),INT(BJ)) =
     +taudy_si(INT(I),INT(J),INT(BI),INT(BJ))%d

                END DO
              END DO
            END DO
          END DO

          !Store the stack pointers
          temp_double_tape_pointer = oad_dt_ptr   
          temp_integer_tape_pointer = oad_it_ptr     
          temp_logical_tape_pointer = oad_lt_ptr  
          temp_string_tape_pointer = oad_st_ptr


#ifdef ALLOW_PETSC
          IF (STREAMICE_OAD_petsc_reuse) then
           if (adj_iter.eq.1) then
            STREAMICE_need2createmat=.true.
            STREAMICE_need2destroymat=.false.
            PETSC_PRECOND_TMP = PETSC_PRECOND_TYPE
            PETSC_PRECOND_TYPE = PETSC_PRECOND_OAD
           else
            STREAMICE_need2createmat=.false.
            STREAMICE_need2destroymat=.false.
           endif
          ENDIF
#endif

          CALL OpenAD_streamice_vel_phi(MYTHID,MAXNLITER,MAXCGITER,CGTOL
     +,CG_ITERS,err_max)

#ifdef ALLOW_PETSC
          IF (STREAMICE_OAD_petsc_reuse) then
           if (adj_iter.eq.MAXNLITER) then
            STREAMICE_need2createmat=.true.
            STREAMICE_need2destroymat=.true.
            CALL streamice_petscmatdestroy(mythid)             
            PETSC_PRECOND_TYPE = PETSC_PRECOND_TMP
           endif
          ENDIF
#endif

          DO BJ = MYBYLO(MYTHID), MYBYHI(MYTHID), 1
            DO BI = MYBXLO(MYTHID), MYBXHI(MYTHID), 1
              DO J = 1-OLy,sNy+OLy
                DO I = 1-OLx,sNx+OLx

                  U_NEW_SI(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +U_NEW_DUMMY_D(INT(I),INT(J),INT(BI),INT(BJ))
                  V_NEW_SI(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +V_NEW_DUMMY_D(INT(I),INT(J),INT(BI),INT(BJ))
#ifdef STREAMICE_HYBRID_STRESS
                  TAUBX_new_si(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +TAUBX_new_DUMMY(INT(I),INT(J),INT(BI),INT(BJ))
                  TAUBY_new_si(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +TAUBY_new_DUMMY(INT(I),INT(J),INT(BI),INT(BJ))
                  DO m=Nr,1,-1
                   visc_full_new_si(INT(I),INT(J),m,INT(BI),INT(BJ))%d=
     +VISC_new_DUMMY(INT(I),INT(J),m,INT(BI),INT(BJ))
                  ENDDO
#endif
       
                END DO
              END DO
            END DO
          END DO

!---- write out number of cg iters

          WRITE(MSGBUF,'(A,I5)') 'streamice adjoint solve number'
     +,ADJ_ITER
!          OAD_CTMP0 = 1
          CALL print_message(MSGBUF,STANDARDMESSAGEUNIT,'R',1)

!---- end write out number of cg iters

!---- conv check

          if (STREAMICE_chkfixedptconvergence) then

           CALL openad_STREAMICE_GET_FP_ERR_OAD ( err_max_fp, myThid )

           WRITE(msgBuf,'(A,1PE22.14)') 'STREAMICE_FP_ADJ_ERROR =',
     &                       err_max_fp
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

           IF(adj_iter.eq.1) then
            err_init = err_max_fp
           ELSEIF (err_max_fp .LE. 
     &             streamice_nonlin_tol_adjoint*err_init) THEN
            ADJ_CONVERGED = .true.
#ifdef ALLOW_PETSC
           IF (STREAMICE_OAD_petsc_reuse) THEN
            STREAMICE_need2createmat=.true.
            STREAMICE_need2destroymat=.true.
            CALL streamice_petscmatdestroy(mythid)             
            PETSC_PRECOND_TYPE = PETSC_PRECOND_TMP
           ENDIF
#endif
           ENDIF

          endif

!--------conv check done

          DO BJ = MYBYLO(MYTHID), MYBYHI(MYTHID), 1
            DO BI = MYBXLO(MYTHID), MYBXHI(MYTHID), 1
              DO J = 1-OLy,sNy+OLy
                DO I = 1-OLx,sNx+OLx

                  U_NEW_SI(INT(I),INT(J),INT(BI),INT(BJ))%d = U_STREAMICE(
     +I,J,BI,BJ)%d
                  V_NEW_SI(INT(I),INT(J),INT(BI),INT(BJ))%d = V_STREAMICE(
     +I,J,BI,BJ)%d
#ifdef STREAMICE_HYBRID_STRESS
                  TAUBX_NEW_SI(INT(I),INT(J),INT(BI),INT(BJ))%d = STREAMIC
     +E_TAUBX(I,J,BI,BJ)%d
                  TAUBY_NEW_SI(INT(I),INT(J),INT(BI),INT(BJ))%d = STREAMIC
     +E_TAUBY(I,J,BI,BJ)%d
                  DO m=Nr,1,-1
                  VISC_FULL_NEW_SI(INT(I),INT(J),m,INT(BI),INT(BJ))%d = 
     +VISC_STREAMICE_FULL(I,J,m,BI,BJ)%d
                  ENDDO
#endif

                  U_streamice(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +U_dummy_d(INT(I),INT(J),INT(BI),INT(BJ))
                  V_streamice(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +V_dummy_d(INT(I),INT(J),INT(BI),INT(BJ))
#ifdef STREAMICE_HYBRID_STRESS
                  streamice_taubx(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +taubx_dummy_d(INT(I),INT(J),INT(BI),INT(BJ))
                  streamice_tauby(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +tauby_dummy_d(INT(I),INT(J),INT(BI),INT(BJ))
                  DO m=Nr,1,-1
                  visc_streamice_full(I,J,m,BI,BJ)%d =
     +visc_full_dummy_d(I,J,m,BI,BJ)
                  ENDDO
#endif       

#ifdef STREAMICE_ALLOW_FRIC_CONTROL
                  C_basal_friction(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +C_basal_dummy_d(INT(I),INT(J),INT(BI),INT(BJ))
#endif
#ifdef STREAMICE_ALLOW_BGLEN_CONTROL
                  b_glen(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +B_glen_dummy_d(INT(I),INT(J),INT(BI),INT(BJ))
#endif
#ifdef STREAMICE_ALLOW_DEPTH_CONTROL
                  R_low_si(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +R_low_si_dummy_d(INT(I),INT(J),INT(BI),INT(BJ))
#endif
                  H_streamice(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +H_streamice_dummy_d(INT(I),INT(J),INT(BI),INT(BJ))
                  taudx_si(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +taudx_dummy_d(INT(I),INT(J),INT(BI),INT(BJ))
                  taudy_si(INT(I),INT(J),INT(BI),INT(BJ))%d =
     +taudy_dummy_d(INT(I),INT(J),INT(BI),INT(BJ))

                END DO
              END DO
            END DO
          END DO

          oad_dt_ptr = temp_double_tape_pointer   
          oad_it_ptr = temp_integer_tape_pointer     
          oad_lt_ptr = temp_logical_tape_pointer   
          oad_st_ptr = temp_string_tape_pointer

          if (adj_iter.eq.1) then
           CALL TIMER_STOP('STREAMICE ADJ FIXED POINT LOOP0',myThid)
          else
           CALL TIMER_STOP('STREAMICE ADJ FIXED POINT LOOP',myThid)
          endif
        end if
      end if

      if(isinloop.eq.0) then

          CALL OpenAD_streamice_vel_phi(MYTHID,MAXNLITER,MAXCGITER,CGTOL
     +,CG_ITERS,err_max)

          DO BJ = MYBYLO(MYTHID), MYBYHI(MYTHID), 1
            DO BI = MYBXLO(MYTHID), MYBXHI(MYTHID), 1
              DO J = 1-OLy,sNy+OLy
                DO I = 1-OLx,sNx+OLx

                  U_streamice(INT(I),INT(J),INT(BI),INT(BJ))%d = 0. _d 0
                  V_streamice(INT(I),INT(J),INT(BI),INT(BJ))%d = 0. _d 0
#ifdef STREAMICE_HYBRID_STRESS
              streamice_taubx(INT(I),INT(J),INT(BI),INT(BJ))%d = 0. _d 0
              streamice_tauby(INT(I),INT(J),INT(BI),INT(BJ))%d = 0. _d 0
                  DO m=Nr,1,-1
        visc_streamice_full(INT(I),INT(J),m,INT(BI),INT(BJ))%d = 0. _d 0
                  ENDDO
#endif

                END DO
              END DO
            END DO
          END DO

      end if


      our_rev_mode=our_orig_mode
      end if 

#endif

      end subroutine template

