      subroutine template()
      use OAD_cp
      use OAD_tape
      use OAD_rev

!$TEMPLATE_PRAGMA_DECLARATIONS

      integer :: cp_loop_variable_1,cp_loop_variable_2,
     +     cp_loop_variable_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      Real*8 cg_Uin_p (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 cg_Vin_p (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 cg_Bu_p  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 cg_Bv_p  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 A_uu_p (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      Real*8 A_vu_p (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      Real*8 A_uv_p (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      Real*8 A_vv_p (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      Real*8 cg_Uin_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 cg_Vin_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 cg_Bu_d  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 cg_Bv_d  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 A_uu_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      Real*8 A_vu_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      Real*8 A_uv_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      Real*8 A_vv_d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)

#ifdef OAD_DEBUG_JOINT
      character*(80):: indentation='                                        
     +                                         '
      our_indent=our_indent+1

      write(standardmessageunit, '(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'enter __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 
#endif
      if (our_rev_mode%plain) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering plain'
#endif
c set up for plain execution
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: runninng plain / down plain'
#endif
         cg_Uin_p = cg_Uin%v
         cg_Vin_p = cg_Vin%v
         cg_Bu_p  = cg_Bu%v
         cg_Bv_p  = cg_Bv%v
         A_uu_p  = A_uu%v
         A_uv_p  = A_uv%v
         A_vu_p  = A_vu%v
         A_vv_p  = A_vv%v
         call STREAMICE_CG_SOLVE( 
     U        cg_Uin_p,     
     U        cg_Vin_p,     
     I        cg_Bu_p,      
     I        cg_Bv_p,      
     I        A_uu_p,       
     I        A_uv_p,       
     I        A_vu_p,       
     I        A_vv_p,       
     I        tolerance, 
     O        iters,
     I        maxIter,
     I        myThid )
         cg_Uin%v = cg_Uin_p
         cg_Vin%v = cg_Vin_p
c reset the mode
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then

#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering tape'
#endif
c set up for plain execution
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: runninng plain / down plain'
#endif
         cg_Uin_p = cg_Uin%v
         cg_Vin_p = cg_Vin%v
         cg_Bu_p  = cg_Bu%v
         cg_Bv_p  = cg_Bv%v
         A_uu_p  = A_uu%v
         A_uv_p  = A_uv%v
         A_vu_p  = A_vu%v
         A_vv_p  = A_vv%v
         call oad_tape_push(cg_Bu_p)      
         call oad_tape_push(cg_Bv_p)      
         call oad_tape_push(A_uu_p)       
         call oad_tape_push(A_uv_p)       
         call oad_tape_push(A_vu_p)       
         call oad_tape_push(A_vv_p)       
         call oad_tape_push(tolerance)
         call oad_tape_push(mythid)
         call STREAMICE_CG_SOLVE( 
     U        cg_Uin_p,     
     U        cg_Vin_p,     
     I        cg_Bu_p,      
     I        cg_Bv_p,      
     I        A_uu_p,       
     I        A_uv_p,       
     I        A_vu_p,       
     I        A_vv_p,       
     I        tolerance, 
     O        iters,
     I        maxIter,
     I        myThid )
         call oad_tape_push(cg_Uin_p)     
         call oad_tape_push(cg_Vin_p)     
         cg_Uin%v = cg_Uin_p
         cg_Vin%v = cg_Vin_p
c reset the mode
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%adjoint) then
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: entering adjoint'
#endif
         call oad_tape_pop(cg_Vin_p)     
         call oad_tape_pop(cg_Uin_p)     
         call oad_tape_pop(mythid)
         call oad_tape_pop(tolerance)
         call oad_tape_pop(A_vv_p)       
         call oad_tape_pop(A_vu_p)       
         call oad_tape_pop(A_uv_p)       
         call oad_tape_pop(A_uu_p)       
         call oad_tape_pop(cg_Bv_p)      
         call oad_tape_pop(cg_Bu_p)   
c set up for plain execution
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
#ifdef OAD_DEBUG_JOINT
         write(standardmessageunit,'(A,A,A)') 
     +'OAD:',indentation(1:our_indent), 
     +' __SRNAME__: runninng self adjoint / down plain'
#endif
         cg_Uin_d = cg_Uin%d
         cg_Vin_d = cg_Vin%d
         cg_Bu_d  = cg_Bu%d
         cg_Bv_d  = cg_Bv%d
         A_uu_d  = A_uu%d
         A_uv_d  = A_uv%d
         A_vu_d  = A_vu%d
         A_vv_d  = A_vv%d
      call ADSTREAMICE_CG_SOLVE(
     U                               cg_Uin_p,   ! velocities - solution 
     I                               cg_Uin_d,      ! adjoint of vel (input)
     U                               cg_Vin_p,   ! velocities - solution
     I                               cg_Vin_d,      ! adjoint of vel (input)
     I                               cg_Bu_p,    ! to recalc velocities
     U                               cg_Bu_d,     ! adjoint of RHS (output)
     I                               cg_Bv_p,    ! to recalc velocities
     U                               cg_Bv_d,     ! adjoint of RHS (output)
     I                               A_uu_p,     ! section of matrix that multiplies u and projects on u
     U                               A_uu_d,     ! adjoint of matrix coeffs (output)
     I                               A_uv_p,     ! section of matrix that multiplies v and projects on u
     U                               A_uv_d,     ! adjoint of matrix coeffs (output)
     I                               A_vu_p,     ! section of matrix that multiplies u and projects on v
     U                               A_vu_d,     ! adjoint of matrix coeffs (output)
     I                               A_vv_p,     ! section of matrix that multiplies v and projects on v
     U                               A_vv_d,     ! adjoint of matrix coeffs (output)
     I                               tolerance,
     I                               maxIter,
     I                               myThid )
c reset the mode
         cg_Uin%d = cg_Uin_d
         cg_Vin%d = cg_Vin_d
         cg_Bu%d  = cg_Bu_d
         cg_Bv%d  = cg_Bv_d
         A_uu%d  = A_uu_d
         A_uv%d  = A_uv_d
         A_vu%d  = A_vu_d
         A_vv%d  = A_vv_d
         our_rev_mode=our_orig_mode
      end if 
#ifdef OAD_DEBUG_JOINT
      write(standardmessageunit,'(A,A,A)', ADVANCE='NO') 
     +'OAD:',indentation(1:our_indent), 'leave __SRNAME__:'
      call oad_dump_revmod(); call oad_dump_tapestats()
      write(standardmessageunit,*) 

      our_indent=our_indent-1
#endif

      end subroutine template
