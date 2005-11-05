C ========== begin copyright notice ==============
C This file is part of 
C ---------------
C xaifBooster
C ---------------
C Distributed under the BSD license as follows:
C Copyright (c) 2005, The University of Chicago
C All rights reserved.
C
C Redistribution and use in source and binary forms, 
C with or without modification, are permitted provided that the following conditions are met:
C
C    - Redistributions of source code must retain the above copyright notice, 
C      this list of conditions and the following disclaimer.
C    - Redistributions in binary form must reproduce the above copyright notice, 
C      this list of conditions and the following disclaimer in the documentation 
C      and/or other materials provided with the distribution.
C    - Neither the name of The University of Chicago nor the names of its contributors 
C      may be used to endorse or promote products derived from this software without 
C      specific prior written permission.
C
C THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY 
C EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
C OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
C SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
C INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
C PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
C INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
C LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
C OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
C 
C General Information:
C xaifBooster is intended for the transformation of 
C numerical programs represented as xml files according 
C to the XAIF schema. It is part of the OpenAD framework. 
C The main application is automatic 
C differentiation, i.e. the generation of code for 
C the computation of derivatives. 
C The following people are the principal authors of the 
C current version: 
C 	Uwe Naumann
C	Jean Utke
C Additional contributors are: 
C	Andrew Lyons
C	Peter Fine
C
C For more details about xaifBooster and its use in OpenAD please visit:
C   http://www.mcs.anl.gov/openad
C
C This work is partially supported by:
C 	NSF-ITR grant OCE-0205590
C ========== end copyright notice ==============
C taping --------------------------------------------


        subroutine push(x)
C $OpenAD$ INLINE DECLS
          use OpenAD_tape
          implicit none
          double precision :: x
C $OpenAD$ END DECLS
          double_tape(double_tape_pointer)=x
          double_tape_pointer=double_tape_pointer+1
        end subroutine 


        subroutine pop(x)
C $OpenAD$ INLINE DECLS
          use OpenAD_tape
          implicit none
          double precision :: x
C $OpenAD$ END DECLS
          double_tape_pointer=double_tape_pointer-1
          x=double_tape(double_tape_pointer)
        end subroutine


        subroutine push_i(x)
C $OpenAD$ INLINE DECLS
          use OpenAD_tape
          implicit none
          integer :: x
C $OpenAD$ END DECLS
          integer_tape(integer_tape_pointer)=x
          integer_tape_pointer=integer_tape_pointer+1
        end subroutine 


        subroutine pop_i(x)
C $OpenAD$ INLINE DECLS
          use OpenAD_tape
          implicit none
          integer :: x
C $OpenAD$ END DECLS
          integer_tape_pointer=integer_tape_pointer-1
          x=integer_tape(integer_tape_pointer)
        end subroutine


        subroutine saxpy(a,x,y)
C $OpenAD$ INLINE DECLS
          double precision, intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
C $OpenAD$ END DECLS
          y%d=y%d+x%d*a
        end subroutine


        subroutine zeroderiv(x)
C $OpenAD$ INLINE DECLS
          type(active), intent(out) :: x
C $OpenAD$ END DECLS
          x%d=0.0d0
        end subroutine


        subroutine setderiv(y,x)
C $OpenAD$ INLINE DECLS
          type(active), intent(out) :: x
          type(active), intent(in) :: y
C $OpenAD$ END DECLS
          x%d=y%d
        end subroutine


        subroutine incderiv(y,x)
C $OpenAD$ INLINE DECLS
          type(active), intent(out) :: x
          type(active), intent(in) :: y
C $OpenAD$ END DECLS
          x%d=x%d+y%d
        end subroutine

        subroutine condinczeroderiv(y,x)
C $OpenAD$ INLINE DECLS
          type(active), intent(out) :: x
          type(active), intent(in) :: y
C $OpenAD$ END DECLS
          if (iaddr(y).ne.iaddr(x)) then
            x%d=x%d+y%d
            y%d=0
          end if
        end subroutine

C Checkpointing stuff ---------------------------------------

C active reals ----------------------------------------------
        subroutine cp_arg_store_real_scalar_a(x)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision :: x
C $OpenAD$ END DECLS
          call cp_store_real_scalar(x%v,theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
        end subroutine 


        subroutine cp_arg_restore_real_scalar_a(x)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision :: x
C $OpenAD$ END DECLS
          x%v=theArgFStack(theArgFStackoffset)
C          write(*,'(A,EN26.16E3)') "restore(s)  ", x%v
          theArgFStackoffset=theArgFStackoffset-1
        end subroutine 


        subroutine cp_res_store_real_scalar_a(x)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision :: x
C $OpenAD$ END DECLS
          call cp_store_real_scalar(x%v,theResFStack,theResFStackoffset,
     +theResFStackSize)
        end subroutine 


        subroutine cp_res_restore_real_scalar_a(x)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision :: x
C $OpenAD$ END DECLS
C          print*, "restore idx, value, x ", theResFStackoffset, x%v
          x%v=theResFStack(theResFStackoffset)
          theResFStackoffset=theResFStackoffset+1
        end subroutine 


        subroutine cp_arg_store_real_vector_a(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          call cp_store_real_vector(x,size(x),
     +theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
        end subroutine 


        subroutine cp_arg_restore_real_vector_a(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
             x(cp_loop_variable_1)%v=theArgFStack(theArgFStackoffset)
             theArgFStackoffset=theArgFStackoffset-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +x(cp_loop_variable_1)%v
          end do
        end subroutine 


        subroutine cp_res_store_real_vector_a(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          call cp_store_real_vector(x,size(x),
     +theResFStack,theResFStackoffset,
     +theResFStackSize)
        end subroutine 


        subroutine cp_res_restore_real_vector_a(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=lbound(x,1),ubound(x,1),1
             x(cp_loop_variable_1)%v=theResFStack(theResFStackoffset)
             theResFStackoffset=theResFStackoffset+1
          end do
        end subroutine 


        subroutine cp_arg_store_real_matrix_a(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=lbound(x,1),ubound(x,1)
          call cp_store_real_vector(x(cp_loop_variable_1),
     +size(x(cp_loop_variable_1)),theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
          end do
        end subroutine 


        subroutine cp_arg_restore_real_matrix_a(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
             do cp_loop_variable_2=ubound(x,2),lbound(x,2),-1
                x(cp_loop_variable_1,cp_loop_variable_2)%v=
     +theArgFStack(theArgFStackoffset)
                theArgFStackoffset=theArgFStackoffset-1
             end do
          end do
        end subroutine 


        subroutine cp_res_store_real_matrix_a(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=lbound(x,1),ubound(x,1)
          call cp_store_real_vector(x(cp_loop_variable_1),
     +size(x(cp_loop_variable_1)),theResFStack,theResFStackoffset,
     +theResFStackSize)
          end do
        end subroutine 


        subroutine cp_res_restore_real_matrix_a(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=lbound(x,1),ubound(x,2),1
             do cp_loop_variable_2=lbound(x,2),ubound(x,2),1
                x(cp_loop_variable_1,cp_loop_variable_2)%v=
     +theResFStack(theResFStackoffset)
                theResFStackoffset=theResFStackoffset+1
             end do
          end do
        end subroutine 


        subroutine cp_arg_store_real_four_tensor_a(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=lbound(x,1),ubound(x,1)
             do cp_loop_variable_2=lbound(x,2),ubound(x,2)
                do cp_loop_variable_3=lbound(x,3),ubound(x,3)
          call cp_store_real_vector(x(cp_loop_variable_1,
     +cp_loop_variable_2, cp_loop_variable_3),
     +size(x(cp_loop_variable_1,cp_loop_variable_2, 
     +cp_loop_variable_3)),
     +theArgFStack,
     +theArgFStackoffset,
     +theArgFStackSize)
               end do
             end do
          end do
        end subroutine 


        subroutine cp_arg_restore_real_four_tensor_a(x,
     +cp_loop_variable_1,cp_loop_variable_2,cp_loop_variable_3,
     +cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
             do cp_loop_variable_2=ubound(x,2),lbound(x,2),-1
                do cp_loop_variable_3=ubound(x,3),lbound(x,3),-1
                   do cp_loop_variable_4=ubound(x,4),lbound(x,4),-1
                      x(cp_loop_variable_1,cp_loop_variable_2,
     +cp_loop_variable_3,cp_loop_variable_4)%v=
     +theArgFStack(theArgFStackoffset)
                      theArgFStackoffset=theArgFStackoffset-1
                   end do
                end do
             end do
          end do
          end subroutine 


        subroutine cp_res_store_real_four_tensor_a(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=lbound(x,1),ubound(x,1)
             do cp_loop_variable_2=lbound(x,2),ubound(x,2)
                do cp_loop_variable_3=lbound(x,3),ubound(x,3)
          call cp_store_real_vector(x(cp_loop_variable_1,
     +cp_loop_variable_2, cp_loop_variable_3),
     +size(x(cp_loop_variable_1,
     +cp_loop_variable_2, 
     +cp_loop_variable_3)),
     +theResFStack,
     +theResFStackoffset,
     +theResFStackSize)
                end do
             end do
          end do
        end subroutine 


        subroutine cp_res_restore_real_four_tensor_a(x,
     +cp_loop_variable_1, +cp_loop_variable_2,cp_loop_variable_3,
     +cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=lbound(x,1),ubound(x,1)
             do cp_loop_variable_2=lbound(x,2),ubound(x,2)
                do cp_loop_variable_3=lbound(x,3),ubound(x,3)
                   do cp_loop_variable_4=lbound(x,4),ubound(x,4)
                      x(cp_loop_variable_1,cp_loop_variable_2,
     +cp_loop_variable_3,cp_loop_variable_4)%v=
     +theResFStack(theResFStackoffset)
                      theResFStackoffset=theResFStackoffset+1
                   end do
                end do
             end do
          end do
        end subroutine 

C passive reals ----------------------------------------------
        subroutine cp_arg_store_real_scalar(x)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision :: x
C $OpenAD$ END DECLS
          call cp_store_real_scalar(x,theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
        end subroutine 


        subroutine cp_arg_restore_real_scalar(x)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision :: x
C $OpenAD$ END DECLS
          x=theArgFStack(theArgFStackoffset)
C          write(*,'(A,EN26.16E3)') "restore(s)  ", x
          theArgFStackoffset=theArgFStackoffset-1
        end subroutine 


        subroutine cp_res_store_real_scalar(x)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision :: x
C $OpenAD$ END DECLS
          call cp_store_real_scalar(x,theResFStack,theResFStackoffset,
     +theResFStackSize)
        end subroutine 


        subroutine cp_res_restore_real_scalar(x)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision :: x
C $OpenAD$ END DECLS
C          print*, "restore idx, value, x ", theResFStackoffset, x
          x=theResFStack(theResFStackoffset)
          theResFStackoffset=theResFStackoffset+1
        end subroutine 


        subroutine cp_arg_store_real_vector(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          call cp_store_p_real_vector(x,size(x),
     +theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
        end subroutine 


        subroutine cp_arg_restore_real_vector(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
             x(cp_loop_variable_1)=theArgFStack(theArgFStackoffset)
             theArgFStackoffset=theArgFStackoffset-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +x(cp_loop_variable_1)
          end do
        end subroutine 


        subroutine cp_res_store_real_vector(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          call cp_store_p_real_vector(x,size(x),
     +theResFStack,theResFStackoffset,
     +theResFStackSize)
        end subroutine 


        subroutine cp_res_restore_real_vector(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_1=lbound(x,1),ubound(x,1),1
             x(cp_loop_variable_1)=theResFStack(theResFStackoffset)
             theResFStackoffset=theResFStackoffset+1
          end do
        end subroutine 


        subroutine cp_arg_store_real_matrix(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_2=lbound(x,2),ubound(x,2)
          call cp_store_p_real_vector(x(:,cp_loop_variable_2),
     +size(x(:,cp_loop_variable_2)),theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
          end do
        end subroutine 


        subroutine cp_arg_restore_real_matrix(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_2=ubound(x,2),lbound(x,2),-1
             do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
                x(cp_loop_variable_1,cp_loop_variable_2)=
     +theArgFStack(theArgFStackoffset)
                theArgFStackoffset=theArgFStackoffset-1
             end do
          end do
        end subroutine 


        subroutine cp_res_store_real_matrix(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_2=lbound(x,2),ubound(x,2)
          call cp_store_p_real_vector(x(:,cp_loop_variable_2),
     +size(x(:,cp_loop_variable_2)),theResFStack,theResFStackoffset,
     +theResFStackSize)
          end do
        end subroutine 


        subroutine cp_res_restore_real_matrix(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_2=lbound(x,2),ubound(x,2),1
             do cp_loop_variable_1=lbound(x,1),ubound(x,1),1
                x(cp_loop_variable_1,cp_loop_variable_2)=
     +theResFStack(theResFStackoffset)
                theResFStackoffset=theResFStackoffset+1
             end do
          end do
        end subroutine 


        subroutine cp_arg_store_real_four_tensor(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_4=lbound(x,4),ubound(x,4)
             do cp_loop_variable_3=lbound(x,3),ubound(x,3)
                do cp_loop_variable_2=lbound(x,2),ubound(x,2)
          call cp_store_p_real_vector(x(:,cp_loop_variable_2,
     +cp_loop_variable_3, cp_loop_variable_4),
     +size(x(:,cp_loop_variable_2,cp_loop_variable_3, 
     +cp_loop_variable_4)),
     +theArgFStack,
     +theArgFStackoffset,
     +theArgFStackSize)
               end do
             end do
          end do
        end subroutine 


        subroutine cp_arg_restore_real_four_tensor(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_4=ubound(x,4),lbound(x,4),-1
             do cp_loop_variable_3=ubound(x,3),lbound(x,3),-1
                do cp_loop_variable_2=ubound(x,2),lbound(x,2),-1
                   do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
                      x(cp_loop_variable_1,cp_loop_variable_2,
     +cp_loop_variable_3,cp_loop_variable_4)=
     +theArgFStack(theArgFStackoffset)
                      theArgFStackoffset=theArgFStackoffset-1
                   end do
                end do
             end do
          end do
          end subroutine 


        subroutine cp_res_store_real_four_tensor(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(::) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_4=lbound(x,4),ubound(x,4)
             do cp_loop_variable_3=lbound(x,3),ubound(x,3)
                do cp_loop_variable_2=lbound(x,2),ubound(x,2)
          call cp_store_p_real_vector(x(:,cp_loop_variable_2,
     +cp_loop_variable_3, cp_loop_variable_4),
     +size(x(:,cp_loop_variable_2,cp_loop_variable_3, 
     +cp_loop_variable_4)),
     +theResFStack,
     +theResFStackoffset,
     +theResFStackSize)
                end do
             end do
          end do
        end subroutine 


        subroutine cp_res_restore_real_four_tensor(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
          implicit none
          double precision, dimension(:) :: x
C $OpenAD$ END DECLS
          do cp_loop_variable_4=lbound(x,4),ubound(x,4)
             do cp_loop_variable_3=lbound(x,3),ubound(x,3)
                do cp_loop_variable_2=lbound(x,2),ubound(x,2)
                   do cp_loop_variable_1=lbound(x,1),ubound(x,1)
                      x(cp_loop_variable_1,cp_loop_variable_2,
     +cp_loop_variable_3,cp_loop_variable_4)=
     +theResFStack(theResFStackoffset)
                      theResFStackoffset=theResFStackoffset+1
                   end do
                end do
             end do
          end do
        end subroutine 


C integers -----------------------------------------------------
        subroutine cp_arg_store_integer_scalar(i)
C $OpenAD$ INLINE DECLS
          implicit none
          integer :: i
C $OpenAD$ END DECLS
          call cp_store_int_scalar(i,theArgIStack,
     +theArgIStackoffset, theArgIStackSize)
        end subroutine 


        subroutine cp_arg_restore_integer_scalar(i)
C $OpenAD$ INLINE DECLS
          implicit none
          integer :: i
C $OpenAD$ END DECLS
          i=theArgIStack(theArgIStackoffset)
          theArgIStackoffset=theArgIStackoffset-1
C          write(*,'(A,I5)') "restore(s)  ", i
        end subroutine 


        subroutine cp_res_store_integer_scalar(i)
C $OpenAD$ INLINE DECLS
          implicit none
          integer :: i
C $OpenAD$ END DECLS
          call cp_store_int_scalar(i,theResIStack,
     +theResIStackoffset, theResIStackSize)
        end subroutine 


        subroutine cp_res_restore_integer_scalar(i)
C $OpenAD$ INLINE DECLS
          implicit none
          integer :: i
C $OpenAD$ END DECLS
          i=theResIStack(theResIStackoffset)
          theResIStackoffset=theResIStackoffset+1
        end subroutine 


C strings  -----------------------------------------------------
        subroutine cp_arg_store_string_scalar(s)
C $OpenAD$ INLINE DECLS
          implicit none
          character*(80) :: s
C $OpenAD$ END DECLS 
          call cp_store_string_scalar(s,theArgSStack,
     +theArgSStackoffset, theArgSStackSize)
        end subroutine 


        subroutine cp_arg_restore_string_scalar(s)
C $OpenAD$ INLINE DECLS
          implicit none
          character*(80) :: s
C $OpenAD$ END DECLS
          s=theArgSStack(theArgSStackoffset)
          theArgSStackoffset=theArgSStackoffset-1
        end subroutine 


        subroutine cp_res_store_string_scalar(s)
C $OpenAD$ INLINE DECLS
          implicit none
          character*(80) :: s
C $OpenAD$ END DECLS
          call cp_store_string_scalar(s,theResSStack,
     +theResSStackoffset, theResSStackSize)
        end subroutine 


        subroutine cp_res_restore_string_scalar(s)
C $OpenAD$ INLINE DECLS
          implicit none
          character*(80) :: s
C $OpenAD$ END DECLS
          s=theResSStack(theResSStackoffset)
          theResSStackoffset=theResSStackoffset+1
        end subroutine 


C bools  -----------------------------------------------------
        subroutine cp_arg_store_bool_scalar(b)
C $OpenAD$ INLINE DECLS
          implicit none
          logical :: b
C $OpenAD$ END DECLS
          call cp_store_bool_scalar(b,theArgBStack,
     +theArgBStackoffset, theArgBStackSize)
        end subroutine 


        subroutine cp_arg_restore_bool_scalar(b)
C $OpenAD$ INLINE DECLS
          implicit none
          logical :: b
C $OpenAD$ END DECLS
          b=theArgBStack(theArgBStackoffset)
          theArgBStackoffset=theArgBStackoffset-1
        end subroutine 


        subroutine cp_res_store_bool_scalar(b)
C $OpenAD$ INLINE DECLS
          implicit none
          logical :: b
C $OpenAD$ END DECLS
          call cp_store_bool_scalar(b,theResBStack,
     +theResBStackoffset, theResBStackSize)
        end subroutine 


        subroutine cp_res_restore_bool_scalar(b)
C $OpenAD$ INLINE DECLS
          implicit none
          logical :: b
C $OpenAD$ END DECLS
          b=theResBStack(theResBStackoffset)
          theResBStackoffset=theResBStackoffset+1
        end subroutine 
