C taping --------------------------------------------


      subroutine push(x)
C $OpenAD$ INLINE DECLS
      double precision :: x
C $OpenAD$ END DECLS
      if(double_tape_size .lt. double_tape_pointer) then 
         allocate(double_tmp_tape(double_tape_size),
     +STAT=cp_loop_variable_1)
         if (cp_loop_variable_1 .gt. 0 ) then 
            print *,'allocation failed with', cp_loop_variable_1
            stop
         end if
         double_tmp_tape=double_tape
         deallocate(double_tape)
         allocate(double_tape(double_tape_size*2))
         print *, "DT+"
         double_tape(1:double_tape_size) = double_tmp_tape
         deallocate(double_tmp_tape)
         double_tape_size=double_tape_size*2
      end if
      double_tape(double_tape_pointer)=x
      double_tape_pointer=double_tape_pointer+1
      end subroutine 


      subroutine pop(x)
C $OpenAD$ INLINE DECLS
      double precision :: x
C $OpenAD$ END DECLS
      double_tape_pointer=double_tape_pointer-1
      x=double_tape(double_tape_pointer)
      end subroutine


      subroutine push_i(x)
C $OpenAD$ INLINE DECLS
      integer :: x
C $OpenAD$ END DECLS
      if(integer_tape_size .lt. integer_tape_pointer) then 
         allocate(integer_tmp_tape(integer_tape_size))
         integer_tmp_tape=integer_tape
         deallocate(integer_tape)
         allocate(integer_tape(integer_tape_size*2))
         print *, "IT+"
         integer_tape(1:integer_tape_size) = integer_tmp_tape
         deallocate(integer_tmp_tape)
         integer_tape_size=integer_tape_size*2
      end if
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

C reals -----------------------------------------------------
      subroutine cp_arg_store_real_scalar(x)
C $OpenAD$ INLINE DECLS
      double precision :: x
C $OpenAD$ END DECLS
      if(cp_double_size .lt. cp_double_pointer+1) then 
         allocate(cp_double_tmp(cp_double_size))
         cp_double_tmp=cp_double
         deallocate(cp_double)
         allocate(cp_double(cp_double_size*2))
         print *,'CPD+'
         cp_double(1:cp_double_size) = cp_double_tmp
         deallocate(cp_double_tmp)
         cp_double_size=cp_double_size*2
      end if
      cp_double_pointer=cp_double_pointer+1
      cp_double(cp_double_pointer)=x
      end subroutine 

      subroutine cp_arg_restore_real_scalar(x)
C $OpenAD$ INLINE DECLS
      implicit none
      double precision :: x
C $OpenAD$ END DECLS
      x=cp_double(cp_double_pointer)
      cp_double_pointer=cp_double_pointer-1
      end subroutine 

      subroutine cp_arg_store_real_scalar_a(x)
C $OpenAD$ INLINE DECLS
      double precision :: x
C $OpenAD$ END DECLS
      if(cp_double_size .lt. cp_double_pointer+1) then 
         allocate(cp_double_tmp(cp_double_size))
         cp_double_tmp=cp_double
         deallocate(cp_double)
         allocate(cp_double(cp_double_size*2))
         print *,'CPD+'
         cp_double(1:cp_double_size) = cp_double_tmp
         deallocate(cp_double_tmp)
         cp_double_size=cp_double_size*2
      end if
      cp_double_pointer=cp_double_pointer+1
      cp_double(cp_double_pointer)=x%v
      end subroutine 

      subroutine cp_arg_restore_real_scalar_a(x)
C $OpenAD$ INLINE DECLS
      implicit none
      double precision :: x
C $OpenAD$ END DECLS
      x%v=cp_double(cp_double_pointer)
      cp_double_pointer=cp_double_pointer-1
      end subroutine 
      
      subroutine cp_arg_store_real_vector(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
      implicit none
      double precision, dimension(:) :: x
C $OpenAD$ END DECLS
      do while (cp_double_size .lt. 
     +cp_double_pointer+ubound(x,1)-lbound(x,1))  
         allocate(cp_double_tmp(cp_double_size))
         cp_double_tmp=cp_double
         deallocate(cp_double)
         allocate(cp_double(cp_double_size*2))
         print *,'CPD+'
         cp_double(1:cp_double_size) = cp_double_tmp
         deallocate(cp_double_tmp)
         cp_double_size=cp_double_size*2
      end do
      do cp_loop_variable_1=lbound(x,1), ubound(x,1), 1 
         cp_double_pointer=cp_double_pointer+1
         cp_double(cp_double_pointer)=
     +x(cp_loop_variable_1)
      end do
      end subroutine 

      subroutine cp_arg_restore_real_vector(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
      implicit none
      double precision, dimension(:) :: x
C $OpenAD$ END DECLS
      do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
         x(cp_loop_variable_1)=cp_double(cp_double_pointer)
         cp_double_pointer=cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +x(cp_loop_variable_1)%v
      end do
      end subroutine 

      subroutine cp_arg_store_real_vector_a(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
      implicit none
      double precision, dimension(:) :: x
C $OpenAD$ END DECLS
      do while (cp_double_size .lt. 
     +cp_double_pointer+ubound(x,1)-lbound(x,1))  
         allocate(cp_double_tmp(cp_double_size))
         cp_double_tmp=cp_double
         deallocate(cp_double)
         allocate(cp_double(cp_double_size*2))
         print *,'CPD+'
         cp_double(1:cp_double_size) = cp_double_tmp
         deallocate(cp_double_tmp)
         cp_double_size=cp_double_size*2
      end do
      do cp_loop_variable_1=lbound(x,1), ubound(x,1), 1 
         cp_double_pointer=cp_double_pointer+1
         cp_double(cp_double_pointer)=
     +x(cp_loop_variable_1)%v
      end do
      end subroutine 

      subroutine cp_arg_restore_real_vector_a(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
      implicit none
      double precision, dimension(:) :: x
C $OpenAD$ END DECLS
      do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
         x(cp_loop_variable_1)%v=cp_double(cp_double_pointer)
         cp_double_pointer=cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +x(cp_loop_variable_1)%v
      end do
      end subroutine 

      subroutine cp_arg_store_real_matrix(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
      implicit none
      double precision, dimension(::) :: x
C $OpenAD$ END DECLS
      do cp_loop_variable_1=lbound(x,1),ubound(x,1)
         do while (cp_double_size .lt. 
     +cp_double_pointer+ubound(x,2)-lbound(x,2))  
            allocate(cp_double_tmp(cp_double_size))
            cp_double_tmp=cp_double
            deallocate(cp_double)
            allocate(cp_double(cp_double_size*2))
            print *,'CPD+'
            cp_double(1:cp_double_size) = cp_double_tmp
            deallocate(cp_double_tmp)
            cp_double_size=cp_double_size*2
         end do
         do cp_loop_variable_2=lbound(x,2), ubound(x,2), 1 
            cp_double_pointer=cp_double_pointer+1
            cp_double(cp_double_pointer)=
     +x(cp_loop_variable_1,cp_loop_variable_2)
         end do
      end do
      end subroutine 

      subroutine cp_arg_restore_real_matrix(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
      implicit none
      double precision, dimension(::) :: x
C $OpenAD$ END DECLS
      do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
         do cp_loop_variable_2=ubound(x,2),lbound(x,2),-1
            x(cp_loop_variable_1,cp_loop_variable_2)=
     +cp_double(cp_double_pointer)
            cp_double_pointer=cp_double_pointer-1
         end do
      end do
      end subroutine 

      subroutine cp_arg_store_real_matrix_a(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
      implicit none
      double precision, dimension(::) :: x
C $OpenAD$ END DECLS
      do cp_loop_variable_1=lbound(x,1),ubound(x,1)
         do while (cp_double_size .lt. 
     +cp_double_pointer+ubound(x,2)-lbound(x,2))  
            allocate(cp_double_tmp(cp_double_size))
            cp_double_tmp=cp_double
            deallocate(cp_double)
            allocate(cp_double(cp_double_size*2))
            print *,'CPD+'
            cp_double(1:cp_double_size) = cp_double_tmp
            deallocate(cp_double_tmp)
            cp_double_size=cp_double_size*2
         end do
         do cp_loop_variable_2=lbound(x,2), ubound(x,2), 1 
            cp_double_pointer=cp_double_pointer+1
            cp_double(cp_double_pointer)=
     +x(cp_loop_variable_1,cp_loop_variable_2)%v
         end do
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
     +cp_double(cp_double_pointer)
            cp_double_pointer=cp_double_pointer-1
         end do
      end do
      end subroutine 

C integers -----------------------------------------------------
      subroutine cp_arg_store_integer_scalar(i)
C $OpenAD$ INLINE DECLS
      implicit none
      integer :: i
C $OpenAD$ END DECLS
      if(cp_integer_size .lt. cp_integer_pointer+1) then 
         allocate(cp_integer_tmp(cp_integer_size))
         cp_integer_tmp=cp_integer
         deallocate(cp_integer)
         allocate(cp_integer(cp_integer_size*2))
         print *,'CPI+'
         cp_integer(1:cp_integer_size) = cp_integer_tmp
         deallocate(cp_integer_tmp)
         cp_integer_size=cp_integer_size*2
      end if
      cp_integer_pointer=cp_integer_pointer+1
      cp_integer(cp_integer_pointer)=i
      end subroutine 

      subroutine cp_arg_restore_integer_scalar(i)
C $OpenAD$ INLINE DECLS
      implicit none
      integer :: i
C $OpenAD$ END DECLS
      i=cp_integer(cp_integer_pointer)
      cp_integer_pointer=cp_integer_pointer-1
      end subroutine 

C strings  -----------------------------------------------------
      subroutine cp_arg_store_string_scalar(s)
C $OpenAD$ INLINE DECLS
      implicit none
      character*(80) :: s
C $OpenAD$ END DECLS 
      if(cp_string_size .lt. cp_string_pointer+1) then 
         allocate(cp_string_tmp(cp_string_size))
         cp_string_tmp=cp_string
         deallocate(cp_string)
         allocate(cp_string(cp_string_size*2))
         print *,'CPS+'
         cp_string(1:cp_string_size) = cp_string_tmp
         deallocate(cp_string_tmp)
         cp_string_size=cp_string_size*2
      end if
      cp_string_pointer=cp_string_pointer+1
      cp_string(cp_string_pointer)=s
      end subroutine 
      
      subroutine cp_arg_restore_string_scalar(s)
C $OpenAD$ INLINE DECLS
      implicit none
      character*(80) :: s
C $OpenAD$ END DECLS
      s=cp_string(cp_string_pointer)
      cp_string_pointer=cp_string_pointer-1
      end subroutine 

C bools  -----------------------------------------------------
      subroutine cp_arg_store_bool_scalar(b)
C $OpenAD$ INLINE DECLS
      implicit none
      logical :: b
C $OpenAD$ END DECLS
      if(cp_boolean_size .lt. cp_boolean_pointer+1) then 
         allocate(cp_boolean_tmp(cp_boolean_size))
         cp_boolean_tmp=cp_boolean
         deallocate(cp_boolean)
         allocate(cp_boolean(cp_boolean_size*2))
         print *,'CPB+'
         cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
         deallocate(cp_boolean_tmp)
         cp_boolean_size=cp_boolean_size*2
      end if
      cp_boolean_pointer=cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer)=b
      end subroutine 

      subroutine cp_arg_restore_bool_scalar(b)
C $OpenAD$ INLINE DECLS
      implicit none
      logical :: b
C $OpenAD$ END DECLS
      b=cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer=cp_boolean_pointer-1
      end subroutine 
