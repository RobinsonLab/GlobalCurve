function simple_convolute(f_1,f_2) result(f_out)
implicit none

real,dimension(:),intent(in) :: f_1 ! impulse response
real,dimension(size(f_1)),intent(in) :: f_2
real,dimension(size(f_1)) :: f_out

! Local Vars

integer :: i,range
	
! Begin

	range=size(f_1)
	f_out(:)=0.
	forall(i=1:range)
		f_out(i)=dot_product(f_1(1:i:1),f_2(i:1:-1))
    end forall !i
	return
end function simple_convolute