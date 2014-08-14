function gaussian(current_model,x,extra_params) result(y)
	use h_params !PI
	use h_struct
	use h_vars
	implicit none
		
	type(model_struct), pointer :: current_model
	real,dimension(:),intent(in),optional :: extra_params
	real,intent(in) :: x
	real :: y

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

! Local Parameters

	integer :: numchan ! number of data points
	integer :: i
	real :: t,F0,dt
	real,dimension(size(current_model%model%param(1)%param_basis)) :: alpha,mu,sigma
	
! Begin 
	alpha=param_list(current_model%model%param(1)%param_basis)%val
	mu=param_list(current_model%model%param(2)%param_basis)%val
	sigma=param_list(current_model%model%param(3)%param_basis)%val

	y=sum( alpha/(exp((x - mu)**2/(2.*sigma**2))*Sqrt(2*PI)*sigma) )
	if (present(extra_params)) y=y-extra_params(1)
return
end function 

