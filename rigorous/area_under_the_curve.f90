function area_under_the_curve(current_model,xl,xr,total_area) result(area)
	use h_struct
	use numerical_libraries, only : anordf
	implicit none

	type(model_struct), pointer :: current_model
	real,intent(in) :: xl,xr,total_area
	real :: area


! Local Var's
	real,dimension(size(current_model%model%param(1)%param_basis)) :: z,cdf
	real :: left_area,right_area
	integer :: i,num_gaussians

! Begin
	num_gaussians=size(current_model%model%param(1)%param_basis)
	! now calculate the integral using these bounds.
	! the easiest way I can think of is to use the CDF of a gaussian.
	! left side first
	z(:)=( xl - param_list(current_model%model%param(2)%param_basis(:))%val ) / &
		param_list(current_model%model%param(3)%param_basis(:))%val
	do i=1,num_gaussians
		cdf(i)=anordf(z(i)) !IMSL routine that calculates the cdf of a gaussian
	enddo !i
	left_area=dot_product(param_list(1:3)%val,cdf)
	! right side
	z(:)=-( xr - param_list(current_model%model%param(1)%param_basis(:))%val ) / &
		param_list(current_model%model%param(3)%param_basis(:))%val
	! note that the z statistic is symmetric so we can just add a - out front.
	do i=1,num_gaussians
		cdf(i)=anordf(z(i))
	enddo !i
	right_area=dot_product(param_list(current_model%model%param(1)%param_basis)%val,cdf)
	area=1.-(left_area+right_area)/total_area
		! now we're back to finding a zero of a function again.
	return
end function area_under_the_curve