function wacko_func(current_model,x_l,extra_params)
	use h_params !default%conf_lim ~ 68%
	use h_vars
!	use h_models ! gaussian
	use h_struct
	use h_utils ! gaussian, bisect2
	use h_rigorous, only : area_under_the_curve

! the only good way to get x_r out is to declare it as a global variable
	! h_vars,intent(out)
	!	x_r
	implicit none

	type(model_struct), pointer :: current_model
	real,dimension(:),intent(in),optional :: extra_params ! really not optional at all
	real,intent(in) :: x_l
	real :: wacko_func

! Local Var's
	real :: x_r1,x_r2,ys
	real :: area,total_area

! debugging
	real :: r
! Begin
	x_r1=extra_params(1)
	x_r2=extra_params(2)
	total_area=extra_params(3)
	ys=gaussian(current_model,x_l)
	! essentially we have to find the zero = x_r of the function on the right side
	x_r=bisect(gaussian,current_model,(/ys/),x_r1,x_r2) 
	area=area_under_the_curve(current_model,x_l,x_r,total_area)
	wacko_func=area-default%conf_lim
end function

