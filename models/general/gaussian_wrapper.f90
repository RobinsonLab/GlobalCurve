function gaussian_wrapper(current_model,current_data) result(y)
	use h_struct
	use h_vars
	use h_utils
	implicit none
		
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: y



! Local Parameters

	integer :: num_points
	integer :: i
	
! Begin 

	num_points=size(current_model%model%param(1)%param_basis)

	do i=1,num_points
		y(i)=gaussian(current_model,current_data%x_pts(i))
	enddo !i
return
end function

