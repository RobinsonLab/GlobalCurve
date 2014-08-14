function four_step_A_M_norm_force(current_model,current_data) result(force)
	use h_struct
	use h_models
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force

! constants
	integer,parameter :: n=4

! Local Var's
	real,dimension(current_data%len,n) :: A_M
	real :: min_force,max_force,range_from,range_to,min,max
! begin
	min=param_list(current_model%model%param(2)%param_basis(1))%val
	max=param_list(current_model%model%param(2)%param_basis(2))%val
	range_to=max-min
	A_M=calc_four_step_A_M(current_model,current_data,n)
	force(:)=A_M(:,3)+A_M(:,4)
!	min_force=minval(force)
!	min force better be zero or else something is very wrong
!	by forcing it to be zero, we will see wacky results if they
!	are there.
	min_force=0.0
	max_force=maxval(force)
	range_from=max_force-min_force
	force=(force-min_force)*range_to/range_from + min 
end function