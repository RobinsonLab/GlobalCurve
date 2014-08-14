function three_state_A_M_force(current_model,current_data) result(force)
	use h_struct
	use h_models
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force

! constants
	integer,parameter :: n=3

! Local Var's
	real,dimension(current_data%len,n) :: A_M
	real :: offset,F_zero
! begin
! the raw F that is calculated is the duty cycle of myosin x-bridges
! so Fo needs to scale this value to the data.
	offset=param_list(current_model%model%param(2)%param_basis(1))%val
	F_zero=param_list(current_model%model%param(2)%param_basis(2))%val
!	range_to=max-min
	A_M=calc_three_state_A_M(current_model,current_data,n)
	force(:)=A_M(:,3)
	force=force*F_zero + offset 
end function