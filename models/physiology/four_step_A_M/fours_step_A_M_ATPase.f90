function four_step_A_M_ATPase(current_model,current_data) result(ATPase)
	use h_struct
	use h_models
	use h_routine_specific_vars
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase

! constants
	integer,parameter :: n=4

! Local Var's
	real,dimension(current_data%len,n) :: A_M
	real :: offset,A_zero
! begin
! the raw ATPase that is calculated is the duty cycle of myosin x-bridges
! so Ao needs to scale this value to the data.
	offset=param_list(current_model%model%param(2)%param_basis(1))%val
	A_zero=param_list(current_model%model%param(2)%param_basis(2))%val
	A_M=calc_four_step_A_M(current_model,current_data,n)
	! g_plus and g_minus were set by calc_A_M
	ATPase(:)=fsv%g_plus*A_M(:,3)+fsv%g_minus*A_M(:,4)
	ATPase=ATPase*A_zero + offset 
end function