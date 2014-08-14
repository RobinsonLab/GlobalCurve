function three_state_ktr_ATPase(current_model,current_data) result(ATPase)
	use h_struct
	use h_models
	use h_routine_specific_vars
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase

! constants
	integer,parameter :: n=3

! Local Var's
	real,dimension(current_data%len,n) :: y
	real :: offset,A_zero
	real,dimension(current_data%len) :: g_save
! begin
! the raw ATPase that is calculated is the duty cycle of myosin x-bridges
! so Ao needs to scale this value to the data.
	offset=param_list(current_model%model%param(2)%param_basis(1))%val
	A_zero=param_list(current_model%model%param(2)%param_basis(2))%val
	y=calc_three_state_ktr(current_model,current_data,n,g_save)
	! y contains the n species concentrations.
	ATPase(:)=g_save(:)*y(:,3)
	ATPase=ATPase*A_zero + offset 
end function