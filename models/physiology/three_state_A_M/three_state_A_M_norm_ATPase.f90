function three_state_A_M_norm_ATPase(current_model,current_data) result(ATPase)
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
	real,dimension(current_data%len,n) :: A_M
	real :: min_ATPase,max_ATPase,range_from,range_to,min,max
	real,dimension(current_data%len) :: g_save
! begin
	min=param_list(current_model%model%param(2)%param_basis(1))%val
	max=param_list(current_model%model%param(2)%param_basis(2))%val
	range_to=max-min
	A_M=calc_three_state_A_M(current_model,current_data,n,g_save)
	ATPase(:)=g_save(:)*A_M(:,3)
!	min_ATPase=minval(ATPase)
!	min ATPase better be zero or else something is very wrong
!	by forcing it to be zero, we will see wacky results if they
!	are there.
	min_ATPase=0.0
	max_ATPase=maxval(ATPase)
	range_from=max_ATPase-min_ATPase
	ATPase=(ATPase-min_ATPase)*range_to/range_from + min 
end function

