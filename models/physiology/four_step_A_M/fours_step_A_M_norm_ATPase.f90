function four_step_A_M_norm_ATPase(current_model,current_data) result(ATPase)
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
	real :: min_ATPase,max_ATPase,range_from,range_to,min,max
! begin
	min=param_list(current_model%model%param(2)%param_basis(1))%val
	max=param_list(current_model%model%param(2)%param_basis(2))%val
	range_to=max-min
	A_M=calc_four_step_A_M(current_model,current_data,n)
	! g_plus and g_minus were set by calc_A_M
	ATPase(:)=fsv%g_plus*A_M(:,3)+fsv%g_minus*A_M(:,4)
!	min_ATPase=minval(ATPase)
!	min_ATPase=minval(ATPase)
!	min ATPase better be zero or else something is very wrong
!	by forcing it to be zero, we will see wacky results if they
!	are there.
	min_ATPase=0.0
	max_ATPase=maxval(ATPase)
	range_from=max_ATPase-min_ATPase
	ATPase=(ATPase-min_ATPase)*range_to/range_from + min 
end function