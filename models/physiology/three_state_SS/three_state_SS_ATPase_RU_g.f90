function three_state_SS_ATPase_RU_g(current_model,current_data) result(ATPase)
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
	real :: offset,A_zero
	real,dimension(current_data%len) :: g_save
	real :: w2
! begin
! the raw F that is calculated is the duty cycle of myosin x-bridges
! so Fo needs to scale this value to the data.
	call init_tsv()

	w2=		param_list(current_model%model%param(1)%param_basis(10))%val
	tsv%w2=exp(-w2)

	offset=param_list(current_model%model%param(2)%param_basis(1))%val
	A_zero=param_list(current_model%model%param(2)%param_basis(2))%val
	A_M=three_state_SS(current_model,current_data,g_save)
	! A_M contains the n species concentrations.
	ATPase(:)=g_save(:)*A_M(:,3)
	ATPase=ATPase*A_zero + offset 
end function