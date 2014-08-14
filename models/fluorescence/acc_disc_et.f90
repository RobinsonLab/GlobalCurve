function acc_disc_et(current_model,param_list,expt_data) result(fluor)
	use h_utils
	use h_models, only : disc_et,lifetime
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type(param_struct),dimension(:),intent(in) :: param_list
	type (data_struct),pointer :: expt_data	
	real,dimension(expt_data%len) :: fluor 

! Local Var's
real,dimension(expt_data%len) :: fluor1,fluor2
type(model_struct), pointer :: temp_model

! Begin

! this model expects 5 arrays of params
! array #		description
!	1			donor alpha
!	2			donor tau
!	3			R0,mu
!	4			acceptor alpha
!	5			acceptor tau

! len=size(fluor)

allocate(temp_model)
allocate(temp_model%model%param(2))
	current_model%flag=1 ! set the flag
	fluor1=disc_et(current_model,param_list,expt_data)
	current_model%flag=0 ! reset the flag

	temp_model%model%param(1)%param_basis=>current_model%model%param(4)%param_basis
	temp_model%model%param(2)%param_basis=>current_model%model%param(5)%param_basis

	fluor2=lifetime(temp_model,param_list,expt_data)
	fluor=simple_convolute(fluor1,fluor2) 
	
	deallocate(temp_model%model%param,temp_model)

end function acc_disc_et
