function gaus_dist_k(current_model,current_data) result(fluor)
	use h_struct
	use h_kinetics_models, only : dist_k,gaus_k ! ,gaus_decay
	use h_vars
	implicit none
		
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 

! Local var's
	real :: F0

! Begin
	F0=param_list(current_model%model%param(3)%param_basis(1))%val
	fluor=dist_k(current_model,current_data,gaus_k)
	fluor= F0 + fluor
return
end function