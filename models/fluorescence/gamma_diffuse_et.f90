function gamma_diffuse_et(current_model,current_data) result(fluor)
	use h_struct
	use h_models, only : diffuse_et
	use h_utils, only : gamma_dist,harmonic
	use h_vars
	implicit none
		
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 

! Begin
	fluor=diffuse_et(current_model,current_data,gamma_dist,harmonic)

return
end function
