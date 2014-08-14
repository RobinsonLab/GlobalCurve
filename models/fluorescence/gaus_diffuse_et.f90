function gaus_diffuse_et(current_model,current_data) result(fluor)
	use h_struct
	use h_models, only : diffuse_et
	use h_utils, only : gaus_dist,harmonic
	use h_vars
	implicit none
		
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 

! Begin
	fluor=diffuse_et(current_model,current_data,gaus_dist,harmonic)

return
end function
