module h_fluor_wrapper_models
interface

function gaus_dist_et(current_model,current_data) result(fluor)
	use h_struct
	use h_fluor_models, only : dist_et,gaus_decay
	use h_vars
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function gaus_dist_et

function gamma_dist_et(current_model,current_data) result(fluor)
	use h_struct
	use h_fluor_models, only : dist_et,gamma_decay
	use h_vars
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function gamma_dist_et

end interface
end module h_fluor_wrapper_models
