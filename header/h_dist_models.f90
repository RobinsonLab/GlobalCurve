module h_dist_models
interface

function get_distance(current_model,current_data) result(E_t)
	use h_struct
	use h_vars
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: E_t
end function

end interface
end module h_dist_models