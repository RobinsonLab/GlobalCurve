module h_kinetics_models
interface

function sum_of_exponentials(current_model,current_data) result(fluor)
	use h_struct
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor
end function

function gaus_dist_k(current_model,current_data) result(fluor)
	use h_struct
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor
end function

function dist_k(current_model,current_data,decay_dist) result(fluor)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data
	interface
		function decay_dist(r) ! should be gaus_decay, gamma_decay or similar routine
			real,intent(in) :: r
		end function decay_dist
	end interface
	real,dimension(current_data%len) :: fluor
end function

function gaus_k(k)
	real,intent(in) :: k
	real :: gaus_k
end function

end interface
end module h_kinetics_models