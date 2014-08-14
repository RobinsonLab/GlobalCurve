module h_fluor_models
interface

function rout(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function

function lifetime(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function

function disc_et(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function

function acc_disc_et(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function


! distributed energy transfer

! wrapper functions -- see h_fluor_wrapper_models

! dist_et driver
function dist_et(current_model,current_data,decay_dist,dist) result(fluor)
!	use h_params ! default%ET_fac
	use h_struct
!	use nr, only: qromb
!	use h_routines, only : trapezoidal_integrate
!	use numerical_libraries
!	use h_vars
!	use h_dist_struct ! gp
!	use h_utils
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	interface
		function decay_dist(t) ! should be gaus_decay, gamma_decay or similar routine
			implicit none	
			real,intent(in) :: t
			real :: decay_dist
		end function decay_dist
		function dist(r)
			implicit none	
			real,intent(in) :: r
			real :: dist
		end function dist
	end interface
	real,dimension(current_data%len) :: fluor 
end function dist_et

! decay functions

function gamma_decay(r)
	use h_params ! PI
	use h_dist_struct
	use h_utils, only: gamma_dist
	implicit none
	real,intent(in) :: r
	real :: gamma_decay
end function gamma_decay

function gaus_decay(r)
	use h_params ! PI
	use h_dist_struct
	use h_utils, only: gaus_dist
	implicit none
	real,intent(in) :: r
	real :: gaus_decay
end function gaus_decay


! distributed energy transfer (acceptor)


function acc_gaus_dist_et(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function

function acc_gamma_dist_et(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function

! diffusion enhanced ET

! driver functions

function diffuse(current_model,current_data) result(fluor)
! function passes the calculated fluorescence decay.  Fluor must be post-processed acording
! to frequency or time-domain by the calling function expt_fit.
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function diffuse


function diffuse_et(current_model,current_data,dist,potential_prime) result(fluor)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	interface
		function dist(N,rmin,dr,params)
			integer,intent(in) :: N
			real,intent(in) :: rmin,dr
			real,dimension(:) :: params
			real,dimension(N) :: dist
		end function dist
	end interface
	interface
		function potential_prime(N,rmin,dr,params)
			integer,intent(in) :: N
			real,intent(in) :: rmin,dr
			real,dimension(:) :: params
			real,dimension(N) :: potential_prime
		end function 
	end interface
	real,dimension(current_data%len) :: fluor 
end function

! wrapper

function gaus_diffuse_et(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function

! driver

function acc_diffuse(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function


! anisotropy models

function vert_polarized(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function

function horiz_polarized(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function

! order_1_anis and order_2_anis are depreciated

function order_1_anis(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function

function order_2_anis(current_model,current_data) result(fluor)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 
end function


end interface
end module h_fluor_models