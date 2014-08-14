module h_physiology_models
interface

! steady state solutions to three state model

function three_state_SS_ATPase(current_model,current_data) result(ATPase)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase
end function

function three_state_SS_force(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function

function three_state_SS_ATPase_RU_g(current_model,current_data) result(ATPase)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase
end function

function three_state_SS_force_RU_g(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function


function three_state_SS_ATPase_TnC_extr(current_model,current_data) result(ATPase)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase
end function

function three_state_SS_force_TnC_extr(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function

function three_state_SS_ATPase_S1_extr(current_model,current_data) result(ATPase)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase
end function

function three_state_SS_force_S1_extr(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function

function three_state_SS_Force_NEM_S1(current_model,current_data) result(ATPase)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase
end function

function three_state_SS(current_model,current_data,g_save) result(species)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len),optional :: g_save
	real,dimension(current_data%len,3) :: species
end function

! until next comment: four_step_A_M

function calc_four_step_A_M(current_model,current_data,n) result(species)
	use h_struct
	implicit none	

	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	integer,intent(in) :: n ! number of equations
	real,dimension(current_data%len,n) :: species
end function

function four_step_A_M_ATPase(current_model,current_data) result(ATPase)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase
end function

function four_step_A_M_force(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function

function four_step_A_M_norm_ATPase(current_model,current_data) result(ATPase)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase
end function

function four_step_A_M_norm_force(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function

! until next comment: three_state_A_M

function calc_three_state_A_M(current_model,current_data,n,g_save) result(species)
	use h_struct
	USE nrtype
	USE nr
	use h_routine_specific_vars
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	integer,intent(in) :: n ! number of equations = 3
	real,dimension(current_data%len),optional :: g_save
	real,dimension(current_data%len,n) :: species
end function

function three_state_A_M_ATPase(current_model,current_data) result(ATPase)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase
end function

function three_state_A_M_force(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function

function three_state_A_M_norm_ATPase(current_model,current_data) result(ATPase)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase
end function

function three_state_A_M_norm_force(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function

function three_state_A_M_bound(current_model,current_data) result(bound)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bound
end function

! depreciated
SUBROUTINE three_state_derivs(y,dydx)
	USE nrtype
	IMPLICIT NONE
	REAL(SP), DIMENSION(2), INTENT(IN) :: y
	REAL(SP), DIMENSION(2), INTENT(OUT) :: dydx
END SUBROUTINE three_state_derivs

SUBROUTINE three_state_t_derivs(x_in,dxdt,t)
	IMPLICIT NONE
	REAL, DIMENSION(2), INTENT(IN) :: x_in
	REAL, DIMENSION(2), INTENT(OUT) :: dxdt
	real,intent(in),optional :: t
END SUBROUTINE  three_state_t_derivs

! depreciated
SUBROUTINE three_state_jacobn(y,dfdy)
	USE nrtype
	IMPLICIT NONE
	REAL(SP), DIMENSION(2), INTENT(IN) :: y
	REAL(SP), DIMENSION(2), INTENT(OUT) :: dfdy
END SUBROUTINE three_state_jacobn

SUBROUTINE three_state_t_jacobn(x,dfdx,t)
	IMPLICIT NONE
	REAL, DIMENSION(2), INTENT(IN) :: x
	REAL, DIMENSION(2), INTENT(OUT) :: dfdx
	REAL, INTENT(IN),optional :: t
end subroutine

SUBROUTINE odeint2(ystart,x1,x2,eps,hmin)
	USE nrtype
	IMPLICIT NONE
	REAL(SP), DIMENSION(2), INTENT(INOUT) :: ystart
	REAL(SP), INTENT(IN) :: x1,x2,eps,hmin
end subroutine

! depreciated
SUBROUTINE stiff2(y,dydx,x,htry,eps,yscal,hdid,hnext)
	USE nrtype
	IMPLICIT NONE
	REAL(SP), DIMENSION(2), INTENT(INOUT) :: y
	REAL(SP), DIMENSION(2), INTENT(IN) :: dydx,yscal
	REAL(SP), INTENT(INOUT) :: x
	REAL(SP), INTENT(IN) :: htry,eps
	REAL(SP), INTENT(OUT) :: hdid,hnext
end subroutine

SUBROUTINE stiff3(y,dydx,x,htry,eps,yscal,hdid,hnext)
	USE nrtype
	IMPLICIT NONE
	REAL(SP), DIMENSION(2), INTENT(INOUT) :: y
	REAL(SP), DIMENSION(2), INTENT(IN) :: dydx,yscal
	REAL(SP), INTENT(INOUT) :: x
	REAL(SP), INTENT(IN) :: htry,eps
	REAL(SP), INTENT(OUT) :: hdid,hnext
end subroutine

! diazo_2 models

function calc_three_state_diazo_2(current_model,current_data,n,g_save) result(species)
	use h_struct
	USE nrtype
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	integer,intent(in) :: n ! number of equations = 3
	real,dimension(current_data%len),optional :: g_save
	real,dimension(current_data%len,n) :: species
end function

function three_state_diazo_2_force(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function
! ktr models

SUBROUTINE odeint_time(ystart,x1,x2,nr_eps,hmin,current_data,species,g)
	USE nrtype
	use h_struct
	IMPLICIT NONE
	REAL(SP), DIMENSION(3), INTENT(in) :: ystart
	REAL(SP), INTENT(IN) :: x1,x2,nr_eps,hmin
	type (data_struct),pointer :: current_data
	real,dimension(current_data%len,3),intent(out) :: species
	real,dimension(current_data%len),intent(out) :: g
end subroutine

function calc_three_state_ktr(current_model,current_data,n,g_save) result(species)
	use h_struct
	USE nrtype
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	integer,intent(in) :: n ! number of equations = 3
	real,dimension(current_data%len),optional :: g_save
	real,dimension(current_data%len,n) :: species
end function

function three_state_ktr_ATPase(current_model,current_data) result(ATPase)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase
end function

function three_state_ktr_force(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function

function three_state_ktr_bound(current_model,current_data) result(bound)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bound
end function

! quin2 models

function calc_three_state_quin2(current_model,current_data,n,g_save) result(species)
	use h_struct
	USE nrtype
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	integer,intent(in) :: n ! number of equations = 3
	real,dimension(current_data%len),optional :: g_save
	real,dimension(current_data%len,n) :: species
end function

function three_state_quin2_force(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function

! bapta models

function calc_three_state_bapta(current_model,current_data,n,g_save) result(species)
	use h_struct
	USE nrtype
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	integer,intent(in) :: n ! number of equations = 3
	real,dimension(current_data%len),optional :: g_save
	real,dimension(current_data%len,n) :: species
end function

function three_state_bapta_force(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function


! twitch models

SUBROUTINE odeint_time_contraction(ystart,x1,x2,nr_eps,hmin,current_data, &
									species,g,kount)
	USE nrtype
	use h_struct
	IMPLICIT NONE
	REAL(SP), DIMENSION(3), INTENT(in) :: ystart
	REAL(SP), INTENT(IN) :: x1,x2,nr_eps,hmin
	type (data_struct),pointer :: current_data
	real,dimension(current_data%len,3),intent(out) :: species
	real,dimension(current_data%len),intent(out) :: g
	integer,intent(inout) :: kount
end subroutine

function calc_isometric_twitch(current_model,current_data,n,g_save) result(species)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	integer,intent(in) :: n ! number of equations = 3
	real,dimension(current_data%len),optional :: g_save
	real,dimension(current_data%len,n) :: species
end function

function isometric_twitch_force(current_model,current_data) result(force)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force
end function

SUBROUTINE odeint_time_isotonic(ystart,x1,x2,nr_eps,hmin,current_data, &
									species,g,kount)
	USE nrtype
	use h_struct
	IMPLICIT NONE
	REAL(SP), DIMENSION(3), INTENT(in) :: ystart
	REAL(SP), INTENT(IN) :: x1,x2,nr_eps,hmin
	type (data_struct),pointer :: current_data
	real,dimension(current_data%len,3),intent(out) :: species
	real,dimension(current_data%len),intent(out) :: g
	integer,intent(inout) :: kount
end subroutine

subroutine load_clamp(y,x,htry,hdid,hnext,nr_eps)
	implicit none
	real,dimension(2),intent(inout) :: y
	real,intent(inout) :: x
	real,intent(in) :: htry
	real,intent(out) :: hdid,hnext
	real,intent(in) :: nr_eps
end subroutine

function calc_isotonic_twitch(current_model,current_data,n,g_save) result(species)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	integer,intent(in) :: n ! number of equations = 3
	real,dimension(current_data%len),optional :: g_save
	real,dimension(current_data%len,n) :: species
end function

function isotonic_twitch_atpase(current_model,current_data) result(ATPase)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase
end function

end interface
end module 