function calc_three_state_quin2(current_model,current_data,n,g_save) result(species)
	use h_struct
	USE nrtype
	USE nr
!	USE ode_path
	use h_routine_specific_vars
	use h_physiology_models, only : odeint_time,odeint2
	implicit none	

	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	integer,intent(in) :: n ! number of equations = 3
	real,dimension(current_data%len),optional :: g_save
	real,dimension(current_data%len,n) :: species


! constants
!	integer(i4b),parameter :: DATA_FILE=5
	real(sp),parameter :: t1=0.0
	real(sp),parameter :: pCa_final=9.5
! the extra computational effort to go from 2 secs to 20 secs is trivial
!	6 more steps than the 133 to get to 2 secs.
	real(sp),parameter :: t2=20.0 ! 20 secs
	real(sp),parameter :: NA_eps=1.0E-6
!	real(sp),parameter :: hstart=1.0E-3
	real(sp),parameter :: A_T=1.54E-4 !Ferenczi et al., 1984
	real(sp),parameter :: M_T=A_T

! local variables
	REAL(SP), DIMENSION(n) :: y
	real(sp) :: pCa,Ka
!	integer :: ios,i,j,len
	real,dimension(current_data%len) :: g

! begin

! if you want to save off the time course then set: save_steps = .true.
	!	save_steps = .true. 
	tsv%kon=	param_list(current_model%model%param(1)%param_basis(1))%val
	tsv%koff=	param_list(current_model%model%param(1)%param_basis(2))%val
	tsv%f=		param_list(current_model%model%param(1)%param_basis(3))%val
	tsv%g=		param_list(current_model%model%param(1)%param_basis(4))%val
	tsv%u=		param_list(current_model%model%param(1)%param_basis(5))%val
	tsv%v=		param_list(current_model%model%param(1)%param_basis(6))%val
	tsv%w=		param_list(current_model%model%param(1)%param_basis(7))%val
	tsv%x=		param_list(current_model%model%param(1)%param_basis(8))%val
	tsv%z=		param_list(current_model%model%param(1)%param_basis(9))%val
	pCa=		param_list(current_model%model%param(3)%param_basis(1))%val
	tsv%Ca=10.**-pCa ! Ca is a global variable

	! calculate initial conditions via a direct call to odeint2
	y(1)=1.
	y(2)=0.
	y(3)=0.
	call odeint2(y,t1,t2,NA_eps,0.0_sp)



	tsv%Ca=10.**-pCa_final

	! for now ignore the values of x_pts because we are only doing simulations.
	call odeint_time(y,t1,t2,NA_eps,0.0_sp,current_data,species,g)
	! current data contains x_pts so pass it on to odeint_time
	if (present(g_save)) g_save=g
! if default%simul is false then num_points better not have changed
end function
