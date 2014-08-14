function calc_three_state_diazo_2(current_model,current_data,n,g_save) result(species)
	use h_struct
	USE nrtype
	USE nr
!	USE ode_path
	use h_routine_specific_vars
	use h_physiology_models, only : odeint_time_contraction,odeint2
	implicit none	

	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	integer,intent(in) :: n ! number of equations = 3
	real,dimension(current_data%len),optional :: g_save
	real,dimension(current_data%len,n) :: species


! constants
!	integer(i4b),parameter :: DATA_FILE=5
	real(sp),parameter :: t1p=0.0
! the extra computational effort to go from 2 secs to 20 secs is trivial
!	6 more steps than the 133 to get to 2 secs.
	real(sp),parameter :: t2p=20.0 ! 20 secs
	real(sp),parameter :: pCap=4.0 ! start out fully activated
	real(sp),parameter :: NA_eps=1.0E-6
!	real(sp),parameter :: hstart=1.0E-3
	real(sp),parameter :: A_T=1.54E-4 !Ferenczi et al., 1984
	real(sp),parameter :: M_T=A_T

! local variables
	REAL(SP), DIMENSION(n) :: y
	real(sp) :: pCa,Ka
!	integer :: ios,i,j,len
	real,dimension(current_data%len) :: g
	real :: t1,t2
	integer :: kount
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


	pCa=pCap
	tsv%Ca=10.**-pCa 
	tsv%fixed=.true. ! ODE integration w/o [Ca] varying in time
	! calculate initial conditions via a direct call to odeint2
	y(1)=1.
	y(2)=0.
	y(3)=0.
	t1=t1p
	t2=t2p
	call odeint2(y,t1,t2,NA_eps,0.0_sp) ! still using constant Ca derivs.

	pCa=		param_list(current_model%model%param(3)%param_basis(1))%val
	tsv%Ca=10.**-pCa ! Ca is a global variable
	t1=current_data%x_pts(1)
	t2=current_data%x_pts(current_data%len)
	kount=1

! tsv%fixed is still true

	call odeint_time_contraction(y,t1,t2,NA_eps,0.0_sp,current_data, &
							species,g,kount)
	! current data contains x_pts so pass it on to odeint_time
	if (present(g_save)) g_save=g
! if default%simul is false then num_points better not have changed
end function
