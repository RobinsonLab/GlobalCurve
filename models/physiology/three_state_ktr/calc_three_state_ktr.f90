function calc_three_state_ktr(current_model,current_data,n,g_save) result(species)
	use h_struct
	USE nrtype
	USE nr
	use h_utils, only: dump_raw
!	USE ode_path
	use h_params ! default%eps
	use h_routine_specific_vars
	use numerical_libraries
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
	real(sp),parameter :: NA_eps=1.0E-6
!	real(sp),parameter :: hstart=1.0E-3
	real(sp),parameter :: A_T=1.54E-4 !Ferenczi et al., 1984
	real(sp),parameter :: M_T=A_T
	real(sp),parameter :: pre_duration = 0.05
	real(sp),parameter :: isotonic_duration = 0.005
	real,parameter :: frac_iso_load = 0.10
	real,parameter :: g_est=2000.
! IMSL params
	real,parameter :: IMSL_EPS = 1.0E-5
	real,parameter :: ERRABS = 1.0E-5
	real,parameter :: ERRREL = 1.0E-5
	real,parameter :: ETA = 1.0E-2
	integer,parameter :: ITMAX = 100
	integer,parameter :: NROOT=1

! local variables
	integer :: kount,kount_save
	REAL(SP), DIMENSION(n) :: y
	real(sp) :: pCa,Ka
!	integer :: ios,i,j,len
	real,dimension(current_data%len) :: g
	real :: t1,t2
	real :: y_target,delta_y,dg,y_save
! for IMSL call
	real,dimension(NROOT) :: x,x_guess
	integer,dimension(NROOT) ::info
interface
	function find_my_zero(x) result(fxn)
		implicit none
		real,intent(in) :: x
		real :: fxn
	end function
end interface

! begin



! if you want to save off the time course then set: save_steps = .true.
	!	save_steps = .true. 
	tsv%kon=	param_list(current_model%model%param(1)%param_basis(1))%val
	tsv%koff=	param_list(current_model%model%param(1)%param_basis(2))%val
!	tsv%f= 0.
	tsv%f=		param_list(current_model%model%param(1)%param_basis(3))%val
	tsv%g=		param_list(current_model%model%param(1)%param_basis(4))%val
	tsv%u=		param_list(current_model%model%param(1)%param_basis(5))%val
	tsv%v=		param_list(current_model%model%param(1)%param_basis(6))%val
	tsv%w=		param_list(current_model%model%param(1)%param_basis(7))%val
	tsv%x=		param_list(current_model%model%param(1)%param_basis(8))%val
	tsv%z=		param_list(current_model%model%param(1)%param_basis(9))%val

	tsv%nu=	param_list(current_model%model%param(4)%param_basis(1))%val
! *****
! calculate initial conditions via a direct call to odeint2
	pCa=		param_list(current_model%model%param(3)%param_basis(1))%val
	tsv%Ca=10.**-pCa ! Ca is a global variable
	tsv%fixed=.true. ! ODE integration w/o [Ca] varying in time

	y(1)=1.
	y(2)=0.
	y(3)=0.
	t1=t1p
	t2=t2p
	call odeint2(y,t1,t2,NA_eps,0.0_sp) ! still using constant Ca derivs.
	kount=0
	do 
		kount=kount+1
		t2=current_data%x_pts(kount)
		if (t2>pre_duration) exit
		species(kount,:)=y(:)
		g(kount)=tsv%g
	enddo
	kount=kount-1
! *****
! want to do an partially loaded contraction


! chi^2 minimization
! CALL UVMIF (minimize_me, 5000., 500., 10000., 0.001, 100, x)

! zero of a function using Muller's method


! tsv%fixed is still true

	x(1)=g_est
	! set the tension value for the isotonic contraction.
	tsv%y_target=y(3)*frac_iso_load
!	kount=1
	t1=current_data%x_pts(kount)
	do 
		tsv%y_save=y
		x_guess=x
		t2=current_data%x_pts(kount+1)
		if (t2>isotonic_duration+pre_duration) exit
		kount=kount+1
		tsv%t1=t1
		tsv%t2=t2
		! find the value of g that gives us the tension we want.
		CALL ZREAL (find_my_zero, ERRABS, ERRREL, IMSL_EPS, ETA, NROOT, ITMAX, &
			x_guess, x, INFO)
		tsv%g=x(1)
		! now actually take the step
		call odeint2(y,t1,t2,NA_eps,0.0_sp)
		species(kount,:)=y(:)
		t1=t2
		g(kount)=tsv%g
	enddo

!! want to do an unloaded contraction
!	kount=1
!	tsv%g=FG_SCALE*tsv%f
!	t1=current_data%x_pts(kount)
!	t2=isotonic_duration+t1
!!	y(2)=y(2)+y(3)
!!	y(3)=0.

!	call odeint_time_contraction(y,t1,t2,NA_eps,0.0_sp,current_data, &
!								species,g,kount)
!! *****
!! isotonic contraction sends all the force generating x-bridges into state II.
!	kount=1
!	tsv%f=0.
!	t1=current_data%x_pts(kount)
!	t2=isotonic_duration+t1
!	y(2)=y(2)+y(3)
!	y(3)=0.

!	call odeint_time_contraction(y,t1,t2,NA_eps,0.0_sp,current_data, &
!								species,g,kount)


! *****
! restretch sends the state III population into state II.
	t1=current_data%x_pts(kount)
	t2=current_data%x_pts(current_data%len)
	y(1)=species(kount,1)
	y(2)=species(kount,2)
	y(3)=species(kount,3)
!	species(1:kount,3)=0. ! state III population can't generate force
							! during isotonic contraction
	! restore g
	tsv%g=		param_list(current_model%model%param(1)%param_basis(4))%val
	kount_save=kount

! tsv%fixed is still true


	call odeint_time_contraction(y,t1,t2,NA_eps,0.0_sp,current_data, &
								species,g,kount)
	! current data contains x_pts so pass it on to odeint_time

!	g(kount:)=g(kount:)
	g(kount_save:)=tsv%g
	if (present(g_save)) g_save=g
! if default%simul is false then num_points better not have changed
	if (default%simul) call dump_raw(current_data%len,current_data%x_pts,species,g)
end function


function find_my_zero(x) result (fxn)
	use h_physiology_models, only : odeint2 
	use h_routine_specific_vars ! tsv
	! intent(in)
	!	tsv%t1,t2,y_save,y_target
	! intent(out)
	!	tsv%g
	implicit none
	real,intent(in) :: x
	real :: fxn

! Local Params
	real,parameter :: NA_eps=1.0E-6

! Local Vars
	real,dimension(3) :: y
	real :: t1,t2
! Begin
	y=tsv%y_save
	t1=tsv%t1
	t2=tsv%t2
	tsv%g=x
	call odeint2(y,t1,t2,NA_eps,0.0)
	fxn=tsv%y_target-y(3)


end function 

