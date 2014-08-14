function calc_isotonic_twitch(current_model,current_data,n,g_save) result(species)
	use h_struct
	USE nrtype
	USE nr
	use numerical_libraries, only: CSINT,gamma
!	USE ode_path
	use h_routine_specific_vars
	use h_physiology_models, only : odeint_time_isotonic,odeint2
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

! local variables
	REAL(SP), DIMENSION(n) :: y
	real(sp) :: Ka ,pCa,t1,t2
!	integer :: ios,i,j,len
	real,dimension(current_data%len) :: g,x_pts,Ca_data !,pCa,t
	integer :: len
	real :: s,t,mf,x,gfn
	integer :: i,kount

! intent(out)
	! h_routine_specific_vars
	!	tsv%kon,koff,f,g,u,v,w,x,z,pCa,break,cscoef,nintv

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
	tsv%y_target=param_list(current_model%model%param(2)%param_basis(1))%val

! setup for being able to get Ca(t)

	! Ca are found in the lamp
	! time points are found in x_pts
	! force, ATPase, or bound Ca++ is found in datum

!	CALL CSINT (NDATA, XDATA, FDATA, BREAK, CSCOEF)

	len=current_data%len
!	s=22.0
!	t=4.0
!	do i=1,len
!		x=real(i-1)
!		x_pts(i)=x*1E-3
!		gfn=gamma(t)
!		Ca_data(i)=exp(-x/s)*(x/s)**(t-1.)/(gfn*s)
!	enddo
!! break the rules again

!	current_data%x_pts=x_pts
!	mf=maxval(Ca_data)
!	Ca_data=1.5E-6*Ca_data/mf
!	where (Ca_data < 1E-9) Ca_data=1E-9
!		fdata=-log10(fdata)

	x_pts=current_data%x_pts
	Ca_data=current_data%lamp

	tsv%nintv=len-1
	allocate(tsv%break(len))
	allocate(tsv%cscoef(4,len))
!	set up the spline

!	for noisy data one can to a least-squares spline.  Consult IMSL manual in
!	the interpolation section
	call csint (len,x_pts,Ca_data,tsv%break,tsv%cscoef)




!	pCa better equal a number larger then 7.  Otherwise we are not starting from
!	diastolic calcium levels.  Just in case, do a pre-equilibration step.
	tsv%Ca=Ca_data(1)

! ***** working on future version here
	tsv%fixed=.true.

	! calculate initial conditions via a direct call to odeint2
	y(1)=1.
	y(2)=0.
	y(3)=0.
	t1=t1p
	t2=t2p
	call odeint2(y,t1,t2,NA_eps,0.0_sp)

	t1=x_pts(1)
	t2=x_pts(len)
	tsv%fixed=.false.
	kount=1
	call odeint_time_isotonic(y,t1,t2,NA_eps,0.0_sp,current_data, &
							species,g,kount)
	! garbage collection
	deallocate(tsv%break,tsv%cscoef)
	! current data contains x_pts so pass it on to odeint_time
	if (present(g_save)) g_save=g
! if default%simul is false then num_points better not have changed
end function
