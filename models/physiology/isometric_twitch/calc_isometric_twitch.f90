function calc_isometric_twitch(current_model,current_data,n,g_save) result(species)
! modified 5/8/02 to impliment (1) a least-squares spline
! (2) a scaling of the [Ca]_i with an offset and a magnitude
! (3) initial condition is obtained with call to three_state_SS

	use h_struct
	USE nrtype
	USE nr
	use numerical_libraries, only: csint,csval
	USE h_utils, only : get_ca
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
	real(sp),parameter :: NA_eps=1.0E-6
!	real(sp),parameter :: hstart=1.0E-3
	real(sp),parameter :: A_T=1.54E-4 !Ferenczi et al., 1984
	real(sp),parameter :: M_T=A_T
	real,parameter :: SMOOTH_TOL_P=0.5
	real,parameter :: ny3=1.0

! local variables
	real,dimension(current_data%len) :: smooth_weight
	real, dimension(n) :: y
	REAL(SP), DIMENSION(n-1) :: ny
	real(sp) :: Ka ,pCa,t1,t2
!	integer :: ios,i,j,len
	real,dimension(current_data%len) :: g,x_pts,Ca_data !,pCa,t
	integer :: len
	real :: s,t,mf,x,gfn
	integer :: i,kount
	real :: ut,vt,wt,xt,zt
	real :: Ca_offset,Ca_scale
	real :: smooth_tol
	real :: sumy

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
! made changes to u,v,w,x,z on 3/13/02
	ut=		param_list(current_model%model%param(1)%param_basis(5))%val
	vt=		param_list(current_model%model%param(1)%param_basis(6))%val
	wt=		param_list(current_model%model%param(1)%param_basis(7))%val
	xt=		param_list(current_model%model%param(1)%param_basis(8))%val
	zt=		param_list(current_model%model%param(1)%param_basis(9))%val
	tsv%u=exp(-ut)
	tsv%v=exp(-vt)
	tsv%w=exp(-wt)
	tsv%x=exp(-xt)
	tsv%z=exp(-zt)

	Ca_offset=	param_list(current_model%model%param(2)%param_basis(1))%val
	Ca_scale=	param_list(current_model%model%param(2)%param_basis(2))%val

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
	Ca_data(:)=Ca_offset + Ca_scale*current_data%lamp(:)

	tsv%nintv=len-1
	allocate(tsv%break(len))
	allocate(tsv%cscoef(4,len))
!	set up the spline


!	regular cubic spline interpolation
	call csint(len,x_pts,Ca_data,tsv%break,tsv%cscoef)


!	for noisy data one can do a least-squares spline.  Consult IMSL manual in
!	the interpolation section

! call to a smoothing cubic spline

!	smooth_weight(:)=maxval(Ca_data)
!	smooth_tol=SMOOTH_TOL_P
!	call cssmh (len,x_pts,Ca_data,smooth_weight,smooth_tol,tsv%break,tsv%cscoef)

!	pCa better equal a number larger then 7.  Otherwise we are not starting from
!	diastolic calcium levels.  Just in case, do a pre-equilibration step.


! **************
! temporarily examine how the ls c-spline is doing

!do i=1,len
!	t=x_pts(i)
!	current_data%lamp(i)=CSVAL(t,tsv%NINTV,tsv%BREAK,tsv%CSCOEF)
!enddo


	t=x_pts(1)
	tsv%Ca=get_ca(t)

!	tsv%Ca=Ca_data(1)

	! calculate initial conditions 

	ny(1)=tsv%g*tsv%koff/(tsv%f*tsv%kon*tsv%Ca)
	ny(2)=tsv%g/tsv%f
	call find_zero(ny)

	sumy=(ny(1)+ny(2)+ny3)/(1.0-tsv%alpha)
	y(1)=ny(1)/sumy 
	y(2)=ny(2)/sumy
	y(3)=ny3/sumy


	t1=x_pts(1)
	t2=x_pts(len)
	tsv%fixed=.false.
	kount=1
	call odeint_time_contraction(y,t1,t2,NA_eps,0.0_sp,current_data, &
							species,g,kount)
	! garbage collection
	deallocate(tsv%break,tsv%cscoef)
	! current data contains x_pts so pass it on to odeint_time
	if (present(g_save)) g_save=g
! if default%simul is false then num_points better not have changed
end function
