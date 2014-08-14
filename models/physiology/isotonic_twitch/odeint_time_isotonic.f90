SUBROUTINE odeint_time_isotonic(ystart,x1,x2,nr_eps,hmin,current_data, &
									species,g,kount)
! this subroutine is a little busy because the variable step sizes that one wants
! to take are great for solving the system of ODE's for a minimum ammount of 
! work.  However, the x_pts that the algorithm wants to take are different
! from the ones we need.  We could just let the algorithm do its thing then 
! use an interpolating polynomial to evaluate the results at the x_pts we 
! need.  Alternatively we can force it to take steps, some of which will be at
! our points of interest.

! Things are kind of quirky.  If doing a simulation make sure current_data%len
! has enough points to hold all the data points.  The number should be
! (t2-t1)/dxsav + 1

	use h_routine_specific_vars
	!intent(out)
	!	g_save
	USE nrtype; USE nrutil, ONLY : nrerror ! ,reallocate
	use h_physiology_models, only : load_clamp !three_state_t_derivs,stiff3
	use h_struct
	IMPLICIT NONE
	REAL(SP), DIMENSION(3), INTENT(in) :: ystart
	REAL(SP), INTENT(IN) :: x1,x2,nr_eps,hmin
	type (data_struct),pointer :: current_data
	real,dimension(current_data%len,3),intent(out) :: species
	real,dimension(current_data%len),intent(out) :: g
	integer,intent(inout) :: kount


!	Local Params
	integer,parameter :: START_SIZE = 10000

!	Local Vars

!	REAL(SP), PARAMETER :: TINY=1.0e-30_sp
	INTEGER(I4B), PARAMETER :: MAXSTP=50000
	INTEGER(I4B) :: nstp
	REAL(SP) :: h,hdid,hnext,x,xsav,xm,htry
	REAL(SP), DIMENSION(2) :: dydx,y,yscal
	logical :: flag
	INTEGER(I4B) :: nok,nbad !,kount
!	LOGICAL(LGT), SAVE :: save_steps=.false.
	REAL(SP),save :: dxsav=0.002 ! 2 msec == maximum resolution
	! set it lower if you want potentially tons of points
!	REAL(SP), DIMENSION(:), POINTER :: xp,gp
!	REAL(SP), DIMENSION(:,:), POINTER :: yp
	real :: g_save
	integer :: len
! Begin

	len = current_data%len
	y(1)=ystart(1)
	y(2)=ystart(3)

! next two lines get a decent estimate for h
!	call three_state_t_jacobn(x1,y,dydx) ! just use dydx as scratch space
					! call to three_state_derivs below will overwrite
!	h=0.1/maxval(abs(dydx))
	x=x1
!	kount=1
	nok=0
	nbad=0
!	xsav=x-2.0_sp*dxsav
!	nullify(xp,gp,yp)
!	allocate(xp(current_data%num_points))
!	allocate(gp(current_data%num_points))
!	allocate(yp(current_data%num_points,3))
	
!	the load clamp is going to tinker with g
!	when the tension falls below the clamp value then we better
!	restore g.
	tsv%g_save=tsv%g

! calculate species concentrations at x_pts


	xm=current_data%x_pts(kount)
!		if (kount>current_data%len .or. xm>=x2) exit dl1
	h=xm-x
	if (h == 0) call save_a_step ! kount is incremented
	yscal=1.0
	dl1: do
! corrected a potential array out of bounds ref.
		if (kount>len) exit dl1
		xm=current_data%x_pts(kount) 
		if (xm>x2) exit dl1
		h=xm-x
		htry=h
		flag=.true.

!	Let's get this straight. xm is the time we are trying to get to.
!	x is the time we are at as we start this loop again. h is the difference

!	Note, tsv%t1 and t2 are only used by the load clamp conditions, not by stiff3
!		tsv%t1=x
!		tsv%t2=htry


		dl2: do
!			tsv%y_save=y
!			tsv%t1=x
!			tsv%t2=htry
			call load_clamp(y,x,htry,hdid,hnext,nr_eps)


!			call three_state_t_derivs(y,dydx,x) ! dydx is needed for call to stiff3
!			call stiff3(y,dydx,x,htry,nr_eps,yscal,hdid,hnext) ! take a step
			if (hdid == htry .and. flag ) then ! x is now at xm
!	test to see if force has exceeded our setpoint
!				if (y(2)>tsv%y_target) then
					! note: we have already set the needed params in tsv
!					call load_clamp(x,t) ! load clamp gets the job done
!				endif				
				call save_a_step 
				exit dl2
			else ! we weren't able to get to the desired time point in one step
				nbad=nbad+1
				if (abs(hnext) < hmin)&
					call nrerror('stepsize smaller than minimum in odeint')
				h=h-hdid ! still have more to go
				if (h > hnext) then
					htry = hnext
					flag=.false. ! we're not going to make it to the next point
				else
					htry=h
					flag=.true.
				endif
			endif
		enddo dl2
		if (kount>current_data%len) exit dl1		
	enddo dl1
	kount=kount-1
! pause
	CONTAINS
!BL
	SUBROUTINE save_a_step
	use h_routine_specific_vars
!	kount=kount+1
!	if (kount > size(xp)) then
!		xp=>reallocate(xp,2*size(xp))
!		gp=>reallocate(gp,size(xp))
!		yp=>reallocate(yp,size(xp),3)
!	end if
!	if (default%simul) current_data%x_pts(kount)=x
!	xp(kount)=x
	species(kount,1)=y(1)
	species(kount,3)=y(2)
	species(kount,2)=1.-(y(1)+y(2))
	g(kount)=tsv%g * (1.+y(2)*(tsv%w-1.))**2 
!	restore tsv%g
	tsv%g=tsv%g_save
!	yp(kount,1)=y(1)
!	yp(kount,3)=y(2)
!	yp(kount,2)=1.-(y(1)+y(2))
!	gp(kount)=tsv%g * (1.+y(2)*(tsv%w-1.))**2 ! rememter here y(2) is actually species 3
	! b/c we've eliminated species 2.
	nok=nok+1
	xsav=x
	kount=kount+1
	END SUBROUTINE save_a_step
	END SUBROUTINE 


