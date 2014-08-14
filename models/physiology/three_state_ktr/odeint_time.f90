SUBROUTINE odeint_time(ystart,x1,x2,nr_eps,hmin,current_data,species,g)
! subroutine is depreciated.  Replaced by odeing_time_contraction

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


	USE nrtype; USE nrutil, ONLY : nrerror,reallocate
	use h_physiology_models, &
		only : three_state_t_derivs,three_state_t_jacobn,stiff3
	use h_struct
	IMPLICIT NONE
	REAL(SP), DIMENSION(3), INTENT(in) :: ystart
	REAL(SP), INTENT(IN) :: x1,x2,nr_eps,hmin
	type (data_struct),pointer :: current_data
	real,dimension(current_data%len,3),intent(out) :: species
	real,dimension(current_data%len),intent(out) :: g


!	Local Params
	integer,parameter :: START_SIZE = 10000

!	Local Vars

!	REAL(SP), PARAMETER :: TINY=1.0e-30_sp
	INTEGER(I4B), PARAMETER :: MAXSTP=50000
	INTEGER(I4B) :: nstp
	REAL(SP) :: h,hdid,hnext,x,xsav,xm
	REAL(SP), DIMENSION(2) :: dydx,y,yscal
	logical :: flag
	INTEGER(I4B) :: nok,nbad,kount
!	LOGICAL(LGT), SAVE :: save_steps=.false.
	REAL(SP),save :: dxsav=0.002 ! 2 msec == maximum resolution
	! set it lower if you want potentially tons of points
!	REAL(SP), DIMENSION(:), POINTER :: xp,gp
!	REAL(SP), DIMENSION(:,:), POINTER :: yp

! Begin

	y(1)=ystart(1)
	y(2)=ystart(3)

! next two lines get a decent estimate for h
	call three_state_t_jacobn(y,dydx) ! just use dydx as scratch space
					! call to three_state_derivs below will overwrite
	h=0.1/maxval(abs(dydx))
	x=x1
	nok=0
	nbad=0
	kount=1

	xsav=x-2.0_sp*dxsav
!	nullify(xp,gp,yp)
!	allocate(xp(current_data%num_points))
!	allocate(gp(current_data%num_points))
!	allocate(yp(current_data%num_points,3))

	call save_a_step
	flag = .false.	
	do nstp=1,MAXSTP
		call three_state_t_derivs(y,dydx)
		yscal(:)=1.0
		if (default%simul) then
			if ( abs(x-xsav) > abs(dxsav) ) call save_a_step
			xm=x2
		else
			xm=current_data%x_pts(kount)
		endif
		if ((x+h-xm)*(x+h-x1) > 0.0) then ! if (x+h) > xm then ...
		! we are about to step over the point we want to calculate
			h=xm-x
			flag = .true.
		endif
		call stiff3(y,dydx,x,h,nr_eps,yscal,hdid,hnext) ! take a step
		if (hdid == h) then
			nok=nok+1
			if ( (.not. default%simul) .and. flag) then
				call save_a_step
				flag = .false.
			endif
		else
			nbad=nbad+1
		end if
		if (default%simul) then
			if ((x-x2)*(x2-x1) >= 0.0) then ! DONE
!			ystart(:)=y(:)
				call save_a_step
				exit
			end if
		else 
			if (kount > current_data%num_points) exit
		endif
		if (abs(hnext) < hmin)&
			call nrerror('stepsize smaller than minimum in odeint')
		h=hnext
	end do
	if (nstp > MAXSTP) call nrerror('too many steps in odeint')
! set up to return
!	if (default%simul) current_data%x_pts(:)=xp(1:kount)
!	g(:)=gp(1:kount)
!	species(:,:)=yp(1:kount,:)
!	deallocate(xp,gp,yp)

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
	if (default%simul) current_data%x_pts(kount)=x
!	xp(kount)=x
	species(kount,1)=y(1)
	species(kount,3)=y(2)
	species(kount,2)=1.-(y(1)+y(2))
	g(kount)=tsv%g * (1.+y(2)*(tsv%w-1.))**2 
!	yp(kount,1)=y(1)
!	yp(kount,3)=y(2)
!	yp(kount,2)=1.-(y(1)+y(2))
!	gp(kount)=tsv%g * (1.+y(2)*(tsv%w-1.))**2 ! rememter here y(2) is actually species 3
	! b/c we've eliminated species 2.
	xsav=x
	kount=kount+1
	END SUBROUTINE save_a_step
	END SUBROUTINE 
