subroutine load_clamp(y,x,htry,hdid,hnext,nr_eps)
	use h_physiology_models, only : three_state_t_derivs,stiff3
	use numerical_libraries, only : zreal
	use h_routine_specific_vars ! tsv
	! intent(in)
	!	tsv%t1,y_save2,y_target
	! intent(inout)
	!	tsv%g	
	implicit none
	
	real,dimension(2),intent(inout) :: y
	real,intent(inout) :: x
	real,intent(in) :: htry
	real,intent(out) :: hdid,hnext
	real,intent(in) :: nr_eps

! IMSL params
	real,parameter :: IMSL_EPS = 1.0E-5
	real,parameter :: ERRABS = 1.0E-5
	real,parameter :: ERRREL = 1.0E-5
	real,parameter :: ETA = 1.0E-2
	integer,parameter :: ITMAX = 100
	integer,parameter :: NROOT=1

!	Local Var's
! for IMSL call
	real,dimension(NROOT) :: g,g_guess
	integer,dimension(NROOT) ::info
!	real :: g_guess,g
	real,dimension(2) :: dydx,yscal
	interface
		function my_zero_func(g) result (fxn)
			implicit none
			real,intent(in) :: g
			real :: fxn
		end	function	
	end interface


!	Begin

	yscal=1.0	
	tsv%y_save2=y
	tsv%t1=x
	tsv%t2=htry
	call three_state_t_derivs(y,dydx,x) ! dydx is needed for call to stiff3
	call stiff3(y,dydx,x,htry,nr_eps,yscal,hdid,hnext) ! take a step

	if (y(2)>tsv%y_target) then
		g_guess=tsv%g
		CALL ZREAL (my_zero_func, ERRABS, ERRREL, IMSL_EPS, ETA, NROOT, ITMAX, &
			g_guess, g, INFO)
! the problem is that zreal only finds the right g.
! we have to calculate all over again using this value to get
! x - the species.
		tsv%g=g(1)
		y=tsv%y_save2
		x=tsv%t1
		call three_state_t_derivs(y,dydx,x) ! dydx is needed for call to stiff3
		call stiff3(y,dydx,x,htry,nr_eps,yscal,hdid,hnext) ! take a step
	endif
!	return with x,t, and g is set in tsv%g


end subroutine


function my_zero_func(g) result (fxn)
! This routine calculates the difference between 
! tension and load clamped tension for a given g.

	use h_physiology_models, only : three_state_t_derivs,stiff3
	use h_routine_specific_vars ! tsv
	! intent(in)
	!	tsv%t1,t2,y_save,y_target
	! intent(inout)
	!	tsv%g

	implicit none
	real,intent(in) :: g
	real :: fxn

! Local Params
	real,parameter :: nr_eps=1.0E-6 ! don't like doing this 

! Local Vars
	
	real,dimension(2) :: y,dydx,yscal
	real :: x
	real :: htry,hdid,hnext

! Begin

!	Here's the issue.  The IMSL routine likes to see a function
! that has very simple parameter specs.  In our case we have just
! a scalar.  So the other params that our function needs must be
! accessed via a module (common block).

! stiff 3 needs y,dydx,x, and htry
	tsv%g=g
	y=tsv%y_save2
	x=tsv%t1
	htry=tsv%t2

	yscal=1.0
! in,out,in
	call three_state_t_derivs(y,dydx,x) ! dydx is needed for call to stiff3
	call stiff3(y,dydx,x,htry,nr_eps,yscal,hdid,hnext)
	fxn=tsv%y_target-y(2) ! note y(2) is force here
	! see three_state_t_derivs for explanation


end function 
