program main
!	use h_global_vars
!	use numerical_libraries
	implicit none

! Local Params
	real,parameter :: TOL = 0.001 ! limit for convergence
	integer,parameter :: DATA_FILE=5
	integer,parameter :: n=5 ! number of equations
	integer,parameter :: m=2 ! number of constraints
	real,parameter :: dt_start=0.0001 ! (0.1 msec) initial time step
	real,parameter :: dt_step=2.
	real,parameter :: dt_max=0.01 ! don't want a step size .ge. 1 sec.
! the following are used to calculate the values of the lagrange
!	multipliers
	real,dimension(m,m),parameter :: mat=(/4.,-2.,-2.,3./)

! Local Var's

	real :: dt,fac,t
	real :: kon_1,koff_1,kon_2,koff_2,f_minus,f_plus,g_minus,g_plus, &
				A_T,M_T,Ca
	integer :: i,j,k
	real :: pCa
	integer :: ios
	real,dimension(n) :: x,xd,xdp,step,p
	real,dimension(m) :: v,lambda
	interface
		function x_dot(kon_1,koff_1,kon_2,koff_2,f_minus,f_plus,g_minus,g_plus, &
			A_T,M_T,Ca,x) result(f)
			implicit none
			real,intent(in) :: kon_1,koff_1,kon_2,koff_2,f_minus,f_plus,g_minus,g_plus, &
				A_T,M_T,Ca
			real,dimension(5),intent(in) :: x
			real,dimension(5) :: f
		end function
	end interface

! Begin
! Everthing is in micro molar.
	kon_1=1.0E7
	kon_2=1.0E6

	koff_1=10.
	koff_2=1.
	g_minus=10.
	f_plus=8.E3
	g_plus=100.

	f_minus=0.
	A_T=3.E-4
	M_T=3.E-4



! Assign initial conditions.  Subsequent relaxations to steady
!	state are from previous calcium steady state conditions.
! Ca = calcium, A = actin, M = myosin

	x(1)=A_T !		A
	x(2)=M_T !		M
	x(3)=0.  !		AM
	x(4)=0.  !		CaA
	x(5)=0.  ! 		CaAM


		open(unit = DATA_FILE,file='non_lin_zero.dat', &
			status='unknown',iostat=ios)
!		if (ios /= 0) call error_handler('Error opening file: ',current_expt%file_name)


!	do i=0,9
!		pCa=9.0-i*0.5
! messing with things here
		pCa=5.0
		Ca=10.**-pCa
		write (DATA_FILE,*)'Solution for pCa = ',pCa
		j=0
		dt=dt_start
		t=0.
		do
			t=t+dt
!			fac=1./(8.*dt)
			j=j+1
			xd=x_dot(kon_1,koff_1,kon_2,koff_2,f_minus,f_plus,g_minus,g_plus, &
				A_T,M_T,Ca,x)
	! calculate the undetermined multipliers
!			v(1)=M_T-(x(2)+x(3)+x(5)+(xd(2)+xd(3)+xd(5))*dt)
!			v(2)=A_T-(x(1)+x(3)+x(4)+x(5)+(xd(1)+xd(3)+xd(4)+xd(5))*dt)
!			lambda=fac*matmul(mat,v)
	! add the constraint force to the force
!			p=(/lambda(2),lambda(1),lambda(1)+lambda(2),lambda(2),lambda(1)+lambda(2)/)
			xdp=xd   ! +p
			step=xdp*dt
			x=x+step
			write (DATA_FILE,10)dt,t,x
! if relative change in a variable is less than TOL then increase our step size
			if (maxval(abs(step/x))<TOL) dt=dt*dt_step 
			if (dt>dt_max) dt=dt_max
			if (t .ge. 5.) exit ! simulate for up to 3 secs.
		enddo

!		write (*,*)'x:'
!		write (*,*)x
!		write (*,*)'b:'
!		write (*,*)b
!		pause
	
!	enddo
	close(DATA_FILE,iostat=ios)

	10 format(<n+2>EN15.4)
end program
