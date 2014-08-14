	PROGRAM xstiff
!	driver for routine stiff
	USE nrtype
	USE nr
	USE ode_path
	IMPLICIT NONE

! constants
	integer(i4b),parameter :: n=4 ! number of equations
	integer(i4b),parameter :: DATA_FILE=5

! local variables

	INTERFACE
		SUBROUTINE derivs(x,y,dydx)
		USE nrtype
		IMPLICIT NONE
		REAL(SP), INTENT(IN) :: x
		REAL(SP), DIMENSION(:), INTENT(IN) :: y
		REAL(SP), DIMENSION(:), INTENT(OUT) :: dydx
		END SUBROUTINE derivs
	END INTERFACE
	REAL(SP) :: eps,hstart,x1,x2
	REAL(SP), DIMENSION(n) :: y
	real(sp) :: pCa,A_T,M_T
	integer(i4b) :: ios,i


! begin
	open(unit = DATA_FILE,file='non_lin_zero.dat', &
		status='unknown',iostat=ios)

	pCa=9.0
	Ca=10.**-pCa
	kon_1=3.9E7
	kon_2=40.*kon_1
	koff_1=19.6
	koff_2=koff_1
	g_minus=15.0
	f_plus=0.95
	g_plus=2.04
	A_T=7.0E-5
	M_T=A_T








	eps=1.0E-4
	hstart=1.0E-8


!	do
!		write(*,*) 'ENTER EPS,HSTART:'
!		read(*,*,END=999) eps,hstart
		save_steps=.true.
		x1=0.0
		x2=20.0 ! the extra computational effort to go from 2 secs to 20 secs is trivial
		! 6 more steps than the 133 to get to 2 secs.
! Assign initial conditions.  Subsequent relaxations to steady
! Note, we are scaling all concentrations to the total concentration of Actin

y(1)=0.0332468
y(2)=0.6612511
y(3)=0.3051195
y(4)=3.829869e-4


!	y(1)=A_T /A_T !		A
!	y(2)=0.  /A_T !		CaA
!	y(3)=0.  /A_T !		CaAM
!	y(4)=0.  /A_T !		AM

		call odeint(y,x1,x2,eps,hstart,0.0_sp,derivs,stiff)
 		write(DATA_FILE,'(/1x,a,t30,i3)') 'Successful steps:',nok
		write(DATA_FILE,'(1x,a,t30,i3)') 'Bad steps:',nbad
		write(DATA_FILE,*) 'Y(END)=',y(1),y(2),y(3)
		do i=1,kount
			write (DATA_FILE,10)xp(i),yp(:,i)
		enddo

	close(DATA_FILE,iostat=ios)

	10 format(<n+1>EN15.4)
	END PROGRAM xstiff
