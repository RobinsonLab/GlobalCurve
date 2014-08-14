function simulate_ca_gr_titr(current_model,current_data) result(bind)
! use this function to calibrate your concentration of EGTA ([EGTA]_T)
! probably want to set tnc_lT, and thc_hT to near zero and grT low as well.
	use h_struct
	use h_routine_specific_vars
	use numerical_libraries, only: neqnf
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bind

interface
         subroutine fcn(x, f, n)
         real, intent(in) :: x(*)
         real, intent(out) :: f(*)
         integer, intent(in) :: n
         end subroutine
end interface

! params

integer,parameter :: n=5 ! dimension size of the problem 
!	real,parameter :: Ka=322580.6 !Ca-gr (pKd=5.511089)
! var's

real :: errel = 0.0001
integer :: itmax=200
real,dimension(n) :: x,xguess,fvec,xscale,fscale
real :: fnorm
!real :: frac,junk
real :: cao,conc,Ca
real :: vol1,vol2 !vol2=1200
real :: Fmin,Fmax,Y,Ka
integer :: i

! Begin


! these var's you might not know
EGTAT=param_list(current_model%model%param(1)%param_basis(1))%val
Cao=param_list(current_model%model%param(1)%param_basis(2))%val
Fmin=param_list(current_model%model%param(1)%param_basis(3))%val
Fmax=param_list(current_model%model%param(1)%param_basis(4))%val
Ka=param_list(current_model%model%param(1)%param_basis(5))%val   !~1E6
! these var's you should know
MgT=param_list(current_model%model%param(2)%param_basis(1))%val  ! .004
conc=param_list(current_model%model%param(2)%param_basis(2))%val ! 0.009
vol1=param_list(current_model%model%param(2)%param_basis(3))%val ! 6.5
vol2=param_list(current_model%model%param(2)%param_basis(4))%val ! 1200


! initial conditions


!	Ca=x(1)
!	Mg=x(2)
!	EGTA=x(3)
!	CaEGTA=x(4)
!	MgEGTA=x(5)

x(1)=1E-8
x(2)=MgT
x(3)=EGTAT
x(4)=0.0
x(5)=0.0

	
	do  i=0,current_data%len-1
		CaT=(cao*vol2+conc*i*vol1)/(vol2+vol1*i)
		xguess=x
		! get ca-gr concentrations
		CALL NEQNF (fcn, errel, n, itmax, xguess, x, fnorm)
		Ca=x(1)
		Y=Ka*Ca/(1.+Ka*Ca)
		bind(i+1)=Fmin + Fmax*Y

	enddo

end function


subroutine fcn(x, f, n)
	use h_routine_specific_vars ! constants (Ka's and ?T's)
	implicit none
	real,intent(in) :: x(*)
	real,intent(out) :: f(*)
	integer, intent(in) :: n



! Variables

	real :: Ca,Mg
	real :: EGTA
	real :: CaEGTA
	real :: MgEGTA


! Begin

	Ca=x(1)
	Mg=x(2)
	EGTA=x(3)
	CaEGTA=x(4)
	MgEGTA=x(5)


! various equilibria
	f(1)=CaEGTA - Ka1*Ca*EGTA
	f(2)=MgEGTA - Ka2*Mg*EGTA

	f(3)=Ca+CaEGTA-CaT
	f(4)=Mg+MgEGTA-MgT
	f(5)=EGTA+CaEGTA+MgEGTA-EGTAT

end subroutine
