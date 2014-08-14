function calc_A_M(current_model,current_data,n) result(species)
	use h_struct
	USE nrtype
	USE nr
	USE ode_path
	use h_routine_specific_vars
	implicit none	

	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	integer,intent(in) :: n ! number of equations
	real,dimension(current_data%len,n) :: species




! constants
!	integer(i4b),parameter :: DATA_FILE=5
	real(sp),parameter :: t1=0.0
! the extra computational effort to go from 2 secs to 20 secs is trivial
!	6 more steps than the 133 to get to 2 secs.
	real(sp),parameter :: t2=20.0
	real(sp),parameter :: NA_eps=1.0E-4
	real(sp),parameter :: hstart=1.0E-8
	real(sp),parameter :: A_T=1.54E-4 !Ferenczi et al., 1984
	real(sp),parameter :: M_T=A_T

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
	REAL(SP), DIMENSION(n) :: y
	real(sp) :: pCa
	integer :: ios,i,j


! begin

! if you want to save off the time course then set: save_steps = .true.
	!	save_steps = .true. 
	fsv%kon_1=param_list(current_model%model%param(1)%param_basis(1))%val
	fsv%kon_2=param_list(current_model%model%param(1)%param_basis(2))%val
	fsv%koff_1=param_list(current_model%model%param(1)%param_basis(3))%val
	fsv%koff_2=param_list(current_model%model%param(1)%param_basis(4))%val
	fsv%f_plus=param_list(current_model%model%param(1)%param_basis(5))%val
!	fsv%f_plus_M=param_list(current_model%model%param(1)%param_basis(6))%val
	fsv%g_plus=param_list(current_model%model%param(1)%param_basis(6))%val
	fsv%g_minus=param_list(current_model%model%param(1)%param_basis(7))%val

! Assign initial conditions. 
! Note, we are scaling all concentrations to the total concentration of Actin
		y(1)=A_T /A_T !		A
		y(2)=0.  /A_T !		CaA
		y(3)=0.  /A_T !		CaAM
		y(4)=0.  /A_T !		AM	



!	y(1)=0.0332468
!	y(2)=0.6612511
!	y(3)=0.3051195
!	y(4)=3.829869e-4

!	kon_1=3.9E7
!	kon_2=40.*kon_1
!	koff_1=19.6
!	koff_2=koff_1
!	f_plus=0.95
!	g_plus=2.04
!	g_minus=15.0

	do i=1,current_data%len
		pCa=current_data%x_pts(i)
		fsv%Ca=10.**-pCa ! Ca is a global variable
		call odeint(y,t1,t2,NA_eps,hstart,0.0_sp,derivs,stiff)
		species(i,:)=y(:)
		if (save_steps) then
! need to work out the out_file_name stuff

!			out_file_name=root+blah(i)
!			open(unit = DATA_FILE,file=out_file_name, &
!				status='unknown',iostat=ios)
!			write(DATA_FILE,*) 'pCa= ',pCa
!			write(DATA_FILE,*) 'Y(END)=',y(:)
!			do j=1,kount
!				write (DATA_FILE,10)xp(j),yp(:,j)
!			enddo
!			close(DATA_FILE,iostat=ios)
		endif
	end do !i


	10 format(<n+1>EN15.4)
end function
