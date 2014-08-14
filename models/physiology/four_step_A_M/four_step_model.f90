	SUBROUTINE four_step_derivs(t,x,f)
	USE nrtype
	use h_routine_specific_vars
		! intent(out): f_plus

	IMPLICIT NONE
	REAL(SP), INTENT(IN) :: t
	REAL(SP), DIMENSION(:), INTENT(IN) :: x
	REAL(SP), DIMENSION(:), INTENT(OUT) :: f
!	real(sp) :: p_0
	
!	p_0=x(3)+x(4) ! here we assume that sum(x)=1 i.e. x's are normalized
!	fsv%f_plus=(1.0-p_0)**2*fsv%f_plus_0 + &
!		p_0*(1.0-p_0)*(fsv%f_plus_0+fsv%f_plus_M) + &
!		p_0**2*fsv%f_plus_M
	f(1)=-fsv%kon_1*fsv%Ca*x(1) + fsv%koff_1*x(2) + fsv%g_minus*x(4)
	f(2)= fsv%kon_1*fsv%Ca*x(1) - (fsv%koff_1+fsv%f_plus)*x(2) + fsv%g_plus*x(3)
	f(3)= fsv%f_plus*x(2) - (fsv%g_plus+fsv%koff_2)*x(3) + fsv%kon_2*fsv%Ca*x(4)
	f(4)= fsv%koff_2*x(3) - (fsv%g_minus+fsv%kon_2*fsv%Ca)*x(4)

	END SUBROUTINE 

	SUBROUTINE four_step_jacobn(t,x,dfdt,dfdx)
	USE nrtype
	use h_routine_specific_vars
	IMPLICIT NONE
	REAL(SP), INTENT(IN) :: t
	REAL(SP), DIMENSION(:), INTENT(IN) :: x
	REAL(SP), DIMENSION(:), INTENT(OUT) :: dfdt
	REAL(SP), DIMENSION(:,:), INTENT(OUT) :: dfdx
	dfdt(:)=0.0

	dfdx(1,1)=-fsv%kon_1*fsv%Ca
	dfdx(1,2)=fsv%koff_1
	dfdx(1,3)=0.0
	dfdx(1,4)=fsv%g_minus

	dfdx(2,1)=fsv%kon_1*fsv%Ca
	dfdx(2,2)=-fsv%koff_1-fsv%f_plus
	dfdx(2,3)=fsv%g_plus
	dfdx(2,4)=0.0

	dfdx(3,1)=0.0
	dfdx(3,2)=fsv%f_plus
	dfdx(3,3)=-fsv%g_plus-fsv%koff_2
	dfdx(3,4)=fsv%kon_2*fsv%Ca

	dfdx(4,1)=0.0
	dfdx(4,2)=0.0
	dfdx(4,3)=fsv%koff_2
	dfdx(4,4)=-fsv%g_minus-fsv%kon_2*fsv%Ca

	END SUBROUTINE