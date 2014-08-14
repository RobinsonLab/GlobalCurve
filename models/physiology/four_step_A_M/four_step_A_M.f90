function four_step_A_M(kon_1,koff_1,kon_2,koff_2,f_minus,f_plus,g_minus,g_plus, &
		A_T,M_T,Ca,x) result(f)
!	use h_global_vars
	implicit none

	real,intent(in) :: kon_1,koff_1,kon_2,koff_2,f_minus,f_plus,g_minus,g_plus, &
						A_T,M_T,Ca
	real,dimension(5),intent(in) :: x
	real,dimension(5) :: f

! Begin


!	assume as Hill that we have one actin per one myosin

	f(1)=koff_1*x(3)+g_minus*x(5)-kon_1*Ca*x(1)-f_minus*x(1)*x(2)
	f(2)=kon_1*Ca*x(1)+g_plus*x(4)-koff_1*x(3)-f_plus*x(2)*x(3)
	f(3)=kon_2*Ca*x(5)+f_plus*x(2)*x(3)-(g_plus+koff_2)*x(4)
	f(4)=-A_T+x(1)+x(5)+x(3)+x(4)
	f(5)=-M_T+x(2)+x(5)+x(4)

end function