module h_utils
interface

! ---- distr

! gaus

function gaus_dist(r)
	implicit none
	real,intent(in) :: r
	real :: gaus_dist
end function gaus_dist

subroutine calc_gaus_mean_hw(current_model,mean,hw,TE)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	real,dimension(2),intent(out) :: mean,hw,TE
end subroutine calc_gaus_mean_hw

function calc_1_mom_gaus(r)
	implicit none
	real,intent(in) :: r
	real :: calc_1_mom_gaus
end function calc_1_mom_gaus

function calc_2_mom_gaus(r)
	implicit none
	real,intent(in) :: r
	real :: calc_2_mom_gaus
end function calc_2_mom_gaus

function gaus_mean_TE(r)
	implicit none
	real,intent(in) :: r
	real :: gaus_mean_TE
end function gaus_mean_TE

! gamma

function gamma_dist(r)
	implicit none
	real,intent(in) :: r
	real :: gamma_dist
end function gamma_dist

subroutine calc_gamma_mean_hw(current_model,mean,hw,TE)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	real,dimension(2),intent(out) :: mean,hw,TE
end subroutine calc_gamma_mean_hw

function calc_1_mom_gamma(r)
	implicit none
	real,intent(in) :: r
	real :: calc_1_mom_gamma
end function calc_1_mom_gamma

function calc_2_mom_gamma(r)
	implicit none
	real,intent(in) :: r
	real :: calc_2_mom_gamma
end function calc_2_mom_gamma

function gamma_mean_TE(r)
	implicit none
	real,intent(in) :: r
	real :: gamma_mean_TE
end function gamma_mean_TE



! ^^^^ distr

function gaussian(current_model,x,extra_params) result(y)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	real,dimension(:),intent(in),optional :: extra_params
	real,intent(in) :: x
	real :: y
end function


FUNCTION bisect(func,current_model,extra_params,x1,x2)
! used by numerical integration solver routine
	use h_struct
	IMPLICIT NONE
	type(model_struct), pointer :: current_model
	real,dimension(:),intent(in) :: extra_params
	REAL, INTENT(IN) :: x1,x2
	REAL :: bisect
	INTERFACE
		FUNCTION func(current_model,x,extra_params)
			use h_struct
			IMPLICIT NONE
			type(model_struct), pointer :: current_model
			real,dimension(:),intent(in),optional :: extra_params
			REAL, INTENT(IN) :: x
			REAL :: func
		END FUNCTION func
	END INTERFACE
end function

function make_out_file_name(str1,str2,str3) result (res)
 	implicit none
	character(*), intent(in) :: str1
	character(*), intent(in) :: str2
	character(len(str1)),intent(out) :: str3
	integer :: res
end function make_out_file_name

function append_string(str1,str2,str3) result(res)
 	implicit none
	character(*), intent(in) :: str1
	character(*), intent(in) :: str2
	character(*),intent(out) :: str3
	integer :: res
end function append_string


subroutine normalize_alpha(alpha,num)
	implicit none
	real,dimension(:),intent(inout) :: alpha
	real,intent(out) :: num
end subroutine normalize_alpha

subroutine error_handler(str1,str2)
	implicit none
	character(*), intent(in) :: str1
	character(*),optional, intent(in) :: str2
end subroutine error_handler

function trim_string(str)
	use h_params
	implicit none
	character(STRING_LEN), intent(in) :: str
	character(STRING_LEN) :: trim_string
end function trim_string

function nitems(line) result(number)
	implicit none
	character(*),intent(in) :: line
	integer :: number
end function

!function simple_convolute(f_1,f_2) result(f_out)
!	implicit none
!	real,dimension(:),intent(in) :: f_1
!	real,dimension(size(f_1)),intent(in) :: f_2
!	real,dimension(size(f_1)) :: f_out
!end function simple_convolute

function derivative(lamp) result(d_lamp)
	! take derivative of data using central difference method
	implicit none
	real,dimension(:),intent(in) :: lamp
	real,dimension(size(lamp)) :: d_lamp
end function derivative

subroutine round(qs,num,delta)
	implicit none
	real,intent(in) :: qs
	integer,intent(out) :: num
	real, intent(out) :: delta
end subroutine round

!function gaus_dist(N,rmin,dr,params)
!	use h_params ! PI
!	implicit none
!	integer,intent(in) :: N
!	real,intent(out) :: rmin,dr
!	real,dimension(:) :: params
!	real,dimension(N) :: gaus_dist
!end function

!function gamma_dist(N,rmin,dr,params)
!	use h_params ! PI
!	implicit none
!	integer,intent(in) :: N
!	real,intent(out) :: rmin,dr
!	real,dimension(:) :: params
!	real,dimension(N) :: gamma_dist
!end function

function harmonic(N,rmin,dr,params)
	implicit none
	integer,intent(in) :: N
	real,intent(in) :: rmin,dr
	real,dimension(:) :: params
	real,dimension(N) :: harmonic
end function

!subroutine calc_gamma_distrn(rzero,x0,s,lambda,N,rmin,dr,Nnot,rsix)
!	implicit none
!	real,intent(in) :: rzero,x0,s,lambda,rmin,dr
!	integer,intent(in) :: N
!	real,dimension(N),intent(out) :: Nnot,rsix
!end subroutine

subroutine calc_first_order_anharmonic(N,rmin,dr,param_ptr,Nnot)
	implicit none
	integer,intent(in) :: N
	real,intent(in) :: rmin,dr
	integer,pointer,dimension(:) :: param_ptr
	real,dimension(:) :: Nnot
end subroutine

!function calc_rsix(N,rmin,dr,rzero) result(rsix)
!	implicit none
!	integer,intent(in) :: N
!	real,intent(in) :: rmin,dr,rzero
!	real,dimension(N) :: rsix
!end function

subroutine dump_raw(len,x_pts,species,g)
	implicit none
	integer, intent(in) :: len
	real,dimension(len), intent(in) :: g,x_pts
	real,dimension(len,3), intent(in) :: species
end subroutine

function get_ca(t)
	use h_routine_specific_vars
	use numerical_libraries, only: csval
	implicit none
	real,intent(in) :: t
	real :: get_ca
end function

end interface
end module h_utils


