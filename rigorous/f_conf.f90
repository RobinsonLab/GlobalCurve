function f_conf(chi_sqr_min,ndeg_1,ndeg_2,conf)

use numerical_libraries
implicit none

	real,intent(in) :: chi_sqr_min
	integer,intent(in) :: ndeg_1,ndeg_2
	real,optional :: conf
	real :: f_conf

! Local Vars

	real :: num,den,f_stat
	real :: w_conf = 0.68269 
					!	0.68269 ! 1 std. of the normal dist.
! Begin

if (present(conf) ) w_conf = conf ! ie 1 standard deviation
	
	num=real(ndeg_1)
	den=real(ndeg_2)
	
! see Bevington

	f_stat=fin(w_conf,num,den)
	f_conf = chi_sqr_min*(1.0 + num/den*f_stat)

return

end function f_conf
