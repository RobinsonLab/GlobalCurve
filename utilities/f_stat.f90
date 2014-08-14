function f_conf(chi_sqr,ndeg_1,ndeg_2,conf)

use numerical_libraries
implicit none

	real,intent(in) :: chi_sqr
	integer,intent(in) :: ndeg_1,ndeg_2
	real,optional,intent(in) :: conf
	real :: fconf

! Local Vars

	real :: num,den
	
! Begin

if (.not. present(conf) ) conf=0.68269 ! ie 1 standard deviation
	
	num=real(ndeg_1)
	den=real(ndeg_2)
	
! as given by straume and johnson in topics in fluorescence spec.
	
	fconf = chi_sqr_min*(1.+(num/den)*fin(conf,num,den) )

return


