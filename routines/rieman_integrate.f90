function rieman_integrate(fcn,N,dr) result (res)
! calculates: integral[fcn,{r,rmin,rmax}] where fcn is an array of length N
! where fcn is evaluated at N equally spaced points in the interval rmin
! to rmax
	implicit none
	real,pointer,dimension(:) :: fcn
	real,intent(in) :: dr
	integer,intent(in) :: N
	real :: res

! Begin
res = sum(fcn)*dr
return
end function rieman_integrate