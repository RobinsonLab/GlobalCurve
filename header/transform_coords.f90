module transform_coords
implicit none

	real, parameter :: z0=-0.04233
	real, parameter :: z1=27.50001
	real, parameter :: r=15.80322
	real, parameter :: theta0=0.00460
	real, parameter :: theta1=3.38318
	! note in fortran matrix storage is B(1,1), B(2,1),...B(1,2),B(2,2),...
	real,parameter,dimension(3,3) :: B=(/0.5659955, 1.9258935E-02, 0.8241832, &    
										0.7988217, 0.2343239, -0.5540544, &    
										-0.2037962, 0.9719678, 0.1172419/)  
  
  ! so z points outward, x point towards the outer domian and y points
  ! up towards DNAse loop

end module