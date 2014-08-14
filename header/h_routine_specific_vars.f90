module h_routine_specific_vars

	type four_step_type
		real :: kon_1,koff_1,kon_2,koff_2,f_plus,g_minus,g_plus,Ca
!		real :: f_plus_0,f_plus_M
	end type
	type three_step_type
!		real :: kon0,koff0,f0,g0
		real :: nu,nu2,alpha
		real :: Ca,u,v,w,x,z
		real :: w2 ! for RU influence on g
		real :: kon,koff,f,g
!		real :: f_plus_0,f_plus_M
		logical :: fixed ! if true then Ca does not vary with time

!	used in the c-spline to calculate Ca(t)
		real,dimension(:),pointer :: break ! dim = ndata
		real,dimension(:,:),pointer :: cscoef ! dim = 4,ndata
		integer :: nintv ! = ndata-1

! used for isotonic contraction
		real,dimension(3) :: y_save  ! used in isotonic contraction as in ktr
		real,dimension(2) :: y_save2 ! used for isotonic twitch
		real :: y_target,t1,t2
		real :: g_save
	end type


	
	type(four_step_type),pointer :: fsv ! four step variable
	type(three_step_type),pointer :: tsv ! three step variable


! for the ca_gr_2 titration routine


!	real,parameter :: Ka1=2.480E6 !Ca-EGTA
!	real,parameter :: Ka2=2.719E1 !Mg-EGTA

	real,parameter :: Ka1=2.480E6 !Ca-EGTA 20degrees,pH=7.0
	real,parameter :: Ka2=2.719E1 !Mg-EGTA



! Concentrations
	real :: CaT,MgT,EGTAT ! set in the main loop


contains
	subroutine init_tsv()

		tsv%nu=1.0
		tsv%nu2=1.0
		tsv%alpha = 0.0  ! NEM-S1
		tsv%w2=1.0
	end subroutine

end module