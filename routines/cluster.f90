function cluster() result(res)
! rewritten from Timmer, G.T.
! f77 code available from: ftp://ftp.jate.u-szeged.hu/pub/math/optimization/fortran



	use h_struct
	use h_vars 
	use h_utils, only: error_handler
	use numerical_libraries, only: rnun 
	implicit none

	integer :: res
	! h_vars,intent(in)
	!	integer,intent(in) :: num_free_global_param 
	!	real,intent(in) :: global_reduced_chi_sqr
	!	integer,intent(in) :: num_iter

! params
	integer,parameter :: arr_len=100

! vars
	
	real,dimension(arr_len) :: f,f0,fcl,f1
!	real,dimension(:),allocatable :: min_param,max_param
	real,dimension(:,:),allocatable :: x,x0,x1,x2,xcl
	real,dimension(:),allocatable :: y,r,w,param

	real,dimension(1) :: temp
	integer,dimension(arr_len) :: ic1
	integer,dimension(arr_len) :: ic

	integer :: num_param
	integer :: i,j,i1,ii,iii,icc,inum2,in1,it,iv,nfe1
	integer :: nfe,ng,ns,nc,ncp,n1,im,ig,maxfn
	integer :: n0,nm
	integer :: n100,ng0,nn100,ng10,n,inum,inum1,icj
	integer :: index,index2

	real :: a
	real :: d,b1,alfa,ff
	real :: fm,b,bb,fc

! begin

	num_param=num_free_global_param 
	if (num_param <= 0) call error_handler('Too few global parameters')


	d=2.*sqrt(float(num_param))*10E-6
	b1=1./float(num_param)

! I inserted this: |||
	n100=100*num_param
	ng0=10
! |||

	NN100 = arr_len
	n=num_param


!	allocate(min_param(num_param))
!	allocate(max_param(num_param))
	allocate(Y(num_param))
	allocate(param(num_param))
	allocate(x(num_param,arr_len))
	allocate(r(n100))
	allocate(W(num_param))



	NG10 = 100
	F(:) =	9.9E10
	IC(:) = 0

	ALFA = .01
	NFE = 0
	NG = 0
	NS = 0
	NC = 0
	NCP = 1
	N0 = 0
	N1 = 0
	IM = 1 ! points to the location of the maximum value in array f
	IG = 0
	FM = 9.9E10
	MAXFN = 500*num_param
!      RELCON = TEN**(-NSIG)



! ****************
!	SAMPLING
! ****************

! values:
! n100=n*nn100
! n = num_param
! nn100 = 100


grand_loop: do
	N0 = N0+N100
	NM = N0-1
	NG = NG+NG0
	NS = NS+1
	if (NS*NG0.GT.100) call error_handler('blah')
	B	= (1.-ALFA**(1./FLOAT(NM)))**B1
	BB = 0.1*B
	do I1=1,N
		call rnun(n100,r)	
!		r=2.0*r-1.0
		do j=1,arr_len
!			Y(:)=2.0*r(j*num_param+1:(j+1)*num_param)-1.0
	!	scale the params	
			index=(j-1)*num_param+1
			index2=index+(num_param-1)
			y=r(index:index2)
!			param=param_scale(y)
!			call update_param(param)
			param_list(map_from(:))%val=param_scale(y)			
			call calc_chi_sqr()
			fc=global_reduced_chi_sqr
		    IF (FC >= FM) cycle
			F(IM) = FC
			x(:,IM)=Y(:)
		    IF ((IM <= NG) .AND. (IC(IM) > 0)) IG = IG-1
			IC(IM) = 0
			temp=maxloc(f)
			im=temp(1)
			fm=f(im)

!			IM = 1
!			FM = F(1)
!			do i=2,NG10
!				if (F(I) >= FM) then
!					IM = I
!					FM = F(I)
!				endif
!			enddo !i
		enddo !j
	enddo ! i1
    NFE = NFE+N100
!      WRITE(IPR,901) N100
	WRITE(*,901) N100
	901 FORMAT(/1X,I5,' FUNCTION EVALUATIONS USED FOR SAMPLING')

! **************************
!	SORTING
! **************************

! values:
! ng10 = 100

	INUM = NG10-1
	DO I=1,INUM
		IM = I
		FM = F(I)
		INUM1 = I+1
		DO J=INUM1,NG10
			IF (F(J).GE.FM) cycle
			IM = J
			FM = F(J)
		enddo
		IF (IM.LE.I) cycle
		A = FM
		y(:)=x(:,im)
		IF (I.GT.NG.OR.IM.LE.NG) then
		! do nothing
		else
			IF (IC(NG).EQ.0.AND.IC(IM).GT.0) IG = IG+1
			IF (IC(NG).GT.0.AND.IC(IM).EQ.0) IG = IG-1
		endif
		ICC = IC(IM)
		INUM1 = IM-I
		DO J=1,INUM1
			INUM2 = IM-J
			F(INUM2+1) = F(INUM2)
			IC(INUM2+1)	= IC(INUM2)
			x(:,inum2+1)=x(:,inum2)
		enddo
		F(I) =	A
		x(:,i)=y(:)
		IC(I) = ICC
	enddo !i

! IF (NC > 0) then

! **************************
!	CLUSTERING TO	X*
! **************************

	do iii=1,nc
		i = 1
		IN1 = i
		FCL(i)	= F0(iii)
		xcl(:,i) = x0(:,iii)
		do j=1,NG
			if (IC(J)==III) then
				IN1 = IN1+1
				xcl(:,in1)=x(:,j)
			endif
		enddo !j
		do
			do	j=1,NG
			    IF (IC(J).NE.0) cycle
				IF (FCL(I).GE.F(J))	cycle
				W(:) = ABS(XCL(:,I)-X(:,J))
				A=maxval(W)
				if (A.GE.B)	cycle
		        WRITE(*,902) III
				W(:)=param_scale(x(:,j))
				WRITE (*,903) F(J),(W(II), II=1,num_param)
			    IG = IG+1
			    IF (IG.GE.NG) exit grand_loop
				IN1	= IN1+1
				FCL(IN1) = F(J)
				xcl(:,in1)=x(:,j)
				IC(J) = iii
			enddo !j
			i = i+1
			IF (I > IN1) exit
		enddo
	enddo !iii

  902	    FORMAT(' SAMPLE POINT ADDED TO THE CLUSTER NO. ',I2)
  903	    FORMAT(1X,G14.8,3(/4X,5(G14.8,1X)))

!    IF (N1 > 0) then

! **************************
!	CLUSTERING TO	X1
! **************************
		do III=1,N1
			I = 1
			IN1 = I
			FCL(I)	= F1(III)
			xcl(:,i)=x1(:,iii)
			do
				do J=1,NG
					IF (IC(J).NE.0) cycle
					IF (FCL(I).GE.F(J))	cycle
					W(:) = ABS(XCL(:,I)-X(:,J))
					A=maxval(W)
					IF (A.GE.B)	cycle
					WRITE(*,902) IC1(III)
					W=param_scale(x(:,j))
					WRITE(*,903) F(J),(W(II), II=1,num_param)
					IG = IG+1
					IF (IG.GE.NG) exit grand_loop
					IN1	= IN1+1
					FCL(IN1) = F(J)
					XCL(:,IN1) = X(:,J)
					IC(J)=IC1(III)
				enddo !j
				I = I+1
				IF (I > IN1) exit
			enddo
		enddo !iii
!	endif
!endif

! --------------------------- Main Loop --------------------------

! ****************
!	LOCAL	SEARCH
! ****************

	IT = 0
	main_loop: do i1=1,ng
		IF (IC(I1).NE.0) cycle main_loop
		y(:)=x(:,i1)
		FF = F(I1)
		! update the param list values
		param_list(map_from(:))%val=param_scale(y)
		call minimize()
		! report function value and converged upon param values.
		y=rev_param_scale(param_list(map_from(:))%val)
		ff=global_reduced_chi_sqr
		nfe1=num_iter
		IF (NC > 0) then
			DO IV=1,NC
			w(:)=abs(x0(:,iv)-y(:))
			a=maxval(w)
		    IF (A.LT.BB) then
				! ****************
				! NEW SEED-POINT
				! ****************
!				new_seed_point:
				N1 = N1+1
				WRITE(*,905) IV,NFE1
				w=param_scale(x(:,i1))
				WRITE(*,903) FF,(W(II), II=1,num_param)
				IF (FF < F0(IV)) then
					WRITE(*,906) IV,F0(IV),FF	
					w=param_scale(y)
					WRITE(*,903) FF,(W(II), II=1,num_param)
					F0(IV)	= FF
					x0(:,iv)=y(:)
				endif
				if (N1 > 20) then
					WRITE(*,916)
					exit grand_loop
					!goto print results (395)
				endif
				x1(:,n1)=x(:,i1)
				xcl(:,1)=x(:,i1)
				F1(N1)	= F(I1)
				FCL(1)	= F(I1)
				IC1(N1) = IV
				ICJ = IV
				goto 10 !new_cluster
			endif
			enddo !iv
		endif

905	 FORMAT(' NEW SEED POINT ADDED TO THE CLUSTER NO. ',I2,', NFEV=',I5)
906	 FORMAT(' *** IMPROVEMENT ON THE LOCAL MINIMUM NO. ',I2,':',G14.8,' FOR ',G14.8)
916 FORMAT(' ***   TOO MANY NEW SEED POINTS')

! ****************
! NEW LOCAL MINIMUM
! ****************

!	290
		NC = NC+1
		NCP = NCP+1
		WRITE(*,907) NC,FF,NFE1
		w=param_scale(y)
		WRITE(*,903) FF,(W(II), II=1,num_param)
		x0(:,nc)=y(:)
		xcl(:,1)=y(:)
		FCL(1)	= FF
		F0(NC)	= FF
		if (NC.GE.20) then
			WRITE(*,917)
			exit grand_loop
		endif
		IT = 1
		ICJ = NC

907	 FORMAT(' *** THE LOCAL MINIMUM NO. ',I2,': ',G14.8,', NFEV=',I5)	
917 FORMAT(' ***   TOO MANY CLUSTERS')

! ****************
! CLUSTERING TO THE NEW POINT
! ****************
!	new_cluster:
 10		 NFE = NFE+NFE1
		IC(I1)	= ICJ
		IG = IG+1
		IF (IG.GE.NG) exit main_loop
		I = 1
		IN1 = I
		do
			do j=1,ng
				IF (IC(J).NE.0) cycle
				IF (FCL(I).GE.F(J)) cycle
				W(:) = ABS(XCL(:,I)-X(:,J))
				a=maxval(w)
				if (A.GE.B) cycle
				IN1 = IN1+1
				xcl(:,in1)=x(:,j)
				FCL(IN1) = F(J)
				IC(J) = ICJ
				WRITE(*,902) ICJ
				w(:)=param_scale(x(:,j))
				WRITE(*,903) F(J),(W(II), II=1,num_param)
				IG = IG+1
				IF (IG.GE.NG) exit main_loop
			enddo !j
			I=I+1
			IF (I.GE.IN1) exit
		enddo
	enddo main_loop
! -------------------------- End Main Loop ------------------------
	IF (IT==0) exit grand_loop
	enddo grand_loop
! -------------------------- End Grand Loop -----------------------



! ****************
! PRINT RESULTS
! ****************

! print_results:
	WRITE(*,908)
	IF (NC > 1) then
	    INUM = NC-1
		do i=1,inum
			IM = I
			FM = F0(I)
			INUM1 = I+1
			do j=inum1,nc
				IF (F0(J).GE.FM) cycle
				IM = J
				FM = F0(J)
			enddo !j
			IF (IM.LE.I) cycle
			A = FM
			y(:)=x0(:,im)
			INUM1 = IM-I
			do j=1,inum1
				INUM2 = IM-J
				F0(INUM2+1)	= F0(INUM2)
				x0(:,inum2+1)=x0(:,inum2)
			enddo !j
			F0(I) = A
			x0(:,i)=y(:)
		enddo !i
	endif
	IF (NC> 0) then
		DO I=1,NC
			X0(:,I)=param_scale(X0(:,I))
			WRITE(*,903) F0(I),(X0(II,I), II=1,num_param)
		enddo !i
	endif

	WRITE(*,911) NFE
	res=0
	RETURN

908 FORMAT(/////,' LOCAL MINIMA FOUND:'//)
911 FORMAT(///,' NORMAL TERMINATION AFTER ',I5,' FUNCTION ','EVALUATIONS',///)

contains


function rev_param_scale(x)
	use h_struct
	use h_vars
	implicit none
	real,dimension(:),intent(in) :: x
	real,dimension(size(x)) :: rev_param_scale

	! h_vars,intent(in)
	!	integer,dimension(num_free_global_param),intent(in) :: map_from
	!	type(param_struct),pointer,dimension(:) :: param_list
! vars

	real,dimension(size(x)) :: max_param,min_param,range
! begin
	max_param=param_list(map_from(:))%max	
	min_param=param_list(map_from(:))%min	
	range=max_param-min_param
	rev_param_scale=(x-min_param)/range

end function


function param_scale(x)
	use h_struct
	use h_vars
	implicit none
	real,dimension(:),intent(in) :: x
	real,dimension(size(x)) :: param_scale

	! h_vars,intent(in)
	!	integer,dimension(num_free_global_param),intent(in) :: map_from
	!	type(param_struct),pointer,dimension(:) :: param_list
! vars

	real,dimension(size(x)) :: max_param,min_param,range
! begin
	max_param=param_list(map_from(:))%max	
	min_param=param_list(map_from(:))%min	
	range=max_param-min_param
	param_scale=range*x+min_param

end function

end function cluster