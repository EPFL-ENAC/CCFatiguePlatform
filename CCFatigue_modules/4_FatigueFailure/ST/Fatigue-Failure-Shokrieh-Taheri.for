	Program ST
      
	integer i,j,k,m,ierr

	real Rangle,RRratio,Dangle,DRratio,RTheta,Theta
	real TenAxStr,ComAxStr,TenTraStr,ComTraStr,Shestr
	real PI,RTransform,Transform
	integer NOR,NOD(100),NN
	real A,B,P,Q,R(100),PN(100),slevel(100,10000),DW(100,10000)
	real asigma(100,10000),N(100,10000),X(100,10000),Y(100,10000)
	real SPN


	CHARACTER*40 NAME, NAM

      ! Enter the input file name and open it for reading
	!####################################################################
          

	OPEN (10, FILE = 'input.txt') 
	OPEN (20, FILE = 'output.txt')
	
	OPEN(100, FILE = 'Refdata.txt') 
	READ(100,*) Rangle,RRratio,Dangle,DRratio
	READ(100,*) TenAxStr,ComAxStr,TenTraStr,ComTraStr,Shestr
	CLOSE(UNIT=100)

	
	PI=3.14159265
	RTheta=(Rangle/180)*PI
	Theta=(Dangle/180)*PI
	if ((RRratio.GE.0).AND.(RRratio.LT.1)) then
		RTransform=cos(RTheta)**4/TenAxStr**2+
     &	sin(RTheta)**4/TenTraStr**2+
     &    (sin(RTheta)*cos(RTheta))**2/Shestr**2
	endif

	if (RRratio.GE.1) then
		RTransform=cos(RTheta)**4/ComAxStr**2+
     &	sin(RTheta)**4/ComTraStr**2+
     &    (sin(RTheta)*cos(RTheta))**2/Shestr**2
	endif		
	
	if ((DRratio.GE.0).AND.(DRratio.LT.1)) then
		Transform=cos(Theta)**4/TenAxStr**2+
     &	sin(Theta)**4/TenTraStr**2+
     &    (sin(Theta)*cos(Theta))**2/Shestr**2
	endif
	
	if (DRratio.GE.1) then
		Transform=cos(Theta)**4/ComAxStr**2+
     &	sin(Theta)**4/ComTraStr**2+
     &    (sin(Theta)*cos(Theta))**2/Shestr**2
	endif		
	!WRITE(20,*) RTransform,Transform
	!WRITE(20,*) TenAxStr,ComAxStr,TenTraStr,ComTraStr,Shestr
      ! ##########################################################################
	m=0
	k=0
	NOR=1
	do  
            m=m+1
	    read(10,*,iostat=ierr) R(NOR),PN(NOR),slevel(NOR,m),
     &                             asigma(NOR,m),N(NOR,m)
            if (ierr /= 0) then
               m=m-1
               exit
            end if
	    if (m.GE.2) then
	    if (PreSR.NE.R(NOR)) then
		OldR=PreSR
		PreSR=R(NOR)
		R(NOR)=OldR
		NOR=NOR+1
		NOD(NOR-1)=m-1
		
		slevel(NOR,1)=slevel(NOR-1,m)
		asigma(NOR,1)=asigma(NOR-1,m)
		N(NOR,1)=N(NOR-1,m) 
		m=1
		go to 20
	    end if
	    end if
	
	    PreSR=R(NOR)
20	end do 
	NOD(NOR)=m
	do j=1,NOR
		do i=1,NOD(j)
		DW(j,i)=(1-RRratio**2)*asigma(j,i)**2*RTransform
		X(j,i)=log10(DW(j,i))
		Y(j,i)=log10(N(j,i))
	        !WRITE(20,*) asigma(j,i), DW(j,i)
		end do
	end do
	
	!Find A and B
	! ##########################################################################	
	do j=1,NOR
		Yb=0
		Xb=0
		P=0
		Q=0
		do i=1,NOD(j)
			Yb=Yb+Y(j,i)
			Xb=Xb+X(j,i)
		end do
		Xb=Xb/NOD(j)
		Yb=Yb/NOD(j)
		
		do i=1,NOD(j)
			P=P+((X(j,i)-Xb)*(Y(j,i)-Yb))
			Q=Q+(X(j,i)-Xb)**2
		end do

		B=P/Q
		A=Yb-B*Xb
		
		AA=10**(-A/B)
		BB=-1/B
		!AA=0.3675
	! Calculating and writing the fitted S-N curve based on found parameters
	! ##########################################################################
	WRITE(20,*) '0' !Seperator
	WRITE(20,*) R(j)
	WRITE(20,*) 50
	WRITE(20,*) AA
	WRITE(20,*) BB
	WRITE(20,*) '0' !af
	WRITE(20,*) '0' !So
	WRITE(20,*) '0' !Pwer
	WRITE(20,*) '0' !af
	WRITE(20,*) '0' !B
	WRITE(20,*) '0' !S
	WRITE(20,*) '0' !C

	
		do NN=1,1000,50
			SPN=((AA*NN**(-BB))/((1-DRratio**2)*Transform))**0.5
			WRITE(20,*) R(j),NN,SPN
		end do

		NN=1000
		SPN=((AA*NN**(-BB))/((1-DRratio**2)*Transform))**0.5
		WRITE(20,*) R(j),NN,SPN
	
		do NN=10000,2000000,10000
			SPN=((AA*NN**(-BB))/((1-DRratio**2)*Transform))**0.5
			WRITE(20,*) R(j),NN,SPN
		end do
	
		do NN=2000000,20000000,1000000
			SPN=((AA*NN**(-BB))/((1-DRratio**2)*Transform))**0.5
			WRITE(20,*) R(j),NN,SPN
		end do    
	
	end do

	CLOSE(UNIT=10)  
	CLOSE(UNIT=20) 
	end    
