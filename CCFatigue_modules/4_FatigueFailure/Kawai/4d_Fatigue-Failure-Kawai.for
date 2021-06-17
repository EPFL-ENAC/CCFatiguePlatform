	Program KW
      
	integer i,j,k,m

	real Rangle,RRratio,Dangle,DRratio,RTheta,Theta
	real TenAxStr,ComAxStr,TenTraStr,ComTraStr,Shestr
	real PI,ROmega, Omega,Rstaticstr
	integer NOR,NOD(100),NN
	real A,B,P,Q,R(100),PN(100),slevel(100,10000),RSigma(100,10000)
	real asigma(100,10000),N(100,10000),X(100,10000),Y(100,10000)
	real SPN,nstar


	CHARACTER*40 NAME, NAM

      ! Enter the input file name and open it for reading
	!####################################################################
          

	OPEN (10, FILE = 'input.txt') 
	OPEN (20, FILE = 'output.txt')
	
	OPEN(100, FILE = 'Refdata.txt') 
	READ(100,*) Rangle,RRratio,Rstaticstr,Dangle,DRratio
	READ(100,*) TenAxStr,TenTraStr,Shestr
	CLOSE(UNIT=100)

	
	PI=3.14159265
	RTheta=(Rangle/180)*PI
	Theta=(Dangle/180)*PI
	if ((abs(RRratio).GE.0).AND.(abs(RRratio).LT.1)) then
		ROmega=(cos(RTheta)**4/TenAxStr**2+
     &	sin(RTheta)**4/TenTraStr**2+
     &    (sin(RTheta)*cos(RTheta))**2*(1/Shestr**2-1/TenAxStr**2))**0.5
	endif
	if ((abs(DRratio).GE.0).AND.(abs(DRratio).LT.1)) then
		Omega=(cos(Theta)**4/TenAxStr**2+
     &	sin(Theta)**4/TenTraStr**2+
     &    (sin(Theta)*cos(Theta))**2*(1/Shestr**2-1/TenAxStr**2))**0.5
	endif		
	
	
	!WRITE(20,*) ROmega,Omega
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
		RSigma(j,i)=(0.5*(1-RRratio)*(asigma(j,i)/Rstaticstr))/
     &	(1-0.5*(1+RRratio)*(asigma(j,i)/Rstaticstr))
		X(j,i)=log10(RSigma(j,i))
		Y(j,i)=log10(2*N(j,i))
		!WRITE(*,*) X(j,i),Y(j,i)
		end do
	end do

	!write(*,*)NOD(1:NOR)
	
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
		
		nstar=-(Yb-NOD(j))/Xb
		!nstar=14.97
		
	! Calculating and writing the fitted S-N curve based on found parameters
	! ##########################################################################
	WRITE(20,*) '0' !Seperator
	WRITE(20,*) R(j)
	WRITE(20,*) 50
	WRITE(20,*) nstar
	WRITE(20,*) Omega
	WRITE(20,*) '0' !af
	WRITE(20,*) '0' !So
	WRITE(20,*) '0' !Pwer
	WRITE(20,*) '0' !af
	WRITE(20,*) '0' !B
	WRITE(20,*) '0' !S
	WRITE(20,*) '0' !C
!	"Data for 50% Reliability level"
	
	do NN=1,1000,50
	Sigma=(2*NN)**(-1/nstar)
	SPN=(2*Sigma)/(Omega*((1-DRratio)+(1+DRratio)*Sigma))
	WRITE(20,*) R(j),NN,SPN
	end do

	NN=1000
	Sigma=(2*NN)**(-1/nstar)
	SPN=(2*Sigma)/(Omega*((1-DRratio)+(1+DRratio)*Sigma))
	WRITE(20,*) R(j),NN,SPN
	
	do NN=10000,2000000,10000
	Sigma=(2*NN)**(-1/nstar)
	SPN=(2*Sigma)/(Omega*((1-DRratio)+(1+DRratio)*Sigma))
	WRITE(20,*) R(j),NN,SPN
	end do
	
	do NN=2000000,20000000,1000000
	Sigma=(2*NN)**(-1/nstar)
	SPN=(2*Sigma)/(Omega*((1-DRratio)+(1+DRratio)*Sigma))
	WRITE(20,*) R(j),NN,SPN
	end do    
	
	end do

	CLOSE(UNIT=10)  
	CLOSE(UNIT=20) 
	end    
