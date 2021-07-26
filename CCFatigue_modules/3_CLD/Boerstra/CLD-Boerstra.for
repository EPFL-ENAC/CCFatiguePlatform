	Program Boerstra
	      
	integer i,j,ierr
      
	real asigma(10000),m(10000),msigma(10000)
	real N(10000),Sap(10000),SAAp(10000),DeltaSa(10000)
	real DeltaN(10000),Deltat(10000),Smf(10000),Saf(10000)
	real mf(10000),R(10000),Sapf(10000),maxsigma(10000)
	real Sapmod(10000),Ne(10000),K1,K2,SDtmin,Dmin,NOC
	real UTS,UCS,NOD,sum,SApav,aT,aC,D,m0,Deltatave,SDt,SumDeltat,Np
	real mr,SB,ONC(100),Npmin,aTmin,aCmin,m0min
	
	CHARACTER*40 NAME, NAM

      ! Enter the input file name and open it for reading
	!####################################################################
      OPEN (10, FILE ="input.txt") 
	p1=0
	p2=0
	p3=0
	p4=0
	i=0
	
	OPEN(100, FILE = 'staticvalue.txt') 
	READ(100,*) UCS,UTS,Rcr
	READ(100,*) ONC(1),ONC(2),ONC(3),ONC(4),ONC(5),ONC(6),ONC(7)
	CLOSE(UNIT=100)
	UCS=-UCS 
	
	do j=1,12
	read(10,*)
	end do

	do
	i=i+1
	READ(10,*,iostat=ierr) R(i),N(i),maxsigma(i) !N(i),asigma(i),msigma(i)!
            if (ierr /= 0) then
               i=i-1
               exit
            end if

		if (R(i).EQ.0) then
			i=i-1
			do j=1,9
				read(10,*)
			end do
			go to 20
		end if

		if (abs(R(i)).GT.1) then
		asigma(i)=(1-(1/R(i)))*maxsigma(i)/2
		msigma(i)=-(1+(1/R(i)))*maxsigma(i)/2
		else 
		asigma(i)=(1-R(i))*maxsigma(i)/2
		msigma(i)=(1+R(i))*maxsigma(i)/2
		end if
20	end do 
	NOD=i

	!Multiple Linear Regression to calculate u,v and f
	!####################################################################

	OPEN (20, FILE = 'output.txt') 	
	Np=100
	m0=10
	D=100
	aT=0.1
	aC=0.1
	SDtmin=100
	counter=0
	Npmin=100
	Dmin=100
	m0min=1
	aCmin=0.1
	aTmin=0.1
		
	
	!#########################################################################################
	
10	do D=100,1000,1
	SumDeltat=0
	SumSAP=0
	sum=0
	
	do j=1,NOD
	m(j)=m0*exp(-msigma(j)/D)
	Sap(j)=asigma(j)*(N(j)/Np)**(1/m(j))
	if (msigma(j).GT.0) then
	if (msigma(j).GT.UTS) then
	msigma(j)=0.99*UTS
	end if
	SAAp(j)=Sap(j)/(1-(msigma(j)/UTS)**aT)
	else
	SAAp(j)=Sap(j)/(1-(msigma(j)/UCS)**aC)
	end if

	end do
	
	do j=1,NOD
	SumSAP=SumSAP+SAAp(j)
	end do
	SApav=SumSAP/NOD
	
	do j=1, NOD
	if (msigma(j).GT.0) then
	if (msigma(j).GT.UTS) then
	msigma(j)=0.99*UTS
	end if
	Sapmod(j)=SApav*(1-(msigma(j)/UTS)**aT)
	else
	Sapmod(j)=SApav*(1-(msigma(j)/UCS)**aC)
	end if

	DeltaSa(j)=log(Sap(j))-log(Sapmod(j))
	Ne(j)=Np*(Sapmod(j)/asigma(j))**m(j)
	DeltaN(j)=log(N(j))-log(Ne(j))
	K1=DeltaSa(j)
	K2=(1/((1/DeltaSa(j)**2)+(1/DeltaN(j)**2)))**0.5
	Deltat(j)=sign(K2,K1)
	end do
	
	do j=1,NOD
	SumDeltat=SumDeltat+Deltat(j)
	end do
	Deltatave=SumDeltat/NOD
	
	do j=1,NOD
	sum=sum+(Deltat(j)-Deltatave)**2
	end do

	SDt=(sum/(NOD-1))**0.5

	if (SDt.LT.SDtmin) then
	SDtmin=SDt
	Dmin=D
	end if
	SumDeltat=0
	SumSAP=0
	sum=0
	end do
	D=Dmin
	
	
	
	!#########################################################################################
	do m0=1,20,0.05
	
	do j=1,NOD
	m(j)=m0*exp(-msigma(j)/D)
	Sap(j)=asigma(j)*(N(j)/Np)**(1/m(j))
	if (msigma(j).GT.0) then
	if (msigma(j).GT.UTS) then
	msigma(j)=0.99*UTS
	end if
	SAAp(j)=Sap(j)/(1-(msigma(j)/UTS)**aT)
	else
	SAAp(j)=Sap(j)/(1-(msigma(j)/UCS)**aC)
	end if

	end do
	
	SumSAP=0
	do j=1,NOD
	SumSAP=SumSAP+SAAp(j)
	end do
	SApav=SumSAP/NOD
	
	do j=1, NOD
	if (msigma(j).GT.0) then
	if (msigma(j).GT.UTS) then
	msigma(j)=0.99*UTS
	end if
	Sapmod(j)=SApav*(1-(msigma(j)/UTS)**aT)
	else
	Sapmod(j)=SApav*(1-(msigma(j)/UCS)**aC)
	end if
	DeltaSa(j)=log(Sap(j))-log(Sapmod(j))
	Ne(j)=Np*(Sapmod(j)/asigma(j))**m(j)
	DeltaN(j)=log(N(j))-log(Ne(j))
	K1=DeltaSa(j)
	K2=(1/((1/DeltaSa(j)**2)+(1/DeltaN(j)**2)))**0.5
	Deltat(j)=sign(K2,K1)
	end do
	
	SumDeltat=0
	do j=1,NOD
	SumDeltat=SumDeltat+Deltat(j)
	end do
	Deltatave=SumDeltat/NOD

	sum=0
	do j=1,NOD
	sum=sum+(Deltat(j)-Deltatave)**2
	end do

	SDt=(sum/(NOD-1))**0.5

	if (SDt.LT.SDtmin) then
	SDtmin=SDt
	m0min=m0
	end if
	SumDeltat=0
	SumSAP=0
	sum=0
	end do
	m0=m0min
	!#########################################################################################
	do Np=1,1e7,1000

	do j=1,NOD
	m(j)=m0*exp(-msigma(j)/D)
	Sap(j)=asigma(j)*(N(j)/Np)**(1/m(j))
	if (msigma(j).GT.0) then
	if (msigma(j).GT.UTS) then
	msigma(j)=0.99*UTS
	end if
	SAAp(j)=Sap(j)/(1-(msigma(j)/UTS)**aT)
	else
	SAAp(j)=Sap(j)/(1-(msigma(j)/UCS)**aC)
	end if

	end do
	
	SumSAP=0
	do j=1,NOD
	SumSAP=SumSAP+SAAp(j)
	end do
	SApav=SumSAP/NOD
	
	do j=1, NOD
	if (msigma(j).GT.0) then
	if (msigma(j).GT.UTS) then
	msigma(j)=0.99*UTS
	end if
	Sapmod(j)=SApav*(1-(msigma(j)/UTS)**aT)
	else
	Sapmod(j)=SApav*(1-(msigma(j)/UCS)**aC)
	end if
	DeltaSa(j)=log(Sap(j))-log(Sapmod(j))
	Ne(j)=Np*(Sapmod(j)/asigma(j))**m(j)
	DeltaN(j)=log(N(j))-log(Ne(j))
	K1=DeltaSa(j)
	K2=(1/((1/DeltaSa(j)**2)+(1/DeltaN(j)**2)))**0.5
	Deltat(j)=sign(K2,K1)
	end do
	
	SumDeltat=0
	do j=1,NOD
	SumDeltat=SumDeltat+Deltat(j)
	end do
	Deltatave=SumDeltat/NOD

	sum=0
	do j=1,NOD
	sum=sum+(Deltat(j)-Deltatave)**2
	end do

	
	SDt=(sum/(NOD-1))**0.5

	if (SDt.LT.SDtmin) then
	SDtmin=SDt
	Npmin=Np
	end if
	SumDeltat=0
	SumSAP=0
	sum=0
	end do
	Np=Npmin
	
	!#########################################################################################
	do aC=0.05,5,0.01

	do j=1,NOD
	m(j)=m0*exp(-msigma(j)/D)
	Sap(j)=asigma(j)*(N(j)/Np)**(1/m(j))
	if (msigma(j).GT.0) then
	if (msigma(j).GT.UTS) then
	msigma(j)=0.99*UTS
	end if
	SAAp(j)=Sap(j)/(1-(msigma(j)/UTS)**aT)
	else
	SAAp(j)=Sap(j)/(1-(msigma(j)/UCS)**aC)
	end if

	end do
	
	SumSAP=0
	do j=1,NOD
	SumSAP=SumSAP+SAAp(j)
	end do
	SApav=SumSAP/NOD
	
	do j=1, NOD
	if (msigma(j).GT.0) then
	if (msigma(j).GT.UTS) then
	msigma(j)=0.99*UTS
	end if
	Sapmod(j)=SApav*(1-(msigma(j)/UTS)**aT)
	else
	Sapmod(j)=SApav*(1-(msigma(j)/UCS)**aC)
	end if

	DeltaSa(j)=log(Sap(j))-log(Sapmod(j))
	Ne(j)=Np*(Sapmod(j)/asigma(j))**m(j)
	DeltaN(j)=log(N(j))-log(Ne(j))
	K1=DeltaSa(j)
	K2=(1/((1/DeltaSa(j)**2)+(1/DeltaN(j)**2)))**0.5
	Deltat(j)=sign(K2,K1)
	end do
	
	SumDeltat=0
	do j=1,NOD
	SumDeltat=SumDeltat+Deltat(j)
	end do
	Deltatave=SumDeltat/NOD
	
	sum=0
	do j=1,NOD
	sum=sum+(Deltat(j)-Deltatave)**2
	end do

	SDt=(sum/(NOD-1))**0.5
	if (SDt.LT.SDtmin) then
	SDtmin=SDt
	aCmin=aC
	end if
	SumDeltat=0
	SumSAP=0
	sum=0
	end do
	aC=aCmin
	!#########################################################################################

	do aT=0.05,5,0.01

	do j=1,NOD
	m(j)=m0*exp(-msigma(j)/D)
	Sap(j)=asigma(j)*(N(j)/Np)**(1/m(j))
	if (msigma(j).GT.0) then
	if (msigma(j).GT.UTS) then
	msigma(j)=0.99*UTS
	end if
	SAAp(j)=Sap(j)/(1-(msigma(j)/UTS)**aT)
	else
	SAAp(j)=Sap(j)/(1-(msigma(j)/UCS)**aC)
	end if

	end do
	
	do j=1,NOD
	SumSAP=SumSAP+SAAp(j)
	end do
	SApav=SumSAP/NOD
	
	do j=1, NOD
	if (msigma(j).GT.0) then
	if (msigma(j).GT.UTS) then
	msigma(j)=0.99*UTS
	end if
	Sapmod(j)=SApav*(1-(msigma(j)/UTS)**aT)
	else
	Sapmod(j)=SApav*(1-(msigma(j)/UCS)**aC)
	end if
	DeltaSa(j)=log(Sap(j))-log(Sapmod(j))
	Ne(j)=Np*(Sapmod(j)/asigma(j))**m(j)
	DeltaN(j)=log(N(j))-log(Ne(j))
	K1=DeltaSa(j)
	K2=(1/((1/DeltaSa(j)**2)+(1/DeltaN(j)**2)))**0.5
	Deltat(j)=sign(K2,K1)
	end do
	
	SumDeltat=0
	do j=1,NOD
	SumDeltat=SumDeltat+Deltat(j)
	end do
	Deltatave=SumDeltat/NOD

	sum=0
	do j=1,NOD
	sum=sum+(Deltat(j)-Deltatave)**2
	end do

	SDt=(sum/(NOD-1))**0.5
	!WRITE(20,*) SDt
	if (SDt.LT.SDtmin) then
	SDtmin=SDt
	aTmin=aT
	end if
	SumDeltat=0
	SumSAP=0
	sum=0
	end do
	aT=aTmin
	!#########################################################################################


	counter=counter+1
	if (counter.LE.20) then
	go to 10
	end if
		
	do t=1,7
	do sm=UCS,UTS,(UTS-UCS)/60
	mr=m0*exp(-sm/D)
	if (sm.GT.0) then
	SB=SApav*(1-(sm/UTS)**aT)
	else
	SB=SApav*(1-(sm/UCS)**aC)
	end if
	sa=SB*((Np/ONC(t))**(1/mr))
	WRITE(20,*) ONC(t),sa,sm 
	end do
	sm=UTS
	sa=0
	WRITE(20,*) ONC(t),sa,sm 
	
	end do
		
	CLOSE(UNIT=20)
	CLOSE(UNIT=10)

	END 
	
	