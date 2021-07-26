	Program Kawai
      
	integer i,j,m,ierr
	real ONC(100),N(100,10000),R(100),NOD(100)
      real maxsigma(100,10000),asigma(100,10000),msigma(100,10000)	
	real UTS,UCS,CII(10000),X(10000),Y(10000)
	real Sa,Sm,maxs,ms,as,K1,K2,p1,sigmaB 

	CHARACTER*40 NAME, NAM

      ! Enter the input file name and open it for reading
	!####################################################################
      
      OPEN (10, FILE = 'input.txt') 
	OPEN (20, FILE = 'output.txt') 

	!"GIVE THE CRITICAL STRESS RATIO, static values and N "
	OPEN(100, FILE = 'staticvalue.txt') 
	READ(100,*) UCS,UTS,Rcr
	READ(100,*) ONC(1),ONC(2),ONC(3),ONC(4),ONC(5),ONC(6),ONC(7)
	CLOSE(UNIT=100)

	m=0
	k=0
	NOR=1
	
	do i=1,12
	read(10,*)
	end do

	do 
	    m=m+1
	    read(10,*,iostat=ierr) R(NOR),N(NOR,m),maxsigma(NOR,m)
            if (ierr /= 0) then
               m=m-1
               exit
            end if
		
		if (R(NOR).EQ.0) then
			R(NOR)=PreSR
			NOD(NOR)=m-1
			m=0
			NOR=NOR+1
			do i=1,9
			read(10,*)
			end do
		go to 20
		end if
	PreSR=R(NOR)
20	end do 
	NOD(NOR)=m

	do j=1,NOR
		if (R(j).EQ.Rcr) then
		RcrN=j
		checkmark=1
		end if
	end do
	if (checkmark.EQ.0)	then 
		WRITE(20,*) 'There is no data for critical R-value'
		go to 10
	end if
	
	!####################################################################
	do i=1,NOD(RcrN)
	asigma(RcrN,i)=(1-R(RcrN))*maxsigma(RcrN,i)/2
	msigma(RcrN,i)=(1+R(RcrN))*maxsigma(RcrN,i)/2
	X(i)=log10(maxsigma(RcrN,i))
	Y(i)=log10(N(RcrN,i))
	end do
	Yb=0
	Xb=0
	do i=1,NOD(RcrN)
		Yb=Yb+Y(i)
		Xb=Xb+X(i)
	end do
	Xb=Xb/NOD(RcrN)
	Yb=Yb/NOD(RcrN)
	P=0
	Q=0
	do i=1,NOD(RcrN)
		P=P+((X(i)-Xb)*(Y(i)-Yb))
		Q=Q+(X(i)-Xb)**2
	end do

	B=P/Q
	A=Yb-B*Xb
	AA=10**(-A/B)
	BB=-1/B
	!####################################################################
	
	sigmaB=(abs(UCS)+UTS)/2
	p1=0

		do i=1,7
			maxs=(AA)*(ONC(i)**(-BB))
			if (abs(Rcr).GT.1) then
				as=(1-(1/Rcr))*maxs/2
				ms=-(1+(1/Rcr))*maxs/2
			else 
				as=(1-Rcr)*maxs/2
				ms=(1+Rcr)*maxs/2
			end if
			CII(i)=maxs/sigmaB
			do Sm=UCS,ms-(ms-UCS)/100,(ms-UCS)/30
				p1=p1+1
				K1=Sm-ms
				K2=UCS-ms
				Sa=as-as*(K1/K2)**(2-CII(i))
				WRITE(20,*) ONC(i),Sa,-Sm
			end do
		
			do Sm=ms,UTS,(UTS-ms)/30
				K1=Sm-ms
				K2=UTS-ms
				Sa=as-as*(K1/K2)**(2-CII(i))
				if (abs(UTS-Sm).LT.abs((UTS-ms)/1000)) then
				Sa=0
				end if
				WRITE(20,*) ONC(i),Sa,Sm
			end do
		end do
			
10	CLOSE(UNIT=20)
	CLOSE(UNIT=10)
	
	END 
	
	