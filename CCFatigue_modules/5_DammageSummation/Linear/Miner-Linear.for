	Program Linear
      
	integer i,j,ierr
	real K,P,Q,m,NOD(1000),Sigma0,A,B,Xb,Yb,Sm,Sa,AA,BB,USm
      real maxsigma(100,1000),asigma(100,1000),msigma(100,1000)	
	real X(1000),Y(1000),ONC(100),N(100,1000),R(100)
	real nn,NNN,Smean,RR,HSF,LSF,STEP,Fact,Cumulative,Damage,Samp


	CHARACTER*40 NAME, NAM

      ! Enter the input file name and open it for reading
	!####################################################################
          

	OPEN (10, FILE = 'input.txt') 
	
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
		
		
	!Find A and B
	! ##########################################################################	
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
	K=1/BB
	Sigma0=AA
		
10	CLOSE(UNIT=10)	
	
	
	!Miner part
	! ##########################################################################	
	OPEN (30, FILE = 'CCinput.txt')
	OPEN (40, FILE = 'Factor.txt')
	OPEN (50, FILE = 'Miner.txt')
	READ (40,*) HSF, LSF, STEP
	do Fact=HSF, LSF, -STEP
		Damage=0
	do
	READ (30,*,iostat=ierr) Range, Smean, RR, nn, Cumulative
            if (ierr /= 0) then
               exit
            end if
			Smean=Fact*Smean
			Range=Fact*Range
			Stlev=Smean+Range/2
			if (MaxStlev.LT.Stlev) then
				MaxStlev=Stlev
			end if
			if (RR.NE.-1) then
				Samp=Smean*((1-RR)/(1+RR))
			else
				Samp=Range/2
			end if
			NNN=((Samp+(abs(Smean)/Sigma0))/Sigma0)**(-K)
			Damage=Damage+(nn/NNN)
		!WRITE (50, *) Range/Fact,nn/NNN
	end do
		WRITE (50, *) MaxStlev, (1/Damage)
		Rewind(30)
	MaxStlev=0
	end do
	CLOSE(UNIT=30)	
	CLOSE(UNIT=40)
	CLOSE(UNIT=50)
	
	END 
	
	