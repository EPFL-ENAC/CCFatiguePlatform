	Program Kawai
      
!	USE NUMERICAL_LIBRARIES
!	USE IMSLF90
	integer i,j,m,ierr
	real ONC(100),N(100,10000),R(100),NOD(100)
      real maxsigma(100,10000),asigma(100,10000),msigma(100,10000)	
	real UTS,UCS,CII(10000),X(10000),Y(10000)
	real Sa,Sm,maxs,ms,as,K1,K2,p1,sigmaB 
	real Range,Smean,RR,nn,NNN,Cumulative,Fact,HSF,LSF,STEP
	real(8) jj,Damage

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
	
	sigmaB=(abs(UCS)+UTS)/2
	
10	CLOSE(UNIT=10)
	
	! Miner part
	!####################################################################
	OPEN (30, FILE = 'CCinput.txt')
	OPEN (40, FILE = 'Factor.txt')
	OPEN (50, FILE = 'Miner.txt')
	READ (40,*) HSF, LSF, STEP
	do Fact=HSF, LSF, -STEP
	Damage=0.0
	jj=1.0
	Sa=10000.0
	write(*,*)Fact
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
		Samp=Smean*((1-RR)/(1+RR))

		do while ((jj.LT.1000).AND.(Sa.GE.Samp)) 
			maxs=(AA)*(jj**(-BB))
			if (abs(Rcr).GT.1) then
				as=(1-(1/Rcr))*maxs/2
				ms=-(1+(1/Rcr))*maxs/2
			else 
				as=(1-Rcr)*maxs/2
				ms=(1+Rcr)*maxs/2
			end if
			CIII=maxs/sigmaB
			K1=Smean-ms
			K2=UCS-ms
			Sa=as-as*(K1/K2)**(2-CIII)
				
			jj=jj+1
		end do


		do while ((jj.GE.1000).AND.(jj.LE.10E5).AND.(Sa.GE.Samp)) 
			maxs=(AA)*(jj**(-BB))
			if (abs(Rcr).GT.1) then
				as=(1-(1/Rcr))*maxs/2
				ms=-(1+(1/Rcr))*maxs/2
			else 
				as=(1-Rcr)*maxs/2
				ms=(1+Rcr)*maxs/2
			end if
			CIII=maxs/sigmaB
			K1=Smean-ms
			K2=UCS-ms
			Sa=as-as*(K1/K2)**(2-CIII)
				
			jj=jj+1
		end do		
		
		do while ((jj.GE.10E5).AND.(jj.LT.10E8).AND.(Sa.GE.Samp)) 
			maxs=(AA)*(jj**(-BB))
			if (abs(Rcr).GT.1) then
				as=(1-(1/Rcr))*maxs/2
				ms=-(1+(1/Rcr))*maxs/2
			else 
				as=(1-Rcr)*maxs/2
				ms=(1+Rcr)*maxs/2
			end if
			CIII=maxs/sigmaB
			K1=Smean-ms
			K2=UCS-ms
			Sa=as-as*(K1/K2)**(2-CIII)
				
			jj=jj+1000
		end do		
		
		do while ((jj.GE.10E8).AND.(jj.LT.10E12).AND.(Sa.GE.Samp)) 
			maxs=(AA)*(jj**(-BB))
			if (abs(Rcr).GT.1) then
				as=(1-(1/Rcr))*maxs/2
				ms=-(1+(1/Rcr))*maxs/2
			else 
				as=(1-Rcr)*maxs/2
				ms=(1+Rcr)*maxs/2
			end if
			CIII=maxs/sigmaB
			K1=Smean-ms
			K2=UCS-ms
			Sa=as-as*(K1/K2)**(2-CIII)
				
			jj=jj+10E6
		end do		
		
		do while ((jj.GT.10E12).AND.(jj.LT.10E14).AND.(Sa.GE.Samp)) 
			maxs=(AA)*(jj**(-BB))
			if (abs(Rcr).GT.1) then
				as=(1-(1/Rcr))*maxs/2
				ms=-(1+(1/Rcr))*maxs/2
			else 
				as=(1-Rcr)*maxs/2
				ms=(1+Rcr)*maxs/2
			end if
			CIII=maxs/sigmaB
			K1=Smean-ms
			K2=UCS-ms
			Sa=as-as*(K1/K2)**(2-CIII)
				
			jj=jj+10E10
		end do	
		
		do while ((jj.GT.10E14).AND.(jj.LT.10E16).AND.(Sa.GE.Samp)) 
			maxs=(AA)*(jj**(-BB))
			if (abs(Rcr).GT.1) then
				as=(1-(1/Rcr))*maxs/2
				ms=-(1+(1/Rcr))*maxs/2
			else 
				as=(1-Rcr)*maxs/2
				ms=(1+Rcr)*maxs/2
			end if
			CIII=maxs/sigmaB
			K1=Smean-ms
			K2=UCS-ms
			Sa=as-as*(K1/K2)**(2-CIII)
				
			jj=jj+10E12
		end do			
		
		do while ((jj.GT.10E16).AND.(jj.LT.10E20).AND.(Sa.GE.Samp)) 
			maxs=(AA)*(jj**(-BB))
			if (abs(Rcr).GT.1) then
				as=(1-(1/Rcr))*maxs/2
				ms=-(1+(1/Rcr))*maxs/2
			else 
				as=(1-Rcr)*maxs/2
				ms=(1+Rcr)*maxs/2
			end if
			CIII=maxs/sigmaB
			K1=Smean-ms
			K2=UCS-ms
			Sa=as-as*(K1/K2)**(2-CIII)
				
			jj=jj+10E14
		end do

		do while ((jj.GT.10E20).AND.(jj.LT.10E25).AND.(Sa.GE.Samp)) 
			maxs=(AA)*(jj**(-BB))
			if (abs(Rcr).GT.1) then
				as=(1-(1/Rcr))*maxs/2
				ms=-(1+(1/Rcr))*maxs/2
			else 
				as=(1-Rcr)*maxs/2
				ms=(1+Rcr)*maxs/2
			end if
			CIII=maxs/sigmaB
			K1=Smean-ms
			K2=UCS-ms
			Sa=as-as*(K1/K2)**(2-CIII)
				
			jj=jj+10E18
		end do

		Damage=Damage+nn/jj
		!WRITE (50, *) Range/Fact,nn,jj
		jj=1.0
		Sa=10000.0
				
		end do
		WRITE(50,*) MaxStlev,1/Damage
		MaxStlev=0
		Rewind(30)
	end do

	CLOSE(UNIT=30)	
	CLOSE(UNIT=40)
	CLOSE(UNIT=50)
	
	
	END 
	
	