	Program PNL
      
	integer i,j,ierr,m
	real k1,k2,k3,p1,p2,p3,p4,Rin,NOR,CL,Nu,Rs(10,1000),NofC,sa
      real maxsigma(10,1000),asigma(10,1000),Rtest(1000)
	real msigma(10,1000),N(10,1000),R(10,1000),X(10,1000),Y(10,1000)
	real UTS,UCS,Nmid,maxsmid,maxasmid,maxmsmid,Rmid
	real ONC(1000),NOD,Xb,Yb,P,Q,A(10),B(10),AN,NR,R1,R2,R3,RR
	real Sm1,Sinf,S0, A1(20),B1(20),A2(20),B2(20),A3(20),B3(20)
	real A4(20),B4(20),Samp,Smean
	
     
      CHARACTER*40 NAME, NAM

      ! Enter the input file name and open it for reading
      
      OPEN (10, FILE = 'input.txt') 
	OPEN (20, FILE = 'output.txt') 	
	OPEN (200, FILE = 'Temp-input.txt') 

	!"GIVE THE CRITICAL STRESS RATIO, static values and N "
	OPEN(100, FILE = 'staticvalue.txt') 
	READ(100,*) UCS,UTS,Rcr
	READ(100,*) NR,R1,R2,R3
	READ(100,*) ONC(1),ONC(2),ONC(3),ONC(4),ONC(5),ONC(6),ONC(7)
	CLOSE(UNIT=100)
	UCS=-UCS
	
	if ((NR.NE.2).AND.(NR.NE.3)) then
		WRITE(20,*) 'Number of R-ratios is not correct!'
		go to 10
	end if
	
	if (R1.NE.-1) then
		WRITE(20,*) 'First R-ratio has to be -1!'
		go to 10
	end if
	
	if ((R2.NE.1000).AND.((R2.LT.1).AND.(R2.GT.-1))) then
		WRITE(20,*) 'Second R-ratio is not correct!'
		go to 10
	end if
	
	if ((R3.NE.0).AND.((R3.LT.-1).AND.(R3.GT.1))) then
		WRITE(20,*) 'Third R-ratio is not correct!'
		go to 10
	end if

	!Contructing the R matrix (Rtest) and temporary input
	!#####################################################
	do j=1,12
		read(10,*)
	end do
	
	i=1
	m=1
	READ(10,*) Rtest(i),Value1,Value2
	PreR=Rtest(i)
	Write(200,*) Rtest(i),Value1,Value2
	
	do
	    m=m+1
	    READ(10,*,iostat=ierr) Rtest(i),Value1,Value2
            if (ierr /= 0) then
               m=m-1
               exit
            end if
		
		if (Rtest(i).eq.0) then
			do j=1,9
				read(10,*)
			end do
			Rtest(i)=PreR
			m=0
			i=i+1
			go to 20
		end if
		PreR=Rtest(i)
		Write(200,*) Rtest(i),Value1,Value2	
20	end do
	
	NOR=i		!number of R
	NOD=m		!number of data at each R
	
	CLOSE(UNIT=10)
	CLOSE(UNIT=200)
	!#####################################################

	OPEN (10, FILE = 'Temp-input.txt') 	
      
	do i=1,NOR
			
		if (Rtest(i).EQ.R1) then	!R1=-1 
			do m=1,NOD 
				READ(10,*) SR,N(1,m),maxsigma(1,m)
				asigma(1,m)=(1-R1)*maxsigma(1,m)/2
				msigma(1,m)=(1+R1)*maxsigma(1,m)/2			
			end do
		end if

		if (Rtest(i).EQ.R2) then	!R2=+-infinity or R>1 or R<-1 
			do m=1,NOD 
				READ(10,*) SR,N(2,m),maxsigma(2,m)
				asigma(2,m)=(1-(1/R2))*maxsigma(2,m)/2
				msigma(2,m)=-(1+(1/R2))*maxsigma(2,m)/2
			end do
		end if

		
		if (Rtest(i).EQ.R3) then	!R3=0 or -1<R <1 
			do m=1,NOD 
				READ(10,*) SR,N(3,m),maxsigma(3,m)
				asigma(3,m)=(1-R3)*maxsigma(3,m)/2
				msigma(3,m)=(1+R3)*maxsigma(3,m)/2
			end do
		end if		
		
		if ((Rtest(i).NE.R1).AND.(Rtest(i).NE.R2).AND.(Rtest(i).NE.R3))
     &	then													!Other R
			do m=1,NOD				
				READ(10,*) SR,N(i+3,m),maxsigma(i+3,m)
				if (abs(Rtest(i)).GT.1) then
					asigma(i+3,m)=(1-(1/Rtest(i)))*maxsigma(i+3,m)/2
					msigma(i+3,m)=-(1+(1/Rtest(i)))*maxsigma(i+3,m)/2
				else
					asigma(i+3,m)=(1-Rtest(i))*maxsigma(i+3,m)/2
					msigma(i+3,m)=(1+Rtest(i))*maxsigma(i+3,m)/2
				end if
			end do
		end if
	end do	
		
	!##############################################
	!Calculation of constants of Log(N)=A+B*Log(S)
	 !index 1:R=-1  
	 !index 2:R2=+-infinity or R>1 or R<-1
	 !index 3:R3=0 or -1<R <1
	!##############################################
	do i=1,3
		Xb=0
		Yb=0
		P=0
		Q=0
		do j=1,NOD,1
			X(i,j)=log10(asigma(i,j))
			Y(i,j)=log10(N(i,j))
			Yb=Yb+Y(i,j)
			Xb=Xb+X(i,j)
		end do
		Xb=Xb/NOD
		Yb=Yb/NOD
		do j=1,NOD
			P=P+((X(i,j)-Xb)*(Y(i,j)-Yb))
			Q=Q+(X(i,j)-Xb)**2
		end do
		B(i)=P/Q	  
		A(i)=Yb-B(i)*Xb	
	end do
	!WRITE(20,*) A(1),A(2),A(3),B(1),B(2),B(3)
	!###############################################
	!Output	
	!###############################################
 	
	do t=1,7,1
		WRITE(20,*) ONC(t),0,UTS
		Sm1=10**((log10(ONC(t))-A(1))/B(1))	  !R1=-1
		Sinf=10**((log10(ONC(t))-A(2))/B(2))  !R2=+-infinity or R2>1 or R2<-1
		S0=10**((log10(ONC(t))-A(3))/B(3))    !R3=0 or -1<R3<1

		!calculation of A1,B1, A4, B4
		!****************************

		if (R2.EQ.1000) then	!standard boundary condition
			A1(t)=-Sinf
			A4(t)=-Sinf
			B1(t)=Sm1/2-Sinf
			B4(t)=UCS/2+Sinf
		else if (R2.GT.1) then	!R2>1
			A4(t)=((Sinf/(1-R2))-(UCS/(2*R2**2)))/(1/R2-1/R2**2)
			B4(t)=UCS/2-A4(t)
			A1(t)=A4(t)
			B1(t)=Sm1/2+A1(t)
		else					!R2<-1
			A1(t)=((Sinf/(1-R2))-(Sm1/(2*R2**2)))/(1/R2+1/R2**2)
			B1(t)=Sm1/2+A1(t)
			A4(t)=A1(t)
			B4(t)=UCS/2-A4(t)				
		end if

		!calculation of A2,B2, A3, B3
		!****************************
		if (NR.EQ.2) then			!2 R-ratios
			A2(t)=1/UTS-1/Sm1
			A3(t)=1/UTS-1/Sm1
			B2(t)=1/UTS+1/Sm1
			B3(t)=B2(t)
		else						!3 R-ratios
			if (R3.EQ.0) then		!standard boundary condition R3=0
				A2(t)=1/S0-2/Sm1
				A3(t)=2/UTS-1/S0
				B2(t)=1/S0
				B3(t)=B2(t)	
			else if ((R3.GT.0).AND.(R3.LT.1)) then	!0<R3<1
				A3(t)=((1-R3)/S0-2/UTS)/(R3**3-1)
				B3(t)=2/UTS-A3(t)
				B2(t)=B3(t)
				A2(t)=B2(t)-2/Sm1	
			else									!-1<R3<0
				A2(t)=((1-R3)/S0-2/Sm1)/(1+R3)
				B2(t)=2/Sm1+A2(t)
				B3(t)=B2(t)
				A3(t)=(2/UTS)-B3(t)			
			end if			
		end if
		

		!Writing the output file
		!****************************	
		!Part III
		do RR=0.95,0.05,-0.05
			Samp=(1-RR)/(A3(t)*RR**3+B3(t))
			Smean=(1+RR)/(1-RR)*Samp
			WRITE(20,*) ONC(t),Samp,Smean
		end do

		!Part II
		do RR=-0.05,-1,-0.05
			Samp=(1-RR)/(A2(t)*RR+B2(t))
			Smean=(1+RR)/(1-RR)*Samp
			WRITE(20,*) ONC(t),Samp,Smean
		end do	
	
		!Part I
		do RR=-1,-3,-0.5
			Samp=(1-RR)*(A1(t)/RR+B1(t)/RR**2)
			Smean=(1+RR)/(1-RR)*Samp
			WRITE(20,*) ONC(t),Samp,Smean
		end do		

		do RR=-4,-30,-1
			Samp=(1-RR)*(A1(t)/RR+B1(t)/RR**2)
			Smean=(1+RR)/(1-RR)*Samp
			WRITE(20,*) ONC(t),Samp,Smean
		end do	
		
		!Part IV
		do RR=30,3,-1
			Samp=(1-RR)*(A4(t)/RR+B4(t)/RR**2)
			Smean=(1+RR)/(1-RR)*Samp
			WRITE(20,*) ONC(t),Samp,Smean
		end do			
	
		do RR=2.95,1.05,-0.05
			Samp=(1-RR)*(A4(t)/RR+B4(t)/RR**2)
			Smean=(1+RR)/(1-RR)*Samp
			WRITE(20,*) ONC(t),Samp,Smean
		end do			
	! ##########################################################################
		WRITE(20,*) ONC(t),0,UCS
		!WRITE(20,*) A1(t),A2(t),A3(t),A4(t),B1(t),B2(t),B3(t),B4(t)
		!WRITE(20,*) Sinf,(1-10)*(A4(t)/10+B4(t)/10**2)		
	end do
10	R=0
	CLOSE(UNIT=20)
	CLOSE(UNIT=10)


	END 
	
	