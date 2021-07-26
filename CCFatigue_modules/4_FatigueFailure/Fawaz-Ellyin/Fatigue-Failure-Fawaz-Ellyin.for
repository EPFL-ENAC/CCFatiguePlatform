	Program EWE
      
!	USE NUMERICAL_LIBRARIES
!	USE IMSLF90
	integer i,j
	real m,b,mr,br,f,g,NN,SPN
	real Rangle,RRratio,Rstaticstr,Dangle,DRratio,Dstaticstr



	CHARACTER*40 NAME, NAM,SNmodel

      ! Enter the input file name and open it for reading
	!####################################################################
          

	OPEN (10, FILE = 'input.txt') 
	OPEN (20, FILE = 'output.txt')
	OPEN(100, FILE = 'Refdata.txt') 
	
	READ(100,*) SNmodel
	READ(100,*) Rangle,RRratio,Rstaticstr
	READ(100,*) Dangle,DRratio,Dstaticstr
	CLOSE(UNIT=100)

	if (SNmodel.EQ.'Lin-Log') then
		f=Dstaticstr/Rstaticstr
	else
		f=log10(Dstaticstr)/log10(Rstaticstr)
	endif
	
	if ((DRratio.EQ.RRratio).OR.(DRratio.LE.0)) then
		g=1
	elseif (DRratio.EQ.1) then
			g=0
	else
		g=abs((1-DRratio)/(1-RRratio))
	endif
	
	do i=1,3
		read(10,*)
	end do
	READ (10,*) br
	READ (10,*) mr
	if (SNmodel.EQ.'Lin-Log') then
		b=f*br
		m=f*g*mr
	else
		b=10**(f*log10(br))
		m=f*g*mr	
	endif

	

	! Calculating and writing the predicted S-N curve based on found parameters
	! ##########################################################################
	WRITE(20,*) '0' !Seperator
	WRITE(20,*) DRratio
	WRITE(20,*) 50
	WRITE(20,*) b
	WRITE(20,*) m
	WRITE(20,*) '0' !af
	WRITE(20,*) '0' !So
	WRITE(20,*) '0' !Pwer
	WRITE(20,*) '0' !af
	WRITE(20,*) '0' !B
	WRITE(20,*) '0' !S
	WRITE(20,*) '0' !C

!	"Data for 50% Reliability level"
	if (SNmodel.EQ.'Lin-Log') then
		do NN=1,1000,50
			SPN=(b)+(log10(NN)*m)
			WRITE(20,*) DRratio,NN,SPN
		end do

		NN=1000
		SPN=(b)+(log10(NN)*m)
		WRITE(20,*) DRratio,NN,SPN
	
		do NN=10000,2e6,10000
			SPN=(b)+(log10(NN)*m)
			WRITE(20,*) DRratio,NN,SPN
		end do
	
		do NN=2e6,20e6,1000000
			SPN=(b)+(log10(NN)*m)
			WRITE(20,*) DRratio,NN,SPN
		end do    
	endif

	if (SNmodel.EQ.'Log-Log') then
		do NN=1,1000,50
			SPN=b*NN**(-m)
			WRITE(20,*) DRratio,NN,SPN
		end do

		NN=1000
		SPN=b*NN**(-m)
		WRITE(20,*) DRratio,NN,SPN
	
		do NN=10000,2e6,10000
			SPN=b*NN**(-m)
			WRITE(20,*) DRratio,NN,SPN
		end do
	
		do NN=2e6,20e6,1000000
			SPN=b*NN**(-m)
			WRITE(20,*) DRratio,NN,SPN
		end do    
	endif
	
	CLOSE(UNIT=10)  
      CLOSE(UNIT=20) 
	End    
