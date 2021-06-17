	Program Rainflow Counting 
      
	real*8 x,y,range,mean,pp1,pp2,pp3,pp4,pp5,cc(100,100)
     	real*8 peak(1000000),ra(1000000),me(1000000),R(1000000)
	real*8 noc(1000000)
	real*8 p1(1000000),p2(1000000),p3(1000000),p4(1000000)
	real*8 p5(1000000)
	real*8 minrange,maxrange,minmean,maxmean,Deltarange,Deltamean
	real*8 Downra,Upra,Mra,Downme,Upme,Mme,MR,DR,UR,Sumcum,Cum
	real*8 halfrange
	integer i,j,ierr,matrixsize

     
        CHARACTER*40 NAME

      ! Enter the input file name and open it for reading        
	OPEN (10, FILE='input.txt')      
      
      ! Rainflow Counting 
      ! ##########################################################################
	
     	OPEN (20, FILE='output.txt')
	i=0
	starp=1
1	i=i+1
	do
		read(10,*,iostat=ierr) peak(i)
          	if (ierr /= 0) then
                  i=i-1
                  exit
                end if
2		if (i.LT.3) go to 1
		x=abs(peak(i)-peak(i-1))						
		y=abs(peak(i-1)-peak(i-2))
		if (x.LT.y) go to 1
      		if (starp.EQ.(i-2).or.starp.EQ.(i-1)) then
			halfrange=y
			mean=(peak(i-1)+peak(i-2))/2
			WRITE(20,*) halfrange, mean, peak(i-2), peak(i-1), 1 !0.5
			i=i-1
			peak(i-1)=peak(i)
			peak(i)=peak(i+1)
			go to 2
		end if
		mean=(peak(i-1)+peak(i-2))/2
		range=y
		WRITE(20,*) range, mean, peak(i-2), peak(i-1), 2 !1
		i=i-2
		peak(i)=peak(i+2)
		go to 2
	end do
	
	if (i.GT.1) then
		do j=1,i-2,1
			y=abs(peak(j+1)-peak(j))
			halfrange=y
			mean=(peak(j+1)+peak(j))/2
			WRITE(20,*) halfrange, mean, peak(i-2), peak(i-1), 1 !0.5
		end do
	end if
	CLOSE(UNIT=10)  
	CLOSE(UNIT=20) 
	
      ! Sorting output file
      ! ##########################################################################
	N=0
	OPEN (20, FILE='output.txt')
	do
	  N=N+1
	  read(20,*,iostat=ierr) p1(N),p2(N),p3(N),p4(N),p5(N)
          if (ierr /= 0) then
             exit
          end if
	end do   
	CLOSE(UNIT=20)
	
	OPEN (20, FILE='output.txt')	
	do k=1,N-1,1
	do j=k+1,N-1,1
		if (p1(k).GT.p1(j)) then
		pp1=p1(k);pp2=p2(k);pp3=p3(k);pp4=p4(k);pp5=p5(k)
		p1(k)=p1(j);p2(k)=p2(j);p3(k)=p3(j);p4(k)=p4(j);p5(k)=p5(j)
		p1(j)=pp1;p2(j)=pp2;p3(j)=pp3;p4(j)=pp4;p5(j)=pp5
		end if
	end do
	end do
	
	do j=1,N-1,1
		Write(20,*) p1(j),p2(j),p3(j),p4(j),p5(j)
	end do
	CLOSE(UNIT=20)  

      ! Creating 64*64 markov matrix
      ! ##########################################################################
	
	OPEN (20, FILE='output.txt')

	read(20,*) ra(1),me(1),p3(1),p4(1),noc(1)
	maxrange=ra(1)
	minrange=ra(1)
	maxmean=me(1)
	minmean=me(1)
	N=2
	do
		read(20,*,iostat=ierr) ra(N),me(N),p3(N),p4(N),noc(N)
		if (ierr /= 0) then
		   exit
		end if
		if (p4(N).GE.p3(N)) then
			R(N)=p4(N)/p3(N)
		else
			R(N)=p3(N)/p4(N) 
		end if
		
		if (maxrange.LT.ra(N)) then
			maxrange=ra(N)
		end if
		if (minrange.GT.ra(N)) then
			minrange=ra(N)
		end if
		if (maxmean.LT.me(N)) then
			maxmean=me(N)
		end if
		if (minmean.GT.me(N)) then
			minmean=me(N)
		end if
		N=N+1
	end do
	CLOSE(UNIT=20)
	
	OPEN (20, FILE='output.txt')
	matrixsize=64
	Deltarange=abs(maxrange-minrange)/matrixsize
	Deltamean=abs(maxmean-minmean)/matrixsize
	
	do k=1,N-1
	
	do i=1,matrixsize
		if (ra(k).GE.(minrange+(i-1)*Deltarange-Deltarange/100)) then
		if (ra(k).LE.(minrange+i*Deltarange+Deltarange/100)) then

		do j=1,matrixsize
			if (me(k).GE.(minmean+(j-1)*Deltamean-Deltamean/100)) then
			if (me(k).LE.(minmean+j*Deltamean+Deltamean/100)) then
			
				cc(i,j)=cc(i,j)+noc(k)
			
			end if
			end if 
		end do
		
		end if
		end if
	end do
	end do
		
!	write (20,*) 'Range ','Mean ','R-ratio ','n '		
	do i=1,matrixsize
		do j=1,matrixsize
!				
			if (cc(i,j).NE.0) then
			Sumcum=Sumcum+cc(i,j)
			end if
	end do
	end do
	
	do i=1,matrixsize
		do j=1,matrixsize
			if (cc(i,j).NE.0) then
			Downra=minrange+(i-1)*Deltarange
			Upra=minrange+i*Deltarange
			Mra=(Upra+Downra)/2
			Downme=minmean+(j-1)*Deltamean
			Upme=minmean+j*Deltamean
			Mme=(Upme+Downme)/2
			DR=-1+(4*Downme)/(2*Downme+Downra)
			UR=-1+(4*Upme)/(2*Upme+Upra)
			MR=-1+(4*Mme)/(2*Mme+Mra)
			Cum=Cum+cc(i,j)
			PCum=dabs(Sumcum-Cum)*100/(Sumcum)
			write (20,*)Mra,Mme,MR,cc(i,j)/2,PCum
			end if
	end do
	end do

	CLOSE(UNIT=20)  
	
	End

