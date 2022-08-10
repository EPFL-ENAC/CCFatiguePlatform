Program Whitney

   integer k,p,q,i,j,m,kk,NOR,NOD(100),ierr,NN,f
   integer rfail(1000),checknumber,strlev
   real rmle,afi, checkmark,rrout,xo,slope,intercept,aff
   real rnos(1000),S(1000),NR(1000)
   real sumnoc,rmean(1000),rout(1000),rno(1000),rn(1000),Noi(1000)
   real sumk1ij(1000),sumk2ij(1000),sumk3ij(1000),af(1000)
   real sumc1,sumc2,sumc3,sumc4,sumc5,c1,c2,c3,c4,c5,a1,a2,a3,a4,a5
   real k1(1000,1000),k2(1000,1000),k3(1000,1000),noc(1000,1000)
   real Nnoc(1000,1000),anoc(1000,1000),Ssigma,averagesigma,So,Pow
   real asigma(100,1000),N(100,1000),slevel(100,1000)
   real R(100),PN(100),b,SSE,RMSE,Nb,SST,RSQ,LSSE,LSST,LRSQ,LNb

   CHARACTER*40 NAME, NAM

   ! Enter the input file name and open it for reading
   CHARACTER(len = 1024) inputFile
   call get_command_argument(1, inputFile)
   if (inputFile == '') inputFile = 'input.txt'

   OPEN (10, FILE = inputFile)
   OPEN (20, FILE = '/dev/stdout')


   ! number of cycles for run out
   rrout=5e7

   ! Read and re-arange the test data based on the stress levels
   ! ##########################################################################
   m=0	 !Number of input data for each r value
   k=0
   NOR=1
   do
      m=m+1
      read(10,*,iostat=ierr) R(NOR),PN(NOR),slevel(NOR,m),
      &	                              asigma(NOR,m),N(NOR,m),b
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
            go to 30
         end if
      end if
      PreSR=R(NOR)
30 end do
   NOD(NOR)=m
   CLOSE(UNIT=10)

   ! Rewrite input in Scratch file > replace asigma in each group
   ! of stress levels by avg(sigma)
   do f=1,NOR
      OPEN (10, STATUS='SCRATCH')
      Ssigma=asigma(f,1)
      checknumber=1
      do i=2,NOD(f)
         if (slevel(f,i).eq.slevel(f,i-1)) then
            Ssigma=Ssigma+asigma(f,i)
            checknumber=checknumber+1
         else
            if (checknumber.GE.2) then
               averagesigma=Ssigma/checknumber
               do j=i-checknumber, i-1
                  write(10,*) averagesigma,N(f,j)
               end do
            end if
            checknumber=1
            Ssigma=asigma(f,i)
         end if
      end do


      !for the last stress level in each R-ratio
      averagesigma=Ssigma/checknumber
      do j=i-checknumber, i-1
         write(10,*) averagesigma,N(f,j)
      end do

      call fseek(10, 0, 0) ! Scratch file > go back to top

      m=0
      do
         m=m+1
         read(10,*,iostat=ierr) asigma(f,m), N(f,m)
         if (ierr /= 0) then
            m=m-1
            exit
         end if
      end do

      CLOSE(UNIT=10)

      strlev=0

      do i=1,m,1 ! For i in NOD

         checkmark=0
         do p=1,i-1,1 ! For p in prev data
            if (asigma(f,i).eq.asigma(f,p)) then
               checkmark=1
            end if
         end do
         if (checkmark.NE.1) then ! If new stress
            k=0
            kk=1
            strlev=strlev+1
            S(strlev)=asigma(f,i)
            if (N(f,i).LT.rrout) then
               k=k+1
               noc(strlev,k)=N(f,i)
            end if
            ! Copy each number of cycle for current stress
            do j=i+1,m,1
               if ((asigma(f,i)-asigma(f,j)).EQ.0) then
                  if (N(f,j).LT.rrout) then

                     k=k+1
                     noc(strlev,k)=N(f,j)	!number of cycle

                  end if
                  kk=kk+1
               end if
            end do
            rnos(strlev)=kk	 !number of data at each stress level
            rfail(strlev)=k	 !number of failed data at each stress level
            !WRITE(20,*) rnos(strlev),rfail(strlev)
         end if
      end do
      !WRITE(20,*) strlev
      Nostrlev=strlev			!number of stress levels
      ! calculate average number of cycles at each stress level
      ! ##########################################################################
      sumnoc=0
      do i=1, Nostrlev
         do j=1, rfail(i)
            sumnoc=sumnoc+noc(i,j)
            Nb=Nb+noc(i,j)
            LNb=LNb+log10(noc(i,j))
         end do
         rmean (i)=sumnoc/rnos(i) !ave. No. of cycles at each stress level
         rout(i)=rrout /rmean(i) !ratio of runout to ave NOC ar each level
         sumnoc=0
         !WRITE(20,*) rrout,rmean(i)
      end do
      Nb=Nb/NOD(f)	!Average of all N
      LNb=LNb/NOD(f)	!Average of all log(N)

      ! calculate and store in an array the normalized average number of cycles
      ! ##########################################################################
      do i=1, Nostrlev
         do j=1, rfail(i)
            anoc(i,j)=noc(i,j) /rmean(i)
            !WRITE(20,*) rmean(i),anoc(i,j)
         end do
      end do


      ! Perform MLE procedure for each stress level
      ! Calculate the value of shape parameter at each stress level
      ! ##########################################################################
      do i=1,Nostrlev
         do kkk=1, 90000, 1
            afi=0.001*kkk
            sumk1ij(i)=0
            sumk2ij(i)=0
            sumk3ij(i)=0
            do j=1, rfail(i)
               k1(i,j)=anoc(i,j)**afi*log(anoc(i,j))
               k2(i,j)=anoc(i,j)**afi
               k3(i,j)=-(1.0/rfail(i))*log(anoc(i,j))
               sumk1ij(i)=sumk1ij(i)+k1(i,j)
               sumk2ij(i)=sumk2ij(i)+k2(i,j)
               sumk3ij(i)=sumk3ij(i)+k3(i,j)
            end do

            rmle=(sumk1ij(i)+((rnos(i)-rfail(i))*
            &		rout(i)**afi*log(rout(i))))/(sumk2ij(i)+
            &		(rnos(i)-rfail(i))*rout(i)**afi)+sumk3ij(i)-(1/afi)
            if (rmle.ge.0) then
               af(i)=afi			!shape parameter at each stress level
               !WRITE(20,*) 'a',i,af(i)
               go to 10
            end if

         end do
10    end do


      ! Perform the second step of MLE procedure
      ! Estimate characteristic number of cycles for each stress level
      ! ##########################################################################
      sumc1=0
      sumc2=0

      do i=1, Nostrlev
         do j=1, rfail(i)
            sumc1=sumc1+anoc(i,j)**af(i)
         end do
         sumc2=(rnos(i)-rfail(i))*rout(i)**af(i) !in this eq. instead
         !of runout, the ratio of runoutto the ave. No. of cycle at each leavel "rout(i)" is used!
         rno(i)=(( 1.0/rfail(i))*(sumc1+sumc2))**(1/af(i))
         rn(i)=rno(i) *rmean(i)	!The char No. of cycles is mult. by mean
         rout(i)=rout(i) *rmean(i)
         WRITE(*,*) Nostrlev,i,rfail(i),af(i)
      end do


      ! Normalization of the No. of cycles at each level by the obtained char. No of cycles
      ! ##########################################################################
      rfailsum=0.
      do i=1, Nostrlev
         rfailsum=rfailsum+rfail(i)	!Total No. of failed specimens
         do j=1, rfail(i)
            Nnoc(i,j)=noc(i,j)/rn(i)	!Normalized No. of cycles
            !WRITE(*,*) i,rn(i)
         end do
         NR(i)=rrout/rn(i)
      end do

      ! Perform MLE procedure
      ! Calculate the value of shape parameter
      ! ##########################################################################

      do kkk=1, 300000, 1
         aff=0.0001*kkk
         sumc1=0
         sumc2=0
         sumc3=0
         sumc4=0
         sumc5=0
         do i=1, Nostrlev
            sumc1=sumc1+((rnos(i)-rfail(i))*NR(i)**aff)*log(NR(i))
            sumc2=sumc2+(rnos(i)-rfail(i))*NR(i)**aff
            do j=1,rfail(i)
               sumc3=sumc3+(Nnoc(i,j)**aff)*log(Nnoc(i,j))
               sumc4=sumc4+Nnoc(i,j)**aff
               sumc5=sumc5+log(Nnoc(i,j))
            end do
         end do

         rmle=((sumc3+sumc1)/(sumc4+sumc2))-(1/rfailsum)*sumc5-(1/aff)
         if (rmle.ge.0) then
            Faf=aff		!shape parameter of the pooled data
            goto 20
         end if

      end do

20    continue
      ! Perform the second step of MLE procedure
      ! ##########################################################################
      sumc1=0
      sumc2=0

      do i=1, Nostrlev
         do j=1, rfail(i)
            sumc1=sumc1+Nnoc(i,j)**Faf
         end do
         sumc2=sumc2+(rnos(i)-rfail(i))*NR(i)**Faf
      end do

      xo=((1/rfailsum)*(sumc1+sumc2))**(1/Faf) !Scale par of the pooled data

      ! Calculating the slope and the intercept of the S/N curve based on Linear Regression data fitting
      ! #########################################################################
      a1=0
      a2=0
      a3=0
      a4=0
      a5=0
      do i=1, Nostrlev

         Noi(i)=xo*rn(i)
         a1=a1+log10(Noi(i))*log10(S(i))
         a2=a2+log10(S(i))
         a3=a3+log10(Noi(i))
         a4=a4+(log10(S(i)))**2
         a5=(a2)**2
         !WRITE(20,*) S(i),Noi(i)
      end do

      slope=(Nostrlev*a1-a2*a3)/(Nostrlev*a4-a5)
      intercept=(a3-slope*a2)/Nostrlev

      So=10**(-intercept/slope)
      Pow=-1/slope

      do i=1, NOD(f)
         SSE=SSE+(N(f,i)-(asigma(f,i)/(So*((-log(PN(f)/100))**(Pow/Faf))))
         &		**(-1/Pow))**2
         LSSE=LSSE+(log10(N(f,i))-log10((asigma(f,i)/
         &		(So*((-log(PN(f)/100))**(Pow/Faf))))**(-1/Pow)))**2

         SST=SST+(N(f,i)-Nb)**2
         LSST=LSST+(log10(N(f,i))-log10(Nb))**2
      end do
      RMSE=sqrt(SSE/NOD(f))
      RSQ=1-SSE/SST
      LRSQ=1-LSSE/LSST

      ! Calculating and writing the fitted S-N curve based on found parameters
      ! ##########################################################################
      WRITE(20,*) '0'				!Seperator
      WRITE(20,*) R(f)			!R-ratio
      WRITE(20,*) PN(f)			!Reliability level
      WRITE(20,*) So				!(c^(-1/b))
      WRITE(20,*) Pow				!1/b
      WRITE(20,*) Faf				!af (shape parameter of the pooled data)
      WRITE(20,*) xo				!Scale par of the pooled data
      WRITE(20,*) LRSQ
      WRITE(20,*) RMSE			!Root mean square error
      WRITE(20,*) SSE				!Sum of squares due to errors
      WRITE(20,*) SST				!Sum of squares about the mean
      WRITE(20,*) RSQ				!R-square


      !	"Data for 50% Reliability level"
      !if (PN(f).EQ.50) then

      do NN=1,1000,50
         SPN=(So)*((-log(PN(f)/100))**(Pow/Faf))*(NN**(-Pow))
         WRITE(20,*) R(f),NN,SPN
      end do

      NN=1000
      SPN=(So)*((-log(PN(f)/100))**(Pow/Faf))*(NN**(-Pow))
      WRITE(20,*) R(f),NN,SPN


      do NN=10000,2000000,10000
         SPN=(So)*((-log(PN(f)/100))**(Pow/Faf))*(NN**(-Pow))
         WRITE(20,*) R(f),NN,SPN
      end do

      do NN=3000000,20000000,1000000
         SPN=(So)*((-log(PN(f)/100))**(Pow/Faf))*(NN**(-Pow))
         WRITE(20,*) R(f),NN,SPN
      end do

      do NN=30000000,1400000000,100000000
         SPN=(So)*((-log(PN(f)/100))**(Pow/Faf))*(NN**(-Pow))
         WRITE(20,*) R(f),NN,SPN
      end do
      !end if

      !	"Data for 95% Reliability level"

      !if (PN(f).EQ.95) then
      !
      !	do NN=1,1000,50
      !		SPN=(So)*((-log(0.95))**(Pow/Faf))*(NN**(-Pow))
      !		WRITE(20,*) R(f),NN,SPN
      !	end do

      !	NN=1000
      !	SPN=(So)*((-log(0.95))**(Pow/Faf))*(NN**(-Pow))
      !	WRITE(20,*) R(f),NN,SPN

      !	do NN=10000,2e6,10000
      !		SPN=(So)*((-log(0.95))**(Pow/Faf))*(NN**(-Pow))
      !		WRITE(20,*) R(f),NN,SPN
      !	end do

      !	do NN=2e6,20e6,1000000
      !		SPN=(So)*((-log(0.95))**(Pow/Faf))*(NN**(-Pow))
      !		WRITE(20,*) R(f),NN,SPN
      !	end do

      !end if

      CLOSE(UNIT=10)
   end do
   CLOSE(UNIT=20)
end
