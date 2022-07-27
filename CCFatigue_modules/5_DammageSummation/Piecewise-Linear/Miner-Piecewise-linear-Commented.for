      ! Ref:
      !  [1] https://doi.org/10.1016/j.ijfatigue.2009.09.008
      Program LINEAR CLD

         integer i,j,ierr
         real k1,k2,k3,p1,p2,p3,p4,Rin,NOR,CL,m,Nu,Rs(10,1000),NofC,sa
         real maxsigma (10,1000,1000),asigma(10,1000,1000),Rtest(1000),AN
         real msigma(10,1000,1000),N(10,1000,1000),R(10,1000)
         real UTS,UCS,Nmid,maxsmid,maxasmid,maxmsmid,Rmid,X(1000),Y(1000)
         real NOD,Xb,Yb,P,Q,A,B,ONC(10),LSF, HSF, increment,Fact


         CHARACTER*40 NAME, NAM

         ! Enter the input file name and open it for reading

         OPEN (10, FILE = 'input.txt')
         OPEN (200, FILE = 'Temp-input.txt')

         !"GIVE THE CRITICAL STRESS RATIO, static values and N "
         OPEN(100, FILE = 'staticvalue.txt')
         READ(100,*) UCS,UTS,Rcr
         READ(100,*) ONC(1),ONC(2),ONC(3),ONC(4),ONC(5),ONC(6),ONC(7)
         CLOSE(UNIT=100)

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !Counting number R and data and creating the Temp-input file

         ! Ignore all params, copy only table values

         ! Ignore params (12 first lines in input files)
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
   20    end do

         NOR=i
         NOD=m

         CLOSE(UNIT=10)
         CLOSE(UNIT=200)

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         !Section 1: between R=1 and 0
         !Section 2: between R=0 and -infinity
         !Section 3: between R=infinity and -1


         OPEN (10, FILE = 'Temp-input.txt')

         p1=0	! Number of R where [0 <= R < 1]
         p2=0	! Number of R where [R < 0]
         p3=0	! Number of R where [R > 1]

         do i=1,NOR

            ! 0 <= R < 1
            if (Rtest(i).LT.1) then
               if (Rtest(i).GE.0) then
                  p1=p1+1
                  R(1,p1)=Rtest(i)
                  do m=1,NOD
                     READ(10,*) Nu,N(1,p1,m),maxsigma(1,p1,m)
                     if (abs(R(1,p1)).GT.1) then
                        asigma(1,p1,m)=(1-(1/R(1,p1)))*maxsigma(1,p1,m)/2
                        msigma(1,p1,m)=-(1+(1/R(1,p1)))*maxsigma(1,p1,m)/2 ! msigma unsed
                     else
                        asigma(1,p1,m)=(1-R(1,p1))*maxsigma(1,p1,m)/2
                        msigma(1,p1,m)=(1+R(1,p1))*maxsigma(1,p1,m)/2
                     end if

                  end do
               end if
            end if

            ! R < 0
            if (Rtest(i).LT.0) then
               p2=p2+1
               R(2,p2)=Rtest(i)
               do m=1,NOD
                  READ(10,*) Nu,N(2,p2,m),maxsigma(2,p2,m)
                  if (abs(R(2,p2)).GT.1) then
                     asigma(2,p2,m)=(1-(1/R(2,p2)))*maxsigma(2,p2,m)/2
                     msigma(2,p2,m)=-(1+(1/R(2,p2)))*maxsigma(2,p2,m)/2
                  else
                     asigma(2,p2,m)=(1-R(2,p2))*maxsigma(2,p2,m)/2
                     msigma(2,p2,m)=(1+R(2,p2))*maxsigma(2,p2,m)/2
                  end if
               end do
            end if

            ! R > 1
            if (Rtest(i).GT.1) then
               p3=p3+1
               R(3,p3)=Rtest(i)
               do m=1,NOD !while (.NOT.EOF (10))
                  READ(10,*)	Nu,N(3,p3,m),maxsigma(3,p3,m)
                  if (abs(R(3,p3)).GT.1) then
                     asigma(3,p3,m)=(1-(1/R(3,p3)))*maxsigma(3,p3,m)/2
                     msigma(3,p3,m)=-(1+(1/R(3,p3)))*maxsigma(3,p3,m)/2
                  else
                     asigma(3,p3,m)=(1-R(3,p3))*maxsigma(3,p3,m)/2
                     msigma(3,p3,m)=(1+R(3,p3))*maxsigma(3,p3,m)/2
                  end if
               end do
            end if
         end do


         ! Sorting of part 1 by stress ratio, descending order
         !#####################################################################
         do i=1,p1
            do j=i+1,p1
               if (R(1,i).LT.R(1,j)) then
                  Rmid=R(1,i)
                  R(1,i)=R(1,j)
                  R(1,j)=Rmid
                  do m=1,NOD
                     Nmid=N(1,i,m)
                     maxsmid=maxsigma(1,i,m)
                     maxasmid=asigma(1,i,m)
                     maxmsmid=msigma(1,i,m)

                     N(1,i,m)=N(1,j,m)
                     maxsigma(1,i,m)=maxsigma(1,j,m)
                     asigma(1,i,m)=asigma(1,j,m)
                     msigma(1,i,m)=msigma(1,j,m)

                     N(1,j,m)=Nmid
                     maxsigma(1,j,m)=maxsmid
                     asigma(1,j,m)=maxasmid
                     msigma(1,j,m)=maxasmid
                  end do
               end if
            end do
         end do

         ! Sorting of part 2
         !#####################################################################
         do i=1,p2
            do j=i+1,p2
               if (R(2,i).LT.R(2,j)) then
                  Rmid=R(2,i)
                  R(2,i)=R(2,j)
                  R(2,j)=Rmid
                  do m=1,NOD
                     Nmid=N(2,i,m)
                     maxsmid=maxsigma(2,i,m)
                     maxasmid=asigma(2,i,m)
                     maxmsmid=msigma(2,i,m)

                     N(2,i,m)=N(2,j,m)
                     maxsigma(2,i,m)=maxsigma(2,j,m)
                     asigma(2,i,m)=asigma(2,j,m)
                     msigma(2,i,m)=msigma(2,j,m)

                     N(2,j,m)=Nmid
                     maxsigma(2,j,m)=maxsmid
                     asigma(2,j,m)=maxasmid
                     msigma(2,j,m)=maxasmid
                  end do
               end if
            end do
         end do

         ! Sorting of part 3
         !#####################################################################
         do i=1,p3
            do j=i+1,p3
               if (R(3,i).LT.R(3,j)) then
                  Rmid=R(3,i)
                  R(3,i)=R(3,j)
                  R(3,j)=Rmid
                  do m=1,NOD
                     Nmid=N(3,i,m)
                     maxsmid=maxsigma(3,i,m)
                     maxasmid=asigma(3,i,m)
                     maxmsmid=msigma(3,i,m)

                     N(3,i,m)=N(3,j,m)
                     maxsigma(3,i,m)=maxsigma(3,j,m)
                     asigma(3,i,m)=asigma(3,j,m)
                     msigma(3,i,m)=msigma(3,j,m)

                     N(3,j,m)=Nmid
                     maxsigma(3,j,m)=maxsmid
                     asigma(3,j,m)=maxasmid
                     msigma(3,j,m)=maxasmid
                  end do
               end if
            end do
         end do
         ! ##########################################################################

         OPEN (30, FILE = 'CCinput.txt')
         OPEN (40, FILE = 'Factor.txt')
         OPEN (50, FILE = 'Miner.txt')
         READ (40,*) HSF, LSF, increment  ! = 1, 1, 1

         ! For each decrementing factor
         do Fact=HSF, LSF, -increment
            Damage=0

            ! For each line in CCinput
            do
               READ(30,*,iostat=ierr)	range,smean,rratio,NofC
               if (ierr /= 0) then
                  exit
               end if
               Smean=Fact*Smean
               Range=Fact*Range
               Stlev=Smean+Range/2
               if (MaxStlev.LT.Stlev) then
                  MaxStlev=Stlev
               end if
               smax=smean+range/2
               smin=smean-range/2
               sa=(smax-smin)/2 ! sa == range / 2
               Yb=0
               Xb=0
               p=0
               q=0
               r_prime=(1+rratio)/(1-rratio)

               ! If at least one stress ratio in [0 <= R < 1]
               if (p1.GE.1) then
                  ! If current stress ratio < 1
                  if (rratio.LT.1) then
                     ! If current stress ratio >= first stress ratio in p1
                     if (rratio.GE.R(1,1)) then

                        Rs(1,1)=(1+R(1,1))/(1-R(1,1))
                        Xb=0
                        Yb=0
                        do j=1,NOD,1
                           ! Eq 3 p661 ref [1]
                           K1=UTS
                           K2=UTS/asigma(1,1,j)
                           sigma_prime_a=K1/(K2+r_prime-Rs(1,1))
                           ! ##########################################################################
                           X(j)=log10(sigma_prime_a)
                           Y(j)=log10(N(1,1,j))
                           Yb=Yb+Y(j)
                           Xb=Xb+X(j)
                        end do
                        Xb=Xb/NOD
                        Yb=Yb/NOD
                        do j=1,NOD
                           P=P+((X(i)-Xb)*(Y(i)-Yb))
                           Q=Q+(X(i)-Xb)**2
                        end do
                        B=P/Q     ! Intercept
                        A=Yb-B*Xb ! Slope
                        AN=10**(A+B*log10(sa))
                        Damage=Damage+NofC/AN
                        !WRITE (50, *) Range/Fact,NofC,AN
                        ! ##########################################################################
                        go to 100
                     end if
                  end if
               else if (p2.GE.1) then
                  if (rratio.LT.1) then
                     if (rratio.GE.R(2,1)) then
                        Rs(2,1)=(1+R(2,1))/(1-R(2,1))
                        Xb=0
                        Yb=0
                        do j=1,NOD
                           K1=UTS
                           K2=UTS/asigma(2,1,j)
                           sigma_prime_a=K1/(K2+r_prime-Rs(2,1))
                           ! ##########################################################################
                           X(j)=log10(sigma_prime_a)
                           Y(j)=log10(N(2,1,j))
                           Yb=Yb+Y(j)
                           Xb=Xb+X(j)
                        end do
                        Xb=Xb/NOD
                        Yb=Yb/NOD
                        do j=1,NOD
                           P=P+((X(i)-Xb)*(Y(i)-Yb))
                           Q=Q+(X(i)-Xb)**2
                        end do
                        B=P/Q
                        A=Yb-B*Xb
                        AN=10**(A+B*log10(sa))
                        Damage=Damage+NofC/AN
                        !WRITE (50, *) Range/Fact,NofC,AN
                        ! ##########################################################################
                        go to 100
                     end if
                  end if
               else if (p3.GE.1) then
                  if (rratio.LT.1) then
                     if (rratio.GE.R(3,1)) then
                        Rs(3,1)=(1+R(3,1))/(1-R(3,1))
                        Xb=0
                        Yb=0
                        do j=1,NOD
                           K1=UTS
                           K2=UTS/asigma(3,1,j)
                           sigma_prime_a=K1/(K2+r_prime-Rs(3,1))
                           ! ##########################################################################
                           X(j)=log10(sigma_prime_a)
                           Y(j)=log10(N(3,1,j))
                           Yb=Yb+Y(j)
                           Xb=Xb+X(j)
                        end do
                        Xb=Xb/NOD
                        Yb=Yb/NOD
                        do j=1,NOD
                           P=P+((X(i)-Xb)*(Y(i)-Yb))
                           Q=Q+(X(i)-Xb)**2
                        end do
                        B=P/Q
                        A=Yb-B*Xb
                        AN=10**(A+B*log10(sa))
                        Damage=Damage+NofC/AN
                        !WRITE (50, *) Range/Fact,NofC,AN
                        ! ##########################################################################
                        go to 100
                     end if
                  end if
               end if

               !  Part 1 ##########################################################################
               do i=1,p1-1
                  ! If P1(i) > R >= P1(i + 1)
                  if (rratio.LT.R(1,i)) then
                     if (rratio.GE.R(1,i+1)) then
                        Rs(1,i)=(1+R(1,i))/(1-R(1,i))
                        Rs(1,i+1)=(1+R(1,i+1))/(1-R(1,i+1))
                        Yb=0
                        Xb=0
                        do j=1,NOD
                           K1=asigma(1,i,j)*(Rs(1,i)-Rs(1,i+1))
                           K2=(Rs(1,i)-r_prime)*asigma(1,i,j)/asigma(1,i+1,j)
                           sigma_prime_a=K1/(K2+r_prime-Rs(1,i+1))
                           ! ##########################################################################
                           X(j)=log10(sigma_prime_a)
                           Y(j)=log10(N(1,i,j))
                           Yb=Yb+Y(j)
                           Xb=Xb+X(j)
                        end do
                        Xb=Xb/NOD
                        Yb=Yb/NOD
                        do j=1,NOD
                           P=P+((X(i)-Xb)*(Y(i)-Yb))
                           Q=Q+(X(i)-Xb)**2
                        end do
                        B=P/Q
                        A=Yb-B*Xb
                        AN=10**(A+B*log10(sa))
                        Damage=Damage+NofC/AN
                        !WRITE (50, *) Range/Fact,NofC,AN
                        ! ##########################################################################
                        go to 100
                     end if
                  end if
               end do
               ! Between Part 1 and 2  ########################################################
               if (p1.GE.1) then
                  if (p2.GE.1) then
                     ! if current R < last R of [p1] and R >= first R of [p2]
                     if (rratio.LT.R(1,p1)) then
                        if (rratio.GE.R(2,1)) then !1111
                           Rs(1,p1)=(1+R(1,p1))/(1-R(1,p1))
                           Rs(2,1)=(1+R(2,1))/(1-R(2,1))
                           Yb=0
                           Xb=0
                           do j=1,NOD
                              ! Eq 4 p661 ref [1]
                              K1=asigma(1,p1,j)*(Rs(1,p1)-Rs(2,1))
                              K2=(Rs(1,p1)-r_prime)*asigma(1,p1,j)/asigma(2,1,j)
                              sigma_prime_a=K1/(K2+r_prime-Rs(2,1))
                              ! ##########################################################################
                              X(j)=log10(sigma_prime_a)
                              Y(j)=log10(N(1,p1,j))
                              Yb=Yb+Y(j)
                              Xb=Xb+X(j)
                           end do
                           Xb=Xb/NOD
                           Yb=Yb/NOD
                           do j=1,NOD
                              P=P+((X(i)-Xb)*(Y(i)-Yb))
                              Q=Q+(X(i)-Xb)**2
                           end do
                           B=P/Q
                           A=Yb-B*Xb
                           AN=10**(A+B*log10(sa))
                           Damage=Damage+NofC/AN
                           !WRITE (50, *) Range/Fact,NofC,AN
                           ! ##########################################################################
                           go to 100
                        end if
                     end if
                  end if
               end if
               ! Part 2   ##########################################################################
               do i=1,p2-1
                  if (rratio.LT.R(2,i)) then
                     if (rratio.GT.R(2,i+1)) then
                        Rs(2,i)=(1+R(2,i))/(1-R(2,i))
                        Rs(2,i+1)=(1+R(2,i+1))/(1-R(2,i+1))
                        Yb=0
                        Xb=0
                        do j=1,NOD
                           K1=asigma(2,i,j)*(Rs(2,i)-Rs(2,i+1))
                           K2=abs(Rs(2,i)-r_prime)*asigma(2,i,j)/asigma(2,i+1,j)
                           sigma_prime_a=K1/(K2+r_prime-Rs(2,i+1))
                           ! ##########################################################################
                           X(j)=log10(sigma_prime_a)
                           Y(j)=log10(N(2,i,j))
                           Yb=Yb+Y(j)
                           Xb=Xb+X(j)
                        end do
                        Xb=Xb/NOD
                        Yb=Yb/NOD
                        do j=1,NOD
                           P=P+((X(i)-Xb)*(Y(i)-Yb))
                           Q=Q+(X(i)-Xb)**2
                        end do
                        B=P/Q
                        A=Yb-B*Xb
                        AN=10**(A+B*log10(sa))
                        Damage=Damage+NofC/AN
                        !WRITE (50, *) Range/Fact,NofC,AN
                        ! ##########################################################################
                        go to 100
                     end if
                  end if
               end do
               ! Between Part 2 and 3  ########################################################
               if (p2.GE.1) then
                  if (p3.GE.1) then ! TODO ask Tassos some cases may not be treated ()
                     if (rratio.LT.R(2,p2)) then
                        Rs(2,p2)=(1+R(2,p2))/(1-R(2,p2))
                        Rs(3,1)=(1+R(3,1))/(1-R(3,1))
                        Yb=0
                        Xb=0
                        do j=1,NOD
                           K1=asigma(2,p2,j)*(Rs(2,p2)-Rs(3,1))
                           K2=(Rs(2,p2)-r_prime)*asigma(2,p2,j)/asigma(3,1,j)
                           sigma_prime_a=K1/(K2+r_prime-Rs(3,1))
                           ! ##########################################################################
                           X(j)=log10(sigma_prime_a)
                           Y(j)=log10(N(2,p2,j))
                           Yb=Yb+Y(j)
                           Xb=Xb+X(j)


                        end do
                        Xb=Xb/NOD
                        Yb=Yb/NOD
                        do j=1,NOD
                           P=P+((X(i)-Xb)*(Y(i)-Yb))
                           Q=Q+(X(i)-Xb)**2
                        end do
                        B=P/Q
                        A=Yb-B*Xb
                        AN=10**(A+B*log10(sa))
                        Damage=Damage+NofC/AN
                        !WRITE (50, *) Range/Fact,NofC,AN
                        ! ##########################################################################


                        go to 100
                     else if (rratio.GE.R(3,1)) then  !1111
                        Rs(2,p2)=(1+R(2,p2))/(1-R(2,p2))
                        Rs(3,1)=(1+R(3,1))/(1-R(3,1))
                        Yb=0
                        Xb=0
                        do j=1,NOD
                           K1=asigma(2,p2,j)*(Rs(2,p2)-Rs(3,1))
                           K2=(Rs(2,p2)-r_prime)*asigma(2,p2,j)/asigma(3,1,j)
                           sigma_prime_a=K1/(K2+r_prime-Rs(3,1))
                           ! ##########################################################################
                           X(j)=log10(sigma_prime_a)
                           Y(j)=log10(N(2,p2,j))
                           Yb=Yb+Y(j)
                           Xb=Xb+X(j)

                        end do
                        Xb=Xb/NOD
                        Yb=Yb/NOD
                        do j=1,NOD
                           P=P+((X(i)-Xb)*(Y(i)-Yb))
                           Q=Q+(X(i)-Xb)**2
                        end do
                        B=P/Q
                        A=Yb-B*Xb
                        AN=10**(A+B*log10(sa))
                        Damage=Damage+NofC/AN
                        !WRITE (50, *) Range/Fact,NofC,AN
                        ! ##########################################################################

                        go to 100
                     end if
                  end if
               end if
               ! Part 3 ##########################################################################

               do i=1,p3-1
                  if (rratio.LT.R(3,i)) then
                     if (rratio.GT.R(3,i+1)) then
                        Rs(3,i)=(1+R(3,i))/(1-R(3,i))
                        Rs(3,i+1)=(1+R(3,i+1))/(1-R(3,i+1))
                        Yb=0
                        Xb=0
                        do j=1,NOD
                           K1=asigma(3,i,j)*(Rs(3,i)-Rs(3,i+1))
                           K2=abs(Rs(3,i)-r_prime)*asigma(3,i,j)/asigma(3,i+1,j)
                           sigma_prime_a=K1/(K2+r_prime-Rs(3,i+1))
                           ! ##########################################################################
                           X(j)=log10(sigma_prime_a)
                           Y(j)=log10(N(3,i,j))
                           Yb=Yb+Y(j)
                           Xb=Xb+X(j)

                        end do
                        Xb=Xb/NOD
                        Yb=Yb/NOD
                        do j=1,NOD
                           P=P+((X(i)-Xb)*(Y(i)-Yb))
                           Q=Q+(X(i)-Xb)**2
                        end do
                        B=P/Q
                        A=Yb-B*Xb
                        AN=10**(A+B*log10(sa))
                        Damage=Damage+NofC/AN
                        !WRITE (50, *) Range/Fact,NofC,AN
                        ! ##########################################################################

                        go to 100
                     end if
                  end if
               end do

               ! ##########################################################################

               if (p3.GE.1) then
                  if (rratio.GT.1) then
                     if (rratio.LT.R(3,p3)) then
                        Rs(3,p3)=(1+R(3,p3))/(1-R(3,p3))
                        Yb=0
                        Xb=0
                        do j=1,NOD
                           K1=UCS
                           K2=UCS/asigma(3,p3,j)
                           sigma_prime_a=K1/(K2-r_prime+Rs(3,p3))

                           ! ##########################################################################
                           X(j)=log10(sigma_prime_a)
                           Y(j)=log10(N(3,p3,j))
                           Yb=Yb+Y(j)
                           Xb=Xb+X(j)

                        end do
                        Xb=Xb/NOD
                        Yb=Yb/NOD
                        do j=1,NOD
                           P=P+((X(i)-Xb)*(Y(i)-Yb))
                           Q=Q+(X(i)-Xb)**2
                        end do
                        B=P/Q
                        A=Yb-B*Xb
                        AN=10**(A+B*log10(sa))
                        Damage=Damage+NofC/AN
                        !WRITE (50, *) Range/Fact,NofC,AN
                        ! ##########################################################################

                        go to 100
                     end if
                  end if
               else if (p2.GE.1) then
                  if (rratio.GT.1) then
                     if (rratio.LT.R(2,p2)) then ! TODO never reached ??
                        Rs(2,p2)=(1+R(2,p2))/(1-R(2,p2))
                        Yb=0
                        Xb=0
                        do j=1,NOD
                           K1=UCS
                           K2=UCS/asigma(2,p2,j)
                           sigma_prime_a=K1/(K2-r_prime+Rs(2,p2))
                           ! ##########################################################################
                           X(j)=log10(sigma_prime_a)
                           Y(j)=log10(N(2,p2,j))
                           Yb=Yb+Y(j)
                           Xb=Xb+X(j)

                        end do
                        Xb=Xb/NOD
                        Yb=Yb/NOD
                        do j=1,NOD
                           P=P+((X(i)-Xb)*(Y(i)-Yb))
                           Q=Q+(X(i)-Xb)**2
                        end do
                        B=P/Q
                        A=Yb-B*Xb
                        AN=10**(A+B*log10(sa))
                        Damage=Damage+NofC/AN
                        !WRITE (50, *) Range/Fact,NofC,AN
                        ! ##########################################################################
                        go to 100
                     end if
                  end if
               else if (p1.GE.1) then
                  if (rratio.GT.1) then
                     if (rratio.LT.R(1,p1)) then ! TODO never reached ??
                        Rs(1,p1)=(1+R(1,p1))/(1-R(1,p1))
                        Yb=0
                        Xb=0
                        do j=1,NOD
                           K1=UCS
                           K2=UCS/asigma(1,p1,j)
                           sigma_prime_a=K1/(K2-r_prime+Rs(1,p1))
                           ! ##########################################################################
                           X(j)=log10(sigma_prime_a)
                           Y(j)=log10(N(1,p1,j))
                           Yb=Yb+Y(j)
                           Xb=Xb+X(j)

                        end do
                        Xb=Xb/NOD
                        Yb=Yb/NOD
                        do j=1,NOD
                           P=P+((X(i)-Xb)*(Y(i)-Yb))
                           Q=Q+(X(i)-Xb)**2
                        end do
                        B=P/Q
                        A=Yb-B*Xb
                        AN=10**(A+B*log10(sa))
                        Damage=Damage+NofC/AN
                        !WRITE (50, *) Range/Fact,NofC,AN
                        ! ##########################################################################

                        go to 100
                     end if
                  end if
  100          end if
               !WRITE(50,*) NofC,AN,NofC/AN
            end do

            CLOSE(UNIT=10)

            WRITE(50,*) MaxStlev,1/Damage
            Rewind(30)
            MaxStlev=0
         end do
         CLOSE(UNIT=30)
         CLOSE(UNIT=40)
         CLOSE(UNIT=50)

      END



