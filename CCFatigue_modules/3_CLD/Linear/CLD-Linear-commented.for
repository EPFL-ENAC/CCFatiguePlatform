      Program Linear

         integer i, j, ierr
         real K, P, Q, m, NOD(1000), Sigma0, A, B, Xb, Yb, Sm, Sa, AA, BB, USm
         real maxsigma(100, 1000), asigma(100, 1000), msigma(100, 1000)
         real X(1000), Y(1000), ONC(100), N(100, 1000), R(100)


         CHARACTER * 40 NAME, NAM

         ! Enter the input file name and open it for reading
         !####################################################################


         OPEN (10, FILE = 'input.txt')
         OPEN (20, FILE = 'output.txt')

         !"GIVE THE CRITICAL STRESS RATIO, static values and N "
         OPEN(100, FILE = 'staticvalue.txt')
         READ(100, *) UCS, UTS, Rcr ! = 27.1, 27.7, 0.1
         READ(100, *) ONC(1), ONC(2), ONC(3), ONC(4), ONC(5), ONC(6), ONC(7) ! = 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9
         CLOSE(UNIT = 100)

         m = 0
         k = 0
         NOR = 1

         ! Ignore 12 first lines of input file
         do i = 1, 12
            read(10,  * )
         end do

         ! Read input file
         do
            m = m + 1
            read(10,  * , iostat = ierr) R(NOR), N(NOR, m), maxsigma(NOR, m)
            if (ierr  /  = 0) then
               m = m-1
               exit
            end if

            ! If line begins with 0 => new stress ratio
            if (R(NOR).EQ.0) then
               R(NOR) = PreSR
               NOD(NOR) = m-1
               m = 0
               NOR = NOR+1

               ! Ignore param lines
               do i = 1, 9
                  read(10,  * )
               end do
               go to 20
            end if
            PreSR = R(NOR)
   20    end do


         NOD(NOR) = m

         ! For each stress ratio
         do j = 1, NOR
            ! If stress ratio == -1
            if (R(j).EQ.Rcr) then
               RcrN = j ! = index of stress ratio == -1
               checkmark = 1
            end if
         end do

         ! If no stress ratio == -1
         if (checkmark.EQ.0)	then
            WRITE(20,  * ) 'There is no data for critical stress ratio!'
            go to 10 ! Exit
         end if


         !Find A and B
         ! ##########################################################################

         ! For each data in -1 stress ratio
         do i = 1, NOD(RcrN)
            asigma(RcrN, i) = (1 - R(RcrN)) * maxsigma(RcrN, i) / 2
            msigma(RcrN, i) = (1 + R(RcrN)) * maxsigma(RcrN, i) / 2
            X(i) = log10(maxsigma(RcrN, i))
            Y(i) = log10(N(RcrN, i))
         end do
         Yb = 0 ! = Mean(Y)
         Xb = 0 ! = Mean(X)
         do i = 1, NOD(RcrN)
            Yb = Yb + Y(i)
            Xb = Xb + X(i)
         end do
         Xb = Xb / NOD(RcrN)
         Yb = Yb / NOD(RcrN)

         P = 0
         Q = 0
         do i = 1, NOD(RcrN)
            P = P + ((X(i) - Xb) * (Y(i) - Yb))
            Q = Q + (X(i) - Xb)**2
         end do

         B = P / Q
         A = Yb - B * Xb
         AA = 10**(-A / B)
         BB = -1 / B
         K = 1 / BB
         Sigma0 = AA

         do j = 1, 7
            USm = Sigma0 * (ONC(j) ** (-1 / K))
            Sm = -USm
            Sa = 0 !Sa = Sigma0 * ((ONC(j) ** (-1 / K))-(abs(Sm) / Sigma0))
            WRITE(20,  * ) ONC(j), Sa, Sm
            Sm = 0
            !Sa = Sigma0 * ((ONC(j) ** (-1 / K))-(abs(Sm) / Sigma0))
            Sa = Sigma0 * (ONC(j) ** (-1 / K))-abs(Sm)
            WRITE(20,  * ) ONC(j), Sa, Sm
            Sm = USm
            Sa = 0 !Sa = Sigma0 * ((ONC(j) ** (-1 / K))-(abs(Sm) / Sigma0))
            WRITE(20,  * ) ONC(j), Sa, Sm
         end do

   10    CLOSE(UNIT = 10)
         CLOSE(UNIT = 20)

      END


