! Fortran module for Python : S-N-Curve-Sendeckyj

! G. P. Sendeckyj, Fitting Models to Composite Materials Fatigue Data.
! ASTM STP 734, ed. By C.C.Chamis, American Society for Testing and Materials, 
! West Conshohocken, 1981, pp 245-260. 
! DOI:10.1520/STP29314S

! Module compilation: f2py -c -m sendeckyj sendeckyj.f 

! Function maximum_likelihood_estimators
! (see ref, p258)
! Params:
! - residual_strength (list of reals) = residual_strength
! - asigma (list of reals) = stress_parameter
! - number_of_cycles (list of reals) = number_of_cycles
! - c (real) = measure of the extent of the "flat" region on the S-N curve at high
!              applied cyclic stress levels (see ref, p 248)
! - s (real) = absolute value of the asymptotic slope at long life on log-log plot
!              of the S-N curve (see ref, p 248)
! - censored_data_count (real) = number of censored data in current stress ratio
! return -alpha (real) (minus alpha for scipy minimization)



      SUBROUTINE maximum_likelihood_estimators(
     $  s, c, residual_strength, asigma, number_of_cycles,
     $  censored_data_count, alpha, data_count)

CF2PY INTENT(HIDE) :: data_count
CF2PY REAL(kind = 8) residual_strength(data_count)
CF2PY REAL(kind = 8) asigma(data_count)
CF2PY REAL(kind = 8) number_of_cycles(data_count)
CF2PY REAL(kind = 8) c
CF2PY REAL(kind = 8) s
CF2PY INTEGER censored_data_count
CF2PY INTENT(OUT) :: alpha
      REAL(kind = 8) residual_strength(*)
      REAL(kind = 8) asigma(*)
      REAL(kind = 8) c, s
      INTEGER data_count, censored_data_count, e, t
      REAL(kind = 8) esigma(data_count)
      REAL(kind = 8) x(data_count)
      REAL(kind = 8) number_of_cycles(data_count)

      REAL(kind = 8) k1, k2, k3, k4, sles, g, x_max, x_min, alpha
      REAL(kind = 8) a, b, d, delta_alpha


        ! ++++++++++++++++++++++++ solving the Alpha equation +++++++++++++++++++++++++++++++++++	
        ! Eq 1 (p248)
        do i = 1, data_count
            k1 = residual_strength(i) / asigma(i)
            k2 = k1 ** (1 / s)
            k3 = (number_of_cycles(i) - 1) * c
            k4 = k3 + k2
            esigma(i) = asigma(i) * (k4 ** s)
        end do
        
        ! Eq 17
        sles = 0;
        do i = 1, data_count - censored_data_count
            sles = sles + log(esigma(i))
        end do
        g = exp((1 / (real(data_count) - censored_data_count)) * sles)
        
        ! Eq 16
        do i = 1, data_count
            x(i) = esigma(i) / g
        end do
        
        ! Get x_max and x_min for Eq 20
        x_max = x(1)
        x_min = x(1)
        do e = 2, data_count
            if (x_max.LT.x(e)) then
                x_max = x(e)
            else if (x_min.GT.x(e)) then
                x_min = x(e)
            end if
            
        end do
        ! Eq 20
        alpha = log((log(real(data_count) / (real(data_count) + 1))) /
     $          (log(1 / (real(data_count) + 1))))
        alpha = alpha / (log(x_min / x_max)) ! TODO ask Tassos why not x_max / x_min like in Eq 20
        
        t = 0
        
        ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
        do while (t.LT.100)
            
            ! Eq 21 = S1
            a = 0;
            do i = 1, data_count
                a = a + (x(i) ** alpha)
            end do
            
            ! Eq 22 = S2
            b = 0
            do i = 1, data_count
                b = b + log(x(i)) * (x(i) ** alpha)
            end do
            
            ! Eq 23 = S3
            d = 0
            do i = 1, data_count
                d = d + (log(x(i)) ** 2) * (x(i) ** alpha)
            end do
            
            ! Eq 24
            delta_alpha = (a - alpha * b) / (alpha * d)
            
            ! Eq 25
            if ((abs(delta_alpha / alpha)).GT.0.000001) then
                alpha = alpha + delta_alpha
            else
                                
                t = 101
                
            end if
            
        end do
        alpha = -alpha
      END

! Simple function to test f2py, especially the real precision
! list (list of reals)
! return sum of list

      SUBROUTINE test(list, sum, nb)
   
CF2PY INTENT(HIDE) :: nb
CF2PY REAL(kind = 8) list(nb)
CF2PY INTENT(OUT) :: sum
            REAL(kind = 8) list(*)
            REAL(kind = 8) sum
            INTEGER nb

            sum = 0
            do i = 1, nb
                sum = sum + list(i)
            end do
        END