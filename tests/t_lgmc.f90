PROGRAM t_logmc
    ! Use the low infant birth weight data from Hosmer & Lemeshow
    
    USE Logistic_Regression
    IMPLICIT NONE

    real(dp)    :: chisq, devnce, beta(0:3), se_beta(0:3), tail_prob
    integer     :: ptl, ht, ui, ftv, bwt, ncases, ndf, ier, NumEnt, i
    CHARACTER (LEN= 6)  :: vname(0:3) = (/  &
                       'Const.' , 'LCH   ', 'LProf ', 'LCHPro' /)
                       
    real(dp), allocatable    :: Xin(:,:)
    integer, allocatable :: IndEntP(:), IndEnt(:)
    

    NumEnt = 49514    
    allocate(IndEntP(NumEnt),IndEnt(NumEnt),Xin(NumEnt,3))

    IndEnt = 1

                       
    !open (UNIT = 10, FILE = 'log_inputs1.dat', ACTION = 'read')
    open (UNIT = 10, FILE = 'log_inputs1_scf.dat', ACTION = 'read')
        read(10,'(F11.5)') Xin(:,1)
    close (10) 
    !open (UNIT = 10, FILE = 'log_inputs2.dat', ACTION = 'read')
    open (UNIT = 10, FILE = 'log_inputs2_scf.dat', ACTION = 'read')    
        read(10,'(F11.5)') Xin(:,2)
    close (10)   
    !open (UNIT = 10, FILE = 'log_inputs3.dat', ACTION = 'read')
    open (UNIT = 10, FILE = 'log_inputs3_scf.dat', ACTION = 'read')    
        read(10,'(F11.5)') Xin(:,3)
    close (10)           
    !open (UNIT = 10, FILE = 'log_ind.dat', ACTION = 'read')
    open (UNIT = 10, FILE = 'log_ind_scf.dat', ACTION = 'read')    
        read(10,'(I5)') IndEntP
    close (10)   
    
    ! Call the logistic function
    call logistic(NumEnt, Xin, 3, IndEntP, IndEnt, &
    chisq, devnce, ndf, beta, se_beta, ier)    

    IF (ier /= 0) THEN
        WRITE(*, *) 'Error number', ier
    ELSE
        WRITE(*, '(a, f9.3, a, f9.3, a, i4, a)')  &
            ' Deviance = ', devnce, '   Chi-squared = ', chisq,   &
            ' with ', ndf, ' deg. of freedom'
        tail_prob = 1.0_dp - chi_squared(ndf, chisq)
        IF (tail_prob < 0.01_dp) THEN
        WRITE(*, *) '*** Significantly bad fit at the 1% level ***'
        END IF
        WRITE(*, '(a, g12.4/)') ' Chi-squared tail prob. = ', tail_prob
        WRITE(*, *) '        Coefficient   Std.error'
        DO i = 0, 3
        WRITE(*, '(" ", a6, "  ", 2g13.5)') vname(i), beta(i), se_beta(i)
        END DO
    
    END IF                       


END PROGRAM 