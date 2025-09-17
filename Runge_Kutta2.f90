! Runge _Kutta 2nd Order Method coded by Veasna
! Part of Assigment at IIT Bombay, Course CMFTE by Prof. Rajneesh Bhardwaj





program RungeKutta2

    implicit none
    integer :: i, ne
    real :: x0, h, k1, k2, xfinal, x,x_pred
    real :: y0, y1, y_pred, y

    ! Initial Conditions
    ! I took these problem from CMFTE Course at IIT Bombay, I wall upload note too
    x0 = 0.0
    xfinal = 280.0  ! After 280 seconds
    y0 = 1200.0     ! Temperature at Initial Condition (K)
    ne = 10         ! Number of steps

    h = (xfinal - x0) / ne  ! Step size

    ! Runge-Kutta 2nd Order Method
    do i = 1, ne
        k1 = f(x0, y0)
        x_pred = x0 +h   ! Predictor x value, you slightly change x to increase a bit
        y_pred = y0 + h * k1         ! Predictor step, good way to write like this for readability 
        k2 = f(x_pred, y_pred)       ! Evaluate slope at predicted point
        
        y1 = y0 + h/2.0 * (k1 + k2)  ! Corrector step (RK2 formula)

        print*, "Step:", i, "X:", x0, "Y:", y0
        ! Update for next step
        x0 = x_pred
        y0 = y1
        
    end do

    ! Final results
    print*, "Final Results:"
    print*, "X:", x0, "Y:", y0

contains
    ! Function corresponding to ODE dy/dx = f(x,y)
    real function f(x, y)
        real, intent(in) :: x, y
        f = -2.2067e-12 * (y**4 - 81.0e8)
    end function f

end program RungeKutta2
