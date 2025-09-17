
program secant_method
    implicit none

    real :: x0, x1, x2, f0,f1,tol

    integer :: max_iter, iter


    ! Initial Guesses
    x0 =-2.00
    x1 = -3.0
    tol = 1e-4


   

    ! Now our Secant Method Starts Here
    max_iter = 5
    do iter = 1, max_iter
        f0 =f(x0)
        f1 = f(x1)

        !Check for division by zero

        if (abs(f1-f0)<1e-10) then 
            print *, "Reach Machine Precsion. Stopping." ! Here I added to avoid machine precision error, not part of method
            exit
        end if
        ! Secant Method
        x2 = x1-f1*(x1-x0)/(f1-f0)
        print *, " Iteration:", iter, "x =", x2,  "f(x) =", f(x2)
        ! Now we check for Convergence Criterion
        if (abs(x2-x1)<tol) then
            print *, "Solution Converged to: x= ", x2, 'after', iter , 'iterations'
            exit
        end if
     if (iter ==max_iter) then
        print *,"Maximum Iterations Reached without Convergence, Try Again haha!"
        end if

        ! Update 
        x0 = x1 ! Old x1 becomes new x0
        x1 = x2 ! New x2  becomes new x1
        
    end do

contains
 ! Define our function
    real function f(x)
        real, intent(in):: x
        f = x**3 -x**2 +2.0
    end function f
    



end program secant_method