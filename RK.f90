! Function f defined in program block with dy/dx = f(x,y) 
function f(x_in, y_in) result(f_out)
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    real(dp), intent(in) :: x_in, y_in
    real(dp) :: f_out
    ! Edit this line to represent the differential equation you want to solve. For example: dy/dx = x*y -> output = x_in*y_in
    f_out = y_in
end function f

! main RungeKutta function, returns y_i+1 for a given x and y_i
! uses y_i+1 = y_i + (a1+a2)/2; a1 = hf(x, y_i), a2 = hf(x+h, y_i+a1)
function RK2(y, x, h) result(RK_out)
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    real(dp), intent(in) :: y, x, h 
    real(dp) :: a1, a2
    real(dp) :: f, RK_out
    a1 = h*f(x,y)
    a2 = h*f(x+h,y+a1)
    RK_out = y + (a1+a2)/2
end function RK2

program RK
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none

    ! solve ODE using Runge Kutta Method, 
    ! Define f as function in x,y such that dy/dx = f(x,y) 
    real(dp) :: y_init, x, x_max, x_min, delta 
    real(dp) :: f, RK2
    integer :: i, i_max, err
    real(dp), dimension(:), allocatable :: y
    
    y_init = 1_dp
    x_min = 0_dp
    x_max = 1_dp
    i_max = 5

    ! Allocate memory for y, over range x_min, x_max with difference delta = (x_max-x_min)/i_max, allocate i_max floats
    x = x_min
    delta = (x_max-x_min)/i_max
    allocate(y(i_max), stat=err)
    if (err /= 0) print *, "y: Allocation request denied"

    y(1) = RK2(y_init, x, delta)
    ! Loop over entire array updating all values
    do i = 1, i_max-1
       y(i+1) = RK2(y(i), x+i*delta, delta)
    end do
   
    ! output the array
    do i = 1, i_max
        print *, y(i)
    end do

    if (allocated(y)) deallocate(y, stat=err)
    if (err /= 0) print *, "y: Deallocation request denied"

end program RK

