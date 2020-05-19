! Module for performing an anti-transpose.
! An anti-transpose is a transpose along the anti-diagonal.
module antitr_module
    implicit none

    public :: antitr

contains

    ! Performs anti-transpose.
    function antitr (a, n) result (at)
        ! a is input matrix, at is anti-transposed matrix.
        integer, dimension (n, n), intent (in) :: a
        integer, intent (in) :: n
        integer, dimension (n, n) :: at

        integer :: i

        do i = 1, n
            at(:, n - i + 1) = a(i, n:1:-1)
        end do

    end function antitr
    
end module antitr_module

! Driver code.
program main
    use antitr_module
    implicit none

    ! Size of the matrix.
    integer, parameter :: n = 5

    ! Declarations.
    integer, dimension (n, n) :: a
    integer, dimension (n) :: temp
    integer :: i

    ! Constructing the matrix to anti-transpose.
    ! Just filling out the values to be 1 through n^2.
    do i = 1, n
        temp(i) = i
    end do

    do i = 1, n
        a(i, :) = temp
        temp = temp + n
    end do

    ! Perform anti-transposes and prints results.
    print '(a)', 'Perform an anti-traspose.'
    print '(a)', 'Before:'
    call print_mat(a, n)
    
    a = antitr(a, n)

    print '(2a)', new_line('a'), 'After:'
    call print_mat(a, n)

contains

    ! Helper subroutine for printing the matrix nicely.
    subroutine print_mat (a, n)
        integer, dimension (n, n), intent (in) :: a
        integer, intent (in) :: n
        integer :: i, j

        do i = 1, n
            do j = 1, n
                write (*, '(i3, x)', advance='no') a(i, j)
            end do
            print *, ''
        end do

    end subroutine print_mat

end program main