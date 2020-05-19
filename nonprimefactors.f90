! Counts the number of non-prime factors of a given number n.
! Works for any n <= 1000000.

module npf_module
    implicit none

    ! Declarations.
    integer, parameter :: max_n = 1000000
    integer, dimension (0:max_n + 1) :: factors, memo = -1

    public :: eratosthenes, npf

contains

    ! Runs the sieve of eratosthenes.
    subroutine eratosthenes ()
        integer :: i, j

        ! Initialize each of the factors to itself.
        do i = 0, max_n
            factors(i) = i
        end do

        ! Run the sieve.
        do i = 2, 1000
            if (factors(i) == i) then
                do j = i + i, max_n, i
                    factors(j) = i
                end do
            end if
        end do

    end subroutine eratosthenes

    ! Counts the number of non-prime factors of a given number.
    function npf (x) result (cnt)
        integer, intent (in) :: x
        integer :: cnt, pfactors, d, prod, n

        cnt = 0
        
        ! Handle trivial cases.
        if (memo(x) /= -1) then
            cnt = memo(x)
            return
        else if (x == 0 .or. x == 1) then
            memo(x) = cnt
            return
        end if

        ! Solve general case.
        n = x
        pfactors = 0
        prod = 1

        do while (n > 1)
            d = factors(n)
            cnt = 0

            do while (mod(n, d) == 0)
                n = n / d
                cnt = cnt + 1
            end do

            prod = prod * (cnt + 1)
            pfactors = pfactors + 1
        end do

        memo(x) = prod - pfactors
        cnt = memo(x)
    end function npf

end module npf_module

! Driver code.
program main
    use npf_module
    implicit none

    character(len = 100) :: input
    integer :: n

    call eratosthenes()

    ! Print information for the user.
    print '(a)', 'For any number of queries, this program gets the number of non-prime factors of a given number x <= 1000000.'
    print '(a)', 'To stop the queries and end the program, input "exit".'

    do while (input /= 'exit')
        read (*, *) input

        if (input /= 'exit') then
            read (input, *) n
            print '(a, i0)', 'The number of non-prime factors is: ', npf(n)
        end if
    end do

end program main