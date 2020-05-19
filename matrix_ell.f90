! Matrix packing using the CRS format.

module matrix_ell_module
    implicit none

    ! Packed matrix type.
    type packed_mat
        integer, dimension (:, :), allocatable :: cols, vals
        integer :: n = 0, max_row = 0
    end type packed_mat

    ! Function declarations.
    public :: pack_matrix, unpack_matrix, destroy_packed
    private :: init_packed

contains

    ! Constructs a packed matrix.
    subroutine pack_matrix (mat, n, packed)
        integer, dimension (n, n), intent (in) :: mat
        integer, intent (in) :: n
        type (packed_mat), intent (out) :: packed

        integer :: i, j, max_row = 0, cnt

        ! Find the number of entries in a given row.
        do i = 1, n
            cnt = 0

            do j = 1, n
                if (mat(j, i) /= 0) then
                    cnt = cnt + 1
                end if
            end do

            max_row = max(max_row, cnt)
        end do

        ! Initialize the packed matrix.
        call init_packed(packed, max_row, n)
    
        ! Populate packed matrix.
        do i = 1, n
            ! 'cnt' is reused to count non-zero columns for indexing.
            cnt = 1

            do j = 1, n
                if (mat(j, i) /= 0) then
                    packed%vals(i, cnt) = mat(j, i)
                    packed%cols(i, cnt) = j

                    cnt = cnt + 1
                end if
            end do
        end do

    end subroutine pack_matrix

    subroutine unpack_matrix (packed, dense)
        type (packed_mat), intent (in) :: packed
        integer, dimension (packed%n, packed%n), intent (out) :: dense
        integer :: i, j

        ! Unpack the matrix.
        dense = 0
        do i = 1, packed%n
            do j = 1, packed%max_row
                if (packed%vals(i, j) /= 0) then
                    dense(packed%cols(i, j), i) = packed%vals(i, j)
                end if
            end do
        end do
    
    end subroutine unpack_matrix

    ! Initialize a packed matrix.
    subroutine init_packed (packed, cnt, n)
        integer, intent (in) :: n, cnt
        type (packed_mat), intent (inout) :: packed

        packed%n = n
        packed%max_row = cnt

        allocate (packed%cols(n, cnt))
        allocate (packed%vals(n, cnt))

        packed%cols = 0
        packed%vals = 0
    end subroutine init_packed

    ! Deallocates a packed matrix.
    subroutine destroy_packed (packed)
        type (packed_mat), intent (inout) :: packed

        deallocate (packed%cols)
        deallocate (packed%vals)
    end subroutine

end module matrix_ell_module


! Driver code.
program main
    use matrix_ell_module
    implicit none

    ! Size of the matrices.
    integer, parameter :: n = 10, m = 2000

    ! Declarations.
    integer, dimension (n, n) :: a = 0, c
    integer, dimension (m, m) :: b = 0
    type (packed_mat) :: pa, pb
    integer :: i, j
    
    ! Assign a few entries to build a sparse matrix.
    a(1, 1) = 1
    a(2, 6) = -5
    a(5, 10) = 4
    a(2, 2) = 5
    a(8, :) = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10/)

    ! Assign a bunch of values in a much larger sparse matrix.
    do i = 1, m, 10
        do j = 1, m, 5
            b(i, j) = i + j
        end do
    end do

    ! Pack each matrix.
    call pack_matrix(a, n, pa)
    call pack_matrix(b, m, pb)

    ! Print results.
    print '(a, i0)', 'Size of first dense matrix: ', size(a)
    print '(a, i0)', 'Size of first packed matrix: ', size(pa%cols) + size(pa%vals) + 2

    print *, ''
    print '(a, i0)', 'Size of second dense matrix: ', size(b)
    print '(a, i0)', 'Size of second packed matrix: ', size(pb%cols) + size(pb%vals) + 2

    ! We can also unpack a matrix.
    call unpack_matrix(pa, c)

    print '(a)', 'Unpacking A into a new matrix C:'
    do i = 1, n
        do j = 1, n
            write (*, '(i3, x)', advance='no') c(j, i)
        end do
        print *, ''
    end do

    ! Finally destroy the packed matrices to free up the memory.
    call destroy_packed(pa)
    call destroy_packed(pb)

end program main

