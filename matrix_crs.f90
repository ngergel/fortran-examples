! Matrix packing using the CRS format.

module matrix_crs_module
    implicit none

    ! Packed matrix type.
    type packed_mat
        integer, dimension (:), allocatable :: row_offsets, cols, vals
        integer :: n = 0, size = 0
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

        integer :: i, j, k, cnt = 0, row_cnts

        ! Count the number of entries.
        do i = 1, n
            do j = 1, n
                if (mat(i, j) /= 0) then
                    cnt = cnt + 1
                end if
            end do
        end do

        ! Initialize the packed matrix.
        call init_packed(packed, cnt, n)
    
        ! Populate packed matrix.
        k = 1
        packed%row_offsets = 0

        do i = 1, n
            row_cnts = 0

            do j = 1, n
                if (mat(j, i) /= 0) then
                    packed%cols(k) = j
                    packed%vals(k) = mat(j, i)
                    row_cnts = row_cnts + 1
                    
                    k = k + 1
                end if
            end do

            if (i < n) then
                packed%row_offsets(i + 1) = packed%row_offsets(i) + row_cnts
            end if
        end do

    end subroutine pack_matrix

    subroutine unpack_matrix (packed, dense)
        type (packed_mat), intent (in) :: packed
        integer, dimension (packed%n, packed%n), intent (out) :: dense
        integer :: i, k = 1

        ! Unpack the matrix.
        dense = 0
        do i = 1, packed%size
            dense(packed%cols(i), k) = packed%vals(i)

            if (i < packed%size .and. i >= packed%row_offsets(k + 1)) then
                k = k + 1
            end if
        end do
    
    end subroutine unpack_matrix

    ! Initialize a packed matrix.
    subroutine init_packed (packed, cnt, n)
        integer, intent (in) :: n, cnt
        type (packed_mat), intent (inout) :: packed

        packed%n = n
        packed%size = cnt

        allocate (packed%row_offsets(n))
        allocate (packed%cols(cnt))
        allocate (packed%vals(cnt))
    end subroutine init_packed

    ! Deallocates a packed matrix.
    subroutine destroy_packed (packed)
        type (packed_mat), intent (inout) :: packed

        deallocate (packed%row_offsets)
        deallocate (packed%cols)
        deallocate (packed%vals)
    end subroutine

end module matrix_crs_module


! Driver code.
program main
    use matrix_crs_module
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
    print '(a, i0)', 'Size of first packed matrix: ', size(pa%row_offsets) + size(pa%cols) + size(pa%vals) + 2

    print *, ''
    print '(a, i0)', 'Size of second dense matrix: ', size(b)
    print '(a, i0)', 'Size of second packed matrix: ', size(pb%row_offsets) + size(pb%cols) + size(pb%vals) + 2

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

