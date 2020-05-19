! Solution for the Cubestat lab in Fortran.
! Due to not really being able to set up the memory to read from,
! The data points are just inputed sequentially via stdin.

program main
    implicit none

    ! Declarations.
    integer :: dim, edge, i, i1, i2, i3, i4
    integer, dimension (4) :: all_dim
    real, dimension (:,:,:,:), allocatable :: cube
    real :: neg_cnt = 0, neg_sum = 0, pos_cnt = 0, pos_sum = 0

    ! Read in the dimension and edge length.
    print '(a)', 'Dimension and edge length of cube:'
    read (*, *) dim, edge

    ! Set dimensions and allocate cube.
    do i = 1, 4
        if (i <= dim) then
            all_dim(i) = edge
        else
            all_dim(i) = 1
        end if
    end do

    ! Allocate memory for the cube.
    allocate (cube(all_dim(1), all_dim(2), all_dim(3), all_dim(4)))

    ! Go through reading in every point.
    do i1 = 1, all_dim(1)
        do i2 = 1, all_dim(2)
            do i3 = 1, all_dim(3)
                do i4 = 1, all_dim(4)
                    if (dim == 1) then
                        print '(a, i0, a)', 'Point (', i1, '):'
                    else if (dim == 2) then
                        print '(a, i0, a, i0, a)', 'Point (', i1, ', ', i2, '):'
                    else if (dim == 3) then
                        print '(a, i0, a, i0, a, i0, a)', 'Point (', i1, ', ', i2, ', ', i3, '):'
                    else if (dim == 4) then
                        print '(a, i0, a, i0, a, i0, a, i0, a)', 'Point (', i1, ', ', i2, ', ', i3, ', ', i4, '):'
                    end if
                    
                    read (*, *) cube(i1, i2, i3, i4)

                    ! Update the stats.
                    if (cube(i1, i2, i3, i4) < 0) then
                        neg_cnt = neg_cnt + 1
                        neg_sum = neg_sum + cube(i1, i2, i3, i4)
                    else if (cube(i1, i2, i3, i4) > 0) then
                        pos_cnt = pos_cnt + 1
                        pos_sum = pos_sum + cube(i1, i2, i3, i4)
                    end if
                end do
            end do
        end do
    end do

    print '(a,f0.5,a,f0.5,a,f0.5)', 'Positive Count, Sum, and Average: ', pos_cnt, ', ', pos_sum, ', ', pos_sum / pos_cnt
    print '(a,f0.5,a,f0.5,a,f0.5)', 'Negative Count, Sum, and Average: ', neg_cnt, ', ', neg_sum, ', ', neg_sum / neg_cnt

    ! Free the memory.
    deallocate (cube)

end program main