! Program that blurs image data, by making every pixel the average of it's neighbours.
! Does not handle any actual image formats, so it just applies the bluring to dummy data.

module img_blur_module
    implicit none

    ! Custom pixel type, just has RGB values.
    type pixel
        integer :: r, g, b
    end type pixel

    ! Function declarations.
    public :: blur, set_pixel

contains

    function blur(img, n) result (blurred)
        integer, intent (in) :: n
        type (pixel), dimension (n, n), intent (in) :: img
        type (pixel), dimension (n, n) :: blurred

        integer :: i, j, cnt, r, g, b
        type (pixel) :: p

        do i = 1, n
            do j = 1, n
                cnt = 1
                r = img(i, j)%r
                g = img(i, j)%g
                b = img(i, j)%b

                ! Get average RGB values.
                if (i > 1) then
                    cnt = cnt + 1
                    r = r + img(i - 1, j)%r
                    g = g + img(i - 1, j)%g
                    b = b + img(i - 1, j)%b
                end if
                if (i < n) then
                    cnt = cnt + 1
                    r = r + img(i + 1, j)%r
                    g = g + img(i + 1, j)%g
                    b = b + img(i + 1, j)%b
                end if
                if (j > 1) then
                    cnt = cnt + 1
                    r = r + img(i, j - 1)%r
                    g = g + img(i, j - 1)%g
                    b = b + img(i, j - 1)%b
                end if
                if (j < n) then
                    cnt = cnt + 1
                    r = r + img(i, j + 1)%r
                    g = g + img(i, j + 1)%g
                    b = b + img(i, j + 1)%b
                end if

                ! Set the new, averaged value.
                call set_pixel(r / cnt, g / cnt, b / cnt, p)
                blurred(i, j) = p
            end do
        end do

    end function blur

    ! Sets a given pixel's RGB values.
    subroutine set_pixel(r, g, b, p)
        integer, intent (in) :: r, g, b
        type (pixel), intent (out) :: p

        p%r = r
        p%g = g
        p%b = b
    end subroutine set_pixel

end module img_blur_module


! Driver code.
program main
    use img_blur_module
    implicit none

    ! Set up a dummy image that is just half green and half red.
    integer, parameter :: n = 6
    type (pixel), dimension (n, n) :: img, blurred

    integer :: i, j
    type (pixel) :: green, red

    ! Set green and red pixel types.
    call set_pixel(0, 255, 0, green)
    call set_pixel(255, 0, 0, red)

    ! Color the dummy image.
    do i = 1, n / 2
        do j = 1, n
            img(i, j) = green
        end do
    end do
    do i = (n / 2) + 1, n
        do j = 1, n
            img(i, j) = red
        end do
    end do

    ! Blur it and print the results.
    blurred = blur(img, n)

    print '(a)', 'Original image:'
    do i = 1, n
        do j = 1, n
            write (*, '(a,i0,a,i0,a,i0,a,x)', advance='no') '(', img(j, i)%r, ',', img(j,i)%g, ',', img(j,i)%b, ')'
        end do
        print *, ''
    end do

    print '(a)', 'Blurred image:'
    do i = 1, n
        do j = 1, n
            write (*, '(a,i0,a,i0,a,i0,a,x)', advance='no') '(', blurred(j, i)%r, ',', blurred(j,i)%g, ',', blurred(j,i)%b, ')'
        end do
        print *, ''
    end do

end program main