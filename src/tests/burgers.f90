module myequations

    use opendiff
    use opendiff_kinds

    implicit none
    private
    public :: burgers_equation

    type, extends(equation) :: burgers_equation
        ! The reason the next is a pointer is just to make it a pointee
        ! when pointed inside forcing_burgers function
        class(spatial_operator_der1), pointer :: der1
        contains
            procedure :: init => init_burgers
            procedure :: forcing => forcing_burgers
            procedure :: bc => bc_burgers
    endtype burgers_equation

contains

    function init_burgers(this) result(res)
        class(burgers_equation) :: this
        integer :: res

        ! The next is to be read by JSON
        allocate(spatial_operator_der1_fd_1d :: this%der1)

        res = 0
    end function init_burgers

    function forcing_burgers(this, inp, t) result(opr)
        class(burgers_equation) :: this
        class(field), target :: inp
        real(R8P) :: t
        class(field), allocatable :: opr
        class(spatial_operator_der1), pointer :: der1_cur

        allocate(opr, source=inp)

        der1_cur => this%der1
        opr = der1_cur%operate(inp)

!OK TOO        opr = this%der1%operate(inp)

    end function forcing_burgers

    subroutine bc_burgers(this, inp, t)
        class(burgers_equation) :: this
        class(field), target :: inp
        real(R8P) :: t
        class(field_fd_1d), pointer :: inp_cur
        class(mesh_fd_1d), pointer :: mesh_cur
        integer :: n,ng,i

        select type(inp)
            type is(field_fd_1d)
                inp_cur => inp
            class default
               STOP 'Error passing field to bc'
        end select

        associate(mm => inp%m)
            select type(mm)
                type is(mesh_fd_1d)
                    mesh_cur => mm
                class default
                   STOP 'Error getting mesh'
            end select
        end associate

        n  = mesh_cur%n
        ng = mesh_cur%ng
        do i=1-ng,0
            inp_cur%val(i) = inp_cur%val(i+n)
        enddo
        do i=n+1,n+ng
            inp_cur%val(i) = inp_cur%val(i-n)
        enddo

    end subroutine bc_burgers

end module myequations

program burgers

    use opendiff
    use myequations

    integer :: it
    integer :: er

    class(mesh), allocatable :: m1
    class(field), allocatable :: u1
    !TEST class(field), allocatable :: u2
    !TEST class(field), allocatable :: u3
    !TEST class(spatialop), allocatable :: der1d
    class(integrator), allocatable :: integ

    integer :: itmin=0, itmax=100000

    type(burgers_equation) :: burg_equ
    character(16) :: output_name

    ! These should be done reading from JSON input files and returning right
    ! pointers following factory pattern or similar
    allocate(mesh_fd_1d :: m1)
    allocate(field_fd_1d :: u1)
    !TEST allocate(field_fd_1d :: u2)
    !TEST allocate(spatialop_fd_1d_der1_c :: der1d)
    allocate(integrator_euler :: integ)

    !TEST allocate(u3, source=u1)

    integ%dt = 0.001

    call m1%init(error=er)

    call u1%init(m1, error=er)

    er = burg_equ%init()

    !TEST call u2%init(m1, error=er)
    !TEST u3 = u1 + u1 * u2
    !TEST u3 = der1d%operate(u1)

    output_name = "out_XXXXXXXX.dat"
    write(output_name(5:12),"(I8.8)") itmin
    call u1%output(output_name, error=er)
    do it = itmin, itmax
        er = integ%integrate(inp=u1, equ=burg_equ, t=it*integ%dt)
        if(mod(it,100)==0) then
            print*,'it: ',it
            write(output_name(5:12),"(I8.8)") it
            call u1%output(output_name, error=er)
        endif
    enddo

    call m1%output(error=er)

end program burgers
