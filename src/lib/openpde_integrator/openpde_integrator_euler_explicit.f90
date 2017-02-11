!< Concrete class of integrator, Euler explicit scheme.
module openpde_integrator_euler_explicit
    !< Concrete class of integrator, Euler explicit scheme.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use json_module
    use openpde_equation_abstract
    use openpde_field_abstract
    use openpde_integrator_abstract
    use openpde_kinds

    implicit none
    private
    public :: integrator_euler_explicit

    type, extends(integrator) :: integrator_euler_explicit
        !< Concrete class of integrator, Euler explicit scheme.
        contains
            ! deferred public methods
            procedure, pass(this) :: init      !< Initilize integrator.
            procedure, pass(this) :: integrate !< Integrate the field accordingly to the equation.
            ! public methods
            generic :: load => load_from_json !< Load integrator definition from file.
            ! private methods
            procedure, pass(this), private :: load_from_json !< Load integrator definition from jSON file.
    endtype integrator_euler_explicit
contains
    ! deferred public methods
    subroutine init(this, equ, description, filename, error)
        !< Initialize integrator.
        class(integrator_euler_explicit), intent(inout)         :: this        !< The integrator.
        class(equation),                  intent(inout), target :: equ         !< The equation.
        character(*),                     intent(in),  optional :: description !< Integrator description.
        character(*),                     intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),                     intent(out), optional :: error       !< Error status.

        call this%free
        if (present(description)) this%description = description
        if (present(filename)) then
            call this%load(filename=filename, error=error)
        else
            this%dt = 0.001_R_P
            if (present(error)) error = 0
        endif
    end subroutine init

    function integrate(this, equ, t, inp) result(error)
        !< Integrate the field accordingly the to equation by means of the Euler explicit scheme.
        use openpde_field_FD_1D
        class(integrator_euler_explicit), intent(inout)         :: this   !< The integrator.
        class(equation),                  intent(inout), target :: equ    !< The equation.
        real(R_P),                        intent(in)            :: t      !< Time.
        class(field),                     intent(inout), target :: inp(:) !< Input field.
        integer(I_P)                                            :: error  !< Error status.
        class(field), allocatable, dimension(:)                 :: for    !< Temporary
        integer(I_P)                                            :: ie     !< Count equation

        ! Imposes boundary conditions: modify "inp" field array
        call equ%bc_e(inp=inp, t=t)

        !allocate(for(size(inp)), mold=inp(1))
        !do ie=1,size(inp)
        !    call for(ie)%init(field_mesh=inp(1)%m)
        !enddo
        ! the temporary variable for seems to be needed by intel compiler
        ! otherwise there is an internal compiler error or seg fault
        ! especially multiplying by this%dt. Why....?
        !associate(for1 => for(1))
        !    select type(for1)
        !        type is(field_FD_1D)
        !            print*,"size of for1: ",size(for1%val)
        !    end select
        !endassociate

        ! Computes the residual term: modify "equ%resvar_e"
        call equ%resid_e(inp=inp, t=t)

        ! Updates the inp field according to Euler scheme
        do ie=1,size(inp)
            inp(ie) = inp(ie) + this%dt * equ%resvar_e(ie)
        enddo
        !RIMETTERE for = equ%resid_e(inp=inp, t=t)
        !RIMETTERE inp = inp + this%dt * for
        error = 0
    end function integrate

    ! private methods
    subroutine load_from_json(this, filename, error)
        !< Load integrator definition from JSON file.
        class(integrator_euler_explicit), intent(inout)         :: this            !< The integrator.
        character(*),                     intent(in)            :: filename        !< File name of JSON file.
        integer(I_P),                     intent(out), optional :: error           !< Error status.
        character(len=:), allocatable                           :: integrator_type !< Integrator type.
        type(json_file)                                         :: json            !< JSON file handler.
        logical                                                 :: found           !< Flag inquiring the result json parsing.

        call json%initialize()
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        call json%load_file(filename=filename)
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        call json%get('integrator.type', integrator_type, found)
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        if (.not.found) then
            write(stderr, "(A)")' error: integrator definition of "'//filename//'" incomplete!'
            write(stderr, "(A)")'   "type" missing'
            stop
        endif
        if (integrator_type=="euler explicit") then
            call json%get('integrator.dt', this%dt, found)
            if (json%failed()) then
                call json%print_error_message(stderr) ; stop
            end if
            if (.not.found) then
                write(stderr, "(A)")' error: integrator definition of "'//filename//'" incomplete!'
                write(stderr, "(A)")'   "dt" missing'
                stop
            endif
        else
            write(stderr, "(A)")' error: integrator definition of "'//filename//'" is not "euler explicit"!'
            stop
        endif
    endsubroutine load_from_json
end module openpde_integrator_euler_explicit
