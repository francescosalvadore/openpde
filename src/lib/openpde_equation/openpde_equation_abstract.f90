!< Abstract class of equation.
module openpde_equation_abstract
    !< Abstract class of equation.
    use json_module
    use openpde_field_abstract
    use openpde_kinds

    implicit none
    private
    public :: equation

    type, abstract :: equation
        !< Abstract class of equation.
        !<
        !< The concrete types are implemented at application level (by the user) predefined examples might be provided as well.
        character(len=:), allocatable :: description !< Equation description.
        contains
            ! deferred public methods
            procedure(abstract_bc),      pass(this), deferred :: bc      !< Equation boundary conditions.
            procedure(abstract_forcing), pass(this), deferred :: forcing !< Forcing equation.
            procedure(abstract_init),    pass(this), deferred :: init    !< Initialize the equation.
            ! public methods
            procedure, pass(this) :: free                   !< Free dynamic memory.
            generic               :: load => load_from_json !< Load equation definition from file.
            ! private methods
            procedure, pass(this) :: load_from_json !< Load equation definition from jSON file.
    endtype equation

    abstract interface
        !< Equation boundary conditions.
        subroutine abstract_bc(this, inp, t)
            !< Equation boundary condition.
            import :: equation, field, R_P
            class(equation), intent(in)            :: this !< The equation.
            class(field),    intent(inout), target :: inp  !< Input field.
            real(R_P),       intent(in)            :: t    !< Time.
        end subroutine abstract_bc
    endinterface

    abstract interface
        !< Initialize equation.
        function abstract_init(this) result(error)
            !< Initialize equation.
            import :: equation, field, I_P
            class(equation), intent(inout) :: this  !< The equation.
            integer(I_P)                   :: error !< Error status.
        end function abstract_init
    endinterface

    abstract interface
        !< Return the field after forcing the equation.
        function abstract_forcing(this, inp, t) result(opr)
            !< Return the field after forcing the equation.
            import :: equation, field, R_P
            class(equation), intent(in)         :: this !< The equation.
            class(field),    intent(in), target :: inp  !< Input field.
            real(R_P),       intent(in)         :: t    !< Time.
            class(field), allocatable           :: opr  !< Field computed.
        end function abstract_forcing
    endinterface
contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(equation), intent(inout) :: this !< The equation.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free

    subroutine load_from_json(this, filename, error)
        !< Load equation definition from JSON file.
        class(equation), intent(inout)         :: this     !< The equation.
        character(*),    intent(in)            :: filename !< File name of JSON file.
        integer(I_P),    intent(out), optional :: error    !< Error status.
        type(json_file)                        :: json     !< JSON file handler.
        !logical                                :: found
        call json%initialize()
        call json%load_file(filename=filename)
        !call json%get('version.major', i, found)
      endsubroutine load_from_json
end module openpde_equation_abstract
