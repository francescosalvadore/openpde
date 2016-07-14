!< Abstract class of equation.
module openpde_equation_abstract
    !< Abstract class of equation.
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
            procedure, pass(this) :: free !< Free dynamic memory.
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
end module openpde_equation_abstract
