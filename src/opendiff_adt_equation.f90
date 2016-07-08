!< Abstract class of equation.
module opendiff_adt_equation
    !< Abstract class of equation.
    use opendiff_adt_field
    use opendiff_kinds

    implicit none
    private
    public :: equation

    type, abstract :: equation
        !< Abstract class for *equation* handling.
        !<
        !< The concrete types are implemented at application level (by the user)
        !< predefined examples might be provided as well.
        character(len=:), allocatable :: description !< Mesh description.
        contains
            ! deferred methods
            procedure(abstract_forcing), deferred :: forcing !< Forcing equation.
            procedure(abstract_init),    deferred :: init    !< Initialize equation.
            ! public methods
            procedure :: free !< Free dynamic memory.
    endtype equation

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

    abstract interface
        !< Initialize equation.
        function abstract_init(this) result(error)
            !< Initialize equation.
            import :: equation, field, I_P
            class(equation), intent(inout) :: this  !< The equation.
            integer(I_P)                   :: error !< Error status.
        end function abstract_init
    endinterface
contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(equation), intent(inout) :: this !< The equation.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module opendiff_adt_equation
