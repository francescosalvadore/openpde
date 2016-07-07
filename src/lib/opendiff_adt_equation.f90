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
            procedure(abstract_forcing), deferred :: forcing   !< Forcing equation.
            procedure(abstract_init),    deferred :: init      !< Initialize the equation.
            ! public methods
            procedure :: free !< Free dynamic memory.
    endtype equation

    abstract interface
        function abstract_forcing(this, inp, t) result(opr)
            import :: equation, field, R8P
            class(equation)           :: this
            class(field), target      :: inp
            class(field), allocatable :: opr
            real(R8P)                 :: t
        end function abstract_forcing
    endinterface

    abstract interface
        function abstract_init(this) result(res)
            import :: equation, field
            class(equation) :: this
            integer         :: res
        end function abstract_init
    endinterface
contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(equation), intent(inout) :: this !< The equation.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module opendiff_adt_equation
