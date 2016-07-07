!< Abstract class of integrator.
module opendiff_adt_integrator
    !< Abstract class of integrator.
    use opendiff_adt_field
    use opendiff_adt_equation
    use opendiff_kinds

    implicit none
    private
    public :: integrator

    type, abstract :: integrator
        !< Abstract class for *integrator* handling.
        character(len=:), allocatable :: description !< Mesh description.
        real(R8P)                     :: dt=0._R8P   !< Time step.
        contains
            procedure                               :: free      !< Free dynamic memory.
            procedure(abstract_integrate), deferred :: integrate !< Integrate the field accordingly the equation.
    endtype integrator

    abstract interface
        function abstract_integrate(this, inp, equ, t) result(res)
            import :: equation, field, integrator, R8P
            class(integrator)       :: this
            class(field), target    :: inp
            class(equation), target :: equ
            real(R8P)               :: t
            integer                 :: res
        end function abstract_integrate
    endinterface

contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(integrator), intent(inout) :: this !< The integrator.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module opendiff_adt_integrator
