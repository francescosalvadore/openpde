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
        function abstract_integrate(this, equ, t, inp) result(res)
            import :: equation, field, integrator, R8P
            class(integrator), intent(in)            :: this
            class(equation),   intent(in),    target :: equ
            real(R8P),         intent(in)            :: t
            class(field),      intent(inout), target :: inp
            integer                                  :: res
        end function abstract_integrate
    endinterface

contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(integrator), intent(inout) :: this !< The integrator.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module opendiff_adt_integrator
