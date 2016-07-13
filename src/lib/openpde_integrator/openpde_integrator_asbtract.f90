!< Abstract class of integrator.
module openpde_integrator_abstract
    !< Abstract class of integrator.
    use openpde_field_abstract
    use openpde_equation_abstract
    use openpde_kinds

    implicit none
    private
    public :: integrator

    type, abstract :: integrator
        !< Abstract class of integrator.
        character(len=:), allocatable :: description !< Integrator description.
        real(R_P)                     :: dt=0._R_P   !< Time step.
        contains
            ! deferred methods
            procedure(abstract_integrate), pass(this), deferred :: integrate !< Integrate the field accordingly to the equation.
            ! public methods
            procedure, pass(this) :: free !< Free dynamic memory.
    endtype integrator

    abstract interface
        !< Integrate the field accordingly the equation.
        function abstract_integrate(this, equ, t, inp) result(error)
            !< Integrate the field accordingly to the equation.
            import :: equation, field, integrator, I_P, R_P
            class(integrator), intent(in)            :: this  !< The integrator.
            class(equation),   intent(in),    target :: equ   !< The equation.
            real(R_P),         intent(in)            :: t     !< Time.
            class(field),      intent(inout), target :: inp   !< Input field.
            integer(I_P)                             :: error !< Error status.
        end function abstract_integrate
    endinterface
contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(integrator), intent(inout) :: this !< The integrator.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module openpde_integrator_abstract
