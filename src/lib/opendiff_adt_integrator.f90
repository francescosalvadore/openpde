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
        real(R_P)                     :: dt=0._R_P   !< Time step.
        contains
            procedure                               :: free      !< Free dynamic memory.
            procedure(abstract_integrate), deferred :: integrate !< Integrate the field accordingly the equation.
    endtype integrator

    abstract interface
        !< Integrate the field accordingly the equation.
        function abstract_integrate(this, equ, t, inp) result(error)
            !< Integrate the field accordingly the equation.
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
end module opendiff_adt_integrator
