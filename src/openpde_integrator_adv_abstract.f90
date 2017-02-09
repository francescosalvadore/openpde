!< Abstract class of integrator.
module openpde_integrator_adv_abstract
    !< Abstract class of integrator.
    use openpde_field_abstract
    use openpde_equation_adv
    use openpde_kinds

    implicit none
    private
    public :: integrator_adv

    type, abstract :: integrator_adv
        !< Abstract class of integrator.
        character(len=:), allocatable :: description !< Integrator description.
        real(R_P)                     :: dt=0._R_P   !< Time step.
        contains
            ! deferred public methods
            procedure(abstract_init),      pass(this), deferred :: init      !< Initilize integrator.
            procedure(abstract_integrate), pass(this), deferred :: integrate !< Integrate the field accordingly to the equation.
            ! public methods
            procedure, pass(this) :: free !< Free dynamic memory.
    endtype integrator_adv

    abstract interface
        !< Initialize integrator.
        subroutine abstract_init(this, equ, description, filename, error)
            !< Initialize integrator.
            import :: I_P, integrator_adv, equation_adv
            class(integrator_adv),  intent(inout)   :: this        !< The integrator.
            class(equation_adv),                  intent(inout),    target :: equ   !< The equation.
            character(*), intent(in),  optional :: description !< Integrator description.
            character(*), intent(in),  optional :: filename    !< Initialization file name.
            integer(I_P), intent(out), optional :: error       !< Error status.
        end subroutine abstract_init
    endinterface

    abstract interface
        !< Integrate the field accordingly the equation.
        function abstract_integrate(this, equ, t, inp) result(error)
            !< Integrate the field accordingly to the equation.
            import :: equation_adv, field, integrator_adv, I_P, R_P
            class(integrator_adv), intent(inout)            :: this  !< The integrator.
            class(equation_adv),   intent(inout),    target :: equ   !< The equation.
            real(R_P),         intent(in)            :: t     !< Time.
            class(field),      intent(inout), target, dimension(:) :: inp   !< Input field.
            integer(I_P)                             :: error !< Error status.
        end function abstract_integrate
    endinterface
contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(integrator_adv), intent(inout) :: this !< The integrator.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module openpde_integrator_adv_abstract
