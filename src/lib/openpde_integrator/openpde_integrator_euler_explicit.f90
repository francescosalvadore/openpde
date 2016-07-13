!< Concrete class of integrator, Euler explicit scheme.
module openpde_integrator_euler_explicit
    !< Concrete class of integrator, Euler explicit scheme.
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
            procedure :: integrate !< Integrate the field accordingly to the equation.
    endtype integrator_euler_explicit
contains
    function integrate(this, equ, t, inp) result(error)
        !< Integrate the field accordingly the to equation by means of the Euler explicit scheme.
        class(integrator_euler_explicit), intent(in)            :: this  !< The integrator.
        class(equation),                  intent(in),    target :: equ   !< The equation.
        real(R_P),                        intent(in)            :: t     !< Time.
        class(field),                     intent(inout), target :: inp   !< Input field.
        integer(I_P)                                            :: error !< Error status.
        class(field), allocatable                               :: for   !< Temporary
        call equ%bc(inp=inp, t=t)
        allocate(for, source=inp)
        ! the temporary variable for seems to be needed by intel compiler
        ! otherwise there is an internal compiler error or seg fault
        ! especially multiplying by this%dt. Why....?
        for = equ%forcing(inp=inp, t=t)
        inp = inp + this%dt * for
        error = 0
    end function integrate
end module openpde_integrator_euler_explicit
