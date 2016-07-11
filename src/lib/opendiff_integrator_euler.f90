!< Concrete class of Euler integrator.
module opendiff_integrator_euler
    !< Concrete class of Euler integrator.
    use opendiff_adt_equation
    use opendiff_adt_field
    use opendiff_adt_integrator
    use opendiff_field_fd_1d
    use opendiff_kinds

    implicit none
    private
    public :: integrator_euler

    type, extends(integrator) :: integrator_euler
        !< Euler integrator.
        contains
            procedure :: integrate !< Integrate the field accordingly the equation.
    endtype integrator_euler
contains
    function integrate(this, equ, t, inp) result(error)
        !< Integrate the field accordingly the equation by means of the Euler scheme.
        class(integrator_euler), intent(in)            :: this  !< The integrator.
        class(equation),         intent(in),    target :: equ   !< The equation.
        real(R_P),               intent(in)            :: t     !< Time.
        class(field),            intent(inout), target :: inp   !< Input field.
        integer(I_P)                                   :: error !< Error status.
        class(field), allocatable                      :: for   !< Temporary
       ! select type(inp)
       !     type is(field_fd_1d)
       !         print *,"t, dt, inp: ",t, this%dt, inp%val
       ! end select
        allocate(for, source=inp)
        ! the temporary variable for seems to be needed by intel compiler
        ! otherwise there is an internal compiler error or seg fault
        ! especially multiplying by this%dt. Why....?
        call equ%bc(inp=inp, t=t)

        for = equ%forcing(inp=inp, t=t)
        inp = inp + this%dt * for
        error = 0
    end function integrate
end module opendiff_integrator_euler
