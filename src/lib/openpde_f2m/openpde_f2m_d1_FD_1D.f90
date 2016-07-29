!< Concrete class of field to matrix (implicit) second derivative operator for FD 1D
module openpde_f2m_d1_FD_1D
    !< Concrete class of field to matrix (implicit) second derivative operator for FD 1D
    use openpde_field_abstract
    use openpde_f2m_d1_abstract
    use openpde_field_FD_1D
    use openpde_kinds
    use openpde_mesh_FD_1D
    use openpde_matrix_abstract

    implicit none
    private
    public :: f2m_d1_FD_1D

    type, extends(f2m_d1) :: f2m_d1_FD_1D
        !< Concrete class of field to matrix (implicit) second derivative operator for FD 1D
        contains
            procedure :: operate !< Operator operation.
    endtype f2m_d1_FD_1D
contains
    function operate(this, inp) result(opr)
        !< Operator 2 derivative implicit FD 1D
        class(f2m_d1_FD_1D), intent(in)                        :: this     !< The operator.
        class(field),  dimension(:), intent(in), target        :: inp      !< Input field.
        class(matrix), allocatable, target                     :: opr      !< Matrix representing the operator application.
        class(field_FD_1D), pointer                            :: inp_cur  !< Dummy pointer for input field.
        class(mesh_FD_1D),  pointer                            :: mesh_cur !< Dummy pointer for mesh.
        integer(I_P)                                           :: i        !< Counter.
        integer(I_P)                                           :: n        !< Number of points.
        integer(I_P)                                           :: n_equ    !< Number of points.
        integer(I_P)                                           :: n_tot    !< Number of points.
        integer(I_P)                                           :: i_equ    !< Counter.
        real(R_P)                                              :: invd     !< Temporary 1/D**2

        n_equ = size(inp)
        mesh_cur => associate_mesh_FD_1D(mesh_input=inp(1)%m)
        n =  mesh_cur%n
        n_tot = n * n_equ

        ! no concrete features of matrix are used so dynamic casting is not needed
        invd = 1._R_P/(2._R_P*mesh_cur%h)

        allocate(opr, mold=this%mat)
        call opr%init(n)

        do i_equ = 1,n_equ
            inp_cur => associate_field_FD_1D(field_input=inp(i_equ), emsg='casting error')

            call opr%set(1_I_P, 1_I_P, invd)

            do i=2, n - 1
                call opr%set(i, i-1,  -1._R_P * invd)
                call opr%set(i, i+1,  1._R_P * invd)
            enddo

            call opr%set(n, n, invd)
        enddo

    end function operate
end module openpde_f2m_d1_FD_1D
