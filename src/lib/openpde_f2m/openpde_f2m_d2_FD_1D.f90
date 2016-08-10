!< Concrete class of field to matrix (implicit) second derivative operator for FD 1D
module openpde_f2m_d2_FD_1D
    !< Concrete class of field to matrix (implicit) second derivative operator for FD 1D
    use openpde_field_abstract
    use openpde_f2m_d2_abstract
    use openpde_field_FD_1D
    use openpde_kinds
    use openpde_mesh_FD_1D
    use openpde_matrix_abstract

    implicit none
    private
    public :: f2m_d2_FD_1D

    type, extends(f2m_d2) :: f2m_d2_FD_1D
        !< Concrete class of field to matrix (implicit) second derivative operator for FD 1D
        contains
            procedure :: operate !< Operator operation.
    endtype f2m_d2_FD_1D
contains
    function operate(this, inp, i_equ, i_fie, dir) result(opr)
        !< Operator 2 derivative implicit FD 1D
        class(f2m_d2_FD_1D), intent(in)                        :: this     !< The operator.
        class(field), dimension(:),       intent(in), target   :: inp      !< Input field.
        class(matrix), allocatable, target                     :: opr      !< Matrix representing the operator application.
        class(field_FD_1D), pointer                            :: inp_cur  !< Dummy pointer for input field.
        class(mesh_FD_1D),  pointer                            :: mesh_cur !< Dummy pointer for mesh.
        integer(I_P)                                           :: i        !< Counter.
        integer(I_P)                                           :: n        !< Number of points.
        integer(I_P)                                           :: n_equ    !< Number of points.
        integer(I_P)                                           :: n_tot    !< Number of points.
        integer(I_P), intent(in), optional                     :: i_equ    !< Counter.
        real(R_P)                                              :: invd     !< Temporary 1/D**2
        integer(I_P),            intent(in), optional :: dir  !< Direction of operation.
        integer(I_P), intent(in), optional                     :: i_fie    !< Counter.
        integer(I_P)                                           :: i_equ_    !< Counter.
        integer(I_P)                                           :: i_fie_    !< Counter.
        integer(I_P)                                           :: i_row_offset
        integer(I_P)                                           :: i_col_offset

        i_equ_= 1 ; if (present(i_equ)) i_equ_ = i_equ                 
        i_fie_= 1 ; if (present(i_fie)) i_fie_ = i_fie                 

        n_equ = size(inp)
        mesh_cur => associate_mesh_FD_1D(mesh_input=inp(i_equ_)%m)
        n =  mesh_cur%n
        n_tot = n * n_equ

        ! no concrete features of matrix are used so dynamic casting is not needed
        invd = 1._R_P/mesh_cur%h**2

        allocate(opr, mold=this%mat)
        call opr%init(n_tot)

        inp_cur => associate_field_FD_1D(field_input=inp(i_equ_), emsg='casting error')

        i_row_offset = (i_equ_-1)*n
        i_col_offset = (i_fie_-1)*n

        call opr%set(1_I_P+i_row_offset, 1_I_P+i_col_offset, invd)

        do i=2, n - 1
            call opr%set(i+i_row_offset, i+i_col_offset, -2._R_P * invd)
            call opr%set(i+i_row_offset, i+i_col_offset-1, 1._R_P * invd)
            call opr%set(i+i_row_offset, i+i_col_offset+1,  1._R_P * invd)
        enddo

        call opr%set(n+i_row_offset, n+i_col_offset, invd)

    end function operate
end module openpde_f2m_d2_FD_1D
