!< Concrete class of field to matrix (implicit) second derivative operator for FD 1D
module openpde_f2m_d2_FD_2D
    !< Concrete class of field to matrix (implicit) second derivative operator for FD 1D
    use openpde_field_abstract
    use openpde_f2m_d2_abstract
    use openpde_field_FD_2D
    use openpde_kinds
    use openpde_mesh_FD_2D
    use openpde_matrix_abstract

    implicit none
    private
    public :: f2m_d2_FD_2D

    type, extends(f2m_d2) :: f2m_d2_FD_2D
        !< Concrete class of field to matrix (implicit) second derivative operator for FD 1D
        contains
            procedure :: operate !< Operator operation.
    endtype f2m_d2_FD_2D
contains
    function operate(this, inp, i_equ, i_fie, dir) result(opr)
        !< Operator 2 derivative implicit FD 1D
        class(f2m_d2_FD_2D), intent(in)                        :: this         !< The operator.
        class(field), dimension(:),       intent(in), target   :: inp          !< Input field.
        class(matrix), allocatable, target                     :: opr          !< Matrix representing the operator application.
        class(field_FD_2D), pointer                            :: inp_cur      !< Dummy pointer for input field.
        class(mesh_FD_2D),  pointer                            :: mesh_cur     !< Dummy pointer for mesh.
        integer(I_P)                                           :: i, j         !< Counter.
        integer(I_P)                                           :: n, nx, ny    !< Number of points.
        integer(I_P)                                           :: n_equ        !< Number of points.
        integer(I_P)                                           :: n_tot        !< Number of points.
        integer(I_P), intent(in), optional                     :: i_equ        !< Counter.
        integer(I_P), intent(in), optional                     :: i_fie        !< Counter.
        integer(I_P)                                           :: i_equ_       !< Counter.
        integer(I_P)                                           :: i_fie_       !< Counter.
        real(R_P)                                              :: invd        !< Temporary 1/D**2
        real(R_P)                                              :: invdx        !< Temporary 1/D**2
        real(R_P)                                              :: invdy        !< Temporary 1/D**2
        integer(I_P),            intent(in), optional :: dir                   !< Direction of operation.
        integer(I_P)                                           :: dir_         !< Direction of operation.
        integer(I_P)                                           :: i_row_offset
        integer(I_P)                                           :: i_col_offset
        integer(I_P)                                           :: i_dir_offset
        integer(I_P)                                           :: i_vec

        dir_= 1   ; if (present(dir))   dir_   = dir
        i_equ_= 1 ; if (present(i_equ)) i_equ_ = i_equ
        i_fie_= 1 ; if (present(i_fie)) i_fie_ = i_fie

        n_equ = size(inp)
        mesh_cur => associate_mesh_FD_2D(mesh_input=inp(i_equ_)%m)
        nx =  mesh_cur%nx
        ny =  mesh_cur%ny
        n = nx * ny
        n_tot = n * n_equ

        ! no concrete features of matrix are used so dynamic casting is not needed
        invdx = 1._R_P/mesh_cur%hx**2
        invdy = 1._R_P/mesh_cur%hy**2

        allocate(opr, mold=this%mat)
        call opr%init(n_tot)

        i_row_offset = (i_equ_-1)*n
        i_col_offset = (i_fie_-1)*n
        if(dir_ == 1) then
            i_dir_offset = 1
            invd = invdx
        endif
        if(dir_ == 2) then
            i_dir_offset = nx
            invd = invdy
        endif

        inp_cur => associate_field_FD_2D(field_input=inp(i_equ_), emsg='casting error')

        do j=2, ny - 1
        do i=2, nx - 1
            i_vec = (j-1)*nx + i
            call opr%set(i_vec+i_row_offset, i_vec+i_col_offset             , -2._R_P * invd)
            call opr%set(i_vec+i_row_offset, i_vec+i_col_offset-i_dir_offset,  1._R_P * invd)
            call opr%set(i_vec+i_row_offset, i_vec+i_col_offset+i_dir_offset,  1._R_P * invd)
        enddo
        enddo

    end function operate
end module openpde_f2m_d2_FD_2D
