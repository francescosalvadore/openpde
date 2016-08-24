!< Concrete class of linsolver using LAPACK with general matrix.
module openpde_multigrid_FD_1D
    !< Concrete class of linsolver using LAPACK with general matrix.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use openpde_multigrid_abstract
    use openpde_field_abstract
    use openpde_field_FD_1D
    use openpde_mesh_FD_1D
    use openpde_kinds

    implicit none
    private
    public :: multigrid_FD_1D

    type, extends(multigrid) :: multigrid_FD_1D
        contains
            ! deferred public methods
            procedure :: init         
            procedure :: create_subgrids         
    endtype multigrid_FD_1D
contains
    subroutine init(this, inp)
        !< Init linsolver
        class(multigrid_FD_1D), intent(inout)   :: this 
        class(field) :: inp

        allocate(this%m(this%n_levels), mold=inp%m)
        this%m(1) = inp%m
        do i_mg=2,this%n_levels
            n_mg = this%m(i_mg-1)%n/2+1
            this%m(i_mg)%n = n_mg
            this%m(i_mg)%ng = this%m(i_mg-1)%ng
        enddo
    end subroutine init

    subroutine create_subgrids(this, inp, subgrids)
        !< Init linsolver
        class(multigrid_FD_1D), intent(inout)   :: this 
        class(field), dimension(:) :: inp
        class(field), dimension(:,:), intent(inout) :: subgrids
        integer(I_P) :: i_mg
        integer(I_P) :: n_mg

        !allocate(subgrids(size(inp), n_levels), mold=inp(1))
        do i_mg=1,this%n_levels
            subgrids(:, i_mg)%m => this%m(i_mg)
        enddo
    end subroutine create_subgrids

end module openpde_multigrid_FD_1D
