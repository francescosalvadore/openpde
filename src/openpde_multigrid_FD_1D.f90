!< Concrete class of linsolver using LAPACK with general matrix.
module openpde_multigrid_FD_1D
    !< Concrete class of linsolver using LAPACK with general matrix.
    use openpde_multigrid_abstract
    use openpde_field_abstract
    use openpde_mesh_FD_1D
    use openpde_kinds

    implicit none
    private
    public :: multigrid_FD_1D

    type, extends(multigrid) :: multigrid_FD_1D
        contains
            ! deferred public methods
            procedure :: create_subgrids_field !< Create subgrid-levels field.
            procedure :: init                  !< Init multigrid solver.
    endtype multigrid_FD_1D
contains

    subroutine create_subgrids_field(this, inp, subgrids)
        !< Create subgrid-levels field.
        class(multigrid_FD_1D), intent(in), target :: this          !< The solver.
        class(field),           intent(in)         :: inp(:)        !< Input field on finest mesh.
        class(field),           intent(inout)      :: subgrids(:,:) !< Subgrid-levels field.
        integer(I_P)                               :: e             !< Counter.
        integer(I_P)                               :: l             !< Counter.

        do l=1, this%levels_number
            do e=1, size(subgrids, dim=1)
               subgrids(e, l)%m => this%meshes(l)
            enddo
        enddo
    end subroutine create_subgrids_field

    subroutine init(this, inp, levels_number)
        !< Init multigrid solver.
        class(multigrid_FD_1D), intent(inout)      :: this          !< The solver.
        class(field),           intent(in), target :: inp           !< Input field on finest mesh.
        integer(I_P),           intent(in)         :: levels_number !< Number of MG levels.
        class(mesh_FD_1D), pointer                 :: mesh_finest   !< Pointer to finest mesh.
        class(mesh_FD_1D), pointer                 :: mesh_mg(:)    !< Pointer to MG meshes.
        integer(I_P)                               :: l             !< Counter.
        integer(I_P)                               :: n_mg          !< Counter.

        allocate(this%meshes(this%levels_number), mold=inp%m)
        mesh_finest => associate_mesh_FD_1D(mesh_input=inp%m, emsg='calling procedure multigrid_FD_1D%init')
        mesh_mg => associate_mesh_FD_1D(mesh_input=this%meshes, emsg='calling procedure multigrid_FD_1D%init')
        mesh_mg(1) = mesh_finest
        do l=2,this%levels_number
            n_mg = mesh_mg(l-1)%n/2+1
            mesh_mg(l)%n = n_mg
            mesh_mg(l)%ng = mesh_mg(l-1)%ng
        enddo
    end subroutine init
end module openpde_multigrid_FD_1D
