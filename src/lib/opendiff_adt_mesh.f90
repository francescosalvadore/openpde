!< Abstract class of mesh.
module opendiff_adt_mesh
    !< Abstract class of mesh.
    use opendiff_kinds

    implicit none
    private
    public :: mesh

    type, abstract :: mesh
        !< Abstract class for *mesh* handling.
        character(128) :: description !< Mesh description.
        contains
            procedure(abstract_meshinit),    deferred :: init   !< Initilize mesh.
            procedure(abstract_meshoutput) , deferred :: output !< Output mesh data.
    endtype mesh

    abstract interface
        function abstract_meshinit(this) result(res)
            import :: I4P, mesh
            class(mesh), intent(inout) :: this
            integer(I4P)               :: res
        end function abstract_meshinit
    endinterface

    abstract interface
        function abstract_meshoutput(this) result(res)
            import :: I4P, mesh
            class(mesh), intent(in) :: this
            integer(I4P)            :: res
        end function abstract_meshoutput
    endinterface
end module opendiff_adt_mesh
