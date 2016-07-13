!< Abstract class of spatial operator of first derivative.
module openpde_spatial_operator_d1_abstract
    !< Abstract class of spatial operator of first derivative.
    use openpde_spatial_operator_abstract

    implicit none
    private
    public :: spatial_operator_d1

    type, abstract, extends(spatial_operator) :: spatial_operator_d1
        !< Abstract class of spatial operator of first derivative.
    endtype spatial_operator_d1
end module openpde_spatial_operator_d1_abstract
