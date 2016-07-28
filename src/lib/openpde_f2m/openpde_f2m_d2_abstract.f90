!< Abstract class of matrix operator of second derivative.
module openpde_f2m_d2_abstract
   !< Abstract class of matrix operator of second derivative.
    use openpde_matrix_abstract
    use openpde_f2m_abstract

    implicit none
    private
    public :: f2m_d2

    type, abstract, extends(f2m) :: f2m_d2
        !< Abstract class of matrix operator of second derivative.
    endtype f2m_d2

end module openpde_f2m_d2_abstract
