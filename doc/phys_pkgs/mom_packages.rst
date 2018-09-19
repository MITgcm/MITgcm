Momentum Packages
-----------------


.. tabularcolumns:: |\Y{.4}|L|L|


+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| CPP Flag Name                                 | Default | Description                                                                                                          |
+===============================================+=========+======================================================================================================================+
| :varlink:`ALLOW_SMAG_3D`                      | #undef  | allow isotropic 3D Smagorinsky viscosity (:filelink:`MOM_COMMON_OPTIONS.h <pkg/mom_common/MOM_COMMON_OPTIONS.h>`)    |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`ALLOW_3D_VISCAH`                    | #undef  | allow full 3D specification of horizontal Laplacian viscosity                                                        |
|                                               |         | (:filelink:`MOM_COMMON_OPTIONS.h <pkg/mom_common/MOM_COMMON_OPTIONS.h>`)                                             |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`ALLOW_3D_VISCA4`                    | #undef  | allow full 3D specification of horizontal biharmonic viscosity                                                       |
|                                               |         | (:filelink:`MOM_COMMON_OPTIONS.h <pkg/mom_common/MOM_COMMON_OPTIONS.h>`)                                             |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+
| :varlink:`MOM_BOUNDARY_CONSERVE`              | #undef  | conserve :math:`u,v` momentum next to a step (vertical plane) or a coastline edge (horizontal plane)                 |
|                                               |         | (:filelink:`MOM_FLUXFORM_OPTIONS.h <pkg/mom_fluxform/MOM_FLUXFORM_OPTIONS.h>`)                                       |
+-----------------------------------------------+---------+----------------------------------------------------------------------------------------------------------------------+


