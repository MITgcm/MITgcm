Parameter sensitivity using the adjoint of MITgcm
-------------------------------------------------

Forward and tangent linear counterparts of MITgcm are supported using an
‘automatic adjoint compiler’. These can be used in parameter sensitivity and
data assimilation studies.

As one example of application of the MITgcm adjoint, :numref:`adj_hf_ocean_figure`
maps the gradient :math:`\frac{\partial J}{\partial\mathcal{H}}` where :math:`J` is the magnitude of the overturning
stream-function shown in :numref:`large-scale-circ2` at
60°N and :math:`\mathcal{H}(\lambda,\varphi)` is the mean, local
air-sea heat flux over a 100 year period. We see that :math:`J` is sensitive
to heat fluxes over the Labrador Sea, one of the important sources of
deep water for the thermohaline circulations. This calculation also
yields sensitivities to all other model parameters.

  .. figure:: figs/adj_hf_ocean.*
    :width: 100%
    :align: center
    :alt: adj_hf_ocean_figure
    :name: adj_hf_ocean_figure

    Sensitivity of meridional overturning strength to surface heat flux changes. Contours show the magnitude of the response (in Sv x 10\ :sup:`-4` \) that a persistent +1 Wm\ :sup:`-2` \ heat flux anomaly at a given grid point would produce.
