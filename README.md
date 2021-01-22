# Notification Opportunity Estimator
Method to generate estimates based on notification opportunity profile.

Function apply.leos.method applies the following method for the estimates:

Notification delay modelling described in
Leonardo S Bastos, Theodoros Economou, Marcelo F C Gomes, Daniel A M Villela, Flavio C Coelho, Oswaldo G Cruz, Oliver Stoner, Trevor Bailey, and Claudia T Codeço. A modelling approach for correcting reporting delays in disease surveillance data. Statistics in Medicine, to appear. [Open access link](https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8303).

An earlier version of the manuscript is available on [arxiv](https://arxiv.org/abs/1709.09150).

The version in this repository only uses temporal and delay effects, without time-deay interaction nor spatial effects.
For the code used in the manuscript, check the dedicated repo [here](https://github.com/lsbastos/Delay).

This method is currently used for arboviruses and severe acute respiratory illness (SARI) surveillance in Brazil, implemented in the corresponding platforms [InfoDengue](http://info.dengue.mat.br) and [InfoGripe](http://info.gripe.fiocruz.br).

N_t - number of notified cases at time t

Y_{t,d} - number of notified cases from time t with notification delay d

D - maximum acceptable time delay

N_t = Y_{t,0} + sum_{d=1}^{D} Y_{t,d}

Y_{0,t} is known forall t

If T is today, Y_{t,d} is unknown for all (t,d) such that t+d > T

Package maintainer: Marcelo F C Gomes marfcg [at] gmail [dot] com

Contributors: Claudia T Codeço and Leo Bastos

License: GPL v3.0
