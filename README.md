# Notification Opportunity Estimator
Method to generate estimates based on notification opportunity profile.

Function apply.leos.method applies the following method for the estimates:
Notification delay modelling
by Leo Bastos

N_t - number of notified cases at time t
Y_{t,d} - number of notified cases from time t with notification delay d
D - maximum acceptable time delay

N_t = Y_{t,0} + sum_{d=1}^{D} Y_{t,d}

Y_{0,t} is known forall t
If T is today, Y_{t,d} is unknown for all (t,d) such that t+d > T

Package maintainer: Marcelo F C Gomes marfcg<at>gmail<dot>com

Contributors: Claudia T Codeço and Leo Bastos

License: GPL v3.0
