"""gaussian.py"""
from scipy.stats import norm
from numpy import linspace
from problog.extern import problog_export

@problog_export('+int', '+float', '+float', '-list', '-list')
def gaussian_probs(k, mean, std):
	values = linspace(mean-2*std, mean+2*std, k)
	probs = norm.pdf(values, loc=mean, scale=std)
	total = sum(probs)
	return [float(value) for value in values], [float(prob/total) for prob in probs]
