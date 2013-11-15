from __future__ import division
__author__ = 'Horea Christian'
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
import numpy as np
base = importr('base')
stats = importr('stats')

def lm(model='',formula='', data='', output='', as_strings='', title='Title for Your Output', label='Label for Your Output', pythontex=True):
    if not output:
	output = 'stargazer'
    if not model:
	model = 'lme4'
    
    if model == 'nlme': #this doesn't really work yet
	nlme = importr('nlme')
    elif model == 'lme4':
	lme4 = importr('lme4')

    if output == 'stargazer':
	stargazer = importr('stargazer')
    elif output == 'texreg':
	texreg = importr('texreg')
		
    if not formula:
	raise ValueError('Please specify a "formula" argument.')
    
    formula = robjects.Formula(formula) #format formula string for R
    
    #~ the following is "nlme"-relevant. not implemented yet
    #~ robjects.Formula(formula + ' + (1|ID)')
    #~ randoms = [
        #~ '~1|ID'
        #~ ]
    #~ for idx, random in enumerate(randoms):
        #~ randoms[idx] = robjects.Formula(random)  #  make a formula    obect


    for colID in as_strings:
	data[colID] = data[colID].astype('S8')
    dfr = com.convert_to_r_dataframe(data, True)  # convert from pandas to R and make string columns factors
    

    if model == 'nlme':
	lin_model = nlme.lme(fixed=formula, data=dfr, random=randoms[0], method='ML')
    elif model == 'lme4':
	lin_model = lme4.lmer(formula=formula, data=dfr, REML='false')
    
    if output == 'stargazer':
	latex = np.array(stargazer.stargazer(lin_model, summary=False, title='lalala', label='fig:sc_aa'))
    if pythontex:
	return ''
    else:
	return '\n'.join(np.array(latex))

def anova_of_lm(model='',formulae='', data='', output='', as_strings='', title='Title for Your Output', label='Label for Your Output'):
    
    if not output:
	output = 'stargazer'
    
    if model == 'nlme': #this doesn't really work yet
	nlme = importr('nlme')
    elif model == 'lme4':
	lme4 = importr('lme4')
    else:
	raise ValueError('Please specify a "model" argument.')
	
    if output == 'stargazer':
	stargazer = importr('stargazer')
    elif output == 'texreg':
	texreg = importr('texreg')
		
    if not formula:
	raise ValueError('Please specify a "formula" argument.')
    
    formulae = [robjects.Formula(formula) for formula in formulae] #format formulae strings for R
    
    #~ the following is "nlme"-relevant. not implemented yet
    #~ robjects.Formula(formula + ' + (1|ID)')
    #~ randoms = [
        #~ '~1|ID'
        #~ ]
    #~ for idx, random in enumerate(randoms):
        #~ randoms[idx] = robjects.Formula(random)  #  make a formula    obect


    for colID in as_strings:
	data[colID] = data[colID].astype('S8')
    dfr = com.convert_to_r_dataframe(mydata, True)  # convert from pandas to R and make string columns factors
    
    lin_models=[]
    for formula in formulae:
	if model == 'nlme':
	    lin_model = nlme.lme(fixed=formula, data=dfr, random=randoms[0], method='ML')
	elif model == 'lme4':
	    lin_model = lme4.lmer(formula=formula, data=dfr, REML='false')
	lin_models.append(lin_model)
    
    if output == 'stargazer':
	latex = np.array(stargazer.stargazer(lin_model, summary=False, title='lalala', label='fig:sc_aa'))
    if pythontex:
	return ''
    else:
	return '\n'.join(np.array(latex))
	
def av(model='',formula='', data='', output='', as_strings='', title='Title for Your Output', label='Label for Your Output', pythontex=True):
    if not output:
	output = 'xtable'
    if not model:
	model = 'aov'
    
    if output == 'stargazer':
	stargazer = importr('stargazer')
    elif output == 'texreg':
	texreg = importr('texreg')
	
    if not formula:
	raise ValueError('Please specify a "formula" argument.')	

    formula = robjects.Formula(formula)
	
    if model == 'aov':
	output = 'xtable' #aov only works with texreg
	av_model = stats.aov(formula, data=data)
	av_model_sum = base.summary(av_model)
    
    if output == 'xtable':
	xtable.xtable(model_sum, caption=title, label=label)
    if pythontex:
	return ''
    else:
	return '\n'.join(np.array(latex))
	

def tex_nr(raw_number):
    raw_number = float(raw_number)
    number = "{:.1e}".format(raw_number)
    multiplier, exponent = number.split('e')
    exponent = int(exponent)
    if exponent:
	latex_expression = '$'+multiplier+'\\times 10^{'+str(exponent)+'}$'
    else:
	latex_expression = '$'+multiplier+'$'
    return latex_expression
    
