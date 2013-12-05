from __future__ import division
__author__ = 'Horea Christian'
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
import pandas.rpy.common as com
import numpy as np
base = importr('base')
stats = importr('stats')
from os import remove
from chr_helpers import flatten_list

def lm(data, fixed, random, model='', output='', as_strings='', title='Title for Your Output', label='Label for Your Output', pythontex=True):
    if not output:
	output = 'texreg'
    if not model:
	model = 'lme4'
    
    if model == 'nlme':
	nlme = importr('nlme')
    elif model == 'lme4':
	lme4 = importr('lme4')

    if output == 'stargazer':
	stargazer = importr('stargazer')
    elif output == 'texreg':
	texreg = importr('texreg')
    
    IVs = fixed.split('~')[1]
    if "*" in IVs:
	IVs = [IVs.split("*")]
    if "+" in IVs:
	IVs = [IVs.split('+')]
    if len([i for i in IVs][0]) > 1:
	IVs = list(flatten_list(IVs))
    else:
	IVs = [IVs]
    
    for IV in IVs:
	data[IV] = ':' + data[IV].astype(str) # add colons here so that the output doesn't directly concatanate tthe factor with the name	

    if model == 'lme4':
	formula = fixed + ' + (1|' + random + ')'
	formula = robjects.Formula(formula) #format formula string for R
    elif model == 'nlme':
	fixed = robjects.Formula(fixed)
	random_formula = ""
	for i in random:
	    random_formula += '~1|' + i + " "
	random = robjects.Formula(random_formula)
    
    for colID in as_strings:
	data[colID] = data[colID].astype('S8')
    #~ dfr = com.convert_to_r_dataframe(data, True)  # convert from pandas to R and make string columns factors this (the proper way of coding the line below) seems to have some issues :-/
    dfr = com.convert_to_r_dataframe(data)  # convert from pandas to R and make string columns factors
    
    if model == 'nlme':
	lin_model = nlme.lme(fixed, random, data=dfr, method='ML')
    elif model == 'lme4':
	lin_model = lme4.lmer(formula=formula, data=dfr, REML='false')
    
    if output == 'stargazer':
	latex = np.array(stargazer.stargazer(lin_model, title=title, label=label))
    elif output == 'texreg':
	import os
	import sys
	f = open(os.devnull, 'w')
	sys.stdout = f
	
	if model == 'lme4':
	    texreg.texreg(lin_model, caption=title, label=label, single_row=True, file='lm-temp.tex', **{'include.loglik': False, 'include.aic':False, 'include.bic':False, 'include.deviance':False})
	elif model == 'nlme':
	    texreg.texreg(lin_model, caption=title, label=label, single_row=True, file='lm-temp.tex', **{'include.loglik': False, 'include.aic':False, 'include.bic':False})
	
	sys.stdout = sys.__stdout__
	latex = open('lm-temp.tex').read()
	remove('lm-temp.tex')
    
    #~ print latex
    return latex

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
    #~ dfr = com.convert_to_r_dataframe(data, True)  # convert from pandas to R and make string columns factors this (the proper way of coding the line below) seems to have some issues :-/
    dfr = com.convert_to_r_dataframe(data)  # convert from pandas to R and make string columns factors

    
    lin_models=[]
    for formula in formulae:
	if model == 'nlme':
	    lin_model = nlme.lme(fixed=formula, data=dfr, random=randoms[0], method='ML')
	elif model == 'lme4':
	    lin_model = lme4.lmer(formula=formula, data=dfr, REML='false')
	lin_models.append(lin_model)
    
    if output == 'stargazer':
	latex = np.array(stargazer.stargazer(lin_model, title=title, label=label))
    if pythontex:
	return ''
    else:
	return '\n'.join(np.array(latex))
	
def av(data, formula, model='', output='', as_strings='', title='Title for Your Output', label='Label for Your Output', pythontex=True):
    if not output:
	output = 'xtable'
    if not model:
	model = 'aov'
    
    if output == 'stargazer':
	stargazer = importr('stargazer')
    elif output == 'texreg':
	texreg = importr('texreg')

    formula = robjects.Formula(formula)
    dfr = com.convert_to_r_dataframe(data)  # convert from pandas to R and make string columns factors
	
    if model == 'aov':
	output = 'xtable' #aov only works with xtable
	av_model = stats.aov(formula, data=dfr)
	av_model_sum = base.summary(av_model)
    
    if output == 'xtable':
	xtable = importr('xtable')
	latex = xtable.xtable(av_model_sum, caption=title, label=label)
    if pythontex:
	return latex
    else:
	return '\n'.join(np.array(latex))
	

def tex_nr(raw_number):
    raw_number = float(raw_number)
    number = "{:.1e}".format(raw_number)
    multiplier, exponent = number.split('e')
    if multiplier == "0.0":
	multiplier = int(float(multiplier))
    exponent = int(exponent)
    if exponent:
	latex_expression = '$'+str(multiplier)+'\\times 10^{'+str(exponent)+'}$'
    else:
	latex_expression = '$'+str(multiplier)+'$'
    return latex_expression
    
def p_table(data, interest, reference, intcap='Conditions of interest', refcap='', caption='', label='', width=0.9, mode='rel',means_only=True): #make table with t-test p-values
    if mode == 'rel':
	from scipy.stats import ttest_rel as ttest
    #~ if mode == 'ind':
	#~ from scipy.stats import ttest_ind as ttest
	
    len_compare = len(reference)-1
    len_interest = len(interest)-1
    table_form = '{r|'+'Y|'*len_compare+'}'
    first_line = intcap+'& \\multicolumn{'+str(len_compare)+'}{c}{'+refcap+'}\\\\\n'
    second_line = '&'+'&'.join([str(i) for i in reference[1:]])+'\\\\\n'+'\\cline{2-'+str(len(reference))+'}\n'
    
    end_tabular = '\\end{tabularx}\n'
    caption = '\\caption{'+caption+'}\n'
    label = '\\label{'+label+'}\n'
    footer = '\\end{center}\n \\end{table}'
    
    latex = '\\begin{table}[!htbp]\n \\begin{center}\n \\begin{tabularx}{'+str(width)+'\\textwidth}'+table_form
    latex += first_line
    latex += second_line
    
    for i in interest[1:]:
	line = i
	for r in reference[1:]:
	    cell = '&'+tex_nr(ttest(data[(data[interest[0]]==i)].groupby('ID')['RT'].mean(),data[(data[reference[0]]==r)].groupby('ID')['RT'].mean())[1])
	    line += cell
	line += '\\\\\n'
	latex += line
	    
    
    latex += end_tabular
    latex += caption
    latex += label
    latex += footer
    
    return latex
    
def tex_mr(data, intcap='Conditions of interest', refcap='', caption='', label='', width=0.95, document_fontsize="large"): #make table with t-test p-values
	
    super_fields = "Peak-Level"
    fields = ["","Cluster[px]", "\\large{p}\\footnotesize$\\mathsf{_{FWE-corr}}$", "\\large{q}\\footnotesize$\\mathsf{_{FDR-corr}}$", "$\\mathsf{T}$", "$\\mathsf{X [mm]}$", "$\\mathsf{Y [mm]}$", "$\\mathsf{Z [mm]}$"]
    table_form = '{'+"r"+'Y'*(len(fields[:-3])-1)+"|"+'Y'*len(fields[-3:])+'}'
    first_line = ' & \\multicolumn{'+str(len(fields)-1)+'}{c}{'+super_fields+'}\\\\\n'+'\\cline{3-'+str(len(fields))+'}\n'
    second_line = '&'.join([str(field) for field in fields])+'\\\\\n'
    
    end_tabular = '\\end{tabularx}\n'
    caption = '\\caption{'+caption+'}\n'
    label = '\\label{'+label+'}\n'
    footer = '\\end{center}\n \\end{table}\\'+document_fontsize
    
    latex = '\\begin{table}[!htbp]\n\\footnotesize \\begin{center}\n \\begin{tabularx}{'+str(width)+'\\textwidth}'+table_form
    latex += first_line
    latex += second_line
    
    for row in data:
	line = ""
	if row[0] != "n":
	    latex += '\\hline\n'
	for value in row:
	    if value == "n":
		line += "&"
	    else:
		line += "&"+str(value)
	line += "\\\\\n"
	latex += line[1:]
    
    latex += end_tabular
    latex += caption
    latex += label
    latex += footer
    return latex  

#~ #THIS IS FOR TEST PURPOSES
if __name__ == '__main__':
    tex_mr([["A",19, 0.993, 0.842, 5.29, -24, -79, -8]])
